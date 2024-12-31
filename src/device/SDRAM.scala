package ysyx

import chisel3._
import chisel3.util._
import chisel3.experimental.{Analog, attach}
import chisel3.experimental.SourceInfo
import chisel3.experimental.UnlocatableSourceInfo
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters

class SDRAMIO extends Bundle {
  val clk = Output(Bool())
  val cke = Output(Bool())
  val cs = Output(Bool())
  val ras = Output(Bool())
  val cas = Output(Bool())
  val we = Output(Bool())
  val a = Output(UInt(13.W))
  val ba = Output(UInt(2.W))
  val dqm = Output(UInt(2.W))
  val dq = Analog(16.W)
}

class sdram_top_axi extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val in = Flipped(
      new AXI4Bundle(
        AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 4)
      )
    )
    val sdram = new SDRAMIO
  })
}

class sdram_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val in =
      Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val sdram = new SDRAMIO
  })
}

class sdram extends BlackBox {
  val io = IO(Flipped(new SDRAMIO))
}

class AXI4SDRAM(address: Seq[AddressSet])(implicit p: Parameters)
    extends LazyModule {
  val beatBytes = 4
  val node = AXI4SlaveNode(
    Seq(
      AXI4SlavePortParameters(
        Seq(
          AXI4SlaveParameters(
            address = address,
            executable = true,
            supportsWrite = TransferSizes(1, beatBytes),
            supportsRead = TransferSizes(1, beatBytes),
            interleavedId = Some(0)
          )
        ),
        beatBytes = beatBytes
      )
    )
  )

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (in, _) = node.in(0)
    val sdram_bundle = IO(new SDRAMIO)

    val msdram = Module(new sdram_top_axi)
    msdram.io.clock := clock
    msdram.io.reset := reset.asBool
    msdram.io.in <> in
    sdram_bundle <> msdram.io.sdram
  }
}

class APBSDRAM(address: Seq[AddressSet])(implicit p: Parameters)
    extends LazyModule {
  val node = APBSlaveNode(
    Seq(
      APBSlavePortParameters(
        Seq(
          APBSlaveParameters(
            address = address,
            executable = true,
            supportsRead = true,
            supportsWrite = true
          )
        ),
        beatBytes = 4
      )
    )
  )

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (in, _) = node.in(0)
    val sdram_bundle = IO(new SDRAMIO)

    val msdram = Module(new sdram_top_apb)
    msdram.io.clock := clock
    msdram.io.reset := reset.asBool
    msdram.io.in <> in
    sdram_bundle <> msdram.io.sdram
  }
}

class MemoryBank extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(24.W))
    val writeData = Input(UInt(16.W))
    val writeEnable = Input(Bool())
    val readData = Output(UInt(16.W))
  })

  val mem = Mem(8192 * 512, UInt(16.W))
  when(io.writeEnable) {
    mem.write(io.addr, io.writeData)
  }
  io.readData := mem.read(io.addr)
}

class sdramChisel extends Module {
  val io = IO(Flipped(new SDRAMIO))

  // Memory array - 4 banks, each 8192 rows x 512 columns x 16 bits
  val banks = 4
  val rows = 8192
  val cols = 512

  // Memory array
  val mem = Mem(8192 * 512 * 4, UInt(16.W))

  // Row buffers for each bank
  val rowBuffers = RegInit(VecInit(Seq.fill(banks)(0.U(16.W))))

  // Active rows for each bank (-1 means no active row)
  val activeRows = RegInit(VecInit(Seq.fill(banks)("hFFF".U(13.W))))

  // Mode register
  val modeReg = RegInit(0.U(13.W))

  // Data I/O
  val dout = RegInit(0.U(16.W))
  val dout_en = RegInit(false.B)

  // Create a wire for the bidirectional data bus
  val data_wire = Wire(Analog(16.W))
  data_wire := DontCare

  // Connect the bidirectional data bus
  attach(io.dq, data_wire)

  // Create output register for reading
  val out_reg = RegInit(0.U(16.W))

  // When output is enabled, drive the bus with dout
  when(dout_en) {
    out_reg := dout
  }

  // Read data from the bus
  val din = out_reg

  // Command decoder
  val isCommand = !io.cs && io.cke
  val cmd = Cat(io.ras, io.cas, io.we)

  // Command constants
  val CMD_NOP = "b111".U
  val CMD_ACTIVE = "b011".U
  val CMD_READ = "b101".U
  val CMD_WRITE = "b100".U
  val CMD_LOADMODE = "b000".U

  // CAS latency counter
  val casCounter = RegInit(0.U(3.W))
  val pendingRead = RegInit(false.B)
  val pendingAddr = RegInit(0.U(24.W))
  val pendingBank = RegInit(0.U(2.W))

  // Get CAS latency and burst length from mode register
  val casLatency = (modeReg >> 4)(2, 0)
  val burstLength = (1.U << modeReg(2, 0))

  // Burst counter
  val burstCounter = RegInit(0.U(4.W))
  val burstActive = RegInit(false.B)

  when(isCommand) {
    switch(cmd) {
      is(CMD_ACTIVE) {
        val bank = io.ba
        val row = io.a
        activeRows(bank) := row
        pendingAddr := Cat(bank, row, 0.U(9.W))
      }

      is(CMD_READ) {
        val bank = io.ba
        val col = io.a(8, 0)
        when(activeRows(bank) =/= "hFFF".U) {
          pendingRead := true.B
          pendingAddr := Cat(bank, activeRows(bank), col)
          pendingBank := bank
          casCounter := casLatency
          burstCounter := 0.U
          burstActive := true.B
        }
      }

      is(CMD_WRITE) {
        val bank = io.ba
        val col = io.a(8, 0)
        when(activeRows(bank) =/= "hFFF".U) {
          when(!io.dqm(0)) {
            val addr = Cat(bank, activeRows(bank), col)
            mem.write(addr, din)
          }
          burstCounter := 0.U
          burstActive := true.B
        }
      }

      is(CMD_LOADMODE) {
        modeReg := io.a
      }
    }
  }

  // Handle read operation
  when(pendingRead && casCounter === 0.U) {
    val readAddr = pendingAddr + burstCounter
    dout := mem.read(readAddr)
    dout_en := true.B
    when(burstCounter === (burstLength - 1.U)) {
      pendingRead := false.B
      burstActive := false.B
    }.otherwise {
      burstCounter := burstCounter + 1.U
    }
  }.elsewhen(pendingRead) {
    casCounter := casCounter - 1.U
  }.otherwise {
    dout_en := false.B
  }

  // Handle write burst
  when(burstActive && cmd === CMD_WRITE) {
    val bank = io.ba
    val baseAddr = Cat(bank, activeRows(bank), io.a(8, 0))
    val addr = baseAddr + burstCounter
    when(!io.dqm(0)) {
      mem.write(addr, din)
    }
    when(burstCounter === (burstLength - 1.U)) {
      burstActive := false.B
    }.otherwise {
      burstCounter := burstCounter + 1.U
    }
  }
}

// TriState buffer for bidirectional data bus
class TriStateBuffer extends Module {
  val io = IO(new Bundle {
    val in = Input(Bool())
    val oe = Input(Bool())
    val out = Output(Bool())
    val io = Analog(1.W)
  })

  // Create a single analog wire for the bidirectional connection
  val data_wire = Wire(Analog(1.W))
  data_wire := DontCare

  // Always attach the IO pin to our data wire
  attach(io.io, data_wire)

  // Create a wire for output sampling
  val out_wire = WireDefault(false.B)
  io.out := out_wire

  // Sample the input
  out_wire := io.in
}
