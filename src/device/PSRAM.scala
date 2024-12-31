package ysyx

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class QSPIIO extends Bundle {
  val sck = Output(Bool())
  val ce_n = Output(Bool())
  val dio = Analog(4.W)
}

class psram_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val in =
      Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val qspi = new QSPIIO
  })
}

class psram extends BlackBox {
  val io = IO(Flipped(new QSPIIO))
}

class psramChisel extends RawModule {
  val io = IO(Flipped(new QSPIIO))

  // Create an async reset for registers
  val asyncReset = Wire(AsyncReset())
  asyncReset := false.B.asAsyncReset

  // Create a clock wire from sck
  val clock = Wire(Clock())
  clock := io.sck.asClock

  // Memory array - 4MB organized as 4M x 8bit
  val mem = Mem(4 * 1024 * 1024, UInt(8.W))

  // FSM states
  val sIDLE :: sCMD :: sADDR :: sDATA :: Nil = Enum(4)

  // All sequential logic wrapped in withClockAndReset
  withClockAndReset(clock, asyncReset) {
    // QSPI interface signals
    val dout = RegInit(0.U(4.W))
    val dout_en = RegInit(false.B)
    val din = TriStateInBuf(io.dio, dout, dout_en)

    // State and control registers
    val state = RegInit(sIDLE)
    val cmd = RegInit(0.U(8.W))
    val addr = RegInit(0.U(24.W))
    val bit_count = RegInit(0.U(6.W))

    // Clock edge detection
    val sck_prev = RegInit(false.B)
    sck_prev := io.sck
    val sck_posedge = io.sck && !sck_prev
    val sck_negedge = !io.sck && sck_prev

    // State updates on clock edges
    when(!io.ce_n) {
      switch(state) {
        is(sIDLE) {
          when(sck_posedge) {
            state := sCMD
            bit_count := 0.U
            dout_en := false.B
          }
        }

        is(sCMD) {
          when(sck_posedge) {
            cmd := Cat(cmd(6, 0), din(0))
            bit_count := bit_count + 1.U
            when(bit_count === 7.U) {
              state := sADDR
              bit_count := 0.U
            }
          }
        }

        is(sADDR) {
          when(sck_posedge) {
            addr := Cat(addr(19, 0), din)
            bit_count := bit_count + 1.U
            when(bit_count === 5.U) {
              state := sDATA
              bit_count := 0.U
              when(cmd === 0xeb.U) {
                dout_en := true.B
              }
            }
          }
        }

        is(sDATA) {
          when(cmd === 0xeb.U) { // Quad IO Read
            when(sck_negedge) {
              val data = mem.read(addr)
              dout := Mux(bit_count(0), data(3, 0), data(7, 4))
              when(bit_count(0)) {
                addr := addr + 1.U
              }
              bit_count := bit_count + 1.U
            }
          }.elsewhen(cmd === 0x38.U) { // Quad IO Write
            when(sck_posedge) {
              when(bit_count(0) === 0.U) {
                mem.write(addr, Cat(din, mem.read(addr)(3, 0)))
              }.otherwise {
                mem.write(addr, Cat(mem.read(addr)(7, 4), din))
                addr := addr + 1.U
              }
              bit_count := bit_count + 1.U
            }
          }
        }
      }
    }.otherwise {
      state := sIDLE
      dout_en := false.B
    }
  }
}

class APBPSRAM(address: Seq[AddressSet])(implicit p: Parameters)
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
    val qspi_bundle = IO(new QSPIIO)

    val mpsram = Module(new psram_top_apb)
    mpsram.io.clock := clock
    mpsram.io.reset := reset
    mpsram.io.in <> in
    qspi_bundle <> mpsram.io.qspi
  }
}
