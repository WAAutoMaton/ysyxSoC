package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class Print extends BlackBox{
  val io = IO(new Bundle{
    val enable = Input(Bool())
    val data   = Input(UInt(32.W))
  })
}

class SPIIO(val ssWidth: Int = 8) extends Bundle {
  val sck = Output(Bool())
  val ss = Output(UInt(ssWidth.W))
  val mosi = Output(Bool())
  val miso = Input(Bool())
}

class spi_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val in =
      Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val spi = new SPIIO
    val spi_irq_out = Output(Bool())
  })
}

class flash extends BlackBox {
  val io = IO(Flipped(new SPIIO(1)))
}

class APBSPI(address: Seq[AddressSet])(implicit p: Parameters)
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
    val spi_bundle = IO(new SPIIO)

    val mspi = Module(new spi_top_apb)
    mspi.io.clock := clock
    mspi.io.reset := reset
    mspi.io.in <> in

    // 为 APB 信号提供默认值
    mspi.io.in.psel := false.B
    mspi.io.in.penable := false.B
    mspi.io.in.pwrite := false.B
    mspi.io.in.paddr := 0.U
    mspi.io.in.pprot := 0.U
    mspi.io.in.pwdata := 0.U
    mspi.io.in.pstrb := 0.U

    // XIP 相关状态定义
    val sXipIdle :: sXipTx0 :: sXipTx1 :: sXipDiv :: sXipCtrl :: sXipSs :: sXipStart :: sXipWait :: sXipRead :: sXipReady:: Nil = Enum(10)
    val xipState = RegInit(sXipIdle)

    // XIP 模式寄存器和信号
    val isXipMode = WireDefault(false.B)
    val xipAddr = Reg(UInt(24.W))
    val xipData = Reg(UInt(32.W))
    val xipError = RegInit(false.B)

    val enable = RegInit(false.B)

    enable := enable || in.penable

    // 检测是否为 XIP 地址范围 (0x3000_0000 ~ 0x3fff_ffff)
    isXipMode := (in.paddr >= "h30000000".U && in.paddr < "h40000000".U)

    assert(!isXipMode || !in.pwrite, "FLASH: Write operation is not supported")

    /*
    val state_prev= RegNext(xipState)
    val printer = Module(new Print())
    printer.io.enable := (xipState===sXipTx0 && state_prev=/=sXipTx0)
    printer.io.data := in.paddr*/

    xipState := Mux(isXipMode, 
        MuxLookup(xipState, sXipIdle)( List(
        sXipIdle -> Mux(in.penable, sXipTx0, sXipIdle),
        sXipTx0 -> Mux(mspi.io.in.pready, sXipTx1, sXipTx0),
        sXipTx1 -> Mux(mspi.io.in.pready, sXipDiv, sXipTx1),
        sXipDiv -> Mux(mspi.io.in.pready, sXipCtrl, sXipDiv),
        sXipCtrl -> Mux(mspi.io.in.pready, sXipSs, sXipCtrl),
        sXipSs -> Mux(mspi.io.in.pready, sXipStart, sXipSs),
        sXipStart -> Mux(mspi.io.in.pready, sXipWait, sXipStart),
        sXipWait -> Mux(mspi.io.in.pready & ~mspi.io.in.prdata(8), sXipRead, sXipWait),
        sXipRead -> Mux(mspi.io.in.pready, sXipReady, sXipRead),
        sXipReady -> sXipIdle
        )), 
    sXipIdle)

    mspi.io.in.paddr := Mux(isXipMode,
        MuxLookup(xipState, 0.U)(Seq(
        sXipTx0 -> "h10001000".U,
        sXipTx1 -> "h10001004".U,
        sXipDiv -> "h10001014".U,
        sXipCtrl -> "h10001010".U,
        sXipSs -> "h10001018".U,
        sXipStart -> "h10001010".U,
        sXipWait -> "h10001010".U,
        sXipRead -> "h10001000".U,
        )),  
      0.U)

    mspi.io.in.pwdata := Mux(isXipMode,
        MuxLookup(xipState, 0.U)(Seq(
        sXipTx0 -> 0.U,
        sXipTx1 -> (((3.U)<<24) + (in.paddr & "h00ffffff".U)),
        sXipDiv -> 8.U,
        sXipCtrl -> "b10010001000000".U,
        sXipSs -> 1.U,
        sXipStart -> "b10010101000000".U,
        )),
      0.U)

    mspi.io.in.psel := in.psel && (xipState =/= sXipIdle) && isXipMode
    mspi.io.in.penable := in.penable && (xipState =/= sXipIdle) && isXipMode
    mspi.io.in.pwrite := xipState =/= sXipIdle && xipState =/= sXipRead && isXipMode
    mspi.io.in.pstrb := Mux(mspi.io.in.pwrite, "b1111".U, 0.U)

    in.pready := xipState === sXipReady && isXipMode
    val data = mspi.io.in.prdata
    in.prdata := Cat(Cat(data(7,0), data(15,8)), Cat(data(23,16), data(31,24)))

    // 连接 SPI 接口
    spi_bundle <> mspi.io.spi
  }
}
