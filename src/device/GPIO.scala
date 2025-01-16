package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class GPIOIO extends Bundle {
  val out = Output(UInt(16.W))
  val in = Input(UInt(16.W))
  val seg = Output(Vec(8, UInt(8.W)))
}

class GPIOCtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Reset())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val gpio = new GPIOIO
}

// BlackBox，用于替换为外部 Verilog 模块时的占位
class gpio_top_apb extends BlackBox {
  val io = IO(new GPIOCtrlIO)
}

// Chisel 版本的 GPIO 模块
class GPIOChisel extends Module {
  val io = IO(new GPIOCtrlIO)

  // 简单的 4 位 BCD 转 7 段数码管译码器
  class SevenSegDecoder extends Module {
    val io = IO(new Bundle {
      val in  = Input(UInt(4.W))   // BCD 输入
      val out = Output(UInt(7.W))  // 7 段输出（a~g）
    })

    // 先用寄存器把每段的亮灭关系存起来，1 表示亮、0 表示灭（或反之）
    val segReg = RegInit(VecInit(Seq.fill(7)(false.B)))

    // 对每个位进行匹配
    segReg(6) := (io.in === 0.U) || (io.in === 2.U) || (io.in === 3.U) ||
                 (io.in === 5.U) || (io.in === 6.U) || (io.in === 7.U) ||
                 (io.in === 8.U) || (io.in === 9.U)

    segReg(5) := (io.in === 0.U) || (io.in === 1.U) || (io.in === 2.U) ||
                 (io.in === 3.U) || (io.in === 4.U) || (io.in === 7.U) ||
                 (io.in === 8.U) || (io.in === 9.U)

    segReg(4) := (io.in === 0.U) || (io.in === 1.U) || (io.in === 3.U) ||
                 (io.in === 4.U) || (io.in === 5.U) || (io.in === 6.U) ||
                 (io.in === 7.U) || (io.in === 8.U) || (io.in === 9.U)

    segReg(3) := (io.in === 0.U) || (io.in === 2.U) || (io.in === 3.U) ||
                 (io.in === 5.U) || (io.in === 6.U) || (io.in === 8.U) ||
                 (io.in === 9.U)

    segReg(2) := (io.in === 0.U) || (io.in === 2.U) || (io.in === 6.U) ||
                 (io.in === 8.U)

    segReg(1) := (io.in === 0.U) || (io.in === 4.U) || (io.in === 5.U) ||
                 (io.in === 6.U) || (io.in === 8.U) || (io.in === 9.U)

    segReg(0) := (io.in === 2.U) || (io.in === 3.U) || (io.in === 4.U) ||
                 (io.in === 5.U) || (io.in === 6.U) || (io.in === 8.U) ||
                 (io.in === 9.U)

    // 由于上面设定的是 “true” 表示亮，外部硬件也许需要低电平有效，此处做一次取反
    io.out := ~segReg.asUInt
  }

  // 分别存储 LED、SW 和 数码管 寄存器值
  val ledReg = RegInit("hFFFF".U(16.W))
  val swReg  = RegInit("hFFFF".U(16.W))
  val segReg = RegInit("hFFFFFFFF".U(32.W))

  // 根据 APB 的地址，将地址空间划分
  val ledAddress = io.in.paddr(3, 2) === "b00".U
  val swAddress  = io.in.paddr(3, 2) === "b01".U
  val segAddress = io.in.paddr(3, 2) === "b10".U

  // 用于读回寄存器数据
  val ledReadData = 0.U(16.W) ## ledReg
  val swReadData  = 0.U(16.W) ## swReg
  val segReadData = segReg

  // fire 用于判断是否是正确的周期（psel && penable && pready）
  val validTrans  = io.in.psel && io.in.penable && io.in.pready

  // 将输入的开关值实时存到 swReg，LED 输出给外部
  swReg       := io.gpio.in
  io.gpio.out := ledReg

  // APB 信号指定
  io.in.pready  := true.B
  io.in.pslverr := false.B

  // 读数据多路选择
  io.in.prdata := MuxCase(
    0.U,
    Seq(
      (!io.in.pwrite && validTrans && ledAddress) -> ledReadData,
      (!io.in.pwrite && validTrans && swAddress)  -> swReadData,
      (!io.in.pwrite && validTrans && segAddress) -> segReadData
    )
  )

  // 写掩码 wmask，用于字节写
  val wmask   = Fill(8, io.in.pstrb(3)) ##
                Fill(8, io.in.pstrb(2)) ##
                Fill(8, io.in.pstrb(1)) ##
                Fill(8, io.in.pstrb(0))

  // 将写入的数据与原有寄存器值合并
  val newSeg = (io.in.pwdata & wmask) | (segReg & ~wmask)
  val newLed = ((io.in.pwdata & "hFFFF".U)(15, 0) & wmask(15, 0)) | (ledReg & ~wmask(15, 0))

  // 写寄存器：如果写操作且地址匹配，则更新寄存器
  ledReg := Mux(io.in.pwrite && validTrans && ledAddress, newLed, ledReg)
  segReg := Mux(io.in.pwrite && validTrans && segAddress, newSeg, segReg)

  // 将 32 位 segReg 划分为 8 组，每组 4 位 BCD，交给子模块进行译码
  for (i <- 0 until 8) {
    val decoder = Module(new SevenSegDecoder)
    decoder.io.in     := segReg(i * 4 + 3, i * 4)
    // 数码管输出 7 段加 1 位小数点或使能位等（若不需要，可以忽略或固定）
    io.gpio.seg(i)    := decoder.io.out ## 1.U(1.W)
  }
}

// 利用 Diplomacy 封装为 LazyModule
class APBGPIO(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
  val node = APBSlaveNode(
    Seq(
      APBSlavePortParameters(
        slaves = Seq(APBSlaveParameters(
          address       = address,
          supportsRead  = true,
          supportsWrite = true,
          executable    = true
        )),
        beatBytes = 4
      )
    )
  )

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (in, _)     = node.in(0)
    val gpioBundle  = IO(new GPIOIO)

    // 实例化自定义的 GPIOChisel 模块
    val gpioChisel = Module(new GPIOChisel)
    gpioChisel.io.clock := clock
    gpioChisel.io.reset := reset
    gpioChisel.io.in    <> in
    gpioBundle          <> gpioChisel.io.gpio
  }
}