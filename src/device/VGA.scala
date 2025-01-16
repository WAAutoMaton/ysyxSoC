package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

/** VGA 输出信号的封装 */
class VGAIO extends Bundle {
  val r     = Output(UInt(8.W))   // 红色通道
  val g     = Output(UInt(8.W))   // 绿色通道
  val b     = Output(UInt(8.W))   // 蓝色通道
  val hsync = Output(Bool())      // 行同步信号
  val vsync = Output(Bool())      // 帧同步信号
  val valid = Output(Bool())      // 当前像素是否有效
}

/** VGA 模块与 APB 总线交互的 IO 定义 */
class VGACtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val in    = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val vga   = new VGAIO
}

/** 提供给外部替换（BlackBox）的顶层占位模块 */
class vga_top_apb extends BlackBox {
  val io = IO(new VGACtrlIO)
}

/**
  * Chisel 版 VGA 模块，带 APB 写帧缓冲功能。
  *
  * - 640 x 480 显示窗口
  * - 800 x 525 完整时序（含前后廊）
  * - 使用 SyncReadMem 存储像素数据，每个像素使用 4 个字节 (RGBA 或者其它用途)
  */
class vgaChisel extends Module {
  val io = IO(new VGACtrlIO)

  // ===============================
  // 1) 帧缓冲存储 (帧缓存)
  // ===============================
  // 存储深度：2^19 = 524288，可容纳 640*480 = 307200 像素，且每像素 32bit = 4 字节
  // 因此绰绰有余。
  val vgaMemSize = 1 << 19
  val vgaMem     = SyncReadMem(vgaMemSize, Vec(4, UInt(8.W)))

  // ===============================
  // 2) VGA 行、场时序参数
  // ===============================
  // 640 x 480 显示区示例 (标准 VGA 60Hz):
  val hFrontPorch = 96   // 行同步脉冲宽度
  val hActive     = 144  // 行显示有效区起始 (前廊+同步)
  val hBackPorch  = 784  // 行显示有效区结束 (前廊+同步+显示区)
  val hTotal      = 800  // 整行周期

  val vFrontPorch = 2    // 帧同步脉冲宽度
  val vActive     = 35   // 帧显示有效区起始 (前廊+同步)
  val vBackPorch  = 515  // 帧显示有效区结束 (前廊+同步+显示区)
  val vTotal      = 525  // 整帧周期

  // ===============================
  // 3) 行、列扫描计数器
  // ===============================
  val hCounter = RegInit(1.U(10.W)) // 水平像素计数
  val vCounter = RegInit(1.U(10.W)) // 垂直像素计数

  // 每个周期+1，到达尾部后归 1
  hCounter := Mux(hCounter === hTotal.U, 1.U, hCounter + 1.U)
  vCounter := Mux(
    (vCounter === vTotal.U) && (hCounter === hTotal.U),
    1.U,
    Mux(hCounter === hTotal.U, vCounter + 1.U, vCounter)
  )

  // ===============================
  // 4) 行/场同步 & 有效显示区判断
  // ===============================
  val hValid = (hCounter > hActive.U) && (hCounter <= hBackPorch.U)
  val vValid = (vCounter > vActive.U) && (vCounter <= vBackPorch.U)

  // 对于地址，只有在有效区才进行映射，否则为 0
  val hAddr = Mux(hValid, hCounter - (hActive + 1).U, 0.U) // 使 hAddr 从 0~639
  val vAddr = Mux(vValid, vCounter - (vActive + 1).U, 0.U) // 使 vAddr 从 0~479

  // 计算像素在帧缓存中的地址: vAddr * 640 + hAddr
  val pixelAddr = vAddr * 640.U + hAddr

  // ===============================
  // 5) 同步读帧缓冲，得到像素数据
  // ===============================
  // color(0)=B, color(1)=G, color(2)=R, color(3)=A(或保留)
  val pixelData = vgaMem.read(pixelAddr, io.vga.valid)

  // ===============================
  // 6) 行/帧同步信号，和有效显示标志
  // ===============================
  // 同步信号一般在对应计数器小于“前廊”结束值时为低
  val hsyncReg = Reg(Bool())
  val vsyncReg = Reg(Bool())
  val validReg = Reg(Bool())

  // 当 hCounter <= hFrontPorch 时，HSYNC 为低；否则为高 (VGA 标准)
  hsyncReg := (hCounter > hFrontPorch.U)
  // 当 vCounter <= vFrontPorch 时，VSYNC 为低；否则为高
  vsyncReg := (vCounter > vFrontPorch.U)
  // 当前像素是否在真正的可见范围
  validReg := hValid && vValid

  // 接口输出
  io.vga.hsync := hsyncReg
  io.vga.vsync := vsyncReg
  io.vga.valid := validReg
  io.vga.r     := pixelData(2) // R
  io.vga.g     := pixelData(1) // G
  io.vga.b     := pixelData(0) // B

  // ===============================
  // 7) APB 写操作 (将数据写入到帧缓存)
  // ===============================
  // paddr(21, 2) => 用于选择要写的地址（32-bit 对齐）
  // pwdata 拆成 4 个字节, pstrb(i) = 1 时表示写该字节
  val writeAddr = io.in.paddr(21, 2)
  val writeData = Wire(Vec(4, UInt(8.W)))
  val writeMask = Wire(Vec(4, Bool()))

  // 按字节拆分并与写使能 pstrb 拼接
  for (i <- 0 until 4) {
    writeData(i) := io.in.pwdata(8*i + 7, 8*i)  // 第 i 个字节
    writeMask(i) := io.in.pstrb(i) && io.in.pwrite && io.in.psel && io.in.penable
  }

  // 执行写操作
  vgaMem.write(writeAddr, writeData, writeMask)

  // ===============================
  // 8) APB 接口的响应信号
  // ===============================
  io.in.pready  := true.B
  io.in.pslverr := false.B
  // 当前设计只支持写操作，读操作暂返回 0
  io.in.prdata  := 0.U
}

/**
  * 通过 Diplomacy 将 vgaChisel 封装为 APB 外设
  */
class APBVGA(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val (in, _)    = node.in(0)
    val vga_bundle = IO(new VGAIO)

    // 实例化自定义的 vgaChisel 模块
    val vgaModule = Module(new vgaChisel)
    vgaModule.io.clock := clock
    vgaModule.io.reset := reset
    vgaModule.io.in    <> in
    vga_bundle         <> vgaModule.io.vga
  }
}