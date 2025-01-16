package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

/** PS/2 顶层 IO 定义 */
class PS2IO extends Bundle {
  val clk  = Input(Bool()) // PS2 时钟输入
  val data = Input(Bool()) // PS2 数据输入
}

/** PS/2 + APB 接口定义 */
class PS2CtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val in    = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val ps2   = new PS2IO
}

/** BlackBox: 外部顶层 Verilog 实现的占位（如需要替换外部实现时使用） */
class ps2_top_apb extends BlackBox {
  val io = IO(new PS2CtrlIO)
}

/**
  * PS2 协议 Chisel 实现
  * 本模块逻辑：
  * 1. 检测 PS2 时钟的下降沿（内部用寄存器同步信号），计数 10bit。
  * 2. 将 8bit 数据放入 FIFO，如果未读就继续存储。
  * 3. APB 侧读取 FIFO 顶部字节（一次读出一个）。
  */
class PS2Chisel extends Module {
  val io = IO(new PS2CtrlIO)

  // ============== 寄存器 & 线网声明区域 ==============
  // 将 PS2 时钟输入打两拍用于检测边沿
  val ps2ClkSync = Reg(UInt(2.W))
  ps2ClkSync := ps2ClkSync(0) ## io.ps2.clk

  // 检测是否在当前时钟周期内出现 ps2Clk 的下降沿
  val sampling: Bool = ps2ClkSync(1) && ~ps2ClkSync(0)

  // 用来计数传输的 bit 数（PS2 一次 10bit：起始位 + 8bit 数据 + 校验/停止位）
  val bitCounter = RegInit(0.U(10.W))
  val isDone     = (bitCounter === 10.U)

  // 用来存放当前正被接收的 10bit 数据
  val shiftReg   = Reg(UInt(10.W))

  // 简易 FIFO, 大小为 16，使用 head / tail 实现环形队列
  val fifoDepth = 16
  val fifoMem   = Reg(Vec(fifoDepth, UInt(8.W)))
  val head      = RegInit(0.U(log2Ceil(fifoDepth).W)) // 读指针
  val tail      = RegInit(0.U(log2Ceil(fifoDepth).W)) // 写指针

  // 记录 FIFO 里是否有数据可读的“标记信号”
  val dataReady = RegInit(false.B)

  // ============== PS2 信号与数据采集逻辑 ==============
  // bitCounter 逻辑：若检测到下降沿，则对 bitCounter 进行加 1，直到到达 10bit 完成一次采集
  when(sampling) {
    bitCounter := Mux(isDone, 0.U, bitCounter + 1.U)
  }

  // shiftReg 用于在下降沿处，移位保存 PS2 的 data 信号
  when(sampling) {
    shiftReg := io.ps2.data ## shiftReg(9, 1)
  }

  // 如果正好完成一帧 10bit，则将提取到的 8bit 数据写入 FIFO
  when(sampling && isDone) {
    fifoMem(tail) := shiftReg(8, 1) // 只取中间 8bit
    tail          := tail + 1.U
  }

  // dataReady 判断 FIFO 是否有数据：在成功写入数据后拉高，在 APB 读出最后一条数据后拉低
  when(sampling && isDone) {
    // 有新数据写入，置 true
    dataReady := true.B
  }

  // ============== APB 侧读逻辑 ==============
  // fire 用于指示本周期是有效的读操作
  val fire = WireDefault(false.B)
  fire := io.in.pready && !io.in.pwrite && io.in.psel && io.in.penable

  // 当 APB 侧执行一次读操作时，如果本次读完后 FIFO 中没有剩余数据，则拉低 dataReady
  when(fire && dataReady) {
    // 读指针前移
    head := head + 1.U
    // 如果读指针追上写指针，则说明 FIFO 读空
    when((head + 1.U) === tail) {
      dataReady := false.B
    }
  }

  // ============== APB 接口输出 ==============
  io.in.pready  := true.B
  io.in.pslverr := false.B
  // 如果 FIFO 有数据，就返回当前 head 指向的数据，否则返回 0
  io.in.prdata  := Mux(dataReady, fifoMem(head), 0.U)

}

/**
  * 通过 Diplomacy 将上面的 PS2Chisel 封装为 APB 外设
  */
class APBKeyboard(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val (in, _)   = node.in(0)
    val ps2Bundle = IO(new PS2IO)

    // 实例化我们重构的 PS2Chisel 模块
    val ps2Module = Module(new PS2Chisel)
    ps2Module.io.clock := clock
    ps2Module.io.reset := reset
    ps2Module.io.in    <> in
    ps2Bundle          <> ps2Module.io.ps2
  }
}