package ysyx

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.subsystem._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

object CPUAXI4BundleParameters {
  def apply() = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = ChipLinkParam.idBits)
}

class TopLevel extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val io_interrupt = Input(Bool())
    val io_master = AXI4Bundle(CPUAXI4BundleParameters())
    val io_slave = Flipped(AXI4Bundle(CPUAXI4BundleParameters()))
    val io_test_pc = Output(UInt(32.W))
    val io_test_regs = Output(Vec(32, UInt(32.W)))
    val io_test_csr = Output(Vec(4, UInt(32.W)))
    val io_test_imem_en = Output(Bool())
  })
}

class CPU(idBits: Int)(implicit p: Parameters) extends LazyModule {
  val masterNode = AXI4MasterNode(p(ExtIn).map(params =>
    AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = "cpu",
        id   = IdRange(0, 1 << idBits))))).toSeq)
  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (master, _) = masterNode.out(0)
    val interrupt = IO(Input(Bool()))
    val slave = IO(Flipped(AXI4Bundle(CPUAXI4BundleParameters())))
    val io = IO(new Bundle {
      val test_pc = Output(UInt(32.W))
      val test_regs = Output(Vec(32, UInt(32.W)))
      val test_csr = Output(Vec(4, UInt(32.W)))
      val test_imem_en = Output(Bool())
    })

    val cpu = Module(new TopLevel)
    cpu.io.clock := clock
    cpu.io.reset := reset
    cpu.io.io_interrupt := interrupt
    cpu.io.io_slave <> slave
    master <> cpu.io.io_master
    io.test_pc := cpu.io.io_test_pc
    io.test_regs := cpu.io.io_test_regs
    io.test_csr := cpu.io.io_test_csr
    io.test_imem_en := cpu.io.io_test_imem_en
  }
}
