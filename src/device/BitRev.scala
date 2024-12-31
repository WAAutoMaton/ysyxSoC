package ysyx

import chisel3._
import chisel3.util._

class bitrev extends BlackBox {
  val io = IO(Flipped(new SPIIO(1)))
}

class bitrevChisel extends RawModule {
  val io = IO(Flipped(new SPIIO(1)))

  // 状态定义 - 移到最前面
  val sIdle :: sReceive :: sSend :: Nil = Enum(3)

  // Create an explicit clock domain using SCK and SS as async reset
  val clockDomain = withClockAndReset(io.sck.asClock, io.ss(0).asAsyncReset) {
    // Create a register for MISO output
    val miso_reg = RegInit(true.B)
    val state = RegInit(sIdle)
    val recvShiftReg = RegInit(0.U(8.W))
    val sendShiftReg = RegInit(0.U(8.W))
    val bitCount = RegInit(0.U(3.W))

    // Return the registers we need to access outside the clock domain
    (miso_reg, state, recvShiftReg, sendShiftReg, bitCount)
  }

  // Unpack the registers
  val (miso_reg, state, recvShiftReg, sendShiftReg, bitCount) = clockDomain
  io.miso := miso_reg

  switch(state) {
    is(sIdle) {
      miso_reg := true.B
      sendShiftReg := 0.U
      recvShiftReg := 0.U
      bitCount := 0.U
      when(!io.ss(0)) {
        state := sReceive
      }
    }
    is(sReceive) {
      recvShiftReg := Cat(recvShiftReg(6, 0), io.mosi)
      bitCount := bitCount + 1.U
      when(bitCount === 7.U) {
        sendShiftReg := Reverse(recvShiftReg)
        bitCount := 0.U
        state := sSend
      }
    }
    is(sSend) {
      miso_reg := sendShiftReg(7)
      sendShiftReg := sendShiftReg << 1
      bitCount := bitCount + 1.U
      when(bitCount === 7.U) {
        state := sIdle
      }
    }
  }
}
