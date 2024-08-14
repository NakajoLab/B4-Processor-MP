package b4processor.modules.AXIDelayReg

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage
import b4processor.utils.axi.ChiselAXI

class AXIDelayReg (delay: Int)extends Module {
  val memSideAXI = IO(new ChiselAXI(64, 64))
  val cpuSideAXI = IO(Flipped(new ChiselAXI(64, 64)))

  //雑にAXIの各信号に対応したレジスタをそれぞれdelay個ずつ作成
  val writeResponseValidRegs = RegInit(VecInit(Seq.fill(delay)(false.B)))
  val writeResponseRegs = Reg(Vec(delay, chiselTypeOf(memSideAXI.writeResponse.bits)))

  val readAddressRegs = Reg(Vec(delay, chiselTypeOf(cpuSideAXI.readAddress.bits)))
  val readAddressValidRegs = RegInit(VecInit(Seq.fill(delay)(false.B)))

  val readRegs = Reg(Vec(delay, chiselTypeOf(memSideAXI.read.bits)))
  val readValidRegs = RegInit(VecInit(Seq.fill(delay)(false.B)))

  //readの遅延記述

  val readHead = RegInit(0.U(10.W))
  /*
  val lastReadBits = RegInit(0.U.asTypeOf(memSideAXI.read.bits))
  when(cpuSideAXI.read.ready || !readValidRegs(readHead+1.U)){
    when(memSideAXI.read.bits === lastReadBits){
      readRegs(readHead) := 0.U.asTypeOf(memSideAXI.read.bits)
      readValidRegs(readHead) := false.B
    }.otherwise{
      readRegs(readHead) := memSideAXI.read.bits
      readValidRegs(readHead) := memSideAXI.read.valid
    }
    lastReadBits := memSideAXI.read.bits
    when(readHead === delay.asUInt - 1.U){
      readHead := 0.U
    }.otherwise {
      readHead := readHead + 1.U
    }
  }
  */
  val lastReadAddress = RegInit(0.U.asTypeOf(cpuSideAXI.readAddress.bits))
  val insertValid = RegInit(false.B)
  when(memSideAXI.readAddress.ready || !readAddressValidRegs(readHead + 1.U)) {
    when(cpuSideAXI.readAddress.bits === lastReadAddress) {
      when(memSideAXI.readAddress.ready){
        readAddressRegs(readHead) := cpuSideAXI.readAddress.bits
        readAddressValidRegs(readHead) := cpuSideAXI.readAddress.valid
      }.otherwise{
        readAddressRegs(readHead) := 0.U.asTypeOf(cpuSideAXI.readAddress.bits)
        readAddressValidRegs(readHead) := false.B
      }
    }.otherwise {
      readAddressRegs(readHead) := cpuSideAXI.readAddress.bits
      readAddressValidRegs(readHead) := cpuSideAXI.readAddress.valid
      insertValid := false.B
    }
    lastReadAddress := cpuSideAXI.readAddress.bits
    when(readHead === delay.asUInt - 1.U) {
      readHead := 0.U
    }.otherwise {
      readHead := readHead + 1.U
    }
  }


  //writeの遅延記述
  val writeHead = RegInit(0.U(10.W))
  val lastWriteResponseValid = RegInit(false.B)
  when(cpuSideAXI.writeResponse.ready || !writeResponseValidRegs(writeHead+1.U)) {
    writeResponseRegs(writeHead) := memSideAXI.writeResponse.bits
    when(memSideAXI.writeResponse.valid === lastWriteResponseValid){
      writeResponseValidRegs(writeHead) := false.B
    }.otherwise {
      writeResponseValidRegs(writeHead) := memSideAXI.writeResponse.valid
    }
    lastWriteResponseValid := memSideAXI.writeResponse.valid
    when(writeHead === delay.asUInt - 1.U) {
      writeHead := 0.U
    }.otherwise {
      writeHead := writeHead + 1.U
    }
  }

  memSideAXI.writeAddress <> cpuSideAXI.writeAddress
  memSideAXI.write <> cpuSideAXI.write

  cpuSideAXI.writeResponse.ready <> memSideAXI.writeResponse.ready
  cpuSideAXI.writeResponse.valid <> memSideAXI.writeResponse.valid
  //cpuSideAXI.writeResponse.valid := writeResponseValidRegs(writeHead + 1.U)
  cpuSideAXI.writeResponse.bits <> memSideAXI.writeResponse.bits
  //cpuSideAXI.writeResponse.bits := writeResponseRegs(writeHead + 1.U)

  memSideAXI.readAddress.ready <> cpuSideAXI.readAddress.ready
  //memSideAXI.readAddress.valid <> cpuSideAXI.readAddress.valid
  //memSideAXI.readAddress.bits <> cpuSideAXI.readAddress.bits
  memSideAXI.readAddress.valid := readAddressValidRegs(readHead + 1.U)
  memSideAXI.readAddress.bits := readAddressRegs(readHead + 1.U)

  cpuSideAXI.read <> memSideAXI.read
  //cpuSideAXI.read.ready <> memSideAXI.read.ready
  //cpuSideAXI.read.valid := readValidRegs(readHead + 1.U)
  //cpuSideAXI.read.bits := readRegs(readHead + 1.U)
}
