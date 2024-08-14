package b4processor.modules.delayMod

import b4processor.Parameters
import b4processor.modules.memory.MemoryAccessChannels
import b4processor.utils.axi.ChiselAXI
import chisel3._

//requestを遅延させる

class dataReadWriteDelayReg(implicit params: Parameters) extends Module {
  val memSide = IO(new MemoryAccessChannels())
  val cpuSide = IO(Flipped(new MemoryAccessChannels()))

  val delay = params.memoryAccessDelay

  //各信号に対応したレジスタをそれぞれdelay個ずつ作成
  val readRequestValidRegs = RegInit(VecInit(Seq.fill(delay)(false.B)))
  val readRequestRegs = Reg(Vec(delay, chiselTypeOf(cpuSide.read.request.bits)))

  val writeRequestValidRegs = RegInit(VecInit(Seq.fill(delay)(false.B)))
  val writeRequestRegs = Reg(Vec(delay, chiselTypeOf(cpuSide.write.request.bits)))
  val writeRequestDataRegs = Reg(Vec(delay, chiselTypeOf(cpuSide.write.requestData.bits)))

  //readの遅延記述
  val readHead = RegInit(0.U(10.W))
  when(memSide.read.request.ready || !readRequestValidRegs(readHead + 1.U)) {
    when(cpuSide.read.request.ready){
      readRequestValidRegs(readHead) := cpuSide.read.request.valid
      readRequestRegs(readHead) := cpuSide.read.request.bits
    }.otherwise{
      readRequestValidRegs(readHead) := false.B
    }

    when(readHead === params.memoryAccessDelay.asUInt - 1.U) {
      readHead := 0.U
    }.otherwise {
      readHead := readHead + 1.U
    }
  }

  //writeの遅延記述
  val writeHead = RegInit(0.U(10.W))
  when(memSide.write.request.ready || !writeRequestValidRegs(writeHead + 1.U)) {
    when(cpuSide.write.request.ready) {
      writeRequestValidRegs(writeHead) := cpuSide.write.request.valid
      writeRequestRegs(writeHead) := cpuSide.write.request.bits
      writeRequestDataRegs(writeHead) := cpuSide.write.requestData.bits
    }.otherwise{
      writeRequestValidRegs(writeHead) := false.B
    }

    when(writeHead === params.memoryAccessDelay.asUInt - 1.U) {
      writeHead := 0.U
    }.otherwise {
      writeHead := writeHead + 1.U
    }
  }

  memSide.read.response <> cpuSide.read.response

  memSide.read.request.ready <> cpuSide.read.request.ready
  memSide.read.request.valid := readRequestValidRegs(readHead + 1.U)
  memSide.read.request.bits := readRequestRegs(readHead + 1.U)


  memSide.write.response <> cpuSide.write.response

  memSide.write.request.ready <> cpuSide.write.request.ready
  memSide.write.request.valid := writeRequestValidRegs(writeHead + 1.U)
  memSide.write.request.bits := writeRequestRegs(writeHead + 1.U)

  memSide.write.requestData.ready <> cpuSide.write.requestData.ready
  memSide.write.requestData.valid := writeRequestValidRegs(writeHead + 1.U)
  memSide.write.requestData.bits := writeRequestDataRegs(writeHead + 1.U)
}
