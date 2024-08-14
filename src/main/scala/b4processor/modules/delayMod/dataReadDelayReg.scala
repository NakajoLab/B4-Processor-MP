package b4processor.modules.delayMod

import circt.stage.ChiselStage
import b4processor.connections.{CollectedOutput, OutputValue}
import b4processor.Parameters
import chisel3._
import chisel3.util.Irrevocable

class dataReadDelayReg(implicit params: Parameters)extends Module {
  val io = IO(new Bundle {
    val input = Flipped(Irrevocable(new OutputValue))
    val output = Irrevocable(new OutputValue)
  })

  //各信号に対応したレジスタをそれぞれdelay個ずつ作成
  val validRegs = RegInit(VecInit(Seq.fill(params.memoryAccessDelay)(false.B)))
  val dataRegs = Reg(Vec(params.memoryAccessDelay, chiselTypeOf(io.input.bits)))

  //readの遅延記述
  val readHead = RegInit(0.U(10.W))
  when(io.output.ready || !validRegs(readHead + 1.U)) {

    validRegs(readHead) := io.input.valid
    dataRegs(readHead) := io.input.bits

    when(readHead === params.memoryAccessDelay.asUInt - 1.U) {
      readHead := 0.U
    }.otherwise {
      readHead := readHead + 1.U
    }
  }

  io.output.valid := validRegs(readHead + 1.U)
  io.output.bits := dataRegs(readHead + 1.U)
  io.output.ready <> io.input.ready
}
