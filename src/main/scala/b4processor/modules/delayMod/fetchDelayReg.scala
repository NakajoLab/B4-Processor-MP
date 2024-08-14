package b4processor.modules.delayMod

import b4processor.Parameters
import b4processor.connections.{FetchBuffer2Uncompresser, OutputValue}
import chisel3._
import chisel3.util.Irrevocable

class fetchDelayReg(implicit params: Parameters)extends Module {
  val io = IO(new Bundle {
    val input = Flipped(new FetchBuffer2Uncompresser())
    val output = new FetchBuffer2Uncompresser()
  })

  //各信号に対応したレジスタをそれぞれdelay個ずつ作成
  val validRegs = RegInit(VecInit(Seq.fill(params.memoryFetchDelay)(false.B)))
  val fetchRegs = Reg(Vec(params.memoryFetchDelay, chiselTypeOf(io.input.bits)))

  //readの遅延記述
  val readHead = RegInit(0.U(10.W))
  when(io.output.ready || !validRegs(readHead + 1.U)) {
    when(io.input.ready) {
      validRegs(readHead) := io.input.valid
      fetchRegs(readHead) := io.input.bits
    }.otherwise{
      validRegs(readHead) := false.B
    }

    when(readHead === params.memoryFetchDelay.asUInt - 1.U) {
      readHead := 0.U
    }.otherwise {
      readHead := readHead + 1.U
    }
  }

  io.output.valid := validRegs(readHead + 1.U)
  io.output.bits := fetchRegs(readHead + 1.U)
  io.output.ready <> io.input.ready
}
