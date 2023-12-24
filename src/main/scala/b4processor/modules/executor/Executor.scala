package b4processor.modules.executor

import circt.stage.ChiselStage
import b4processor.Parameters
import b4processor.connections._
import chisel3.util._
import chisel3._
import chisel3.experimental.prefix

class Executor(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val reservationStation =
      Flipped(Irrevocable(new ReservationStation2Executor))
    val out = Irrevocable(new OutputValue)
    val fetch = Irrevocable(new BranchOutput)
  })

  private val operation = io.reservationStation.bits.operation

  /** リザベーションステーションから実行ユニットへデータを送信 op,fuctionによって命令を判断し，計算を実行
    */
  io.reservationStation.ready := io.out.ready && io.fetch.ready

  io.fetch.bits.programCounterOffset := 0.S
  io.fetch.valid := false.B
  io.fetch.bits.threadId := 0.U
  io.out.valid := false.B
  io.out.bits := DontCare

  val a = io.reservationStation.bits.value1
  val b = io.reservationStation.bits.value2

  import b4processor.utils.operations.ALUOperation._

  val nextOffset =
    Mux(io.reservationStation.bits.wasCompressed, 2.S, 4.S)

  val executeOutput = Wire(UInt(64.W))
  prefix("execute_output_tmp") {
    executeOutput :=
      MuxLookup(operation, 0.U)(
        Seq(
          Add -> (a + b),
          Sub -> (a - b),
          And -> (a & b),
          Or -> (a | b),
          Xor -> (a ^ b),
          Slt -> (a.asSInt < b.asSInt).asUInt,
          Sltu -> (a < b),
          Sll -> (a << b(5, 0)).asUInt,
          Srl -> (a >> b(5, 0)).asUInt,
          Sra -> (a.asSInt >> b(5, 0)).asUInt,
          AddJALR -> (a + b),
          AddW -> (a(31, 0).asSInt + b(31, 0).asSInt).pad(64).asUInt,
          SllW -> (a(31, 0) << b(4, 0))(31, 0).asSInt.pad(64).asUInt,
          SrlW -> (a(31, 0) >> b(4, 0))(31, 0).asSInt.pad(64).asUInt,
          SraW -> (a(31, 0).asSInt >> b(4, 0)).asSInt.pad(64).asUInt,
          SubW -> (a(31, 0).asSInt - b(31, 0).asSInt).pad(64).asUInt,
          AddJAL -> (b + nextOffset.asUInt),
          AddJALR -> (b + nextOffset.asUInt),
          Div ->  (a / b),
          Mul ->  (a * b),
          Rem ->  (a % b),
        ),
      )
  }

  val isBranch = Seq(
    BranchEqual,
    BranchNotEqual,
    BranchLessThan,
    BranchLessThanUnsigned,
    BranchGreaterThanOrEqual,
    BranchGreaterThanOrEqualUnsigned,
  ).map(_ === operation).reduce(_ || _)
  val branchedOffset = MuxCase(
    0.S,
    Seq(
      isBranch -> (io.reservationStation.bits.branchOffset ## 0.S(1.W)).asSInt,
      (operation === AddJALR) -> (a.asSInt - b.asSInt + io.reservationStation.bits.branchOffset),
    ),
  )

  val fetchOffset = Wire(SInt(64.W))
  prefix("fetch_output_tmp") {
    fetchOffset := MuxLookup(operation, 0.S)(
      Seq(
        BranchEqual -> Mux(a === b, branchedOffset, nextOffset),
        BranchNotEqual -> Mux(a =/= b, branchedOffset, nextOffset),
        BranchLessThan ->
          Mux(a.asSInt < b.asSInt, branchedOffset, nextOffset),
        BranchGreaterThanOrEqual ->
          Mux(a.asSInt >= b.asSInt, branchedOffset, nextOffset),
        BranchLessThanUnsigned ->
          Mux(a < b, branchedOffset, nextOffset),
        BranchGreaterThanOrEqualUnsigned ->
          Mux(a >= b, branchedOffset, nextOffset),
        AddJALR -> branchedOffset,
      ),
    )
  }

  io.fetch.valid := io.reservationStation.valid && io.reservationStation.ready && (isBranch || io.reservationStation.bits.operation === AddJALR)
  io.fetch.bits.programCounterOffset := fetchOffset
  io.fetch.bits.threadId := io.reservationStation.bits.destinationTag.threadId

  when(io.reservationStation.valid && io.reservationStation.ready) {
    io.out.valid := io.reservationStation.ready
    io.out.bits.value := executeOutput
    io.out.bits.tag := io.reservationStation.bits.destinationTag
  }

  io.out.bits.isError := false.B
}

object Executor extends App {
  implicit val params = Parameters()
  ChiselStage.emitSystemVerilogFile(new Executor())
}
