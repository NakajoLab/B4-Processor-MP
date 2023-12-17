package b4processor.modules.decoder

import b4processor.Parameters
import b4processor.connections._
import b4processor.utils.{FormalTools, Tag, TagValueBundle}
import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage
import b4processor.modules.Decoder2AtomicLSU
import b4processor.modules.reservationstation.ReservationStationEntry
import b4processor.utils.operations.{DecodingMod, LoadStoreOperation, SendReceiveOperation} //added by akamatsu

/** デコーダ
  */
class Decoder(implicit params: Parameters) extends Module with FormalTools {
  val io = IO(new Bundle {
    val threadId = Input(UInt(log2Up(params.threads).W))

    val instructionFetch = Flipped(new Uncompresser2Decoder())
    val reorderBuffer = new Decoder2ReorderBuffer
    val outputCollector = Flipped(new CollectedOutput())
    val registerFile = new Decoder2RegisterFile()

    val reservationStation = new Decoder2ReservationStation
    val loadStoreQueue = Decoupled(new Decoder2LoadStoreQueue)
    val sendReceiveQueue = Decoupled(new Decoder2SendReceiveQueue) //added by akamatsu
    val csr = Decoupled(new Decoder2CSRReservationStation())
    val amo = Decoupled(new Decoder2AtomicLSU)
  })

  val operations = DecodingMod(
    io.instructionFetch.bits.instruction,
    io.instructionFetch.bits.programCounter,
  )

  io.reorderBuffer.isDecodeError := io.instructionFetch.valid && !operations.valid

  val operationInorder =
    LoadStoreOperation.Store === operations.loadStoreOp.validDataOrZero ||
      operations.amoOp.isValid ||
      operations.csrOp.isValid ||
      operations.sendReceiveOp.isValid // added by akamatsu+

  val operationSendRaceive = operations.sendReceiveOp.isValid

  // リオーダバッファへの入力
  io.reorderBuffer.sources zip operations.sources foreach { case (rob, o) =>
    rob.sourceRegister := o.reg
  }
  io.reorderBuffer.destination.destinationRegister := operations.rd
  io.reorderBuffer.destination.operationInorder := operationInorder
  io.reorderBuffer.programCounter := io.instructionFetch.bits.programCounter
  io.reorderBuffer.operationSendReceive := operationSendRaceive

  // レジスタファイルへの入力
  io.registerFile.sourceRegisters zip operations.sources foreach {
    case (rf, o) => rf := o.reg
  }

  // リオーダバッファから一致するタグを取得する
  val sourceTags = Wire(Vec(3, Valid(new Tag)))
  sourceTags := io.reorderBuffer.sources.map(_.matchingTag)
  if (!params.enablePExt && !params.enableSR) {
    sourceTags(2).valid := false.B
    sourceTags(2).bits := Tag(io.threadId, 0.U)
  }
  val destinationTag = io.reorderBuffer.destination.destinationTag

  for (s <- sourceTags) {
    when(s.valid) {
      assert(s.bits =/= destinationTag, "tag wrong?")
    }
  }

  // Valueの選択
  val values = Wire(Vec(3, Valid(UInt(64.W))))
  values := io.reorderBuffer.sources zip io.registerFile.values zip sourceTags map {
    case ((rob, rf), st) =>
      ValueSelector.getValue(rob.value, rf, io.outputCollector, st)
  }
  if (!params.enablePExt && !params.enableSR)
    values(2) := 0.U.asTypeOf(Valid(UInt(64.W)))


  // 命令をデコードするのはリオーダバッファにエントリの空きがあり、リザベーションステーションにも空きがあるとき
  io.instructionFetch.ready := io.reservationStation.ready && io.reorderBuffer.ready && io.loadStoreQueue.ready && io.csr.ready && io.amo.ready && io.sendReceiveQueue.ready
  // リオーダバッファやリザベーションステーションに新しいエントリを追加するのは命令がある時
  io.reorderBuffer.valid := io.instructionFetch.ready &&
    io.instructionFetch.valid

  // RSへの出力を埋める
  val rs = io.reservationStation.entry
  rs := 0.U.asTypeOf(new ReservationStationEntry)
  rs.valid := io.instructionFetch.ready &&
    io.instructionFetch.valid &&
    (operations.aluOp.isValid || operations.pextOp.isValid)
  when(rs.valid) {
    rs.operation := operations.aluOp.validDataOrZero
    rs.pextOperation := operations.pextOp.validDataOrZero
    rs.ispext := operations.pextOp.valid
    rs.destinationTag := destinationTag
    rs.sources zip values zip sourceTags zip operations.sources foreach {
      case (((rs_s, v), st), o) =>
        rs_s := new TagValueBundle().fromConditional(
          !(v.valid || o.valid),
          st.bits,
          MuxCase(0.U, Seq(o.valid -> o.value, v.valid -> v.bits)),
        )
    }

    rs.branchOffset := operations.branchOffset
    rs.wasCompressed := io.instructionFetch.bits.wasCompressed
  }

  // load or store命令の場合，LSQへ発送
  io.loadStoreQueue.bits := 0.U.asTypeOf(new Decoder2LoadStoreQueue)
  io.loadStoreQueue.valid := io.loadStoreQueue.ready && io.instructionFetch.ready && io.instructionFetch.valid && operations.loadStoreOp.isValid
  when(io.loadStoreQueue.valid) {
    io.loadStoreQueue.bits.operation := operations.loadStoreOp.validDataOrZero
    io.loadStoreQueue.bits.operationWidth := operations.loadStoreWidth
    io.loadStoreQueue.bits.destinationTag := destinationTag
    io.loadStoreQueue.bits.addressValid := values(0).valid
    io.loadStoreQueue.bits.address := values(0).bits
    io.loadStoreQueue.bits.addressTag := sourceTags(0).bits
    io.loadStoreQueue.bits.addressOffset := operations
      .sources(1)
      .value(11, 0)
      .asSInt
    when(operationInorder) {
      io.loadStoreQueue.bits.storeDataTag := sourceTags(1).bits
      io.loadStoreQueue.bits.storeData := values(1).bits
      io.loadStoreQueue.bits.storeDataValid := values(1).valid
    }.otherwise {
      io.loadStoreQueue.bits.storeDataTag := Tag(io.threadId, 0.U)
      io.loadStoreQueue.bits.storeData := 0.U
      io.loadStoreQueue.bits.storeDataValid := true.B
    }
  }
  // thread ids are constant
  io.loadStoreQueue.bits.destinationTag.threadId := io.threadId
  io.loadStoreQueue.bits.storeDataTag.threadId := io.threadId
  io.loadStoreQueue.bits.addressTag.threadId := io.threadId

  /** added for SRQ */
  // send or receive命令の場合，SRQへ発送 added by akamatsu
  io.sendReceiveQueue.bits := 0.U.asTypeOf(new Decoder2SendReceiveQueue)
  io.sendReceiveQueue.valid := io.sendReceiveQueue.ready && io.instructionFetch.ready && io.instructionFetch.valid && operations.sendReceiveOp.isValid
  when(io.sendReceiveQueue.valid) {
    io.sendReceiveQueue.bits.operation := operations.sendReceiveOp.validDataOrZero
    io.sendReceiveQueue.bits.destinationTag.id := destinationTag.id
    when(operations.sendReceiveOp.bits === SendReceiveOperation.Send) {
      io.sendReceiveQueue.bits.destinationTag.threadId := values(0).bits //自身のThreadIDではなく送信先のThreadIDを指定
      io.sendReceiveQueue.bits.sendData := values(1).bits
      io.sendReceiveQueue.bits.sendDataTag := sourceTags(1).bits
      io.sendReceiveQueue.bits.sendDataValid := values(1).valid
      io.sendReceiveQueue.bits.channel := values(2).bits
      io.sendReceiveQueue.bits.channelValid := values(2).valid
      io.sendReceiveQueue.bits.channelTag := sourceTags(2).bits
    }.otherwise {
      io.sendReceiveQueue.bits.destinationTag.threadId := destinationTag.threadId
      io.sendReceiveQueue.bits.sendDataTag.threadId := values(0).bits
      io.sendReceiveQueue.bits.channel := values(1).bits
      io.sendReceiveQueue.bits.channelValid := values(1).valid
      io.sendReceiveQueue.bits.channelTag := sourceTags(1).bits
      io.sendReceiveQueue.bits.sendDataTag.id := 0.U
      io.sendReceiveQueue.bits.sendData := 0.U
      io.sendReceiveQueue.bits.sendDataValid := false.B
    }
  }


  io.csr.valid := io.csr.ready && io.instructionFetch.ready && io.instructionFetch.valid & operations.csrOp.isValid
  io.csr.bits := 0.U.asTypeOf(new Decoder2CSRReservationStation)
  when(io.csr.valid) {
    io.csr.bits.operation := operations.csrOp.validDataOrZero
    io.csr.bits.address := operations.sources(1).value
//    printf(p"decoder out ${operations.csrOp}\n")
    io.csr.bits.sourceTag := sourceTags(0).bits
    io.csr.bits.value := MuxCase(
      0.U,
      Seq(
        operations.sources(0).valid -> operations.sources(0).value,
        values(0).valid -> values(0).bits,
      ),
    )
    io.csr.bits.ready := operations.sources(0).valid || values(0).valid
    io.csr.bits.destinationTag := io.reorderBuffer.destination.destinationTag
  }
  io.csr.bits.sourceTag.threadId := io.threadId
  io.csr.bits.destinationTag.threadId := io.threadId

  io.amo.valid := io.amo.ready && io.instructionFetch.ready && io.instructionFetch.valid && operations.amoOp.isValid
  io.amo.bits := 0.U.asTypeOf(new Decoder2AtomicLSU)
  when(io.amo.valid) {
    io.amo.bits.valid := true.B
    io.amo.bits.operation := operations.amoOp.validDataOrZero
    io.amo.bits.operationWidth := operations.amoWidth
    io.amo.bits.ordering := operations.amoOrdering

    io.amo.bits.addressReg := sourceTags(0).bits
    io.amo.bits.addressValue := values(0).bits
    io.amo.bits.addressValid := values(0).valid

    io.amo.bits.srcReg := sourceTags(1).bits
    io.amo.bits.srcValue := values(1).bits
    io.amo.bits.srcValid := values(1).valid

    io.amo.bits.destinationTag := destinationTag
  }
  // thread ids are const
  io.amo.bits.srcReg.threadId := io.threadId
  io.amo.bits.addressReg.threadId := io.threadId
  io.amo.bits.destinationTag.threadId := io.threadId

  // FORMAL
  takesEveryValue(io.reorderBuffer.valid)
  takesEveryValue(io.reservationStation.entry.valid)
  takesEveryValue(io.loadStoreQueue.valid)
  takesEveryValue(io.sendReceiveQueue.valid)
  takesEveryValue(io.csr.valid)
  takesEveryValue(io.amo.valid)

  // source and destination tags threadid should be const
  rs.sources.zipWithIndex foreach { case (s, i) =>
    when(s.isTag) {
      assert(
        s.getTagUnsafe.threadId === io.threadId,
        s"thread id wrong on source($i)",
      )
    }
  }
  assert(
    io.amo.bits.srcReg.threadId === io.threadId,
    "amo source thread id wrong",
  )
  assert(
    io.amo.bits.destinationTag.threadId === io.threadId,
    "amo destination thread id wrong",
  )
  assert(io.csr.bits.sourceTag.threadId === io.threadId)
  assert(io.csr.bits.destinationTag.threadId === io.threadId)
  assert(io.loadStoreQueue.bits.destinationTag.threadId === io.threadId)
  assert(io.loadStoreQueue.bits.storeDataTag.threadId === io.threadId)
  assert(io.loadStoreQueue.bits.addressTag.threadId === io.threadId)

  // assumptions
  assume(stable(io.threadId))
//  io.reorderBuffer.sources.zipWithIndex foreach { case (s, i) =>
//    assume(
//      s.matchingTag.bits.threadId === io.threadId,
//      s"reorder buffer source $i threadid should be the same as decoder"
//    )
//  }
//  assume(
//    io.reorderBuffer.destination.destinationTag.threadId === io.threadId,
//    "reorder buffer destination threadid should be the same as decoder"
//  )
}

object Decoder extends App {
  implicit val params = Parameters(enablePExt = false)
  ChiselStage.emitSystemVerilogFile(new Decoder)
}
