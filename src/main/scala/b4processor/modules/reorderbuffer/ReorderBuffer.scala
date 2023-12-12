package b4processor.modules.reorderbuffer

import circt.stage.ChiselStage
import b4processor.Parameters
import b4processor.connections.{
  BranchPrediction2ReorderBuffer,
  CollectedOutput,
  Decoder2ReorderBuffer,
  LoadStoreQueue2ReorderBuffer,
  ReorderBuffer2CSR,
  ReorderBuffer2RegisterFile,
  SendReceiveQueue2ReorderBuffer,
}
import b4processor.riscv.{CSRs, Causes}
import b4processor.utils.RVRegister.{AddRegConstructor, AddUIntRegConstructor}
import b4processor.utils.{RVRegister, Tag}
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.prefix
import chisel3.util._

object Debug {
  def dbg[T <: Data](t: T): T = {
    printf(p"$t\n")
    t
  }

  def dbghex[T <: Bits](t: T): T = {
    printf("%x\n", t)
    t
  }
}

/** リオーダバッファ
  *
  * @param params
  *   パラメータ
  */
class ReorderBuffer(implicit params: Parameters) extends Module {
  private val tagWidth = params.tagWidth

  val io = IO(new Bundle {
    val decoders =
      Vec(params.decoderPerThread, Flipped(new Decoder2ReorderBuffer))
    val collectedOutputs = Flipped(new CollectedOutput)
    val registerFile =
      Vec(
        params.maxRegisterFileCommitCount,
        Valid(new ReorderBuffer2RegisterFile()),
      )
    val loadStoreQueue = Vec(
      params.maxRegisterFileCommitCount,
      Valid(new LoadStoreQueue2ReorderBuffer),
    )
    val sendReceiveQueue = Vec(                   //added by Akamatsu
      params.maxRegisterFileCommitCount,
      Valid(new SendReceiveQueue2ReorderBuffer),
    )
    val isEmpty = Output(Bool())
    val csr = new ReorderBuffer2CSR

    val isError = Output(Bool())
    val threadId = Input(UInt(log2Up(params.threads).W))

    val head = if (params.debug) Some(Output(UInt(tagWidth.W))) else None
    val tail = if (params.debug) Some(Output(UInt(tagWidth.W))) else None
    val bufferIndex0 =
      if (params.debug) Some(Output(new ReorderBufferEntry)) else None
  })

  val head = RegInit(0.U(tagWidth.W))
  val tail = RegInit(0.U(tagWidth.W))
  val buffer = RegInit(
    VecInit(Seq.fill(math.pow(2, tagWidth).toInt)(ReorderBufferEntry.default)),
  )

  class RegisterTagMapContent extends Bundle {
    val valid = Bool()
    val tagId = UInt(tagWidth.W)
  }

  private object RegisterTagMapContent {
    def default: RegisterTagMapContent =
      new RegisterTagMapContent().Lit(_.valid -> false.B)
    def apply(tagId: UInt): RegisterTagMapContent = {
      val w = Wire(new RegisterTagMapContent)
      w.valid := true.B
      w.tagId := tagId
      w
    }
  }

  io.csr.mcause.valid := false.B
  io.csr.mcause.bits := 0.U
  io.csr.mepc.valid := false.B
  io.csr.mepc.bits := 0.U

  private val registerTagMap = RegInit(
    VecInit(Seq.fill(32)(RegisterTagMapContent.default)),
  )

  class DecoderMap extends Bundle {
    val valid = Bool()
    val tagId = UInt(tagWidth.W)
    val destinationRegister = new RVRegister()
  }

  private object DecoderMap {
    def default: DecoderMap =
      new DecoderMap().Lit(
        _.valid -> false.B,
        _.tagId -> 0.U,
        _.destinationRegister -> 0.reg,
      )
  }

  private val previousDecoderMap =
    Seq.fill(params.decoderPerThread - 1)(WireDefault(DecoderMap.default))
  // レジスタファイルへの書き込み
  io.isError := false.B
  private var lastValid = true.B
  for (lsq <- io.loadStoreQueue) {
    lsq.valid := false.B
    lsq.bits := 0.U.asTypeOf(new LoadStoreQueue2ReorderBuffer)
  }
  for (srq <- io.sendReceiveQueue) { //added by akamatsu
    srq.valid := false.B
    srq.bits := 0.U.asTypeOf(new SendReceiveQueue2ReorderBuffer)
  }
  // レジスタファイルとロードストアキューへのコミット
  for ((((rf, lsq), srq), i) <- io.registerFile.zip(io.loadStoreQueue).zip(io.sendReceiveQueue).zipWithIndex) { //added by akamatsu
    // プレフィックスの表示（デバッグ目的）
    prefix(s"to_rf$i") {
      // リオーダーバッファのインデックスを計算
      val index = tail + i.U
      // リオーダーバッファのエントリを取得
      val biVal = buffer(index)

      // Load-Store Queueへの書き込み
      // インデックスが0の場合、ストア命令の場合にキューにデータを書き込む
      if (i == 0) when(biVal.operationInorder) {
        when(biVal.isSendReceiveOp){ //added by akamatsu
          srq.valid := true.B
          srq.bits.destinationTag.id := index
          srq.bits.destinationTag.threadId := io.threadId
          biVal.operationInorder := false.B
        }.otherwise{
          lsq.valid := true.B
          lsq.bits.destinationTag.id := index
          lsq.bits.destinationTag.threadId := io.threadId
          biVal.operationInorder := false.B
        }
      }

      // エラーフラグの確認
      val isError = biVal.isError
      // 命令が実行可能かどうかの確認
      val instructionOk = (biVal.valueReady || isError) && !biVal.operationInorder
      // 命令がコミット可能かどうかの確認
      val canCommit = lastValid && index =/= head && instructionOk

      // レジスタファイルへの書き込み
      rf.valid := canCommit
      rf.bits.value := 0.U
      rf.bits.destinationRegister := 0.reg
      when(canCommit) {
        // エラーが発生していない場合
        when(!isError) {
          // レジスタファイルにデータを書き込み
          rf.bits.value := biVal.value
          rf.bits.destinationRegister := biVal.destinationRegister
          // 対応するレジスタへのタグをリセット
          when(index === registerTagMap(biVal.destinationRegister.inner).tagId) {
            registerTagMap(biVal.destinationRegister.inner) :=
              RegisterTagMapContent.default
          }
        }.otherwise {
          // エラーが発生した場合、例外処理とエラーフラグの設定
          io.csr.mcause.valid := true.B
          io.csr.mcause.bits := biVal.value
          io.csr.mepc.valid := true.B
          io.csr.mepc.bits := biVal.programCounter
          io.isError := true.B
        }
        // リオーダーバッファエントリのリセット
        biVal := ReorderBufferEntry.default
      }
      // 最後のコミットの有効性を更新
      lastValid = canCommit
    }
  }





  private val tailDelta = MuxCase(
    params.maxRegisterFileCommitCount.U,
    io.registerFile.zipWithIndex.map { case (entry, index) =>
      !entry.valid -> index.U
    },
  )

  // デコーダからの読み取りと書き込み
  private var insertIndex = head
  private var lastReady = true.B
  for (i <- 0 until params.decoderPerThread) {
    prefix(s"from_dec$i") {
      val decoder = io.decoders(i)
      decoder.ready := lastReady && (insertIndex + 1.U) =/= tail
      when(decoder.valid && decoder.ready) {
        buffer(insertIndex) := {
          val entry = Wire(new ReorderBufferEntry)
          entry.destinationRegister := decoder.destination.destinationRegister
          entry.operationInorder := decoder.destination.operationInorder
          entry.isSendReceiveOp := decoder.operationSendReceive //added by akamatsu
          entry.programCounter := decoder.programCounter
          entry.isError := decoder.isDecodeError
          entry.value := Mux(
            decoder.isDecodeError,
            Causes.illegal_instruction.U,
            0.U,
          )
          entry.valueReady := false.B
          entry
        }
      }
      if (i < params.decoderPerThread - 1) {
        previousDecoderMap(i).valid :=
          decoder.valid && decoder.destination.destinationRegister =/= 0.reg
        previousDecoderMap(i).tagId := insertIndex
        previousDecoderMap(i).destinationRegister :=
          decoder.destination.destinationRegister
      }
      decoder.destination.destinationTag := Tag(io.threadId, insertIndex)
      registerTagMap(decoder.destination.destinationRegister.inner) :=
        RegisterTagMapContent(insertIndex)

      locally {
        for (source_index <- 0 until 3) {
          prefix(s"prev_dec_check_${i}_src$source_index") {
            val matches_prev_decoder = (0 until i)
              .map(n =>
                (previousDecoderMap(n).valid &&
                  previousDecoderMap(n).destinationRegister
                  === decoder.sources(source_index).sourceRegister),
              )
              .fold(false.B)(_ || _)
            val matchingBits = MuxCase(
              registerTagMap(
                decoder.sources(source_index).sourceRegister.inner,
              ),
              (0 until i)
                .map(n =>
                  (previousDecoderMap(n).valid &&
                    previousDecoderMap(n).destinationRegister
                    === decoder.sources(source_index).sourceRegister) ->
                    RegisterTagMapContent(previousDecoderMap(n).tagId),
                )
                .reverse,
            )
            val hasMatching = matchingBits.valid
            val matchingBuf = buffer(matchingBits.tagId)

            decoder.sources(source_index).matchingTag.valid := hasMatching
            decoder.sources(source_index).matchingTag.bits := Tag(
              io.threadId,
              matchingBits.tagId,
            )
            decoder
              .sources(source_index)
              .value
              .valid := matchingBuf.valueReady && !matches_prev_decoder
            decoder.sources(source_index).value.bits := matchingBuf.value
          }
        }
      }

      // 次のループで使用するinserIndexとlastReadyを変える
      // わざと:=ではなく=を利用している
      insertIndex =
        Mux(decoder.valid && decoder.ready, insertIndex + 1.U, insertIndex)
      lastReady = decoder.ready
    }
  }
  head := insertIndex
  tail := Mux(io.isError, insertIndex, tail + tailDelta)
  io.csr.retireCount := Mux(io.isError, 0.U, tailDelta)
  io.isEmpty := head === tail

  // 出力の読み込み
  private val output = io.collectedOutputs.outputs
  for (o <- output) {
    when(o.valid && (o.bits.tag.threadId === io.threadId)) {
      val writePort = buffer(o.bits.tag.id)
      writePort.value := o.bits.value
      writePort.isError := o.bits.isError
      writePort.valueReady := true.B
    }
  }

  registerTagMap(0) := new RegisterTagMapContent()
    .Lit(_.valid -> false.B, _.tagId -> 0.U)

  // デバッグ
  if (params.debug) {
    io.head.get := head
    io.tail.get := tail
    io.bufferIndex0.get := buffer(0)
    //    printf(p"reorder buffer pc=${buffer(0).programCounter} value=${buffer(0).value} ready=${buffer(0).ready} rd=${buffer(0).destinationRegister}\n")
  }

  for (d <- io.decoders)
    for (s <- d.sources)
      s.matchingTag.bits.threadId := io.threadId

  // FORMAL
  for (d <- io.decoders)
    for (s <- d.sources)
      assert(s.matchingTag.bits.threadId === io.threadId)
}

object ReorderBuffer extends App {
  implicit val params =
    Parameters(
      threads = 2,
      decoderPerThread = 2,
      maxRegisterFileCommitCount = 2,
    )
  ChiselStage.emitSystemVerilogFile(new ReorderBuffer)
}
