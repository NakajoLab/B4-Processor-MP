package b4processor.modules.fetch

import b4processor.Parameters
import b4processor.connections.{Fetch2BranchPrediction, Fetch2FetchBuffer}
import b4processor.modules.branch_output_collector.CollectedBranchAddresses
import b4processor.modules.cache.InstructionMemoryCache
import b4processor.modules.memory.{ExternalMemoryInterface, InstructionMemory}
import b4processor.utils.{InstructionUtil, SimpleAXIMemory, SymbiYosysFormal}
import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.math

/** フェッチのラッパー
  *
  * フェッチ、キャッシュ、命令メモリを含む
  */
class FetchWrapper()(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {

    /** 分岐予測 */
    val prediction = Vec(params.decoderPerThread, new Fetch2BranchPrediction)

    /** 実行ユニットからの分岐先の値 */
    val collectedBranchAddresses = Flipped(new CollectedBranchAddresses)

    /** デコーダ */
    val decoders = new Fetch2FetchBuffer

    /** ロードストアキューのエントリが空か */
    val loadStoreQueueEmpty = Input(Bool())

    /** リオーダバッファのエントリが空か */
    val reorderBufferEmpty = Input(Bool())

    /** キャッシュに要求されているアドレス */
    val cacheAddress = Vec(params.decoderPerThread, Valid(UInt(64.W)))

    /** キャッシュからの出力 */
    val cacheOutput = Vec(params.decoderPerThread, Valid(UInt(64.W)))

    /** プログラムカウンタ */
    val PC = Output(UInt(64.W))

    /** 次のクロックのプログラムカウンタ */
    val nextPC = Output(UInt(64.W))

    /** 各命令の分岐の種類 */
    val branchTypes = Output(Vec(params.decoderPerThread, new BranchType.Type))

    val memorySetup = Flipped(Valid(UInt(64.W)))

    val interrupt = Input(Bool())
    val isError = Input(Bool())
  })

  val fetch = Module(new Fetch(1))
  val cache = Module(new InstructionMemoryCache)
  val memoryInterface = Module(new ExternalMemoryInterface)
  val axiMemory = Module(new SimpleAXIMemory)

  fetch.io.prediction <> io.prediction
  fetch.io.fetchBuffer <> io.decoders
  fetch.io.collectedBranchAddresses <> io.collectedBranchAddresses
  fetch.io.reorderBufferEmpty <> io.reorderBufferEmpty
  fetch.io.loadStoreQueueEmpty <> io.loadStoreQueueEmpty
  fetch.io.branchTypes.get <> io.branchTypes
  fetch.io.csr := DontCare
  fetch.io.csrReservationStationEmpty := true.B
  fetch.io.fetchBuffer.empty := true.B
  fetch.io.isError := io.isError
  fetch.io.threadId := 0.U
  fetch.io.interrupt := io.interrupt

  cache.io.fetch <> fetch.io.cache
  cache.io.memory.request <> memoryInterface.io.instruction(0)
  cache.io.memory.response <> memoryInterface.io.instruction(0)
  cache.io.threadId := 0.U

  memoryInterface.io.data.read.request.valid := false.B
  memoryInterface.io.data.read.request.bits := DontCare
  memoryInterface.io.data.write.request.valid := false.B
  memoryInterface.io.data.write.request.bits := DontCare
  memoryInterface.io.data.read.response.ready := true.B
  memoryInterface.io.amo.read.response.valid := false.B
  memoryInterface.io.amo.read.request.bits := DontCare
  memoryInterface.io.amo.write.request.valid := DontCare
  memoryInterface.io.amo.write.requestData.bits := DontCare
  memoryInterface.io.data.write.requestData.ready := true.B
  memoryInterface.io.amo.read.response.ready := true.B
  memoryInterface.io.amo.write.response.ready := true.B

  axiMemory.axi <> memoryInterface.io.coordinator
  axiMemory.simulationSource.input <> io.memorySetup

  fetch.io.cache.zip(io.cacheAddress).foreach { case (f, c) => f.address <> c }
  cache.io.fetch.zip(io.cacheOutput).foreach { case (c, f) => c.output <> f }

  io.PC := fetch.io.PC.get
  io.nextPC := fetch.io.nextPC.get

  fetch.io.fetchBuffer.toBuffer.foreach(v => v.ready := true.B)

  /** 初期化 */
  def initialize(memoryInit: => Seq[UInt]): Unit = {
    this.setPrediction(Seq.fill(params.decoderPerThread)(false))
    this.io.memorySetup.valid.poke(true)
    this.io.memorySetup.bits.poke(memoryInit.length)
    for (i <- memoryInit.indices) {
      this.clock.step()
      this.io.memorySetup.bits.poke(memoryInit(i))
    }
    this.clock.step()
    this.io.memorySetup.valid.poke(false)
    this.clock.step()
    this.clock.setTimeout(200)
  }

  def waitForCacheValid(): Unit = {
    while (!this.io.cacheOutput(0).valid.peekBoolean())
      this.clock.step()
  }

  /** 予測をセット */
  def setPrediction(values: Seq[Boolean]): Unit = {
    for (i <- 0 until params.decoderPerThread) {
      this.io.prediction(i).prediction.poke(values(i))
    }
  }

  /** 分岐先をセット */
  def setExecutorBranchResult(results: Option[Int] = None): Unit = {
    io.collectedBranchAddresses.addresses.valid.poke(results.isDefined)
    io.collectedBranchAddresses.addresses.bits.programCounterOffset
      .poke(results.getOrElse(0))
  }
}

class FetchTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with SymbiYosysFormal {
  behavior of "Fetch"
  implicit val defaultParams =
    Parameters(
      debug = true,
      threads = 1,
      decoderPerThread = 2,
      instructionStart = 0x1000_0000,
    )

  // 普通の命令と分岐を区別できるか
  // 書き込む命令
  // nop                  00000013
  // beq zero,zero,LABEL  00000463
  // nop                  00000013
  // LABEL:
  it should "load memory" in {
    test(
      new FetchWrapper(
      ),
    ).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.initialize(
        InstructionUtil
          .fromStringSeq32bit(Seq("00000013", "00000463", "00000013")),
      )

      c.waitForCacheValid()
      c.io.cacheAddress(0).bits.expect(0x10000000)
      c.io.cacheAddress(1).bits.expect(0x10000004)

      c.io.cacheOutput(0).bits.expect("x00000013".U)
      c.io.cacheOutput(0).valid.expect(true)

      c.io.cacheOutput(1).bits.expect("x00000463".U)
      c.io.cacheOutput(1).valid.expect(true)
    }
  }

  // 予測で値がどちらとも読み取れる
  // 書き込む命令
  // LOOP:
  // beq zero,zero,LOOP 00000063
  it should "read both values in loop" in {
    test(
      new FetchWrapper(
      ),
    ) { c =>
      c.initialize(
        InstructionUtil.fromStringSeq32bit(Seq("00000063", "00000000")),
      )
      c.setPrediction(Seq(true, true))

      c.waitForCacheValid()
      c.io.cacheAddress(0).bits.expect(0x10000000)
      c.io.cacheAddress(0).bits.expect(0x10000000)

      c.io.cacheOutput(0).bits.expect("x00000063".U)
      c.io.cacheOutput(0).valid.expect(true)

      c.io.cacheOutput(1).bits.expect("x00000063".U)
      c.io.cacheOutput(1).valid.expect(true)

      c.clock.step()
    }
  }

  // 普通の命令と分岐先を認識できているか
  // 書き込む命令
  // nop                  00000013
  // beq zero,zero,LABEL  00000463
  // nop                  00000013
  // LABEL:
  it should "understand branch prediction=false" in {
    test(
      new FetchWrapper(
      ),
    ) { c =>
      c.initialize(
        InstructionUtil
          .fromStringSeq32bit(Seq("00000013", "00000463", "00000013")),
      )
      c.waitForCacheValid()
      c.io.nextPC.expect(0x10000004)
    }
  }

  // 普通の命令と分岐先を認識できているか
  // 書き込む命令
  // nop                  00000013
  // beq zero,zero,LABEL  00000463
  // nop                  00000013
  // LABEL:
  it should "understand branch prediction=true" in {
    test(
      new FetchWrapper(
      ),
    ) { c =>
      c.initialize(
        InstructionUtil
          .fromStringSeq32bit(Seq("00000013", "00000463", "00000013")),
      )
      c.waitForCacheValid()
      c.io.nextPC.expect(0x10000004)
    }
  }

  // ループで自身と同じアドレスに戻ってくるか
  // 書き込む命令
  // nop                00000013
  // LOOP:
  // beq zero,zero,LOOP 00000063
  it should "understand loop to self" in {
    test(
      new FetchWrapper(
      ),
    ) { c =>
      c.initialize(
        InstructionUtil.fromStringSeq32bit(Seq("00000013", "00000063")),
      )
      c.waitForCacheValid()
      c.io.nextPC.expect(0x10000004)

      c.clock.step()

      c.setExecutorBranchResult(Some(0x10000004))

      c.io.nextPC.expect(0x10000004)
    }
  }

  // ループで同じ命令に戻る
  // 書き込む命令
  // LOOP:
  // nop                00000013
  // beq zero,zero,LOOP 00000063
  it should "understand loop" in {
    test(
      new FetchWrapper(
      ),
    ).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.initialize(
        InstructionUtil.fromStringSeq32bit(Seq("00000013", "00000063")),
      )
      c.waitForCacheValid()
      c.io.nextPC.expect(0x10000004)
      c.clock.step()

      c.setExecutorBranchResult(Some(0x00000000))
      c.io.nextPC.expect(0x10000004)
      c.clock.step()

      c.io.nextPC.expect(0x10000004)
      c.clock.step()
    }
  }

  // jalで無限ループ
  // 書き込む命令
  // LOOP:
  // nop                00000013
  // j                  ffdff06f
  it should "understand jal loop" in {
    test(
      new FetchWrapper(
      ),
    ) { c =>
      c.initialize(
        InstructionUtil.fromStringSeq32bit(Seq("00000013", "ffdff06f")),
      )
      c.waitForCacheValid()
      c.io.nextPC.expect(0x10000000)
    }
  }

  // jalで無条件アドレス替え
  // 書き込む命令
  // START:
  // nop      00000013
  // j LABEL  0180006f
  // nop      00000013
  // nop      00000013
  // nop      00000013
  // nop      00000013
  // nop      00000013
  // LABEL:
  // nop      00000013
  // j START  fe1ff06f
  it should "understand jal jumps" in {
    test(
      new FetchWrapper(
      ),
    ) { c =>
      c.initialize(
        InstructionUtil.fromStringSeq32bit(
          Seq(
            "00000013",
            "0180006f",
            "00000013",
            "00000013",
            "00000013",
            "00000013",
            "00000013",
            "00000013",
            "fe1ff06f",
          ),
        ),
      )
      c.waitForCacheValid()
      c.io.nextPC.expect(0x1000001c)

      c.clock.step()
      c.waitForCacheValid()
      c.io.nextPC.expect(0x10000020)

      c.clock.step()
      c.waitForCacheValid()
      c.io.nextPC.expect(0x10000004)
    }
  }

  // Fenceを使う
  // 書き込む命令
  // nop    00000013
  // fence  0ff0000f
  // nop    00000013
  // nop    00000013
  it should "understand fence" in {
    test(
      new FetchWrapper(
      ),
    ).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.initialize(
        InstructionUtil.fromStringSeq32bit(
          Seq("00000013", "0ff0000f", "00000013", "00000013"),
        ),
      )
      c.waitForCacheValid()
      c.io.branchTypes(0).expect(BranchType.Next4)
      c.io.branchTypes(1).expect(BranchType.Fence)
      c.io.decoders.toBuffer(0).valid.expect(true)
      c.io.decoders.toBuffer(1).valid.expect(false)
      c.io.nextPC.expect(0x10000004)

      c.clock.step()
      c.io.nextPC.expect(0x10000004)
      c.io.decoders.toBuffer(0).valid.expect(false)
      c.io.decoders.toBuffer(1).valid.expect(false)

      c.clock.step()
      c.io.reorderBufferEmpty.poke(true)
      c.io.nextPC.expect(0x10000004)
      c.io.decoders.toBuffer(0).valid.expect(false)
      c.io.decoders.toBuffer(1).valid.expect(false)

      c.clock.step()
      c.io.loadStoreQueueEmpty.poke(true)
      c.io.nextPC.expect(0x10000004)
      c.io.decoders.toBuffer(0).valid.expect(false)
      c.io.decoders.toBuffer(1).valid.expect(false)

      c.clock.step()
      c.io.nextPC.expect(0x10000010)
      c.io.decoders.toBuffer(0).valid.expect(true)
      c.io.decoders.toBuffer(1).valid.expect(true)
    }
  }

  it should "check formal" in {
    symbiYosysCheck(new FetchWrapper())
  }
}
