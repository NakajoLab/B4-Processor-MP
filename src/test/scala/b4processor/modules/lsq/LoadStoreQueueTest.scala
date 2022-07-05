package b4processor.modules.lsq

import b4processor.Parameters
import b4processor.utils.{DecodeEnqueue, LSQ2Memory, LSQfromALU}
import b4processor.utils.ExecutorValue
import chisel3._
import chisel3.util.{BitPat, DecoupledIO}
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class LoadStoreQueueWrapper(implicit params: Parameters) extends LoadStoreQueue {

  def initialize(): Unit = {
    this.setDecoder()
    this.setOutputs()
    this.setReorderBuffer()
  }

  def setOutputs(values: Seq[Option[LSQfromALU]] = Seq.fill(params.runParallel + 1)(None)): Unit = {
    for (i <- 0 until params.runParallel + 1) {
      val output = this.io.outputCollector.outputs(i)
      val value = values(i)
      output.validAsResult.poke(value.exists(_.valid))
      output.validAsLoadStoreAddress.poke(value.exists(_.valid))
      output.value.poke(value.map(_.value).getOrElse(0))
      output.tag.poke(value.map(_.destinationtag).getOrElse(0))
      //      output.programCounter.poke(value.map(_.ProgramCounter).getOrElse(0))
    }
  }

  def setDecoder(values: Seq[Option[DecodeEnqueue]] = Seq.fill(params.runParallel)(None)): Unit = {
    for (i <- 0 until params.runParallel) {
      val decoder = this.io.decoders(i)
      val value = values(i)
      decoder.valid.poke(value.isDefined)
      decoder.bits.addressAndLoadResultTag.poke(value.map(_.addressTag).getOrElse(0))
      decoder.bits.storeDataTag.poke(value.map(_.storeDataTag).getOrElse(0))
      decoder.bits.storeData.poke(value.flatMap(_.storeData).getOrElse(0L))
      decoder.bits.storeDataValid.poke(value.map(_.storeData).exists(_.isDefined))
      decoder.bits.opcode.poke(value.map(_.opcode).getOrElse(0))
      decoder.bits.function3.poke(value.map(_.function3).getOrElse(0))
    }
  }

  def setReorderBuffer(DestinationTags: Seq[Int] = Seq.fill(params.maxRegisterFileCommitCount)(0),
                       valids: Seq[Boolean] = Seq.fill(params.maxRegisterFileCommitCount)(false)): Unit = {
    for (i <- 0 until params.maxRegisterFileCommitCount) {
      val tag = DestinationTags(i)
      val v = valids(i)
      io.reorderBuffer.destinationTag(i).poke(tag)
      io.reorderBuffer.valid(i).poke(v)
    }
  }

  def expectMemory(values: Seq[Option[LSQ2Memory]]): Unit = {
    for (i <- 0 until params.maxRegisterFileCommitCount) {
      this.io.memory(i).valid.expect(values(i).isDefined)
      if (values(i).isDefined) {
        this.io.memory(i).bits.address.expect(values(i).get.address)
        this.io.memory(i).bits.tag.expect(values(i).get.tag)
        this.io.memory(i).bits.isLoad.expect(values(i).get.isLoad)
        this.io.memory(i).bits.function3.expect(values(i).get.function3)
        this.io.memory(i).bits.data.expect(values(i).get.data)
      }
    }
  }
}

class LoadStoreQueueTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Load Store Queue"
  implicit val defaultParams = Parameters(tagWidth = 4, runParallel = 2, maxRegisterFileCommitCount = 2, debug = true)

  it should "Both Of Instructions Enqueue LSQ" in {
    test(new LoadStoreQueueWrapper) { c =>
      c.initialize()
      c.io.head.get.expect(0)
      c.io.tail.get.expect(0)
      c.io.decoders(0).ready.expect(true)
      c.io.decoders(1).ready.expect(true)
      c.setDecoder(values =
        Seq(Some(DecodeEnqueue(addressTag = 10, storeDataTag = 5, storeData = None, opcode = 3, function3 = 0)),
          Some(DecodeEnqueue(addressTag = 11, storeDataTag = 6, storeData = None, opcode = 3, function3 = 0))))
      c.clock.step(1)

      c.setDecoder()
      c.io.head.get.expect(2)
      c.io.tail.get.expect(0)
      c.clock.step(2)
    }
  }

  it should "invalid enqueue" in {
    test(new LoadStoreQueueWrapper) { c =>
      c.initialize()
      // 初期化
      c.io.head.get.expect(0)
      c.io.tail.get.expect(0)
      c.io.decoders(0).ready.expect(true)
      c.io.decoders(1).ready.expect(true)

      c.setDecoder(values =
        Seq(Some(DecodeEnqueue(addressTag = 10, storeDataTag = 5, storeData = None, opcode = 30, function3 = 0)),
          Some(DecodeEnqueue(addressTag = 11, storeDataTag = 6, storeData = None, opcode = 3, function3 = 0))))
      c.clock.step(1)

      c.setDecoder()
      c.io.head.get.expect(1)
      c.io.tail.get.expect(0)
      c.clock.step(2)
    }
  }

  it should "load check" in {
    // runParallel = 1, maxRegisterFileCommitCount = 1
    test(new LoadStoreQueueWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.initialize()
      // 初期化
      c.io.head.get.expect(0)
      c.io.tail.get.expect(0)
      c.io.decoders(0).ready.expect(true)
      c.io.memory(0).ready.poke(true)
      // c.io.memory(1).ready.poke(true) (if runParallel = 2)
      c.expectMemory(Seq(None, None))
      // c.expectMemory(Seq(None, None)) (if runParallel = 2)

      // 値のセット
      c.setDecoder(values =
        Seq(Some(DecodeEnqueue(addressTag = 10, storeDataTag = 5, storeData = Some(0), opcode = 3, function3 = 0)), None))
      c.clock.step(1)

      c.setDecoder()
      c.setOutputs(values = Seq(
        Some(LSQfromALU(valid = true, destinationtag = 10, value = 150)), None, None))
      c.io.head.get.expect(1)
      c.io.tail.get.expect(0)
      c.expectMemory(Seq(None, None))
      c.clock.step(1)

      // 値の確認
      c.expectMemory(values =
        Seq(Some(LSQ2Memory(address = 150, tag = 10, data = 0, isLoad = true, function3 = 0)), None))
      c.io.tail.get.expect(0)
      c.io.head.get.expect(1)
      c.clock.step(1)

      c.io.head.get.expect(1)
      c.io.tail.get.expect(1)
      c.clock.step(3)

    }
  }
  // (if runParallel = 2)
  // c.setDecoder(values =
  //   Seq(Some(DecodeEnqueue(addressTag = ???, storeDataTag = ???, storeData = ???, opcode = ???, function3 = ???)),
  //     Some(DecodeEnqueue(addressTag = ???, storeDataTag = ???, storeData = ???, opcode = ???, function3 = ???))))
  // c.setExecutor(values =
  //   Seq(Some(LSQfromALU(valid = ???, destinationtag = ???, value = ???)),
  //     Some(LSQfromALU(valid = ???, destinationtag = ???, value = ???))))
  //  c.expectMemory(values =
  //    Seq(Some(LSQ2Memory(address = ???, tag = ???, data = ???, opcode = ???, function3 = ???)),
  //      Some(LSQ2Memory(address = ???, tag = ???, data = ???, opcode = ???, function3 = ???))))

  it should "store check" in {
    // runParallel = 1, maxRegisterFileCommitCount = 1
    test(new LoadStoreQueueWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.initialize()
      // 初期化
      c.io.head.get.expect(0)
      c.io.tail.get.expect(0)
      c.io.decoders(0).ready.expect(true)
      c.io.memory(0).ready.poke(true)
      c.io.memory(1).ready.poke(true)
      // c.io.memory(1).ready.poke(true) (if runParallel = 2)
      c.expectMemory(Seq(None, None))
      // c.expectMemory(Seq(None, None)) (if runParallel = 2)

      // 値のセット
      c.setDecoder(values =
        Seq(Some(DecodeEnqueue(addressTag = 10, storeDataTag = 5, storeData = None, opcode = 35, function3 = 0)), None))
      c.clock.step(1)

      c.setDecoder()
      c.setOutputs(values = Seq(
        Some(LSQfromALU(valid = true, destinationtag = 5, value = 123)),
        None,
        None
      ))
      c.io.head.get.expect(1)
      c.io.tail.get.expect(0)
      c.clock.step(1)

      c.setOutputs(values = Seq(
        Some(LSQfromALU(valid = true, destinationtag = 10, value = 150)),
        None,
        None
      ))
      c.setReorderBuffer(valids = Seq(true, false), DestinationTags = Seq(10, 1))
      c.io.head.get.expect(1)
      c.io.tail.get.expect(0)
      c.expectMemory(Seq(None, None))
      c.clock.step(1)

      // 値の確認
      c.expectMemory(values =
        Seq(Some(LSQ2Memory(address = 150, tag = 10, data = 123, isLoad = false, function3 = 0)), None))
      c.io.head.get.expect(1)
      c.io.tail.get.expect(0)
      c.clock.step(1)

      c.io.head.get.expect(1)
      c.io.tail.get.expect(1)
      c.clock.step(1)
    }
  }
  // (if runParallel = 2)
  // c.setDecoder(values =
  //   Seq(Some(DecodeEnqueue(addressTag = ???, storeDataTag = ???, storeData = ???, opcode = ???, function3 = ???)),
  //     Some(DecodeEnqueue(addressTag = ???, storeDataTag = ???, storeData = ???, opcode = ???, function3 = ???))))
  // c.setExecutor(values =
  //   Seq(Some(LSQfromALU(valid = ???, destinationtag = ???, value = ???)),
  //     Some(LSQfromALU(valid = ???, destinationtag = ???, value = ???))))
  //  c.expectMemory(values =
  //    Seq(Some(LSQ2Memory(address = ???, tag = ???, data = ???, opcode = ???, function3 = ???)),
  //      Some(LSQ2Memory(address = ???, tag = ???, data = ???, opcode = ???, function3 = ???))))

  it should "2 Parallel load check" in {
    test(new LoadStoreQueueWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.initialize()
      // 初期化
      c.io.head.get.expect(0)
      c.io.tail.get.expect(0)
      c.io.decoders(0).ready.expect(true)
      c.io.decoders(1).ready.expect(true)
      c.io.memory(0).ready.poke(true)
      c.io.memory(1).ready.poke(true)
      c.expectMemory(Seq(None, None))

      // 値のセット
      c.setDecoder(values =
        Seq(Some(DecodeEnqueue(addressTag = 10, storeDataTag = 5, storeData = None, opcode = 3, function3 = 0)),
          Some(DecodeEnqueue(addressTag = 11, storeDataTag = 6, storeData = None, opcode = 3, function3 = 0))))
      c.clock.step(1)

      c.setDecoder()
      c.io.head.get.expect(2)
      c.io.tail.get.expect(0)
      c.setOutputs(values =
        Seq(Some(LSQfromALU(valid = true, destinationtag = 10, value = 150)),
          Some(LSQfromALU(valid = true, destinationtag = 11, value = 100)),
          None))
      c.clock.step(1)

      // 値の確認
      c.setOutputs()
      c.expectMemory(values =
        Seq(Some(LSQ2Memory(address = 150, tag = 10, data = 0, isLoad = true, function3 = 0)),
          Some(LSQ2Memory(address = 100, tag = 11, data = 0, isLoad = true, function3 = 0))))
      c.clock.step(2)

    }
  }

  it should "2 Parallel store check" in {
    test(new LoadStoreQueueWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.initialize()
      // 初期化
      c.io.head.get.expect(0)
      c.io.tail.get.expect(0)
      c.io.decoders(0).ready.expect(true)
      c.io.decoders(1).ready.expect(true)
      c.io.memory(0).ready.poke(true)
      c.io.memory(1).ready.poke(true)
      c.expectMemory(Seq(None, None))

      // 値のセット
      c.setDecoder(values =
        Seq(Some(DecodeEnqueue(addressTag = 10, storeDataTag = 5, storeData = None, opcode = 35, function3 = 0)),
          Some(DecodeEnqueue(addressTag = 11, storeDataTag = 6, storeData = Some(123), opcode = 35, function3 = 0))))
      c.clock.step(1)

      c.setDecoder()
      c.io.head.get.expect(2)
      c.io.tail.get.expect(0)
      c.setOutputs(values =
        Seq(Some(LSQfromALU(valid = true, destinationtag = 10, value = 150)),
          Some(LSQfromALU(valid = true, destinationtag = 5, value = 100)),
          None))
      c.clock.step(1)

      c.setOutputs(values =
        Seq(Some(LSQfromALU(valid = true, destinationtag = 8, value = 456)),
          Some(LSQfromALU(valid = true, destinationtag = 11, value = 789)),
          None))
      c.setReorderBuffer(valids = Seq(false, true), DestinationTags = Seq(1, 10))
      c.clock.step(1)

      // 値の確認
      c.setOutputs()
      c.setReorderBuffer(valids = Seq(true, false), DestinationTags = Seq(11, 15))
      c.expectMemory(values =
        Seq(Some(LSQ2Memory(address = 150, tag = 10, data = 100, isLoad = false, function3 = 0)), None))
      c.clock.step(1)

      c.setReorderBuffer()
      c.expectMemory(values =
        Seq(Some(LSQ2Memory(address = 789, tag = 11, data = 123, isLoad = false, function3 = 0)), None))
      c.clock.step(2)

    }
  }

  it should "2 Parallel check (1 clock wait)" in {
    test(new LoadStoreQueueWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.initialize()
      // 初期化
      c.io.head.get.expect(0)
      c.io.tail.get.expect(0)
      c.io.decoders(0).ready.expect(true)
      c.io.decoders(1).ready.expect(true)
      c.io.memory(0).ready.poke(true)
      c.io.memory(1).ready.poke(true)
      c.expectMemory(Seq(None, None))

      // 値のセット
      c.setDecoder(values =
        Seq(Some(DecodeEnqueue(addressTag = 10, storeDataTag = 5, storeData = None, opcode = 3, function3 = 0)),
          Some(DecodeEnqueue(addressTag = 11, storeDataTag = 6, storeData = None, opcode = 35, function3 = 0))))
      // load instruction + store instruction
      c.clock.step(1)

      c.setDecoder(values =
        Seq(Some(DecodeEnqueue(addressTag = 12, storeDataTag = 10, storeData = None, opcode = 100, function3 = 0)),
          Some(DecodeEnqueue(addressTag = 13, storeDataTag = 0, storeData = None, opcode = 3, function3 = 0))))
      // invalid instruction + load instruction
      c.io.head.get.expect(2)
      c.io.tail.get.expect(0)
      c.setOutputs(values =
        Seq(Some(LSQfromALU(valid = true, destinationtag = 10, value = 150)), // 1st load address
          Some(LSQfromALU(valid = true, destinationtag = 11, value = 100)),
          None)) // store address
      c.clock.step(1)

      // 値の確認
      c.setDecoder()
      c.io.head.get.expect(3)
      c.io.tail.get.expect(0)
      c.setOutputs(values =
        Seq(Some(LSQfromALU(valid = true, destinationtag = 12, value = 123)), // invalid
          Some(LSQfromALU(valid = true, destinationtag = 13, value = 500)),
          None)) // 2nd load address
      c.expectMemory(values =
        Seq(Some(LSQ2Memory(address = 150, tag = 10, data = 0, isLoad = true, function3 = 0)), None))
      c.clock.step(1)

      c.setOutputs(values =
        Seq(Some(LSQfromALU(valid = true, destinationtag = 6, value = 456)), // store data
          Some(LSQfromALU(valid = true, destinationtag = 14, value = 200)), // invalid
          None))
      // store命令を飛び越えてload命令を送出
      c.expectMemory(values =
        Seq(None, Some(LSQ2Memory(address = 500, tag = 13, data = 0, isLoad = true, function3 = 0))))
      c.clock.step(1)

      c.setReorderBuffer(valids = Seq(false, true), DestinationTags = Seq(1, 11))
      c.setOutputs()
      c.expectMemory(Seq(None, None))
      c.clock.step(1)

      c.expectMemory(values =
        Seq(Some(LSQ2Memory(address = 100, tag = 11, data = 456, isLoad = false, function3 = 0)), None))
      c.clock.step(2)
    }
  }

  // ロードがストアを追い越さないようにチェック
  // ストアのアドレスが確定したいないときにOverlap=falseとしてしまっていたバグのチェック
  it should "wait load for the overlapping store" in {
    // runParallel = 1, maxRegisterFileCommitCount = 1
    test(new LoadStoreQueueWrapper()(defaultParams.copy(runParallel = 1, maxRegisterFileCommitCount = 1))).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.initialize()
      // 初期化
      c.io.head.get.expect(0)
      c.io.tail.get.expect(0)
      c.io.decoders(0).ready.expect(true)
      c.io.memory(0).ready.poke(true)
      // c.io.memory(1).ready.poke(true) (if runParallel = 2)
      c.expectMemory(Seq(None))
      // c.expectMemory(Seq(None, None)) (if runParallel = 2)

      // ストア命令を追加
      c.setDecoder(values =
        Seq(Some(DecodeEnqueue(addressTag = 10, storeDataTag = 5, storeData = None, opcode = 35, function3 = 0))))

      c.clock.step()
      //ロード命令の追加
      c.setDecoder(values =
        Seq(Some(DecodeEnqueue(addressTag = 11, storeDataTag = 0, storeData = None, opcode = 3, function3 = 0))))
      c.io.head.get.expect(1)
      c.io.tail.get.expect(0)


      c.clock.step()
      c.initialize()
      c.io.head.get.expect(2)
      c.io.tail.get.expect(0)

      c.clock.step(2)
      // ロード命令のアドレス値を先に指定
      c.setOutputs(values = Seq(
        Some(LSQfromALU(valid = true, destinationtag = 11, value = 150)), None))
      c.expectMemory(Seq(None))

      c.clock.step()
      c.initialize()
      // まだ出力はない
      c.expectMemory(Seq(None))

      c.clock.step()
      // ストア命令の値確定
      c.setOutputs(values = Seq(
        Some(LSQfromALU(valid = true, destinationtag = 10, value = 150)),
        Some(LSQfromALU(valid = true, destinationtag = 5, value = 300))))
      c.setReorderBuffer(Seq(10), Seq(true))

      c.clock.step()
      c.initialize()
      // ストア命令送出
      c.expectMemory(values =
        Seq(Some(LSQ2Memory(address = 150, tag = 10, data = 300, isLoad = false, function3 = 0))))

      c.clock.step()
      // ロード命令送出
      c.expectMemory(values =
        Seq(Some(LSQ2Memory(address = 150, tag = 11, data = 0, isLoad = true, function3 = 0))))

      c.clock.step()

      c.io.tail.get.expect(2)
      c.io.head.get.expect(2)

      c.clock.step(3)
    }
  }

}