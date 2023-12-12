package b4processor.modules.reservationstation

import b4processor.Parameters
import b4processor.utils.{ExecutorValue, Tag}
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ReservationStationWrapper(implicit params: Parameters)
    extends ReservationStation {
  def initialize(): Unit = {
    setExecutorReady(true)
    setDecoderInput(None)
    setExecutors(None)
  }

  def setExecutorReady(value: Boolean): Unit = {
    this.io.issue(0).ready.poke(value)
  }

  def setDecoderInput(
    programCounter: Option[Int],
    value1: Option[Int] = None,
    value2: Option[Int] = None,
  ): Unit = {
    this.io.decoder(0).entry.poke(ReservationStationEntry.zero)
    this.io.decoder(0).entry.valid.poke(programCounter.isDefined)
    if (value1.isDefined) {
      this.io.decoder(0).entry.sources(0).__internal_is_left.poke(false)
      this.io
        .decoder(0)
        .entry
        .sources(0)
        .__internal_common_data
        .poke(value1.get)
    }
    if (value2.isDefined) {
      this.io.decoder(0).entry.sources(0).__internal_is_left.poke(false)
      this.io
        .decoder(0)
        .entry
        .sources(0)
        .__internal_common_data
        .poke(value2.get)
    }
  }

  def setExecutors(values: Option[ExecutorValue]): Unit = {
    val bypassValue = io.collectedOutput.outputs
    bypassValue(0).valid.poke(values.isDefined)
    bypassValue(0).bits.value.poke(
      values.getOrElse(ExecutorValue(destinationTag = 0, value = 0)).value,
    )
    bypassValue(0).bits.tag.poke(
      Tag(
        0,
        values
          .getOrElse(ExecutorValue(destinationTag = 0, value = 0))
          .destinationTag,
      ),
    )

  }

  def expectExecutor(programCounter: Option[Int]): Unit = {
    this.io.issue(0).valid.expect(programCounter.isDefined)
    //    if (programCounter.isDefined)
    //      this.io.executor.bits.programCounter.expect(programCounter.get)
  }
}

class ReservationStationTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Reservation Station"
  implicit val params =
    Parameters(threads = 1, decoderPerThread = 1, tagWidth = 4)

  // エントリを追加してALUから値をうけとり、実行ユニットに回す
  it should "store a entry and release it" in {
    test(new ReservationStationWrapper())
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        c.initialize()
        c.setDecoderInput(programCounter = Some(1))
        c.clock.step()
        c.setDecoderInput(None)
        c.setExecutors(Some(ExecutorValue(destinationTag = 0, value = 0)))
        c.expectExecutor(None)
        c.clock.step()
        c.expectExecutor(Some(1))
        c.clock.step()
        c.expectExecutor(None)
        c.clock.step()
      }
  }

  // 空きがないときready=0になる
  it should "make decoder become not ready" in {
    test(new ReservationStationWrapper())
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        c.initialize()
        c.setExecutorReady(false)
        var loop = 0
        while (loop < 100 && c.io.decoder(0).ready.peek().litToBoolean) {
          loop += 1
          c.setDecoderInput(
            programCounter = Some(1),
            value1 = Some(2),
            value2 = Some(3),
          )
          c.clock.step()
        }
        c.io.decoder(0).ready.expect(false)

        c.clock.step()
      }
  }

  // 空きがなくなるまで命令を入れて
  it should "fill and flush instructions" in {
    test(new ReservationStationWrapper())
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        c.initialize()
        c.setExecutorReady(false)
        var loop = 0
        while (loop < 100 && c.io.decoder(0).ready.peekBoolean()) {
          loop += 1
          c.setDecoderInput(
            programCounter = Some(1),
            value1 = Some(2),
            value2 = Some(3),
          )
          c.clock.step()
        }
        c.io.decoder(0).ready.expect(false)

        c.clock.step()

        c.setExecutorReady(true)
        c.setDecoderInput(None)
        loop = 0
        while (loop < 100 && c.io.issue(0).valid.peekBoolean()) {
          loop += 1
          c.clock.step()
        }
        c.io.issue(0).valid.expect(false)
      }
  }
}
