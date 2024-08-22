package b4processor.modules.outputcollector

import _root_.circt.stage.ChiselStage
import b4processor.Parameters
import b4processor.connections.{CollectedOutput, OutputValue}
import b4processor.utils.{
  B4RRArbiter,
  FIFO,
  FormalTools,
  MMArbiter,
  PassthroughBuffer,
  SignalSplitWith,
}
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._

class OutputCollector2(implicit params: Parameters)
    extends Module
    with FormalTools {
  val io = IO(new Bundle {
    val outputs = Vec(params.threads, new CollectedOutput)
    val executor = Flipped(Vec(params.executors, Irrevocable(new OutputValue)))
    val dataMemory = Flipped(Irrevocable(new OutputValue))
    val amo = Flipped(Irrevocable(new OutputValue))
    val csr = Flipped(Vec(params.threads, Irrevocable(new OutputValue)))
    val pextExecutor =
      Flipped(Vec(params.pextExecutors, Irrevocable(new OutputValue)))
    val sendReceiveQueue = Flipped(Vec(params.threads, Irrevocable(new OutputValue)))
  })

  val allInputs =
    io.csr ++ Seq(io.amo, io.dataMemory ) ++ io.pextExecutor ++ io.executor ++ io.sendReceiveQueue

  val bufferedInputs = allInputs map { PassthroughBuffer(_) }

  val splitted = bufferedInputs map {
    SignalSplitWith(params.threads, _) { _.tag.threadId }
  }

//  val mmarb = Seq.fill(params.threads)(
//    Module(
//      new MMArbiter(new OutputValue, splitted.length, params.parallelOutput)
//    )
//  )

  val splittedTransposed = Wire(
    Vec(
      splitted(0).length,
      Vec(splitted.length, Decoupled(new OutputValue()).cloneType),
    ),
  )

  for (s <- splitted.indices) {
    for (i <- splitted(s).indices) {
      splittedTransposed(i)(s) <> splitted(s)(i)
    }
  }

  val mmarb = splittedTransposed map { MMArbiter(params.parallelOutput, _) }

  for (t <- 0 until params.threads) {
    for (i <- 0 until params.parallelOutput) {
      io.outputs(t).outputs(i).valid := mmarb(t)(i).valid
      io.outputs(t).outputs(i).bits := mmarb(t)(i).bits
      when(mmarb(t)(i).valid) {
        assert(
          mmarb(t)(i).bits.tag.threadId === t.U,
          s"check for thread $t, output ${i}",
        )
      }
      takesEveryValue(io.outputs(t).outputs(i).valid)
      io.outputs(t).outputs(i).bits.tag.threadId := t.U
      mmarb(t)(i).ready := true.B
    }
    // I added all signals to the splitter so this should not be an issue
//    assume(
//      io.csr(t).bits.tag.threadId === t.U,
//      s"csr outputs should have correct threadid ${t}"
//    )
  }

  for (i <- allInputs) {
    cover(i.ready)
  }

  // if all inputs are !valid
  when(
    !(splittedTransposed map (_ map (_.valid) reduce (_ || _)) reduce (_ || _)),
  ) {
    // all outputs should not be valid
    assert(
      !(io.outputs map (_.outputs map (_.valid) reduce (_ || _)) reduce (_ || _)),
    )
  }
}

object OutputCollector2 extends App {
  implicit val params = Parameters(threads = 4, executors = 4)
  ChiselStage.emitSystemVerilogFile(new OutputCollector2)
}
