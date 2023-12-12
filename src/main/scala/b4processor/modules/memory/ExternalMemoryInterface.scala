package b4processor.modules.memory

import b4processor.Parameters
import b4processor.connections.OutputValue
import b4processor.structures.memoryAccess.MemoryAccessWidth
import b4processor.utils.{B4RRArbiter, FIFO, PriorityArbiterWithIndex, Tag}
import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage
import b4processor.utils.axi.{BurstSize, BurstType, ChiselAXI, Lock, Response}

class ExternalMemoryInterface(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val instruction = Vec(params.threads, Flipped(new MemoryReadChannel()))
    val data = Flipped(new MemoryAccessChannels())
    val amo = Flipped(new MemoryAccessChannels())
    val coordinator = new ChiselAXI(64, 64)
  })

  // setDefaultOutputs
  locally {
    import io.coordinator._
    writeAddress.bits.BURST := BurstType.Incr
    writeResponse.ready := false.B
    write.valid := false.B
    readAddress.bits.USER := 0.U
    writeAddress.bits.USER := 0.U
    writeAddress.bits.PROT := 0.U
    readAddress.bits.CACHE := 0.U
    writeAddress.bits.ADDR := 0.U
    writeAddress.bits.LOCK := Lock.Normal
    write.bits.USER := 0.U
    writeAddress.bits.ID := 0.U
    readAddress.valid := false.B
    writeAddress.bits.CACHE := 0.U
    readAddress.bits.LEN := 0.U
    readAddress.bits.QOS := 0.U
    writeAddress.bits.SIZE := BurstSize.Size1
    write.bits.STRB := 0.U
    readAddress.bits.PROT := 0.U
    readAddress.bits.ADDR := 0.U
    readAddress.bits.LOCK := Lock.Normal
    readAddress.bits.SIZE := BurstSize.Size1
    read.ready := false.B
    write.bits.LAST := false.B
    readAddress.bits.ID := 0.U
    writeAddress.bits.QOS := 0.U
    writeAddress.bits.LEN := 0.U
    writeAddress.valid := false.B
    write.bits.DATA := 0.U
    readAddress.bits.BURST := BurstType.Incr
  }
  io.data.write.request.ready := false.B
  io.data.read.response.bits.value := 0.U
  io.data.read.response.bits.isError := false.B
  io.data.read.response.bits.tag := Tag(0, 0)
  io.data.read.request.ready := false.B
  io.data.write.response.valid := false.B
  io.data.write.response.bits := DontCare
  io.data.read.response.valid := false.B
  io.amo.read.response.valid := false.B
  io.amo.read.response.bits := 0.U.asTypeOf(new MemoryReadResponse())
  io.amo.write.response.valid := false.B
  io.amo.write.response.bits := 0.U.asTypeOf(new MemoryWriteResponse())
  for (tid <- 0 until params.threads) {
    io.instruction(tid).request.ready := false.B
    io.instruction(tid).response.valid := false.B
    io.instruction(tid).response.bits := 0.U.asTypeOf(new MemoryReadResponse())
  }

  // READ OPERATION -------------------------------------------
  val all_inputs =
    VecInit(io.instruction ++ Seq(io.data.read, io.amo.read))

  all_inputs foreach (i => {
    i.response.valid := false.B
    i.response.bits := 0.U.asTypeOf(new MemoryReadResponse())
  })

  val priorityArb = Module(
    new PriorityArbiterWithIndex(
      new MemoryReadRequest,
      all_inputs.length,
      Seq.fill(io.instruction.length)(0) ++ Seq(1) ++ Seq(2),
    ),
  )
  priorityArb.in zip all_inputs foreach { case (a, i) => a <> i.request }

  val transactionQueue = Queue(priorityArb.out, 8, useSyncReadMem = true)
  transactionQueue.ready := false.B

  val readQueue = Module(new FIFO(3)(new Bundle {
    val burstLength = UInt(8.W)
    val tag = new Tag
    val size = new MemoryAccessWidth.Type()
    val offset = UInt(3.W)
    val signed = Bool()
    val responseIndex = UInt(log2Up(all_inputs.length).W)
  }))
  readQueue.output.ready := false.B
  readQueue.input.valid := false.B
  readQueue.input.bits := DontCare

  val readQueued = RegInit(false.B)
  val burstLen = RegInit(0.U(8.W))
  when(!readQueue.full) {
    when(transactionQueue.valid) {
      val readTransaction = transactionQueue.bits.data
      locally {
        import io.coordinator.readAddress._
        valid := true.B
        bits.ADDR := readTransaction.address(63, 3) ## 0.U(3.W)
        bits.LEN := readTransaction.burstLength
        bits.SIZE := BurstSize.Size8
        bits.CACHE := "b0010".U
      }
      readQueue.input.valid := !readQueued
      readQueue.input.bits.burstLength := readTransaction.burstLength
      readQueue.input.bits.tag := readTransaction.outputTag
      readQueue.input.bits.offset := readTransaction.address(2, 0)
      readQueue.input.bits.size := readTransaction.size
      readQueue.input.bits.signed := readTransaction.signed
      readQueue.input.bits.responseIndex := transactionQueue.bits.index
      readQueued := true.B
      when(io.coordinator.readAddress.ready) {
        transactionQueue.ready := true.B
        readQueued := false.B
      }
    }

  }

  when(!readQueue.empty) {
    val resp_channel = all_inputs(readQueue.output.bits.responseIndex).response
    io.coordinator.read.ready := resp_channel.ready

    resp_channel.bits.tag := readQueue.output.bits.tag

    when(io.coordinator.read.valid) {
      when(io.coordinator.read.ready) {
        burstLen := burstLen + 1.U
        when(burstLen === readQueue.output.bits.burstLength) {
          readQueue.output.ready := true.B
          burstLen := 0.U
        }
      }
      resp_channel.valid := true.B
      val data = io.coordinator.read.bits.DATA
      val isError = io.coordinator.read.bits.RESP =/= Response.Okay
      resp_channel.bits.value := Mux(
        isError,
        5.U,
        Mux1H(
          Seq(
            (readQueue.output.bits.size === MemoryAccessWidth.Byte) -> Mux1H(
              (0 until 8).map(i =>
                (readQueue.output.bits.offset === i.U) -> Cat(
                  Mux(
                    readQueue.output.bits.signed && data(i * 8 + 7),
                    "xFFFF_FFFF_FFFF_FF".U,
                    0.U,
                  ),
                  data(i * 8 + 7, i * 8),
                ),
              ),
            ),
            (readQueue.output.bits.size === MemoryAccessWidth.HalfWord) -> Mux1H(
              Seq(0, 2, 4, 6).map(i =>
                (readQueue.output.bits.offset === i.U) -> Cat(
                  Mux(
                    readQueue.output.bits.signed && data(i * 8 + 15),
                    "xFFFF_FFFF_FFFF".U,
                    0.U,
                  ),
                  data(i * 8 + 15, i * 8),
                ),
              ),
            ),
            (readQueue.output.bits.size === MemoryAccessWidth.Word) -> Mux1H(
              Seq(0, 4).map(i =>
                (readQueue.output.bits.offset === i.U) -> Cat(
                  Mux(
                    readQueue.output.bits.signed && data(i * 8 + 31),
                    "xFFFF_FFFF".U,
                    0.U,
                  ),
                  data(i * 8 + 31, i * 8),
                ),
              ),
            ),
            (readQueue.output.bits.size === MemoryAccessWidth.DoubleWord) -> data,
          ),
        ),
      )
      resp_channel.bits.isError := isError
      resp_channel.bits.burstIndex := burstLen
    }
  }

  // ----------------------------------------
  // WRITE OPERATION ------------------------
  // ----------------------------------------
  val all_writes = VecInit(Seq(io.data.write, io.amo.write))
  all_writes foreach (w => {
    w.response.valid := false.B
    w.response.bits := 0.U.asTypeOf(new MemoryReadResponse())
    w.requestData.ready := false.B
  })

  val all_writes_data_queue = VecInit(
    Seq(io.data.write.requestData, io.amo.write.requestData) map (v =>
      Queue(v, 8, useSyncReadMem = true)
    ),
  )
  all_writes_data_queue foreach (w => {
    w.ready := false.B
  })

  val writePriorityArb = Module(
    new PriorityArbiterWithIndex(new MemoryWriteRequest(), 2, Seq(0, 0)),
  )
  writePriorityArb.in zip all_writes foreach { case (a, w) => a <> w.request }

  val writeTransactionQueue = {
    Queue(writePriorityArb.out, 8, useSyncReadMem = true)
  }
  writeTransactionQueue.ready := false.B

  val dataWriteQueue = Module(new FIFO(3)(new Bundle {
    val index = UInt(log2Up(all_writes.length).W)
    val burstLength = UInt(8.W)
  }))
  dataWriteQueue.input.valid := false.B
  dataWriteQueue.input.bits := DontCare
  dataWriteQueue.output.ready := false.B

//  dataWriteQueue.flush := false.B
  val writeResponseQueue = Module(new FIFO(3)(new Bundle {
    val tag = new Tag()
    val index = UInt(log2Up(all_writes.length).W)
  }))
  writeResponseQueue.output.ready := false.B
  writeResponseQueue.input.valid := false.B
  writeResponseQueue.input.bits := DontCare
//  writeResponseQueue.flush := false.B

  val writeQueued = RegInit(false.B)
  when(writeTransactionQueue.valid) {
    locally {
      import io.coordinator.writeAddress._
      valid := true.B
      bits.ADDR := writeTransactionQueue.bits.data.address
      bits.LEN := 0.U
      bits.SIZE := BurstSize.Size8
      bits.BURST := BurstType.Incr
      bits.CACHE := "b0010".U

      dataWriteQueue.input.valid := !writeQueued
      dataWriteQueue.input.bits.index := writeTransactionQueue.bits.index
      dataWriteQueue.input.bits.burstLength := writeTransactionQueue.bits.data.burstLen
      writeResponseQueue.input.valid := !writeQueued
      writeResponseQueue.input.bits.tag := writeTransactionQueue.bits.data.outputTag
      writeResponseQueue.input.bits.index := writeTransactionQueue.bits.index
      writeQueued := true.B
    }
    when(io.coordinator.writeAddress.ready) {
      writeTransactionQueue.ready := true.B
      writeQueued := false.B
    }
  }

  val burstCounter = RegInit(0.U(8.W))

  when(!dataWriteQueue.empty) {
    val data_channel =
      all_writes_data_queue(dataWriteQueue.output.bits.index)
    when(data_channel.valid) {
      io.coordinator.write.valid := true.B
      io.coordinator.write.bits.DATA := data_channel.bits.data
      io.coordinator.write.bits.STRB := data_channel.bits.mask
      io.coordinator.write.bits.LAST := burstCounter === dataWriteQueue.output.bits.burstLength

      when(io.coordinator.write.ready) {
        burstCounter := burstCounter + 1.U
        data_channel.ready := true.B
        when(burstCounter === dataWriteQueue.output.bits.burstLength) {
          dataWriteQueue.output.ready := true.B
          burstCounter := 0.U
        }
      }
    }
  }

  when(!writeResponseQueue.empty) {
    val write_response_channel =
      all_writes(writeResponseQueue.output.bits.index).response
    io.coordinator.writeResponse.ready := write_response_channel.ready
    val isError = io.coordinator.writeResponse.bits.RESP =/= Response.Okay
    when(io.coordinator.writeResponse.valid) {
      write_response_channel.valid := true.B
      write_response_channel.bits.tag := writeResponseQueue.output.bits.tag
      write_response_channel.bits.isError := isError
      write_response_channel.bits.value := Mux(isError, 7.U, 0.U)
      when(write_response_channel.ready) {
        writeResponseQueue.output.ready := true.B
      }
    }
  }

}

object ExternalMemoryInterface extends App {
  implicit val params = {
    Parameters(decoderPerThread = 1, tagWidth = 4)
  }
  ChiselStage.emitSystemVerilogFile(new ExternalMemoryInterface)
}
