package b4processor.utils

import b4processor.{B4Processor, Parameters}
import chisel3._
import chiseltest._
import chisel3.util._
import _root_.circt.stage.ChiselStage

class B4ProcessorWithMemory()(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val simulation = Flipped(Valid(UInt(64.W)))
    val registerFileContents =
      if (params.debug) Some(Output(Vec(params.threads, Vec(32, UInt(64.W)))))
      else None
    val accessMemoryAddress = new Bundle {
      val readAddress = Valid(UInt(64.W))
      val readData = Valid(UInt(64.W))
      val writeAddress = Valid(UInt(64.W))
      val writeData = Valid(UInt(64.W))
    }
    val simulationIO = new Bundle {
      val input = Flipped(Decoupled(UInt(8.W)))
      val output = Decoupled(UInt(8.W))
    }
  })
  val core = Module(new B4Processor)
  val axiMemory = Module(new SimpleAXIMemoryWithSimulationIO(1024*1024*512))
  core.axi <> axiMemory.axi
  io.simulation <> axiMemory.simulationSource.input

  axiMemory.simulationIO <> io.simulationIO

  if (params.debug)
    io.registerFileContents.get <> core.registerFileContents.get

  io.accessMemoryAddress.readAddress.valid := core.axi.readAddress.valid
  io.accessMemoryAddress.readAddress.bits := core.axi.readAddress.bits.ADDR
  io.accessMemoryAddress.readData.valid := core.axi.read.valid
  io.accessMemoryAddress.readData.bits := core.axi.read.bits.DATA
  io.accessMemoryAddress.writeAddress.valid := core.axi.writeAddress.valid
  io.accessMemoryAddress.writeAddress.bits := core.axi.writeAddress.bits.ADDR
  io.accessMemoryAddress.writeData.valid := core.axi.write.valid
  io.accessMemoryAddress.writeData.bits := core.axi.write.bits.DATA

  def initialize(instructions: String, binary: Boolean = false): Unit = {
    val memoryInit =
      if (binary)
        InstructionUtil.fromBinaryFile(instructions)
      else
        InstructionUtil.fromFile8bit(instructions + ".hex")
    this.io.simulation.valid.poke(true)
    this.io.simulation.bits.poke(memoryInit.length)
    for (i <- memoryInit.indices) {
      this.clock.step()
      this.io.simulation.bits.poke(memoryInit(i))
    }
    for (i <- Seq.fill(20)(0)) {
      this.clock.step()
      this.io.simulation.bits.poke(i)
    }
    this.clock.step()
    this.io.simulation.valid.poke(false)
    this.clock.step()
  }

  def initialize64(instructions: String, binary: Boolean = false): Unit = {
    val memoryInit =
      if (binary)
        InstructionUtil.fromBinaryFile(instructions)
      else
        InstructionUtil.fromFile64bit(instructions + ".64.hex")
    this.io.simulation.valid.poke(true)
    this.io.simulation.bits.poke(memoryInit.length)
    for (i <- memoryInit.indices) {
      this.clock.step()
      this.io.simulation.bits.poke(memoryInit(i))
    }
    for (i <- Seq.fill(20)(0)) {
      this.clock.step()
      this.io.simulation.bits.poke(i)
    }
    this.clock.step()
    this.io.simulation.valid.poke(false)
    this.clock.step()
  }

  def checkForWrite(address: UInt, value: UInt, timeout: Int = 500): Unit = {
    this.clock.setTimeout(timeout)
    while (
      this.io.accessMemoryAddress.writeAddress.valid.peekBoolean() &&
      this.io.accessMemoryAddress.writeAddress.bits.peek() == address
    )
      this.clock.step()
    this.io.accessMemoryAddress.writeAddress.bits.expect(address)
    while (
      this.io.accessMemoryAddress.writeData.valid.peekBoolean() &&
      this.io.accessMemoryAddress.writeData.bits.peek() == value
    )
      this.clock.step()
    this.io.accessMemoryAddress.writeData.bits.expect(value)
    this.clock.step(10)
  }

  def checkForRegister(
    regNum: Int,
    value: BigInt,
    timeout: Int = 500,
    thread: Int = 0,
  ): Unit = {
    this.clock.setTimeout(timeout)
    while (this.io.registerFileContents.get(thread)(regNum).peekInt() != value)
      this.clock.step()
    this.io.registerFileContents.get(thread)(regNum).expect(value)
  }

  def checkForOutput(
    value: Char,
    timeout: Int = 500,
    print_value: Boolean = false,
  ): Unit = {
    this.clock.setTimeout(timeout)
    while (!this.io.simulationIO.output.valid.peekBoolean())
      this.clock.step()
    this.io.simulationIO.output.ready.poke(true)
    this.io.simulationIO.output.bits.expect(value)
    if (print_value) {
      print(this.io.simulationIO.output.bits.peekInt().toChar)
    }
    this.clock.step()
    this.io.simulationIO.output.ready.poke(false)
  }

  def checkForOutputAny(
    timeout: Int = 500,
    print_value: Boolean = false,
  ): Unit = {
    this.clock.setTimeout(timeout)
    while (!this.io.simulationIO.output.valid.peekBoolean())
      this.clock.step()
    this.io.simulationIO.output.ready.poke(true)
    if (print_value) {
      print(this.io.simulationIO.output.bits.peekInt().toChar)
    }
    this.clock.step()
    this.io.simulationIO.output.ready.poke(false)
  }

  def getOutput(timeout: Int = 500, print_value: Boolean = false): Char = {
    this.clock.setTimeout(timeout)
    while (!this.io.simulationIO.output.valid.peekBoolean())
      this.clock.step()
    this.io.simulationIO.output.ready.poke(true)
    val c = this.io.simulationIO.output.bits.peekInt().toChar
    if (print_value) {
      print(this.io.simulationIO.output.bits.peekInt().toChar)
    }
    this.clock.step()
    this.io.simulationIO.output.ready.poke(false)
    c
  }

  def checkForRegisterChange(
    regNum: Int,
    value: BigInt,
    timeout: Int = 500,
    thread: Int = 0,
  ): Unit = {
    this.clock.setTimeout(timeout)
    while (this.io.registerFileContents.get(thread)(regNum).peekInt() == 0)
      this.clock.step()
    this.io.registerFileContents.get(thread)(regNum).expect(value)
    this.clock.step(3)
  }
}

object B4ProcessorWithMemory extends App {
  implicit val params = Parameters(
//    debug = true,
    threads = 1,
    decoderPerThread = 2,
    maxRegisterFileCommitCount = 4,
    tagWidth = 5,
    loadStoreQueueIndexWidth = 3,
  )
  ChiselStage.emitSystemVerilogFile(new B4ProcessorWithMemory())
}
