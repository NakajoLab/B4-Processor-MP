package b4processor.connections

import b4processor.Parameters
import chisel3._
import b4processor.utils.Tag
import chisel3.util._

class MPQ2MPQ(implicit params: Parameters) extends Bundle{
  val request = new request()
  val response = Flipped(new response())
}

class request(implicit params: Parameters) extends Bundle{
  val ThreadId = UInt(log2Up(params.threads).W)
  val valid = Bool()
}

class response(implicit params: Parameters) extends Bundle{
  val SendData = UInt(64.W)
  val valid = Bool()
}

