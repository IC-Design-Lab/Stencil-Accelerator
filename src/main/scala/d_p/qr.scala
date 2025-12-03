
import Binary_Modules.BinaryDesigns._

import chisel3._

import circt.stage.ChiselStage





import chisel3._
import circt.stage.ChiselStage

object Elaborate2 {
  def main(args: Array[String]): Unit = {
    (new ChiselStage).execute(
      Array("--target", "verilog", "--target-dir", "verification/dut"),
      Seq(chisel3.stage.ChiselGeneratorAnnotation(() => new dot(32, 32)))
    )
  }
}

object Elaborate3 {
  def main(args: Array[String]): Unit = {
    (new ChiselStage).execute(
      Array("--target", "verilog", "--target-dir", "verification/dut"),
      Seq(chisel3.stage.ChiselGeneratorAnnotation(() => new axpy(32, 32)))
    )
  }
}




  class dot(bw: Int, streaming_width: Int) extends Module {

    val io = IO {
      new Bundle() {

        val in_a = Input(Vec(streaming_width, UInt(bw.W)))
        val in_b = Input(Vec(streaming_width, UInt(bw.W)))
        val out_s = Output(UInt((bw).W))

      }
    }



    // Step 1: multiply pairs
    val products = (0 until streaming_width).map { i =>
      val m = Module(new FP_mult(bw)).io
      m.aclk:= this.clock
      m.s_axis_a_tdata:= io.in_a(i)
      m.s_axis_b_tdata:= io.in_b(i)
      m.s_axis_a_tvalid:= true.B
      m.s_axis_b_tvalid:= true.B

      m.m_axis_result_tdata
    }

    // Step 2: reduce with adders
    val sum = products.reduce { (x, y) =>
      val a = Module(new FP_adder(bw)).io
      a.aclk:= this.clock
      a.s_axis_a_tdata := x
      a.s_axis_b_tdata := y
      a.s_axis_a_tvalid:= true.B
      a.s_axis_b_tvalid:= true.B
      a.m_axis_result_tdata
    }

    io.out_s := sum
  }

  class FP_mult(bw:Int)extends BlackBox{
    val io = IO {
      new Bundle() {
        val aclk = Input(Clock())
        val s_axis_a_tvalid = Input(Bool())
        val s_axis_b_tvalid = Input(Bool())
        val s_axis_a_tdata = Input(UInt(bw.W))
        val s_axis_b_tdata = Input(UInt(bw.W))
        val m_axis_result_tvalid = Output(Bool())
        val m_axis_result_tdata = Output(UInt(bw.W))
      }
    }
  }


  class FP_adder(bw:Int)extends BlackBox{
    val io = IO {
      new Bundle() {
        val aclk = Input(Clock())
        val s_axis_a_tvalid = Input(Bool())
        val s_axis_b_tvalid = Input(Bool())
        val s_axis_a_tdata = Input(UInt(bw.W))
        val s_axis_b_tdata = Input(UInt(bw.W))
        val m_axis_result_tvalid = Output(Bool())
        val m_axis_result_tdata = Output(UInt(bw.W))
      }
    }
  }

  class axpy(bw: Int, streaming_width: Int) extends Module {
    val io = IO(new Bundle {
      val in_a   = Input(UInt(bw.W))         // scalar multiplier
      val in_b   = Input(Vec(streaming_width, UInt(bw.W))) // input vector x
      val in_c = Input(Vec(streaming_width, UInt(bw.W))) // input vector y
      val out_s= Output(Vec(streaming_width, UInt(bw.W)))// output vector
    })


    for (i <- 0 until streaming_width) {
      val m = Module(new FP_mult(bw)).io
      m.aclk:=this.clock
      m.s_axis_a_tdata:= io.in_a
      m.s_axis_b_tdata:= io.in_b(i)
      m.s_axis_a_tvalid:= true.B
      m.s_axis_b_tvalid:= true.B

      m.m_axis_result_tdata

      val a = Module(new FP_adder(bw)).io
      a.aclk:= this.clock
      a.s_axis_a_tdata := m.m_axis_result_tdata
      a.s_axis_b_tdata := io.in_c(i)
      a.s_axis_a_tvalid:= true.B
      a.s_axis_b_tvalid:= true.B
      io.out_s(i) := a.m_axis_result_tdata
    }
  }

