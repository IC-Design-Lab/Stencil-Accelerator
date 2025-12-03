package stencil


import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation}

import chisel3.util.{log2Ceil, MuxCase, ShiftRegister,Cat, switch, is}
import circt.stage.{ChiselStage}
import firrtl.options.TargetDirAnnotation
import java.io._

import FPPackageMario.FP_Modules.FPUnits._




object Elaborate2 {
  def main(args: Array[String]): Unit = {
    // make sure directory exists
    val dir = new File("verification/dut")
    dir.mkdirs()

    val sw2 = new PrintWriter(new File(dir, "stencil_core_2d.sv"))
    sw2.println(circt.stage.ChiselStage.emitSystemVerilog(new stencil_core_2d(32, 3 ,10, 13,11)))
    sw2.close()
  }
}

object datapath {
  def main(args: Array[String]): Unit = {
    // make sure directory exists
    val dir = new File("verification/dut")
    dir.mkdirs()

    val sw2 = new PrintWriter(new File(dir, "datapath_2d.sv"))
    sw2.println(circt.stage.ChiselStage.emitSystemVerilog(new datapath_2d(32, 3 ,24, 12,10,13,11)))
    sw2.close()
  }
}



class datapath_2d(bw: Int, sw: Int, rows: Int, cols:Int, mult_pd: Int, add_pd: Int, shape:Int) extends Module{
  val io = IO(new Bundle(){

    //  val clk = Input(Clock())
    //  val rst = Input(Bool())
    val datapath_data_in = Input(UInt((bw*(sw)).W))
    //val datapath_weights_in = Input(UInt((bw*(cols)).W))
    val datapath_ready_in = Input(Bool())
    val datapath_data_out1 = Output(UInt(bw.W))
    val datapath_data_out2 = Output(UInt(bw.W))
    val datapath_data_out3 = Output(UInt(bw.W))
    val datapath_valid_out1 = Output(Bool())
    val datapath_valid_out2 = Output(Bool())
    val datapath_valid_out3 = Output(Bool())
  })

  // min # of scs (horizontally) = sw of sc
  // min # of scs per row = 1

  val WRITE_DELAY = 1 // 1 cycle to enable Load_st and  W_en
  val READ_DELAY = 1 + 1 + 1 // 1 cycle to enable r_en and one to enable compute_st and another cycle for reading from memory

  val fsm = Module(new sram_controller(bw,rows,cols,sw))
  fsm.io.input_ready:= io.datapath_ready_in

  val input_sram_m = Module (new SRAM(bw, rows,cols, sw)).io
  input_sram_m.dina:= ShiftRegister(io.datapath_data_in, WRITE_DELAY )
  input_sram_m.addra:= fsm.io.addr_a
  input_sram_m.addrb:=fsm.io.addr_b
  input_sram_m.ena := fsm.io.w_en
  input_sram_m.enb := fsm.io.r_en


  val dest_sc = ShiftRegister(fsm.io.destination, READ_DELAY)



  val norm_factor = Seq.fill(sw)("h3f800000".U(bw.W))
  val norm_coeff = Cat(norm_factor)
  val sc_bundle = for (i <- 0 until sw) yield {
    val sc = Module(new stencil_core_2d(bw,sw, mult_pd, add_pd, shape)).io
    sc.in_weight:= norm_coeff
    sc.in_matrix:= input_sram_m.doutb

    sc
  }

  for (i <- 0 until sw) {sc_bundle(i).in_ready := dest_sc(i)}

  io.datapath_data_out1 := sc_bundle(0).out_data
  io.datapath_data_out2 := sc_bundle(1).out_data
  io.datapath_data_out3 := sc_bundle(2).out_data

  io.datapath_valid_out1 := sc_bundle(0).out_valid
  io.datapath_valid_out2 := sc_bundle(1).out_valid
  io.datapath_valid_out3 := sc_bundle(2).out_valid
}

class sram_controller(bw: Int, rows: Int, cols:Int, sw:Int) extends Module{
  val io = IO(new Bundle(){


    val input_ready = Input(Bool())
    val addr_b = Output(UInt((bw).W))
    val destination = Output(UInt(sw.W))
    val addr_a = Output(UInt((bw).W))
    val w_en = Output(Bool())
    val r_en = Output(Bool())

  })
// latencies
  val MEM_READ = 1
  val SRAM_CC = rows


  //states
  val Load_st = RegInit(false.B) // load input sram matrix state
  val Compute_st = RegInit(false.B) // load from sram to buff state
  val Idle_st = RegInit(false.B)
  //regs
  val input_addra = RegInit(0.U(bw.W)) // write addr of input matrix
  val input_addrb = RegInit(0.U(bw.W))  // read addr from the input matrix
  val write_en =RegInit(false.B)
  val read_en =RegInit(false.B)
  val dest_sc = RegInit(1.U(sw.W))
  val dest_mask = (1.U(sw.W))
  val ld_cy_cnt = RegInit(0.U(bw.W))  // full cycle of loading sram into buffers
  val comp_cy_cnt = RegInit(0.U(bw.W)) // counter for a full cycle of columns
  val total_cnt = RegInit(0.U(bw.W))

  io.addr_b := input_addrb
  io.addr_a := input_addra
  io.r_en:= read_en
  io.w_en := write_en
  io.destination := dest_sc



  when (io.input_ready === true.B){
    Load_st := true.B
    write_en := true.B
  }


when(Load_st === true.B & write_en === true.B){
    input_addra := input_addra + 1.U
    ld_cy_cnt := ld_cy_cnt + 1.U
  when(ld_cy_cnt === SRAM_CC.U){
    Load_st := false.B
    input_addra := 0.U
    write_en := 0.U
    ld_cy_cnt := 0.U
    Compute_st := true.B
  }

}.elsewhen(Compute_st === true.B){
  read_en := true.B
  when(read_en){
    input_addrb := input_addrb + 1.U
    comp_cy_cnt := comp_cy_cnt + 1.U
    total_cnt := total_cnt + 1.U}
  when(total_cnt === SRAM_CC.U){
    comp_cy_cnt := 0.U
    input_addrb := 0.U
    read_en := false.B
    Compute_st := false.B
    total_cnt := 0.U
  }.elsewhen(comp_cy_cnt === cols.U){
    comp_cy_cnt := 0.U
  }

  when(comp_cy_cnt < (sw-1).U ){
    dest_sc := dest_sc | (dest_sc << 1)(sw-1, 0)
  }.elsewhen(comp_cy_cnt > (sw-1).U & comp_cy_cnt <= (cols-(sw-1)).U){
    dest_sc := ~(0.U(sw.W))
  }.elsewhen( comp_cy_cnt > (cols-(sw)).U & comp_cy_cnt < cols.U){
    dest_sc := (dest_sc << 1)(sw-1, 0)
  }.elsewhen(comp_cy_cnt === cols.U){
    dest_sc := (1.U(sw.W))
  }
}





}

class SRAM(bw: Int, rows: Int, cols: Int, sw: Int) extends Module{

  val io = IO(new Bundle(){

   // val clka = Input(Clock())
   // val clkb = Input(Clock())
    val ena = Input(Bool())
    val enb = Input(Bool())
   // val wea = Input(UInt((rows).W)) // for masking
    val addra = Input(UInt((bw).W))
    val addrb = Input(UInt((bw).W))
    val dina = Input(UInt((sw*bw).W))
    val doutb = Output(UInt((sw*bw).W))
  })


  val memory = SyncReadMem(rows, UInt((sw*bw).W))

  val doutReg = Reg(UInt((sw*bw).W))
  when(io.enb) {
    doutReg := memory.read(io.addrb, io.enb)
  }
  io.doutb := doutReg
  when(io.ena){
     memory.write(io.addra, io.dina)
  }
}


// for shape = 12
// to pipeline the cross-shape, having two parallel dot modules, one with zero-padding and
//the other without, and using the column_counter, and a mux before the first adder
// it could switch between the central column and other edge columns
// or a MAC in parallel to the DOT, and shift the ouput to match the latency of the DOT then have
class stencil_core_2d( bw: Int, sw:Int, mult_pd: Int, add_pd: Int, shape:Int ) extends Module {

  //shape parameter:
  // B (Box) = 1011 (11)
  // C (Cross) = 1100 (12)

  val io = IO(new Bundle() {

    //  val clk = Input(Clock())
    //  val rst = Input(Bool())

    val in_matrix = Input(UInt((bw * (sw)).W))
    val in_weight = Input(UInt((bw * (sw)).W))
    val in_ready = Input(Bool())
    val out_data = Output(UInt(bw.W))
    val out_valid = Output(Bool())
  })


  val ADD_CC = 13
  val MULT_CC = 10
  val ADD_TREE_CC = log2Ceil(sw - 1) * ADD_CC // latency of the cascading adders except for the last one
  val DOT_CC = MULT_CC + ADD_TREE_CC + ADD_CC // latency of the full dot product
  val ACC_CC = ADD_CC * (sw - 1)
  val DATAPATH_CC = DOT_CC + ADD_CC * (sw - 1) // latency of a full stencil computation
  val OUT_VAL_CC = DOT_CC + ADD_CC * (sw - 1) + (sw)
  ////
  // the latency of the sc is as follows:
  // DOT latency + adder(0) latency + adder index+1 + adder(1) latency +adder index +1 + .... etc
  ////////////////////////////////////////////////////


  val data_reg = RegInit(0.U((sw * bw).W))





    ///preprocess matrix vector if shape = 12 (C)
    val middle = ~(0.U(bw.W))
    val edge = 0.U(((sw - 1) / 2 * bw).W)
    val mask = Cat(edge, middle, edge)
    dontTouch(mask)
    // column tracker, for zero-padding
    val column_cnt = RegInit(0.U(log2Ceil(sw).W))
    dontTouch(column_cnt)

    column_cnt := Mux(column_cnt === (sw - 1).U, 0.U, column_cnt + 1.U)


    val dot = Module(new Dot_real(bw, sw, mult_pd, add_pd)).io
    //add a mux before in_matrix, selector is shape parameter & column_cnt

    when(io.in_ready){
      data_reg := io.in_matrix
    }

    if (shape == 12) {
      dot.in_data := Mux((column_cnt === ((sw - 1) / 2).U), data_reg, data_reg & mask)
    }
    else {
      dot.in_data := data_reg
    }
    dot.in_weight := io.in_weight
    dot.in_ready := io.in_ready


    //shift reg that spans the sc
    val acc_reg = RegInit(VecInit(Seq.fill(ACC_CC)(0.U(bw.W)))) // shift reg that goes between dot and external adder for acc operation
    acc_reg(0) := dot.out_data
    for (z <- 1 until ACC_CC) {
      acc_reg(z) := acc_reg(z - 1)
    }

    val sc_count = RegInit(0.U(bw.W)) // counter for full stencil - consider increasing size of reg
    dontTouch(sc_count)
    sc_count := sc_count + 1.U
    when(sc_count === ((OUT_VAL_CC).U)) {
      sc_count := 0.U
    }

    // signal to indicate when output is valid
    val output_valid = ShiftRegister(io.in_ready, OUT_VAL_CC)
    io.out_valid := output_valid

    //shift regs for systolic adders
    val shift_regs: Seq[UInt] = (0 until sw).map { i =>

      if (i == 0) RegNext(dot.out_data) // first adder, 1 cycle delay
      else ShiftRegister(dot.out_data, (ADD_CC * i) - i)
    }
    //external adders instantitation
    val systolic_adders = for (i <- 0 until sw - 1) yield {
      val adder = Module(new FP_add(bw, 13)).io
      adder.in_en := true.B
      adder.in_valid := true.B
      adder
    }

    //external adders connections
    for (i <- 0 until sw - 1) {
      if (i == 0) { // first adder connects to dot with 1 shift reg
        systolic_adders(i).in_a := shift_regs(i)
        systolic_adders(i).in_b := dot.out_data
      }
      else { // remaining adders connect in series with a shift reg of 13 ccs inbetween
        systolic_adders(i).in_a := shift_regs(i)
        systolic_adders(i).in_b := systolic_adders(i - 1).out_s
      }

      io.out_data := systolic_adders(sw - 2).out_s
    }
 // }
}

class Dot_real(bw: Int, sw: Int, mult_pd: Int, add_pd: Int ) extends Module{
  val io = IO(new Bundle(){

    //  val clk = Input(Clock())
    //  val rst = Input(Bool())

    val in_data = Input(UInt((bw*sw).W))
    val in_weight = Input(UInt((bw*sw).W))
    val in_ready = Input(Bool())
    val out_data = Output(UInt(bw.W))
  })
  val ADD_CC = 13
  val MULT_CC = 10
  val ADD_TREE_CC = log2Ceil(sw-1)*ADD_CC
  val DOT_CC = MULT_CC + ADD_TREE_CC + ADD_CC


  val matrix = (0 until sw).map(i=> io.in_data(sw*bw-(i*bw)-1,(sw * bw - (bw * (i + 1)))))
  val weights = (0 until sw).map(i=> io.in_weight(sw*bw-(i*bw)-1,(sw * bw - (bw * (i + 1)))))
  val terms = (0 until sw).map{ i =>

    val mult = Module(new FP_mult(bw,mult_pd)).io
    mult.in_valid:= true.B
    mult.in_en:=true.B
    mult.in_a:= matrix(i)
    mult.in_b:= weights(i)
    mult.out_s
  }


  def Redux_Tree(inputs: Seq[UInt], depth: Int = 0): UInt = {


    if (inputs.length == 1) {inputs.head}
    else {
      val pairs = inputs.grouped(2).toList
      val next = pairs.map {
      case Seq(a, b) =>
        val add = Module(new FP_add(bw,add_pd)).io
        add.in_en := true.B
        add.in_valid := true.B
        add.in_a := a
        add.in_b := b
        add.out_s

      case Seq(single) =>
        ShiftRegister(single, ADD_CC)
  }
  Redux_Tree(next)
}

  }
  io.out_data := Redux_Tree(terms)
}