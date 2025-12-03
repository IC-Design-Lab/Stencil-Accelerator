


import Binary_Modules.BinaryDesigns._
import chisel3._

import circt.stage.ChiselStage
import chisel3.util._
import chisel3.util.{log2Ceil, log2Floor}



import java.io.PrintWriter
import scala.collection.mutable

object hh_datapath_chisel {

  def main(args: Array[String]): Unit = {
    val sw2 = new PrintWriter("qr16x8_chisel.sv")
    sw2.println(getVerilogString(new qr_original(12, 32, 16,8)))
    sw2.close()
  }

//  def main(args: Array[String]): Unit = {
//    val sw2 = new PrintWriter("test8x4_cnt.sv")
//    sw2.println(getVerilogString(new test_fxn(12, 32, 8,4)))
//    sw2.close()
//  }


  class qr_original(name: Int, bw: Int, streaming_width: Int, cols: Int) extends Module {
    val io = IO {
      new Bundle() {
        //val rst = Input(Bool())
        //val qr_en = Input(Bool())
       // val qr_fi = Output(Bool())
        // val matrix_in = (Input(UInt((streaming_width*bw).W)))
        val matrix_out = (Output(UInt((1).W)))
      }
    }

    val DOT_CY = 8+11*log2Ceil(streaming_width); //hqr2,  6, 9: 8+11*log2(8)=41
    val HQR3_CY = 28; //hqr3:  sqrt
    val HQR5_CY = 11; //hqr5:  xk(1)+sign(xk(1))*d2
    val HQR7_CY = 29; //hqr7:  -2/d3
    val HQR10_CY = 8; //hqr10: d5=tk*d4
    val HQR11_CY = 19; //hqr11: axpy
    val VK_CY = DOT_CY + HQR3_CY + HQR5_CY;
    val TK_CY = VK_CY + DOT_CY + HQR7_CY;
    val HH_CY = TK_CY + DOT_CY + HQR10_CY + HQR11_CY;

    val cnt = Reg(UInt(bw.W))
//    val qr_en = Reg(Bool())
//    qr_en := io.qr_en
//        when(qr_en) {
//          cnt := cnt + 1.U
//        }.otherwise{
//          cnt := cnt
//        }

    cnt := cnt + 1.U

    val ena = Reg(Bool())
    val enb = Reg(Bool())
    val wea = Reg(UInt((bw*cols/4).W))
    val addra = Reg(UInt(log2Ceil(cols).W))
    val addrb = Reg(UInt(log2Ceil(cols).W))
    val dina = Reg(UInt((streaming_width * bw).W))
    val doutb = Reg(UInt((streaming_width * bw).W))


    val mem = Module(new simple_dual(bw, streaming_width))

    mem.io.clka := clock
    mem.io.clkb := clock
    mem.io.ena := ena
    mem.io.enb := enb
    mem.io.wea := wea
    mem.io.addra := addra
    mem.io.addrb := addrb
    mem.io.dina := dina

    doutb := mem.io.doutb

    val dot_in = Reg(UInt((streaming_width * bw * 2).W))
    val dot_out = Wire(UInt((streaming_width * bw ).W))
    val vk0 = Wire(UInt(bw.W))


    ///////// enb and addrb setting/////
    //tk stage
    for (i <- 0 until cols) {
      //tk_cy stage enb and addrb
      for (j <- -1 until cols - 1-i) {
        when(cnt === ((i.U * HH_CY.U) + TK_CY.U + j.S.asUInt) || cnt === (i.U * HH_CY.U + HQR10_CY.U + TK_CY.U + j.S.asUInt)) {
          enb := true.B
          addrb := (j + 1).U
        }
      }
      when(cnt === 0.U || cnt === ((i.U * HH_CY.U) - 1.U) && (i.U * HH_CY.U) =/= 0.U ||cnt === ((i.U * HH_CY.U) + VK_CY.U - 1.U) ) {// double check this
        enb := true.B
        addrb := i.U
      }
    }

    //HH_cy and vk_cy no iiner loop needed
    for (i <- 0 until cols) {
      when(cnt === (i.U * HH_CY.U )) {
        val dout_slice = doutb(((streaming_width-i)*bw)-1, 0)
        val dout_padded = Cat(0.U(i * bw), dout_slice)
        dot_in := Cat(dout_padded, dout_padded)
      }.elsewhen(cnt === ((i.U * HH_CY.U) + VK_CY.U )){
        val dout_slice = doutb(((streaming_width-i-1)*bw)-1,0)
        val dout_padded = Cat(0.U(i*bw),vk0,dout_slice)
        dot_in:= Cat(dout_padded,dout_padded)
      }
      ///////// tk_cy with inner loop
      for (j <- 0 until cols-i) {
        when(cnt === ((i.U * HH_CY.U) + TK_CY.U + j.S.asUInt)) {
          val dout_slice1 = doutb(((streaming_width-i-1)*bw)-1,0)
          val dout_slice2 = doutb(((streaming_width-i)*bw)-1,0)
          val dout_padded = Cat(0.U(i*bw),vk0,dout_slice1)
          dot_in:= Cat(dout_padded,0.U(i*bw),dout_slice2)
        }
      }
    } // end of for cols loop

    val dot = Module(new DOT(streaming_width,bw))
    for (i <- 0 until streaming_width) {
      dot.io.in_a(i) := dot_in((2*streaming_width-i)*bw-1, (2*streaming_width-i-1)*bw) // upper half
      dot.io.in_b(i) := dot_in((streaming_width-i)*bw-1, (streaming_width-i-1)*bw)     // lower half
    }

    dot_out:=dot.io.out_s

    val d2 = Wire(UInt(bw.W))

    val sqrt = Module(new FP_sqrt(bw)).io
    sqrt.aclk := this.clock
    sqrt.s_axis_a_tdata:= dot_out
    sqrt.s_axis_a_tvalid:= true.B
    d2:= sqrt.m_axis_result_tdata

    val x0 =  Reg(UInt(bw.W))


    for ( i <-1 until cols){
      when(cnt === ((i.U * HH_CY.U)) || cnt === 1.U ) {
        x0:= doutb((streaming_width-i)*bw-1, (streaming_width-i-1)*bw)
      }
    }


    val d2_s = Mux(x0(31), Cat(~d2(31), d2(30,0)), d2)
    val adder = Module(new FP_adder(bw)).io
    adder.aclk := this.clock
    adder.s_axis_a_tvalid:= true.B
    adder.s_axis_b_tvalid:= true.B
    adder.s_axis_a_tdata:= x0
    adder.s_axis_b_tdata:=d2_s
    vk0:= adder.m_axis_result_tdata

    val tk = Wire(UInt(bw.W))
    //
    val divider = Module(new FP_divider(bw)).io
    divider.aclk := this.clock
    divider.s_axis_a_tvalid:= true.B
    divider.s_axis_b_tvalid:= true.B
    divider.s_axis_a_tdata:= "hc0000000".U(bw.W)
    divider.s_axis_b_tdata:=dot_out
    tk:= divider.m_axis_result_tdata
    //
    val d5  = Wire(UInt(bw.W))

    val mult = Module(new FP_mult(bw)).io
    mult.aclk:= this.clock
    mult.s_axis_a_tvalid:= true.B
    mult.s_axis_b_tvalid:= true.B
    mult.s_axis_a_tdata:= tk
    mult.s_axis_b_tdata:=dot_out
    d5:= mult.m_axis_result_tdata
    //
    val vk = Reg(UInt((streaming_width*bw).W))


    for ( i <-0 until cols){
      when(cnt === (((i.U * HH_CY.U) + VK_CY.U)) ) {
        val padding =  (0.U(i*bw))
        val doutb_slice = doutb(((streaming_width-i-1)*bw)-1,0)
        vk := Cat(padding,vk0,doutb_slice)
      }
    }

    val axpy_output = WireInit(VecInit(Seq.fill(streaming_width)(0.U(bw.W))))
    val axpy = Module(new AXPY(bw, streaming_width)).io

    axpy.in_a:=d5
    for (i <- 0 until streaming_width) {
      axpy.in_b(i) := vk((streaming_width-i)*bw-1, (streaming_width-i-1)*bw)
      axpy.in_c(i) := doutb((streaming_width-i)*bw-1, (streaming_width-i-1)*bw)
      axpy_output(i) := axpy.out_s(i)
    }
    //
    //    // wb stage


    val wea_mask = ~0.U((bw*cols/4).W)
    for (i <- 0 until cols) {
      //HH writeback stage
      for (j <- -1 until cols - 1-i) {
        when(cnt === ((i.U * HH_CY.U)+ j.S.asUInt)) {
          ena := true.B
          addra := (j + 1).U
          val shift = ((j+1).U  << 2)
          wea := wea_mask >> shift
        }
      }
      dina := axpy_output.asUInt
    }
    //io.matrix_out := doutb

    /// use this only for large sw , ie 32





//    when(cnt === HH_CY.U*cols.U){
//      io.qr_fi := true.B
//    }.otherwise{
//      io.qr_fi :=false.B
//    }
    //
   // val out_1 = doutb((streaming_width*bw)-1,(streaming_width*bw/2))
    val doutb_reg = Reg((Bool()))
    doutb_reg:= doutb(0)
    io.matrix_out := doutb_reg



  }

  class test_fxn(name: Int, bw: Int, streaming_width: Int, cols: Int) extends Module {
    val io = IO {
      new Bundle() {
        val rst = Input(Bool())
        val qr_en = Input(Bool())
        val qr_fi = Output(Bool())
        // val matrix_in = (Input(UInt((streaming_width*bw).W)))
         val matrix_out = (Output(UInt((streaming_width * bw/2).W)))
      }
    }


    val DOT_CY = 8+11*log2Ceil(streaming_width); //hqr2,  6, 9: 8+11*log2(8)=41
    val HQR3_CY = 28; //hqr3:  sqrt
    val HQR5_CY = 11; //hqr5:  xk(1)+sign(xk(1))*d2
    val HQR7_CY = 29; //hqr7:  -2/d3
    val HQR10_CY = 8; //hqr10: d5=tk*d4
    val HQR11_CY = 19; //hqr11: axpy
    val VK_CY = DOT_CY + HQR3_CY + HQR5_CY;
    val TK_CY = VK_CY + DOT_CY + HQR7_CY;
    val HH_CY = TK_CY + DOT_CY + HQR10_CY + HQR11_CY;

    val cnt = RegInit(0.U(bw.W))
    val cnt_nxt = Mux(io.qr_en, cnt + 1.U, cnt)
    dontTouch(cnt_nxt)
    when(io.rst) {
      cnt := 0.U
    }.otherwise {
      cnt := cnt_nxt
    }

//    when(io.qr_en) {
//      cnt := 0.U
//    }.otherwise{
//      cnt := cnt + 1.U
//    }
    val nxt_ena = RegInit(false.B)
    val nxt_enb = Reg(Bool())
    val nxt_wea = Reg(UInt(bw.W))
    val nxt_addra = Reg(UInt(log2Ceil(cols).W))
    val nxt_addrb = RegInit(0.U(log2Ceil(cols).W))
    val nxt_dina = Reg(UInt((streaming_width * bw).W))
    val ena = Reg(Bool())
    val enb = RegInit(false.B)
    val wea = Reg(UInt(bw.W))
    val addra = Reg(UInt(log2Ceil(cols).W))
    val addrb = RegInit(0.U(log2Ceil(cols).W))
    val dina = Reg(UInt((streaming_width * bw).W))
    val doutb = Reg(UInt((streaming_width * bw).W))

    val mem = Module(new simple_dual(bw, streaming_width))

    mem.io.clka := clock
    mem.io.clkb := clock
    mem.io.ena := ena
    mem.io.enb := enb
    mem.io.wea := wea
    mem.io.addra := addra
    mem.io.addrb := addrb
    mem.io.dina := dina

    doutb := mem.io.doutb

    val dot_in = Reg(UInt((streaming_width * bw * 2).W))
    val dot_out = Reg(UInt((streaming_width * bw ).W))
    val vk0 = Reg(UInt(bw.W))

    for (i <- 0 until cols) {
      printf(p"start of tk_cy enb and addrb, i = ${i}\n")
      for (j <- -1 until cols - 1-i) {
        when(cnt === ((i.U * HH_CY.U) + TK_CY.U + j.S.asUInt)) {
          printf(p"start of tk_cy enb and addrb, j = ${j}\n")
          nxt_enb := true.B
          nxt_addrb := (j + 1).U
        }.elsewhen(cnt === (i.U * HH_CY.U + HQR10_CY.U + TK_CY.U + j.S.asUInt)) {
          //tk_cy + hqr10_cy
          nxt_enb := true.B
          nxt_addrb := (j + 1).U
        }.otherwise{
          nxt_enb := false.B
          nxt_addrb := addrb

        }
      }

      // HH stage
      when(cnt === 0.U){
        nxt_enb := true.B
        nxt_addrb := i.U
      }.elsewhen(cnt === ((i.U * HH_CY.U) - 1.U) && (i.U * HH_CY.U) =/= 0.U) {// double check this
        nxt_enb := true.B
        nxt_addrb := i.U
      }.elsewhen(cnt === ((i.U * HH_CY.U) + VK_CY.U - 1.U)) {
        //vk stage
        nxt_enb := true.B
        nxt_addrb := i.U
      }.otherwise{
        nxt_enb := false.B
        nxt_addrb := addrb
      }
    }

    dontTouch(enb)
    dontTouch(nxt_enb)
    dontTouch(addrb)
    dontTouch(nxt_addrb)
    enb:= nxt_enb
    addrb:= nxt_addrb
   // for (i <- 0 until cols) {

//      //tk_cy stage enb and addrb
//      printf(p"start of tk_cy enb and addrb, i = ${i}\n")
//      for (j <- -1 until cols - 1-i) {
//        printf(p"start of tk_cy enb and addrb, j = ${j}\n")
//        when(cnt === ((i.U * HH_CY.U) + TK_CY.U + j.S.asUInt)) {
//          enb := true.B
//          addrb := (j + 1).U
//        }.elsewhen(cnt === (i.U * HH_CY.U + HQR10_CY.U + TK_CY.U + j.S.asUInt)) {
//          //tk_cy + hqr10_cy
//          printf(p"tk + hqr10 enb and addrb, j = ${j}\n")
//          enb := true.B
//          addrb := (j + 1).U
//        }
//
//      }



//      // HH stage
//      when(cnt === 0.U){
//        printf(p"when cnt = 0\n")
//        enb := true.B
//        addrb := i.U
//      }.elsewhen(cnt === ((i.U * HH_CY.U) - 1.U) && (i.U * HH_CY.U) =/= 0.U) {// double check this
//        printf(p"HH_CY enb and addrb, i = ${i}\n")
//        enb := true.B
//        addrb := i.U
//      }.elsewhen(cnt === ((i.U * HH_CY.U) + VK_CY.U - 1.U)) {
//        printf(p"VK_CY enb and addrb, i = ${i}\n")
//        //vk stage
//        enb := true.B
//        addrb := i.U
//      }
//    }


    //HH_cy and vk_cy no iiner loop needed
//    for (i <- 0 until cols) {
//      printf(p"HH_CY dot in, i = ${i}\n")
//      when(cnt === (i.U * HH_CY.U)) {
//        printf(p"HH_CY dout_slice  = ${((streaming_width - i) * bw) - 1}\n")
//        val dout_slice = doutb(((streaming_width - i) * bw) - 1, 0)
//        printf(p"HH_CY padding  = ${(0.U(i*bw))}\n")
//
//        val dout_padded = Cat(0.U(i * bw), dout_slice)
//        dot_in := Cat(dout_padded, dout_padded)
//      }.elsewhen(cnt === ((i.U * HH_CY.U) + VK_CY.U)) {
//        printf(p"VK_CY dot in, i = ${i}\n")
//        val dout_slice = doutb(((streaming_width - i - 1) * bw) - 1, 0)
//        printf(p"VK_CY dout_slice  = ${((streaming_width - i-1) * bw) - 1}\n")
//        val dout_padded = Cat(0.U(i * bw), vk0, dout_slice)
//        dot_in := Cat(dout_padded, dout_padded)
//      }
//    }

//    for (i <- 0 until cols) {
//      printf(p"TK_CY dot in, i = ${i}\n")
//    for (j <- 0 until cols-i) {
//      printf(p"TK_CY dot in, j = ${j}\n")
//      when(cnt === ((i.U * HH_CY.U) + TK_CY.U + j.S.asUInt)) {
//        val dout_slice1 = doutb(((streaming_width-i-1)*bw)-1,0)
//        val dout_slice2 = doutb(((streaming_width-i)*bw)-1,0)
//        val dout_padded = Cat(0.U(i*bw),vk0,dout_slice1)
//        dot_in:= Cat(dout_padded,0.U(i*bw),dout_slice2)
//      }
//    }
//  } // end of for cols loop
//
//    val dot = Module(new DOT(streaming_width,bw))
//    for (i <- 0 until streaming_width) {
//      dot.io.in_a(i) := dot_in((2*streaming_width-i)*bw-1, (2*streaming_width-i-1)*bw) // upper half
//      dot.io.in_b(i) := dot_in((streaming_width-i)*bw-1, (streaming_width-i-1)*bw)     // lower half
//    }
//
//
//    dot_out:=dot.io.out_s
//
//    val d2 = Reg(UInt(bw.W))
//
//    val sqrt = Module(new FP_sqrt(bw)).io
//    sqrt.aclk := this.clock
//    sqrt.s_axis_a_tdata:= dot_out
//    sqrt.s_axis_a_tvalid:= true.B
//    d2:= sqrt.m_axis_result_tdata

//    val x0 =  Reg(UInt(bw.W))
//
//    for ( i <-1 until cols){
//      printf(p"x0 dot in, i = ${i}\n")
//      when(cnt === ((i.U * HH_CY.U)) ) {
//        x0:= doutb((streaming_width-i)*bw-1, (streaming_width-i-1)*bw)
//      }.elsewhen( cnt === 1.U){
//        x0:= doutb((streaming_width)*bw-1, (streaming_width-1)*bw)
//      }
//    }

//
//    val d2_s = Mux(x0(31), Cat(~d2(31), d2(30,0)), d2)
//    val adder = Module(new FP_adder(bw)).io
//    adder.aclk := this.clock
//    adder.s_axis_a_tvalid:= true.B
//    adder.s_axis_b_tvalid:= true.B
//    adder.s_axis_a_tdata:= x0
//    adder.s_axis_b_tdata:=d2_s
//    vk0:= adder.m_axis_result_tdata

    //
//    val tk = Wire(UInt(bw.W))
//    //
//    val divider = Module(new FP_divider(bw)).io
//    divider.aclk := this.clock
//    divider.s_axis_a_tvalid:= true.B
//    divider.s_axis_b_tvalid:= true.B
//    divider.s_axis_a_tdata:= "hc0000000".U(bw.W)
//    divider.s_axis_b_tdata:=dot_out
//    tk:= divider.m_axis_result_tdata
    //
//    val d5  = Wire(UInt(bw.W))
//    //
//    val mult = Module(new FP_mult(bw)).io
//    mult.aclk:= this.clock
//    mult.s_axis_a_tvalid:= true.B
//    mult.s_axis_b_tvalid:= true.B
//    mult.s_axis_a_tdata:= tk
//    mult.s_axis_b_tdata:=dot_out
//    d5:= mult.m_axis_result_tdata
//    //
//    val vk = Reg(UInt((streaming_width*bw).W))
//    //
//
//
//    for ( i <-0 until cols){
//      printf(p"vk doutb, i = ${i}\n")
//      when(cnt === (((i.U * HH_CY.U) + VK_CY.U)) ) {
//        val padding =  Cat(0.U(i*bw))
//        val doutb_slice = doutb(((streaming_width-i-1)*bw)-1,0)
//        vk := Cat(padding,vk0,doutb_slice)
//      }
//    }
//
//    val axpy = Module(new AXPY(bw, streaming_width)).io
//    axpy.in_a:=d5
//    for (i <- 0 until streaming_width) {
//      printf(p"axpy, i = ${i}\n")
//      axpy.in_b(i) := vk((streaming_width-i)*bw-1, (streaming_width-i-1)*bw)
//      axpy.in_c(i) := doutb((streaming_width-i)*bw-1, (streaming_width-i-1)*bw)
//    }


//    val wea_mask = ~0.U(bw.W)
//    for (i <- 0 until cols) {
//      printf(p"HH WB, i = ${i}\n")
//      //HH writeback stage
//      for (j <- -1 until cols - 1-i) {
//        printf(p"HH WB, j = ${j}\n")
//        when(cnt === ((i.U * HH_CY.U)+ j.S.asUInt)) {
//          ena := true.B
//          addra := (j + 1).U
//          dina := axpy.out_s.asUInt
//          val shift = ((j+1).U  << 2)
//          wea := wea_mask >> shift
//        }
//      }
//    }

    io.matrix_out := dot_in(255,0)
    io.qr_fi := cnt(0)


  }

  class qr_old(name: Int, bw: Int, streaming_width: Int, cols: Int) extends Module{
    val io = IO {
      new Bundle() {
        val rst = Input(Bool())
        val qr_en = Input(Bool())
        val qr_fi = Output(Bool())
        // val matrix_in = (Input(UInt((streaming_width*bw).W)))
        val matrix_out = (Output(UInt((streaming_width * bw/2).W)))
      }
    }

    val DOT_CY = 8+11*log2Ceil(streaming_width); //hqr2,  6, 9: 8+11*log2(8)=41
    val HQR3_CY = 28; //hqr3:  sqrt
    val HQR5_CY = 11; //hqr5:  xk(1)+sign(xk(1))*d2
    val HQR7_CY = 29; //hqr7:  -2/d3
    val HQR10_CY = 8; //hqr10: d5=tk*d4
    val HQR11_CY = 19; //hqr11: axpy
    val VK_CY = DOT_CY + HQR3_CY + HQR5_CY;
    val TK_CY = VK_CY + DOT_CY + HQR7_CY;
    val HH_CY = TK_CY + DOT_CY + HQR10_CY + HQR11_CY;





    val cnt = Reg(UInt(bw.W))
    when(io.rst) {
      cnt := 0.U
    }.otherwise {
      cnt := cnt + 1.U
    }


    val ena = Reg(Bool())
    val enb = Reg(Bool())
    val wea = Reg(UInt((bw*cols/4).W))
    val addra = Reg(UInt(log2Ceil(cols).W))
    val addrb = Reg(UInt(log2Ceil(cols).W))
    val dina = Reg(UInt((streaming_width * bw).W))
    val doutb = Reg(UInt((streaming_width * bw).W))

    val mem = Module(new simple_dual(bw, streaming_width))

    val dina_reg = RegNext(dina)
    val ena_reg   = RegNext(ena)
    val wea_reg   = RegNext(wea)
    val addra_reg = RegNext(addra)
    val addrb_reg = RegNext(addrb)
    val enb_reg   = RegNext(ena)



    mem.io.clka := clock
    mem.io.clkb := clock
    mem.io.ena := ena_reg
    mem.io.enb := enb_reg
    mem.io.wea := wea_reg
    mem.io.addra := addra_reg
    mem.io.addrb := addrb_reg
    mem.io.dina := dina_reg

    doutb := mem.io.doutb
    //val dot_in = Reg(UInt((streaming_width * bw * 2).W))
    val dot_out = Reg(UInt((streaming_width * bw ).W))
    val vk0 = Reg(UInt(bw.W))

    val dot_in_vec = Reg(Vec(streaming_width, UInt(bw.W)))
    val dot_in_vec_w = Wire(Vec(streaming_width, UInt(bw.W)))

    // Default: hold current values
    for (i <- 0 until streaming_width) {dot_in_vec_w(i) := dot_in_vec(i)}

    val dot = Module(new DOT(streaming_width,bw))


    for (i <- 0 until cols) {
      //tk_cy stage enb and addrb
      for (j <- -1 until cols - 1-i) {
        when(cnt === ((i.U * HH_CY.U) + TK_CY.U + j.S.asUInt) || cnt === (i.U * HH_CY.U + HQR10_CY.U + TK_CY.U + j.S.asUInt)) {
          enb := true.B
          addrb := (j + 1).U
        }
      }
      when(cnt === 0.U || cnt === ((i.U * HH_CY.U) - 1.U) && (i.U * HH_CY.U) =/= 0.U ||cnt === ((i.U * HH_CY.U) + VK_CY.U - 1.U) ) {// double check this
        enb := true.B
        addrb := i.U
      }
    }


    for (i <- 0 until cols) {

      // HH stage
      when(cnt === (i.U * HH_CY.U)) {
        dot_in_vec_w(0) := vk0  // already registered
        for (lane <- 1 until streaming_width) {
          if (lane <= streaming_width - i - 1) {
            dot_in_vec_w(lane) := doutb(((streaming_width - lane)*bw -1), ((streaming_width - lane -1)*bw))
          } else {
            dot_in_vec_w(lane) := 0.U
          }
        }
      }

      // tk_cy stage
      for (j <- 0 until cols-i) {
        when(cnt === ((i.U * HH_CY.U) + TK_CY.U + j.S.asUInt)) {
          dot_in_vec_w(0) := vk0  // already registered
          for (lane <- 1 until streaming_width) {
            if (lane <= streaming_width - i - 2) {
              dot_in_vec_w(lane) := doutb(((streaming_width - lane)*bw -1), ((streaming_width - lane -1)*bw))
            } else {
              dot_in_vec_w(lane) := 0.U
            }
          }
        }
      }
    }

    for (i <- 0 until streaming_width) {
      dot_in_vec(i) := RegNext(dot_in_vec_w(i))
    }



//    for (i <- 0 until cols) {
//      // HH stage
//      when(cnt === (i.U * HH_CY.U)) {
//        dot_in_vec_w(0) := vk0  // MSB lane
//        for (lane <- 1 until streaming_width) {
//          val hi = (streaming_width - lane) * bw - 1
//          val lo = (streaming_width - lane - 1) * bw
//          if (hi >= lo) dot_in_vec_w(lane) := doutb(hi, lo)
//          else dot_in_vec_w(lane) := 0.U
//        }
//      }
//
//      // tk_cy stage (sliding window)
//      for (j <- 0 until cols-i) {
//        when(cnt === ((i.U * HH_CY.U) + TK_CY.U + j.S.asUInt)) {
//          // example mapping: first lane vk0, rest from slices
//          dot_in_vec_w(0) := vk0
//          for (lane <- 1 until streaming_width) {
//            val hi = (streaming_width - lane) * bw - 1
//            val lo = (streaming_width - lane - 1) * bw
//            if (hi >= lo) dot_in_vec_w(lane) := doutb(hi, lo)
//            else dot_in_vec_w(lane) := 0.U
//          }
//        }
//      }
//    }
//
//
//    for (i <- 0 until streaming_width) {
//      dot_in_vec(i) := RegNext(dot_in_vec_w(i))
//    }
//
//    val dot = Module(new DOT(streaming_width,bw))
//
    for (i <- 0 until streaming_width) {
      dot.io.in_a(i) := dot_in_vec(i)  // upper half
      dot.io.in_b(i) := dot_in_vec(i)  // lower half (or adjust per your design)
    }



    //old dot block
    //dot input block
    //HH_cy and vk_cy no iiner loop needed
//    for (i <- 0 until cols) {
//      when(cnt === (i.U * HH_CY.U )) {
//        val dout_slice = doutb(((streaming_width-i)*bw)-1, 0)
//        val dout_padded = Cat(0.U(i * bw), dout_slice)
//        dot_in := Cat(dout_padded, dout_padded)
//      }.elsewhen(cnt === ((i.U * HH_CY.U) + VK_CY.U )){
//        val dout_slice = doutb(((streaming_width-i-1)*bw)-1,0)
//        val dout_padded = Cat(0.U(i*bw),vk0,dout_slice)
//        dot_in:= Cat(dout_padded,dout_padded)
//      }
//      ///////// tk_cy with inner loop
//      for (j <- 0 until cols-i) {
//        when(cnt === ((i.U * HH_CY.U) + TK_CY.U + j.S.asUInt)) {
//          val dout_slice1 = doutb(((streaming_width-i-1)*bw)-1,0)
//          val dout_slice2 = doutb(((streaming_width-i)*bw)-1,0)
//          val dout_padded = Cat(0.U(i*bw),vk0,dout_slice1)
//          dot_in:= Cat(dout_padded,0.U(i*bw),dout_slice2)
//        }
//      }
//    } // end of for cols loop
//
//    val dot_in_reg = RegNext(dot_in)


//
//    for (i <- 0 until streaming_width) {
//      dot.io.in_a(i) := dot_in_reg((2*streaming_width-i)*bw-1, (2*streaming_width-i-1)*bw) // upper half
//      dot.io.in_b(i) := dot_in_reg((streaming_width-i)*bw-1, (streaming_width-i-1)*bw)     // lower half
//    }


    dot_out:=dot.io.out_s



    val d2 = Reg(UInt(bw.W))

    val sqrt = Module(new FP_sqrt(bw)).io
    sqrt.aclk := this.clock
    sqrt.s_axis_a_tdata:= dot_out
    sqrt.s_axis_a_tvalid:= true.B
    d2:= sqrt.m_axis_result_tdata

    val x0 =  Reg(UInt(bw.W))


    for ( i <-1 until cols){
      when(cnt === (i.U * HH_CY.U) || (cnt === 1.U) ) {
        x0:= doutb((streaming_width-i)*bw-1, (streaming_width-i-1)*bw)
      }
    }

    val d2_s = Mux(x0(31), Cat(~d2(31), d2(30,0)), d2)
    val adder = Module(new FP_adder(bw)).io
    adder.aclk := this.clock
    adder.s_axis_a_tvalid:= true.B
    adder.s_axis_b_tvalid:= true.B
    adder.s_axis_a_tdata:= x0
    adder.s_axis_b_tdata:=d2_s
    vk0:= adder.m_axis_result_tdata
    //
    //
    val tk = Reg(UInt(bw.W))
    //
    val divider = Module(new FP_divider(bw)).io
    divider.aclk := this.clock
    divider.s_axis_a_tvalid:= true.B
    divider.s_axis_b_tvalid:= true.B
    divider.s_axis_a_tdata:= "hc0000000".U(bw.W)
    divider.s_axis_b_tdata:=dot_out
    tk:= divider.m_axis_result_tdata
    //
    val d5  = Reg(UInt(bw.W))
    //
    val mult = Module(new FP_mult(bw)).io
    mult.aclk:= this.clock
    mult.s_axis_a_tvalid:= true.B
    mult.s_axis_b_tvalid:= true.B
    mult.s_axis_a_tdata:= tk
    mult.s_axis_b_tdata:=dot_out
    d5:= mult.m_axis_result_tdata
    //
    //val vk = Reg(UInt((streaming_width*bw).W))
//    val vk_vec = Reg(Vec(streaming_width, UInt(bw.W)))
//
//    for (i <- 0 until cols) {
//      when(cnt === ((i.U * HH_CY.U) + VK_CY.U)) {
//        // MSB lane = vk0
//        vk_vec(0) := vk0
//
//        // Next lanes come from doutb slices
//        for (j <- 0 until (streaming_width - 1)) {
//          val hi = (streaming_width - j - 1) * bw - 1
//          val lo = (streaming_width - j - 2) * bw
//          vk_vec(j + 1) := doutb(hi, lo)
//        }
//
////zero pad
//        for (k <- (streaming_width - i) until streaming_width) {
//          vk_vec(k) := 0.U
//        }
//      }
//    }


    // Stage 0: combinational assignment per lane
    val vk_vec_w = Wire(Vec(streaming_width, UInt(bw.W)))
    for (i <- 0 until cols) {
    for (lane <- 0 until streaming_width) {
      vk_vec_w(lane) := 0.U  // default

      when(cnt === ((i.U * HH_CY.U) + VK_CY.U)) {
        if(lane == 0) {
          vk_vec_w(lane) := vk0  // MSB lane
        } else if(lane <= streaming_width - i - 1) {
          val hi = (streaming_width - lane) * bw - 1
          val lo = (streaming_width - lane - 1) * bw
          vk_vec_w(lane) := doutb(hi, lo)  // independent slice
        }
      }
    }
      }

    // Stage 1: register every lane
    val vk_vec_stage = Reg(Vec(streaming_width, UInt(bw.W)))
    for (lane <- 0 until streaming_width) vk_vec_stage(lane) := RegNext(vk_vec_w(lane))

    // Stage 2: final register for downstream
    val vk_vec = Reg(Vec(streaming_width, UInt(bw.W)))
    for (lane <- 0 until streaming_width) vk_vec(lane) := RegNext(vk_vec_stage(lane))


//
//    for ( i <-0 until cols){
//      when(cnt === (((i.U * HH_CY.U) + VK_CY.U)) ) {
//        val padding =  Cat(0.U(i*bw))
//        val doutb_slice = doutb(((streaming_width-i-1)*bw)-1,0)
//        vk := Cat(padding,vk0,doutb_slice)
//      }
//    }

    //val vk_reg = RegNext(vk)

    val axpy_output = Reg(Vec(streaming_width, UInt(bw.W)))
    val axpy = Module(new AXPY(bw, streaming_width)).io

    axpy.in_a:=d5
    for (i <- 0 until streaming_width) {
      axpy.in_b(i) := vk_vec(i)
      axpy.in_c(i) := doutb((streaming_width-i)*bw-1, (streaming_width-i-1)*bw)
      axpy_output(i) := axpy.out_s(i)
    }
    //
    //    // wb stage


    val axpy_stage = Reg(Vec(streaming_width, UInt(bw.W)))
    for (k <- 0 until streaming_width) {
      axpy_stage(k) := axpy_output(k)
    }

    // Pack after register
    val dina_wide = Cat((0 until streaming_width).map(k => axpy_stage(k)).reverse)

    // Second stage (optional, safer for timing)
    val dina_stage = RegNext(dina_wide)


    val wea_mask = ~0.U((bw*cols/4).W)
    for (i <- 0 until cols) {
      //HH writeback stage
      for (j <- -1 until cols - 1-i) {
        when(cnt === ((i.U * HH_CY.U)+ j.S.asUInt)) {
          ena := true.B
          addra := (j + 1).U

          val shift = ((j+1).U  << 4)
          wea := wea_mask >> shift
        }
        //        .otherwise{
        //          nxt_ena := false.B
        //          nxt_addra := addra
        //          nxt_dina := dina
        //          nxt_wea := ~0.U(bw.W)
        //        }
      }
    }

    dina := dina_stage


    io.matrix_out := doutb

    /// use this only for large sw , ie 32





    when(cnt === HH_CY.U*cols.U){
      io.qr_fi := true.B
    }.otherwise{
      io.qr_fi :=false.B
    }
    //
    //val out_1 = doutb((streaming_width*bw)-1,(streaming_width*bw/2))
    //io.matrix_out := out_1
    //    val out_2 = doutb((streaming_width*bw/2)-1, 0)
    //    val toggle = Reg(Bool())
    //    io.matrix_out:= 0.U
    //    when(io.qr_fi){
    //      io.matrix_out:= Mux(toggle, out_1, out_2)
    //      toggle := ~ toggle
    //
    //    }.otherwise{
    //      toggle := false.B
    //    }





  }

  //main datapath design module
  class qr(name: Int, bw: Int, streaming_width: Int) extends Module {
    val io = IO {
      new Bundle() {
        // val clk = Input(Clock())
        //  val rst = Input(Bool())
        val tsqr_en = Input(Bool())
        val tsqr_fi = Output(Bool())
        val matrix_in = (Input(UInt((streaming_width * bw).W)))
        val matrix_out = (Output(UInt((streaming_width * bw).W)))
      }
    }

    //hardcoded latencies
    val adder_cc = 11
    val subber_cc = 11
    val mult_cc = 8
    val recip_cc = 29
    val sqrt_cc = 28
    val DOT_CC = mult_cc + 2 + adder_cc * log2Ceil(streaming_width)
    val HQR3 = sqrt_cc
    val HQR5 = adder_cc
    val HQR7 = recip_cc
    val HQR10 = mult_cc
    val AXPY = mult_cc + adder_cc
    val d2_valid = DOT_CC + HQR3 + HQR5
    val d3_ready = DOT_CC + HQR3 + HQR5
    val d3_valid = DOT_CC + HQR3 + HQR5 + DOT_CC
    val tk_ready = DOT_CC + HQR3 + HQR5 + DOT_CC
    val tk_valid = DOT_CC + HQR3 + HQR5 + DOT_CC + HQR7
    val d4_ready = DOT_CC + HQR3 + HQR5 + DOT_CC + HQR7
    val d4_valid = DOT_CC + HQR3 + HQR5 + DOT_CC + HQR7 + DOT_CC
    val d5_ready = DOT_CC + HQR3 + HQR5 + DOT_CC + HQR7 + DOT_CC
    val d5_valid = DOT_CC + HQR3 + HQR5 + DOT_CC + HQR7 + DOT_CC + HQR10
    val yj_out_valid = DOT_CC + HQR3 + HQR5 + DOT_CC + HQR7 + DOT_CC + HQR10 + AXPY
    val INNER_LOOP_CC = DOT_CC + HQR10 + AXPY
    val YJ_SHIFT = DOT_CC + HQR10
    val COLS_NUMBER = ((streaming_width / 2) - 1).U
    val D4_SHIFT = HQR7

    // counter
    val cnt = RegInit(0.U(bw.W))
    val col_cnt = RegInit(0.U(bw.W)) // column counter
    val trl_col_cnt = RegInit(0.U(bw.W)) // trailing (inner loop ) column counter
    val rem_cols = RegInit(((streaming_width / 2) - 1).U(bw.W)) // remaining submatrix cols


    cnt := cnt + 1.U
    when(cnt === yj_out_valid.U) {
      col_cnt := col_cnt + 1.U
      cnt := 0.U
    }

    ///regs
    val yj_out = RegInit(VecInit(Seq.fill(streaming_width)(0.U(bw.W))))
    //mem FSM
    val load = RegInit(true.B) // init state
    val wb = RegInit(false.B)
    val read = RegInit(false.B)
    val read_yj = RegInit(false.B)
    val bram0_ena = RegInit(true.B) // init state
    val bram0_enb = RegInit(false.B)
    val addr_counter = RegInit(0.U((((log2Ceil(streaming_width) - 1)).W)))
    val data_in = RegInit(0.U((streaming_width * bw).W))
    val addr_flag = RegInit(false.B)
    val wea_mask = RegInit(((BigInt(1) << (streaming_width * bw / 8)) - 1).U)
    val read_mask = RegInit(((BigInt(1) << (streaming_width * bw / 8)) - 1).U)

    when(load) {
      data_in := io.matrix_in
    }.elsewhen(wb) {
      data_in := yj_out.asUInt
    }

    when(load) {

      bram0_ena := true.B
      addr_counter := addr_counter + 1.U
      when(addr_counter === COLS_NUMBER) {
        addr_counter := 0.U
        bram0_ena := false.B
        load := false.B
        read := true.B
      }
    }


    when(read) {

      bram0_enb := true.B
      addr_counter := addr_counter + 1.U
      when(addr_counter === COLS_NUMBER) {

        addr_counter := 0.U
        bram0_enb := false.B
        read := false.B
        when(rem_cols < ((streaming_width / 2) - 1).U) {
          wea_mask := (wea_mask >> 8.U)
          read_mask := read_mask >> bw.U
        }

      }

    }
    when(wb) {
      addr_flag := false.B
      bram0_ena := true.B
      addr_counter := addr_counter + 1.U
      when(addr_counter === COLS_NUMBER) {

        addr_counter := 0.U
        bram0_ena := false.B
        wb := false.B
      }


    }
    when(read_yj) {
      bram0_enb := true.B
      when(addr_flag === false.B) {
        addr_counter := rem_cols
        addr_flag := true.B
      }
      addr_counter := addr_counter + 1.U
      when(addr_counter === COLS_NUMBER) {
        addr_counter := 0.U
        bram0_enb := false.B
        read_yj := false.B
        wb := true.B
      }


    }


    val bramModule = Module(new simple_dual(bw, streaming_width))
    val bram0 = bramModule.io

    bram0.clka := this.clock
    bram0.ena := bram0_ena
    bram0.wea := wea_mask
    bram0.addra := addr_counter
    bram0.dina := data_in
    bram0.clkb := this.clock
    bram0.enb := bram0_enb
    bram0.addrb := addr_counter
    val bram0_read = bram0.doutb & read_mask


    //reg init

    val ddot_din_b = RegInit(0.U((streaming_width * bw).W))


    val ddot_din_a = RegInit(0.U((streaming_width * bw).W))
    val ddot_dout = RegInit(0.U(bw.W))
    val d2 = RegInit(0.U(bw.W))
    val d3 = RegInit(0.U(bw.W))
    val vk = RegInit(0.U((streaming_width * bw).W))
    val tk = RegInit(0.U(bw.W))
    val yj = RegInit(0.U((streaming_width * bw).W))
    val d4_hold = RegInit(false.B)

    val axpy_ready = RegInit(false.B)
    val d4 = RegInit(0.U(bw.W))
    val d5 = RegInit(0.U(bw.W))
    val d4_valid_cnt = RegInit(0.U(bw.W))
    val yj_hold = RegInit(0.U((streaming_width * 32).W)) // loaded from SRAM(trl_col_cnt) at d4_ready
    val yj_hold_raw = RegInit(0.U((streaming_width * 32).W)) // loaded from SRAM(trl_col_cnt) at d4_ready

    val yj_shift = RegInit(VecInit(Seq.fill(YJ_SHIFT)(((false.B)))))
    val wb_flag = RegInit(VecInit(Seq.fill(YJ_SHIFT)(((false.B)))))
    val yj_pipe = RegInit(VecInit(Seq.fill(YJ_SHIFT)(0.U((streaming_width * bw).W))))
    val yj_axpy = RegInit(0.U((streaming_width * bw).W))
    yj_axpy := yj_pipe.last

    //val yj_out = RegInit(VecInit(Seq.fill(streaming_width)(0.U(bw.W))))

    val inner_loop = RegInit(false.B)

    // start of outer loop
    // d1
    val dot = Module(new FP_DDOT_dp(streaming_width,bw ) ).io


    dot.in_a := ddot_din_a
    dot.in_b := ddot_din_b
    ddot_dout := dot.out_s

    when(read) {

      //ddot_din_a := sram(col_cnt)
      ddot_din_a := bram0_read
    }.elsewhen(cnt === d3_ready.U) {
      ddot_din_a := vk
    }.elsewhen(d4_hold === true.B) {
      ddot_din_a := yj
    }.otherwise {
      ddot_din_a := 0.U
    }

    when(read) {
      //ddot_din_b := sram(col_cnt)
      ddot_din_b := bram0_read
    }.elsewhen(cnt === d3_ready.U) {
      ddot_din_b := vk
    }.elsewhen(d4_hold === true.B) {
      ddot_din_b := vk
    }.otherwise {
      ddot_din_b := 0.U
    }


    // d2

    val hqr3 = Module(new FP_sqrt(bw)).io
    hqr3.s_axis_a_tvalid := true.B
    hqr3.aclk := this.clock
    hqr3.s_axis_a_tdata := dot.out_s
    d2 := hqr3.m_axis_result_tdata
    // vk =xk
    val hqr5 = Module(new hqr5(bw)).io

    //hqr5.in_a := sram(col_cnt)
    hqr5.in_a := bram0_read
    hqr5.in_b := d2
    // vk := sram(col_cnt)

    when(cnt === d2_valid.U) {
      vk := Cat(hqr5.out_s, vk((streaming_width - 1) * bw - 1, 0))
    }.otherwise {
      vk := bram0_read
    }

    //d3

    when(cnt === (tk_ready.U)) {
      d3 := dot.out_s
    }

    //tk
    val hqr7 = Module(new hqr7(bw)).io

    hqr7.in_a := d3
    when(cnt === (tk_valid.U)) {
      tk := hqr7.out_s
    }


    when(cnt === d4_ready.U) {
      inner_loop := true.B
    }

    val yj_shifted = ShiftRegister(yj, YJ_SHIFT)
    when(inner_loop) {

      //enable mux for dot input
      when(trl_col_cnt < rem_cols) {
        d4_hold := true.B
        read_yj := true.B

        yj := bram0_read

        trl_col_cnt := trl_col_cnt + 1.U

      }

      when(trl_col_cnt === rem_cols) {
        inner_loop := false.B
        trl_col_cnt := 0.U
        rem_cols := rem_cols - 1.U
        wb := false.B
        read := true.B
      }
    }


    d4_valid_cnt := rem_cols

    when(cnt >= (d4_valid.U) | cnt < (d4_valid.U) + rem_cols) {

      d4 := dot.out_s

    }

    val d4_shifted = ShiftRegister(d4, D4_SHIFT)


    //d5
    val hqr10 = Module(new FP_mult(bw)).io
    hqr10.aclk := this.clock
    hqr10.s_axis_a_tvalid := true.B
    hqr10.s_axis_b_tvalid := true.B
    hqr10.s_axis_a_tdata := tk
    hqr10.s_axis_b_tdata := d4_shifted
    when(cnt === (d5_valid.U)) {
      d5 := hqr10.m_axis_result_tdata
    }

    //axpy
    val axpy = Module(new axpy_dp(bw, streaming_width)).io

    axpy.in_a := d5
    for (i <- 0 until streaming_width) {
      axpy.in_b(i) := vk(streaming_width * 32 - (i * 32) - 1, (streaming_width * 32 - (32 * (i + 1))))
      axpy.in_c(i) := yj_shifted(streaming_width * 32 - (i * 32) - 1, (streaming_width * 32 - (32 * (i + 1))))
    }
    yj_out := axpy.out_s
    when(cnt >= yj_out_valid.U && cnt < (yj_out_valid.U + rem_cols)) {

      wb := true.B


    }

    when(rem_cols === 0.U) {

      io.tsqr_fi := 1.U

    }.otherwise {
      io.tsqr_fi := 0.U
    }


    io.matrix_out := bram0_read
  }




  /// for sw = 32
  /// made the out_matrix port
  // half of the actual datastream size
  // due to fpga io bandwidth limit




  class qr_cnt(name: Int, bw: Int, streaming_width: Int, cols: Int) extends Module {
    val io = IO {
      new Bundle() {
        val rst = Input(Bool())
        val qr_en = Input(Bool())
        val qr_fi = Output(Bool())
        // val matrix_in = (Input(UInt((streaming_width*bw).W)))
        val matrix_out = (Output(UInt((streaming_width * bw).W)))
      }
    }


    val DOT_CY = 8+11*log2Ceil(streaming_width); //hqr2,  6, 9: 8+11*log2(8)=41
    val HQR3_CY = 28; //hqr3:  sqrt
    val HQR5_CY = 11; //hqr5:  xk(1)+sign(xk(1))*d2
    val HQR7_CY = 29; //hqr7:  -2/d3
    val HQR10_CY = 8; //hqr10: d5=tk*d4
    val HQR11_CY = 19; //hqr11: axpy
    val VK_CY = DOT_CY + HQR3_CY + HQR5_CY;
    val TK_CY = VK_CY + DOT_CY + HQR7_CY;
    val HH_CY = TK_CY + DOT_CY + HQR10_CY + HQR11_CY;

    val cnt = Reg(UInt(bw.W))
    val cnt_nxt = Mux(io.qr_en, cnt + 1.U, cnt)

    when(io.rst) {
      cnt := 0.U
    }.otherwise {
      cnt := cnt_nxt
    }


    val ena = Reg(Bool())
    val enb = Reg(Bool())
    val wea = Reg(UInt(bw.W))
    val addra = Reg(UInt(log2Ceil(cols).W))
    val addrb = Reg(UInt(log2Ceil(cols).W))
    val dina = Reg(UInt((streaming_width * bw).W))
    val doutb = Wire(UInt((streaming_width * bw).W))

//    val nxt_ena = WireInit(false.B)
//    val nxt_enb = WireInit(false.B)
//    val nxt_wea = WireInit(wea)
//    val nxt_addra = WireInit(addra)
//    val nxt_addrb = WireInit(addrb)
//    val nxt_dina = WireInit(dina)

    val nxt_ena = Reg(Bool())
    val nxt_enb = Reg(Bool())
    val nxt_wea = Reg(UInt(bw.W))
    val nxt_addra = Reg(UInt(log2Ceil(cols).W))
    val nxt_addrb = Reg(UInt(log2Ceil(cols).W))
    val nxt_dina = Reg(UInt((streaming_width * bw).W))

    nxt_ena := false.B
    nxt_enb := false.B
    nxt_wea := wea
    nxt_addra := addra
    nxt_addrb := addrb
    nxt_dina := dina



    val mem = Module(new simple_dual(bw, streaming_width))

    mem.io.clka := clock
    mem.io.clkb := clock
    mem.io.ena := ena
    mem.io.enb := enb
    mem.io.wea := wea
    mem.io.addra := addra
    mem.io.addrb := addrb
    mem.io.dina := dina

    doutb := mem.io.doutb

    val nxt_dot_in = Reg(UInt((streaming_width * bw * 2).W))
    val dot_in = Reg(UInt((streaming_width * bw * 2).W))
    val dot_out = Reg(UInt((streaming_width * bw ).W))
    val vk0 = Reg(UInt(bw.W))



    ///////// enb and addrb setting/////
      //tk stage
      for (i <- 0 until cols) {
        //tk_cy stage enb and addrb
        for (j <- -1 until cols - 1-i) {
          when(cnt === ((i.U * HH_CY.U) + TK_CY.U + j.S.asUInt)) {
            nxt_enb := true.B
            nxt_addrb := (j + 1).U
          }.elsewhen(cnt === (i.U * HH_CY.U + HQR10_CY.U + TK_CY.U + j.S.asUInt)) {
            //tk_cy + hqr10_cy
            nxt_enb := true.B
            nxt_addrb := (j + 1).U
          }
        }

        // HH stage
      when(cnt === 0.U){
        nxt_enb := true.B
        nxt_addrb := i.U
      }.elsewhen(cnt === ((i.U * HH_CY.U) - 1.U) && (i.U * HH_CY.U) =/= 0.U) {// double check this
          nxt_enb := true.B
          nxt_addrb := i.U
        }.elsewhen(cnt === ((i.U * HH_CY.U) + VK_CY.U - 1.U)) {
          //vk stage
          nxt_enb := true.B
          nxt_addrb := i.U
        }
      }

    enb:= nxt_enb
    addrb:= nxt_addrb
        /////////////////////////////////////////////

      //dot input block


    val hh_table: Seq[UInt] = Seq.tabulate(cols)(i => (i * HH_CY).U)
    val vk_table: Seq[UInt] = Seq.tabulate(cols)(i => (i * HH_CY + VK_CY).U)
    val tk_table: Seq[Seq[UInt]] = Seq.tabulate(cols, cols) { (i, j) =>
      if (j < cols - i) (i * HH_CY + TK_CY + j).U else 0.U
    }

//    val nxt_dot_in_stage = Reg(UInt((2 * streaming_width * bw).W))
//    val stage_1 = Reg(UInt(( streaming_width * bw/2).W))
//    val stage_2 = Reg(UInt((streaming_width * bw).W))
//    val stage_3 = Reg(UInt((streaming_width * bw).W))

//    //HH_cy and vk_cy no iiner loop needed
//        for (i <- 0 until cols) {
//          switch(cnt) {
//
//            is(hh_table(i)) {
//
//              val dout_slice = doutb(((streaming_width - i) * bw) - 1, 0)
//              val dout_padded = Cat(0.U(i * bw), dout_slice)
//              nxt_dot_in_stage := Cat(dout_padded, dout_padded)
//
//
//            }
//            is(vk_table(i)) {
//              val dout_slice = doutb(((streaming_width - i - 1) * bw) - 1, 0)
//              stage_1 := Cat(0.U(i * bw), vk0)
//              stage_2 := Cat(stage_1, dout_slice)
//              nxt_dot_in_stage :=  Cat(stage_2, stage_2)
//
//            }
//
//          }
//
//          for (j <- 0 until cols-i) {
//            switch(cnt){
//            is(tk_table(i)(j)) {
//              val dout_slice1 = doutb(((streaming_width-i-1)*bw)-1,0)
//              val dout_slice2 = doutb(((streaming_width-i)*bw)-1,0)
//              stage_1 := Cat(0.U(i*bw),vk0)
//              stage_2 := Cat(stage_1, dout_slice1)
//              stage_3 := Cat(0.U(i*bw), dout_slice2)
//              nxt_dot_in_stage := Cat(stage_2,stage_3)
//
//
//            }
//          }
//          }
//
//
//      } // end of for cols loop
//    nxt_dot_in := nxt_dot_in_stage


    for (i <- 0 until cols) {
      switch(cnt) {

        is(hh_table(i)) {

          val dout_slice = doutb(((streaming_width - i) * bw) - 1, 0)
          nxt_dot_in := Cat(0.U(i * bw), dout_slice,0.U(i * bw), dout_slice)


        }
        is(vk_table(i)) {
          val dout_slice = doutb(((streaming_width - i - 1) * bw) - 1, 0)

          nxt_dot_in := Cat(0.U(i * bw), vk0, dout_slice,0.U(i * bw), vk0, dout_slice)

        }

      }

      for (j <- 0 until cols - i) {
        switch(cnt) {
          is(tk_table(i)(j)) {
            val dout_slice1 = doutb(((streaming_width - i - 1) * bw) - 1, 0)
            val dout_slice2 = doutb(((streaming_width - i) * bw) - 1, 0)

            nxt_dot_in := Cat(0.U(i * bw), vk0, dout_slice1,0.U(i * bw), dout_slice2)
          }
        }
      }
    }


//
//

    dot_in := nxt_dot_in




    val dot = Module(new FP_DDOT_dp(streaming_width,bw))
    for (i <- 0 until streaming_width) {
      dot.io.in_a(i) := dot_in((2*streaming_width-i)*bw-1, (2*streaming_width-i-1)*bw) // upper half
      dot.io.in_b(i) := dot_in((streaming_width-i)*bw-1, (streaming_width-i-1)*bw)     // lower half
    }


    dot_out:=dot.io.out_s

    val d2 = Reg(UInt(bw.W))

    val sqrt = Module(new FP_sqrt(bw)).io
    sqrt.aclk := this.clock
    sqrt.s_axis_a_tdata:= dot_out
    sqrt.s_axis_a_tvalid:= true.B
    d2:= sqrt.m_axis_result_tdata

    val x0 =  Reg(UInt(bw.W))
    val nxt_x0 = WireInit(x0)

    for ( i <-1 until cols){
      when(cnt === ((i.U * HH_CY.U)) ) {
        nxt_x0:= doutb((streaming_width-i)*bw-1, (streaming_width-i-1)*bw)
      }.elsewhen( cnt === 1.U){
        nxt_x0:= doutb((streaming_width)*bw-1, (streaming_width-1)*bw)
      }
    }

    x0:= nxt_x0

    val d2_s = Mux(x0(31), Cat(~d2(31), d2(30,0)), d2)
   val adder = Module(new FP_adder(bw)).io
       adder.aclk := this.clock
       adder.s_axis_a_tvalid:= true.B
       adder.s_axis_b_tvalid:= true.B
       adder.s_axis_a_tdata:= x0
       adder.s_axis_b_tdata:=d2_s
      vk0:= adder.m_axis_result_tdata
//
//
     val tk = Reg(UInt(bw.W))
//
    val divider = Module(new FP_divider(bw)).io
    divider.aclk := this.clock
    divider.s_axis_a_tvalid:= true.B
    divider.s_axis_b_tvalid:= true.B
    divider.s_axis_a_tdata:= "hc0000000".U(bw.W)
    divider.s_axis_b_tdata:=dot_out
    tk:= divider.m_axis_result_tdata
//
    val d5  = Reg(UInt(bw.W))
//
    val mult = Module(new FP_mult(bw)).io
    mult.aclk:= this.clock
    mult.s_axis_a_tvalid:= true.B
    mult.s_axis_b_tvalid:= true.B
    mult.s_axis_a_tdata:= tk
    mult.s_axis_b_tdata:=dot_out
    d5:= mult.m_axis_result_tdata
//
    val d5_reg = RegNext(d5)

    val vk = Reg(UInt((streaming_width*bw).W))
    val nxt_vk = WireInit(0.U((streaming_width*bw).W))


    for ( i <-0 until cols){
      when(cnt === (((i.U * HH_CY.U) + VK_CY.U)) ) {
        val padding =  Cat(0.U(i*bw))
        val doutb_slice = doutb(((streaming_width-i-1)*bw)-1,0)
        nxt_vk := Cat(padding,vk0,doutb_slice)
      }
    }

    vk:= nxt_vk

    val axpy_output = WireInit(VecInit(Seq.fill(streaming_width)(0.U(bw.W))))
    val axpy = Module(new axpy_dp(bw, streaming_width)).io

      axpy.in_a:=d5_reg
      for (i <- 0 until streaming_width) {
        axpy.in_b(i) := vk((streaming_width-i)*bw-1, (streaming_width-i-1)*bw)
        axpy.in_c(i) := doutb((streaming_width-i)*bw-1, (streaming_width-i-1)*bw)
        axpy_output(i) := axpy.out_s(i)
    }
//
//    // wb stage


    val wea_mask = ~0.U(bw.W)
    for (i <- 0 until cols) {
      //HH writeback stage
      for (j <- -1 until cols - 1-i) {
        when(cnt === ((i.U * HH_CY.U)+ j.S.asUInt)) {
          nxt_ena := true.B
          nxt_addra := (j + 1).U
          nxt_dina := axpy_output.asUInt
          val shift = ((j+1).U  << 2)
          nxt_wea := wea_mask >> shift
        }
          //        .otherwise{
//          nxt_ena := false.B
//          nxt_addra := addra
//          nxt_dina := dina
//          nxt_wea := ~0.U(bw.W)
//        }
      }
    }

    ena := nxt_ena
    addra:= nxt_addra
    dina:=nxt_dina
    wea:= nxt_wea



    io.matrix_out := doutb

    /// use this only for large sw , ie 32





    when(cnt === HH_CY.U*cols.U){
      io.qr_fi := true.B
    }.otherwise{
      io.qr_fi :=false.B
    }
//
    //val out_1 = doutb((streaming_width*bw)-1,(streaming_width*bw/2))
   // io.matrix_out := out_1
//    val out_2 = doutb((streaming_width*bw/2)-1, 0)
//    val toggle = Reg(Bool())
//    io.matrix_out:= 0.U
//    when(io.qr_fi){
//      io.matrix_out:= Mux(toggle, out_1, out_2)
//      toggle := ~ toggle
//
//    }.otherwise{
//      toggle := false.B
//    }

  }



  class axpy_dp(bw: Int, level: Int) extends Module {
    val io = IO(new Bundle() {
      val in_a  = Input(UInt(bw.W))
      val in_b  = Input(Vec(level, UInt(bw.W)))
      val in_c  = Input(Vec(level, UInt(bw.W)))
      val out_s = Output(Vec(level, UInt(bw.W)))
    })

    // Multiplication stage
    val multiply_layer = for (i <- 0 until level) yield {
      val multiply = Module(new FP_mult(bw)).io
      multiply.s_axis_a_tvalid := true.B
      multiply.aclk := this.clock
      multiply.s_axis_b_tvalid := true.B
      multiply
    }

    // Addition stage
    val adder_layer = for (i <- 0 until level) yield {
      val adder = Module(new FP_adder(bw)).io
      adder.s_axis_a_tvalid := true.B
      adder.s_axis_b_tvalid := true.B
      adder.aclk := this.clock
      adder
    }

    // Pipeline registers for C values (no global reset)
    val reg_array_h = Vector.fill(level) {
      val r = RegInit(VecInit.fill(10)(0.U(bw.W))) // depth=10 from FPReg
      r
    }

    // Connect datapath
    for (i <- 0 until level) {
      multiply_layer(i).s_axis_a_tdata := io.in_a
      multiply_layer(i).s_axis_b_tdata := io.in_b(i)

      // Shift register for C input
      reg_array_h(i)(0) := io.in_c(i)
      for (d <- 1 until 10) {
        reg_array_h(i)(d) := reg_array_h(i)(d - 1)
      }

      adder_layer(i).s_axis_a_tdata := multiply_layer(i).m_axis_result_tdata
      adder_layer(i).s_axis_b_tdata := reg_array_h(i)(9)

      io.out_s(i) := adder_layer(i).m_axis_result_tdata
    }
  }

  class hqr7(bw: Int) extends Module {
    require(bw == 16 || bw == 32 || bw == 64 || bw == 128)
    val io = IO(new Bundle() {
      // val clk = Input(Clock())
      //val rst = Input(Bool())
      val in_a = Input(UInt(bw.W))
      val out_s = Output(UInt(bw.W))
    })

    //withClockAndReset(io.clk,io.rst){
    val multiplier = Module(new FP_mult(bw)).io
    val divider = Module(new FP_divider(bw)).io
    //multiplier.in_en := true.B
    divider.s_axis_a_tvalid := true.B
    divider.s_axis_b_tvalid := true.B

    divider.aclk := this.clock
    divider.s_axis_a_tdata := io.in_a

    divider.s_axis_b_tdata := "hc0000000".U
    multiplier.aclk := this.clock
    multiplier.s_axis_a_tvalid := true.B
    multiplier.s_axis_b_tvalid := true.B
    multiplier.s_axis_a_tdata := "hc0000000".U
    multiplier.s_axis_b_tdata := divider.m_axis_result_tdata

    io.out_s := multiplier.m_axis_result_tdata
  }

  class hqr5(bw: Int) extends Module {
    require(bw == 16 || bw == 32 || bw == 64 || bw == 128)
    val io = IO(new Bundle() {
      //val clk = Input(Clock())
      //val rst = Input(Bool())
      val in_a = Input(UInt(bw.W))
      val in_b = Input(UInt(bw.W))
      val out_s = Output(UInt(bw.W))
    })

    // withClockAndReset(io.clk,io.rst){
    val adder = Module(new FP_adder(bw)).io
    val subtractor = Module(new FP_sub(bw)).io
    //adder.in_en := true.B
    adder.s_axis_a_tvalid := true.B
    adder.aclk:= this.clock
    adder.s_axis_b_tvalid := true.B
    adder.s_axis_a_tdata := io.in_a
    adder.s_axis_b_tdata := io.in_b

    // subtractor.in_en := true.B
    subtractor.aclk := this.clock
    subtractor.s_axis_a_tvalid := true.B
    subtractor.s_axis_b_tvalid := true.B
    subtractor.s_axis_a_tdata := io.in_a
    subtractor.s_axis_b_tdata := io.in_b

    when(io.in_a(bw - 1)) {
      io.out_s := subtractor.m_axis_result_tdata
    }.otherwise {
      io.out_s := adder.m_axis_result_tdata
    }
  }

  class FP_DDOT_dp(n: Int, bw: Int) extends Module {
    val io = IO(new Bundle {

      val in_a = Input(Vec(n, UInt(bw.W)))// changed from Vec(n, UInt(bw.W))
      val in_b = Input(Vec(n, UInt(bw.W))) // changed from Vec(n, UInt(bw.W))
      val out_s = Output(UInt(bw.W))
    })

    // convert UInt to Vec internally
    val vec_a = Wire(Vec(n, UInt(bw.W)))
    val vec_b = Wire(Vec(n, UInt(bw.W)))

    vec_a := io.in_a
    vec_b := io.in_b
//    for (i <- 0 until n) {
//      vec_a(i) := io.in_a((n - i) * bw - 1, (n - i - 1) * bw)
//      vec_b(i) := io.in_b((n - i) * bw - 1, (n - i - 1) * bw)
//    }

    var temp_n = n
    val add_per_layer = mutable.ArrayBuffer[Int]()
    val regs_per_layer = mutable.ArrayBuffer[Int]()
    while (temp_n > 1) {
      if (temp_n % 2 == 1) {
        add_per_layer += temp_n / 2
        temp_n /= 2
        temp_n += 1
        regs_per_layer += 1
      } else {
        add_per_layer += temp_n / 2
        temp_n /= 2
        regs_per_layer += 0
      }
    }

    val multipliers = Vector.fill(n)(Module(new FP_mult(bw)).io)

    // connect Vec version internally
    multipliers.zipWithIndex.map(x => x._1.s_axis_a_tdata := vec_a(x._2))
    multipliers.zipWithIndex.map(x => x._1.s_axis_b_tdata := vec_b(x._2))
    multipliers.map(_.s_axis_a_tvalid := true.B)
    multipliers.map(_.s_axis_b_tvalid := true.B)
    multipliers.map(_.aclk := this.clock)

    if (add_per_layer.nonEmpty) {
      val regs_and_adds = for (i <- 0 until add_per_layer.length) yield {
        val adds = for (j <- 0 until add_per_layer(i)) yield {
          Module(new FP_adder(bw)).io
        }
        adds.map(_.s_axis_b_tvalid := true.B)
        adds.map(_.s_axis_a_tvalid := true.B)
        adds.map(_.aclk := this.clock)
        val regs = for (j <- 0 until regs_per_layer(i)) yield {
          Module(new FPReg(13, bw)).io
        }
        (adds, regs)
      }

      for (i <- 0 until regs_and_adds.length) {
        for (j <- 0 until add_per_layer(i) * 2 by 2) {
          val temp = if (i == 0) {
            multipliers(j).m_axis_result_tdata
          } else {
            regs_and_adds(i - 1)._1(j).m_axis_result_tdata
          }
          val temp2 = if (i == 0) {
            multipliers(j + 1).m_axis_result_tdata
          } else {
            if (j / 2 == add_per_layer(i) - 1) {
              if (regs_per_layer(i - 1) == 1 && add_per_layer(i - 1) % 2 == 1) {
                regs_and_adds(i - 1)._2(0).out
              } else {
                regs_and_adds(i - 1)._1(j + 1).m_axis_result_tdata
              }
            } else {
              regs_and_adds(i - 1)._1(j + 1).m_axis_result_tdata
            }
          }
          regs_and_adds(i)._1(j / 2).s_axis_a_tdata := temp
          regs_and_adds(i)._1(j / 2).s_axis_b_tdata := temp2
        }
        for (j <- 0 until regs_per_layer(i)) {
          val temp = if (i == 0) {
            multipliers(add_per_layer(i) * 2).m_axis_result_tdata
          } else {
            if (regs_per_layer(i - 1) == 1) {
              regs_and_adds(i - 1)._2(0).out
            } else {
              regs_and_adds(i - 1)._1(add_per_layer(i) * 2).m_axis_result_tdata
            }
          }
          regs_and_adds(i)._2(j).in := temp
        }
      }

      io.out_s := RegNext(regs_and_adds(add_per_layer.length - 1)._1(0).m_axis_result_tdata)
    } else {
      io.out_s := RegNext(multipliers(0).m_axis_result_tdata)
    }
  }


  class FPReg(depth: Int,bw: Int) extends Module{
    val io = IO(new Bundle() {
      val in = Input(UInt(bw.W))
      val out = Output(UInt(bw.W))
    })
    val reg = RegInit(VecInit.fill(depth)(0.U(bw.W)))
    //    val reg = RegInit(0.U.asTypeOf(new ComplexNum(bw)))
    reg(0) := io.in
    for(i <- 1 until depth){
      reg(i) := reg(i-1)
    }
    io.out := reg(depth - 1)
  }


  class FP_reciprocal(bw:Int)extends BlackBox{
    val io = IO {
      new Bundle() {
        val aclk = Input(Clock())
        val s_axis_a_tvalid = Input(Bool())
        val s_axis_a_tdata = Input(UInt(bw.W))
        val m_axis_result_tvalid = Output(Bool())
        val m_axis_result_tdata = Output(UInt(bw.W))
      }
    }
  }


  class FP_sqrt(bw:Int)extends BlackBox{
    val io = IO {
      new Bundle() {
        val aclk = Input(Clock())
        val s_axis_a_tvalid = Input(Bool())
        val s_axis_a_tdata = Input(UInt(bw.W))
        val m_axis_result_tvalid = Output(Bool())
        val m_axis_result_tdata = Output(UInt(bw.W))
      }
    }
  }



  class FP_sub(bw:Int)extends BlackBox{
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

  class FP_divider(bw:Int)extends BlackBox{
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



  class simple_dual(bw:Int, streaming_width:Int)extends BlackBox{
    val io = IO {
      new Bundle() {
        val clka = Input(Clock())
        val clkb = Input(Clock())
        val ena = Input(Bool())
        val enb = Input(Bool())
        val wea = Input(UInt((streaming_width*4).W))
        val addra = Input(UInt((log2Ceil(streaming_width)-1).W))
        val addrb = Input(UInt((log2Ceil(streaming_width)-1).W))
        val dina = Input(UInt((streaming_width*bw).W))
        val doutb = Output(UInt((streaming_width*bw).W))
      }
    }
  }


  class DOT(streaming_width: Int, bw:Int) extends Module {

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


  class AXPY(bw: Int, streaming_width: Int) extends Module {
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

}

  // end of object
