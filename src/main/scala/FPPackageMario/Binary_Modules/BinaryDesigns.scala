package Binary_Modules

import chisel3._
import chisel3.util.{Counter, Mux1H, MuxCase, OHToUInt, PriorityMux, ShiftRegister, is, log2Ceil, log2Floor, switch}

object BinaryDesigns {

  class LZC_enc(n: Int) extends Module {
    val io = IO(new Bundle{
      val in_r = Input(UInt(n.W))
      val out_e = Output(UInt((log2Ceil(n)+1).W))
    })
    override def desiredName = s"LZC_enc${n}"
    val rounds = Math.pow(2,n).toInt
    val swmap = Array.fill(rounds)(0)
    swmap(0) = n
    var prev = 0
    for(i <- 0 until n) {
      for(j <- 0 until Math.pow(2, i).toInt) {
        swmap(prev + j + 1) = n - (i+1)
      }
      prev += Math.pow(2, i).toInt
    }
    val seq = (0 until rounds).map(i=>(io.in_r === i.U(n.W), swmap(i).U((log2Ceil(n)+1).W)))
    val out_enc = Wire(UInt((log2Ceil(n)+1).W))
    out_enc := MuxCase(0.U,seq)
    io.out_e := out_enc
  }

  class LZC_enc_name(n: Int,name:Int) extends Module {
    val io = IO(new Bundle{
      val in_r = Input(UInt(n.W))
      val out_e = Output(UInt((log2Ceil(n)+1).W))
    })
    override def desiredName = s"LZC_enc${n}_${name}"
    val rounds = Math.pow(2,n).toInt
    val swmap = Array.fill(rounds)(0)
    swmap(0) = n
    var prev = 0
    for(i <- 0 until n) {
      for(j <- 0 until Math.pow(2, i).toInt) {
        swmap(prev + j + 1) = n - (i+1)
      }
      prev += Math.pow(2, i).toInt
    }
    val seq = (0 until rounds).map(i=>(io.in_r === i.U(n.W), swmap(i).U((log2Ceil(n)+1).W)))
    val out_enc = Wire(UInt((log2Ceil(n)+1).W))
    out_enc := MuxCase(0.U,seq)
    io.out_e := out_enc
  }

  class LZC(bw: Int, N: Int) extends Module{
    val io = IO(new Bundle (){
      val in_d = Input(UInt(bw.W))
      val out_c = Output(UInt((log2Ceil(bw) + 1).W))
    })
    override def desiredName = s"LZC${bw}_${N}"
    val encoders = (0 until bw / N).map(i=>Module(new LZC_enc(N)).io)
    val encoded = (0 until bw / N).map(i=>{
      val enc_in = io.in_d(N*(i+1) - 1,N*i)
      encoders(i).in_r := enc_in
      encoders(i).out_e
    }).reverse
    val stages = log2Ceil(bw/N)
    val init_width = log2Ceil(N)+1
    val mergers = (0 until stages).map(i=>{
      val range = Math.pow(2, stages - (i+1)).toInt
      (0 until range).map(j=>{Module(new LZC_Merge(init_width+i)).io})
    })
    for(i <- 0 until stages){
      val range = Math.pow(2, stages - (i+1)).toInt
      for(j <- 0 until range){
        if(i == 0){
          mergers(i)(j).in_h := encoded(j*2)
          mergers(i)(j).in_l := encoded(j*2+1)
        }else{
          mergers(i)(j).in_h := mergers(i-1)(j*2).out_m
          mergers(i)(j).in_l := mergers(i-1)(j*2+1).out_m
        }
      }
    }
    io.out_c := mergers(stages-1)(0).out_m
  }
  class LZC_name(bw: Int, N: Int, name:Int) extends Module{
    val io = IO(new Bundle (){
      val in_d = Input(UInt(bw.W))
      val out_c = Output(UInt((log2Ceil(bw) + 1).W))
    })
    override def desiredName = s"LZC${bw}_${N}_${name}"
    val encoders = (0 until bw / N).map(i=>Module(new LZC_enc_name(N, name)).io)
    val encoded = (0 until bw / N).map(i=>{
      val enc_in = io.in_d(N*(i+1) - 1,N*i)
      encoders(i).in_r := enc_in
      encoders(i).out_e
    }).reverse
    val stages = log2Ceil(bw/N)
    val init_width = log2Ceil(N)+1
    val mergers = (0 until stages).map(i=>{
      val range = Math.pow(2, stages - (i+1)).toInt
      (0 until range).map(j=>{Module(new LZC_Merge_name(init_width+i, name)).io})
    })
    for(i <- 0 until stages){
      val range = Math.pow(2, stages - (i+1)).toInt
      for(j <- 0 until range){
        if(i == 0){
          mergers(i)(j).in_h := encoded(j*2)
          mergers(i)(j).in_l := encoded(j*2+1)
        }else{
          mergers(i)(j).in_h := mergers(i-1)(j*2).out_m
          mergers(i)(j).in_l := mergers(i-1)(j*2+1).out_m
        }
      }
    }
    io.out_c := mergers(stages-1)(0).out_m
  }

  class LZC_Merge(bw:Int) extends Module{
    val io = IO(new Bundle(){
      val in_h = Input(UInt(bw.W))
      val in_l = Input(UInt(bw.W))
      val out_m = Output(UInt((bw+1).W))
    })
    override def desiredName = s"LZC_Merge${bw}"
    val result_h = Wire(Vec(2,Bool()))
    val result_l = Wire(UInt((bw-1).W))
    result_h(1) := io.in_h(bw-1) & io.in_l(bw-1)
    result_h(0) := Mux(io.in_h(bw-1).asBool, !io.in_l(bw-1), io.in_h(bw-1))
    result_l := Mux(io.in_h(bw-1).asBool, io.in_l(bw-2,0), io.in_h(bw-2,0))
    io.out_m := result_h.asUInt ## result_l
  }
  class LZC_Merge_name(bw:Int, name:Int) extends Module{
    val io = IO(new Bundle(){
      val in_h = Input(UInt(bw.W))
      val in_l = Input(UInt(bw.W))
      val out_m = Output(UInt((bw+1).W))
    })
    override def desiredName = s"LZC_Merge${bw}_${name}"
    val result_h = Wire(Vec(2,Bool()))
    val result_l = Wire(UInt((bw-1).W))
    result_h(1) := io.in_h(bw-1) & io.in_l(bw-1)
    result_h(0) := Mux(io.in_h(bw-1).asBool, !io.in_l(bw-1), io.in_h(bw-1))
    result_l := Mux(io.in_h(bw-1).asBool, io.in_l(bw-2,0), io.in_h(bw-2,0))
    io.out_m := result_h.asUInt ## result_l
  }

  class full_adder(bw: Int) extends Module{
    val io = IO(new Bundle() {
      val in_a = Input(UInt(bw.W))
      val in_b = Input(UInt(bw.W))
      val in_c = Input(UInt(1.W))
      val out_s = Output(UInt(bw.W))
      val out_c = Output(UInt(1.W))
    })
    val result = Wire(UInt((bw+1).W))
    result := io.in_a +& io.in_b +& io.in_c
    io.out_s := result(bw-1,0)
    io.out_c := result(bw)
  }
  class full_adder_name(bw: Int, name:Int) extends Module{
    val io = IO(new Bundle() {
      val in_a = Input(UInt(bw.W))
      val in_b = Input(UInt(bw.W))
      val in_c = Input(UInt(1.W))
      val out_s = Output(UInt(bw.W))
      val out_c = Output(UInt(1.W))
    })

    override def desiredName = s"full_adder_${name}"
    val result = Wire(UInt((bw+1).W))
    result := io.in_a +& io.in_b +& io.in_c
    io.out_s := result(bw-1,0)
    io.out_c := result(bw)
  }

  class full_subtractor(bw: Int) extends Module{
    val io = IO(new Bundle() {
      val in_a = Input(UInt(bw.W))
      val in_b = Input(UInt(bw.W))
      val in_c = Input(UInt(1.W))
      val out_s = Output(UInt(bw.W))
      val out_c = Output(UInt(1.W))
    })
    val result = Wire(UInt((bw+1).W))
    result := io.in_a -& io.in_b -& io.in_c
    io.out_s := result(bw-1,0)
    io.out_c := result(bw)
  }

  class full_subtractor_name(bw: Int, name:Int) extends Module{
    val io = IO(new Bundle() {
      val in_a = Input(UInt(bw.W))
      val in_b = Input(UInt(bw.W))
      val in_c = Input(UInt(1.W))
      val out_s = Output(UInt(bw.W))
      val out_c = Output(UInt(1.W))
    })

    override def desiredName = s"full_subtractor_${name}"
    val result = Wire(UInt((bw+1).W))
    result := io.in_a -& io.in_b -& io.in_c
    io.out_s := result(bw-1,0)
    io.out_c := result(bw)
  }

  class multiplier(bw: Int) extends Module{
    val io = IO(new Bundle() {
      val in_a = Input(UInt(bw.W))
      val in_b = Input(UInt(bw.W))
      val out_s = Output(UInt((bw*2).W))
    })

    val result = Wire(UInt((bw*2).W))
    result := io.in_a * io.in_b
    io.out_s := result
  }

  class multiplier_name(bw: Int, name:Int) extends Module{
    val io = IO(new Bundle() {
      val in_a = Input(UInt(bw.W))
      val in_b = Input(UInt(bw.W))
      val out_s = Output(UInt((bw*2).W))
    })
    override def desiredName = s"full_multiplier_${name}"
    val result = Wire(UInt((bw*2).W))
    result := io.in_a * io.in_b
    io.out_s := result
  }

  class divider(bw: Int, L: Int, frac: Boolean) extends Module{
    require(L <= bw)
    val io = IO(new Bundle(){
      val in_ready = Input(Bool())
      val in_valid = Input(Bool())
      val in_reset = Input(Bool())
      val in_a = Input(UInt(bw.W))
      val in_b = Input(UInt(bw.W))
      val out_s = Output(UInt(bw.W))
      val out_r = Output(UInt(bw.W))
      val out_valid = Output(Bool())
    })
    override def desiredName = s"divider_BW$bw"
    //cc latency equal to bw
    val a_aux_reg = RegInit(VecInit.fill(L)(0.U((bw*2).W)))
    val b_aux_reg = RegInit(VecInit.fill(L)(0.U((bw*2).W)))
    val init_res = Wire(UInt(bw.W))
    init_res := 0.U
    val wire_res = Wire(Vec(L,UInt(bw.W)))
    val result_reg = RegInit(VecInit.fill(L)(0.U(bw.W)))
    val sr_out_valid = RegInit(VecInit.fill(L)(false.B))
    wire_res := result_reg
    io.out_valid := sr_out_valid.last
    io.out_s := result_reg.last.asUInt
    io.out_r  := a_aux_reg.last
    val ina = if (frac) io.in_a ## 0.U(bw.W) else 0.U(bw.W) ## io.in_a
    val inb = if (frac) io.in_b ## 0.U(bw.W) else 0.U(bw.W) ## io.in_b
    when(io.in_ready){
      when(io.in_reset){
        a_aux_reg := VecInit.fill(L)(0.U((bw*2).W))
        b_aux_reg := VecInit.fill(L)(0.U((bw*2).W))
        result_reg := VecInit.fill(L)(0.U(bw.W))
        sr_out_valid := VecInit.fill(L)(false.B)
      }.otherwise {
        sr_out_valid(0) := io.in_valid
        for (i <- 0 until L) {
          if (i == 0) {
            val t = VecInit(init_res.asBools)
            val shifted_b = if (frac) inb else (inb << (bw-1)).asUInt
            when(ina >= shifted_b) {
              a_aux_reg(0) := ina - shifted_b
              t(bw - 1) := 1.U
            }.otherwise {
              a_aux_reg(0) := ina
              t(bw - 1) := 0.U
            }
            result_reg(0) := t.asUInt
            b_aux_reg(0) := shifted_b
          } else {
            sr_out_valid(i) := sr_out_valid(i-1)
            val shifted_b = (b_aux_reg(i - 1) >> 1).asUInt
            val t = VecInit(wire_res(i - 1).asBools)
            when(a_aux_reg(i - 1) >= shifted_b) {
              a_aux_reg(i) := a_aux_reg(i - 1) - shifted_b
              t(bw - 1 - i) := 1.U
            }.otherwise {
              a_aux_reg(i) := a_aux_reg(i - 1)
              t(bw - 1 - i) := 0.U
            }
            result_reg(i) := t.asUInt
            b_aux_reg(i) := shifted_b
          }
        }
      }
    }
  }

  class divider_name(bw: Int, L: Int, frac: Boolean, name: Int) extends Module{
    require(L <= bw)
    val io = IO(new Bundle(){
      val in_ready = Input(Bool())
      val in_valid = Input(Bool())
      val in_reset = Input(Bool())
      val in_a = Input(UInt(bw.W))
      val in_b = Input(UInt(bw.W))
      val out_s = Output(UInt(bw.W))
      val out_r = Output(UInt(bw.W))
      val out_valid = Output(Bool())
    })
    override def desiredName = s"divider_BW${bw}_${name}"
    //cc latency equal to bw
    val a_aux_reg = RegInit(VecInit.fill(L)(0.U((bw*2).W)))
    val b_aux_reg = RegInit(VecInit.fill(L)(0.U((bw*2).W)))
    val init_res = Wire(UInt(bw.W))
    init_res := 0.U
    val wire_res = Wire(Vec(L,UInt(bw.W)))
    val result_reg = RegInit(VecInit.fill(L)(0.U(bw.W)))
    val sr_out_valid = RegInit(VecInit.fill(L)(false.B))
    wire_res := result_reg
    io.out_valid := sr_out_valid.last
    io.out_s := result_reg.last.asUInt
    io.out_r  := a_aux_reg.last
    val ina = if (frac) io.in_a ## 0.U(bw.W) else 0.U(bw.W) ## io.in_a
    val inb = if (frac) io.in_b ## 0.U(bw.W) else 0.U(bw.W) ## io.in_b
    when(io.in_ready){
      when(io.in_reset){
        a_aux_reg := VecInit.fill(L)(0.U((bw*2).W))
        b_aux_reg := VecInit.fill(L)(0.U((bw*2).W))
        result_reg := VecInit.fill(L)(0.U(bw.W))
        sr_out_valid := VecInit.fill(L)(false.B)
      }.otherwise {
        sr_out_valid(0) := io.in_valid
        for (i <- 0 until L) {
          if (i == 0) {
            val t = VecInit(init_res.asBools)
            val shifted_b = if (frac) inb else (inb << (bw-1)).asUInt
            when(ina >= shifted_b) {
              a_aux_reg(0) := ina - shifted_b
              t(bw - 1) := 1.U
            }.otherwise {
              a_aux_reg(0) := ina
              t(bw - 1) := 0.U
            }
            result_reg(0) := t.asUInt
            b_aux_reg(0) := shifted_b
          } else {
            sr_out_valid(i) := sr_out_valid(i-1)
            val shifted_b = (b_aux_reg(i - 1) >> 1).asUInt
            val t = VecInit(wire_res(i - 1).asBools)
            when(a_aux_reg(i - 1) >= shifted_b) {
              a_aux_reg(i) := a_aux_reg(i - 1) - shifted_b
              t(bw - 1 - i) := 1.U
            }.otherwise {
              a_aux_reg(i) := a_aux_reg(i - 1)
              t(bw - 1 - i) := 0.U
            }
            result_reg(i) := t.asUInt
            b_aux_reg(i) := shifted_b
          }
        }
      }
    }
  }

  // Sqrt specialized for 1.fractional numbers (really useful for floating point normalized numbers)
  class frac_sqrt(bw: Int, L: Int) extends Module{
    val io = IO(new Bundle(){
      val in_en = Input(Bool())
      val in_a = Input(UInt((bw+2).W))
      val in_valid = Input(Bool())
      val out_valid = Output(Bool())
      val out_s = Output(UInt(bw.W))
    })
    val P = RegInit(VecInit.fill(L-1)(0.U((bw*2+1).W)))
    val X = RegInit(VecInit.fill(L-1)(0.U((bw*2+2).W)))
    val one = 1.U(1.W) ## 0.U((bw*2).W)
    val temp_res = Wire(Vec(L,UInt(bw.W)))
    val results = RegInit(VecInit.fill(L)(0.U(bw.W)))
    val shifted_sqrd_ones = Wire(Vec(L, UInt((bw*2+1).W)))
    shifted_sqrd_ones.zipWithIndex.foreach(x=> x._1 := (one >> ((x._2 + 1)*2)))
    val shifted_ones = Wire(Vec(L, UInt((bw*2+1).W)))
    shifted_ones.zipWithIndex.foreach(x=> x._1 := (one >> (x._2 + 1)))
    val shifted_ps = Wire(Vec(L-1, UInt((bw*2+1).W)))
    shifted_ps.zipWithIndex.foreach(x=>x._1 := (P(x._2) >> (x._2+1)))
    temp_res(0) := 0.U
    temp_res.slice(1,L).zip(results.slice(0,L-1)).foreach(x=>x._1 := x._2)
    io.out_valid := ShiftRegister(io.in_valid,L,io.in_en)
    io.out_s := results.last
    val in = (io.in_a ## 0.U(bw.W)) - one
    //    printf(p"results: ${results}\n")
    when(io.in_en){
      for(i <- 0 until L){
        if(i == 0){
          val shifted_one = shifted_sqrd_ones(i)((bw*2)-1,0)
          val y = shifted_one +& one
          val yleqx = y <= in
          P(0) := Mux(yleqx, shifted_ones(i) + one, one)
          X(0) := Mux(yleqx, in - y, in)
          val t = VecInit(temp_res(0).asBools)
          t(bw-1) := yleqx
          results(0) := t.asUInt
        }else{
          val shifted_one = shifted_sqrd_ones(i)((bw*2)-1,0)
          val shifted_P = shifted_ps(i-1)
          val y = shifted_P +& shifted_one
          val yleqx = y <= X(i-1)
          if(i != L - 1) {
            P(i) := Mux(yleqx, shifted_ones(i) + P(i-1), P(i-1))
            X(i) := Mux(yleqx, X(i-1) - y, X(i-1))
          }
          val t = VecInit(temp_res(i).asBools)
          t(bw-1 - i):= yleqx
          results(i) := t.asUInt
        }
      }
    }
  }

  // Fixed Point Cordic Design

  // parameters:
  // bw - width of numbers (note that actual bitwidth will be bw + 1 due to sign)
  // v - vectoring mode
  // fbits - number of fractional bits out of bw
  // n - number of iterations

  // Description:
  // - use in rotation mode (v = false) for out_x = cos(in_d) & out_y = sin(in_d)
  // - use in vectoring mode (v = true) for out_z = atan(in_d)

  class cordic(bw: Int, v: Boolean, fbits: Int, n: Int) extends Module{
    val io = IO(new Bundle{
      val in_en = Input(Bool())
      val in_valid = Input(Bool())
      val in_d = Input(SInt((bw+1).W))
      val out_x = Output(SInt((bw+1).W))
      val out_y = Output(SInt((bw+1).W))
      val out_z = Output(SInt((bw+1).W))
      val out_valid = Output(Bool())
    })
    override def desiredName = if(!v) s"cordic_${bw}_$n" else s"vcordic_${bw}_$n"

    private val scale_f = BigDecimal(2).pow(fbits)
    private val Kbase = (0 until n).map(i => BigDecimal(Math.sqrt(1 + Math.pow(2, -i * 2)))).product
    private val K_inv = (((1/Kbase) * scale_f).toBigInt).asSInt((bw+1).W)
    private val angles = VecInit((0 until n).map(i=> (BigDecimal(Math.atan(Math.pow(2,-i))) * scale_f).toBigInt.asSInt((bw+1).W)))
    private val one = (BigDecimal(1.0) * scale_f).toBigInt.asSInt((bw+1).W)
    private val zero = 0.S((bw+1).W)
    private val quad_ang = (0 until 9).map(i=>(BigDecimal(Math.PI * (4-i) / 2) * scale_f).toBigInt.asSInt((bw+1).W))

    val cond = (1 until 9).map(i=>(io.in_d > quad_ang(i) && io.in_d <= quad_ang(i-1)))
    val res = (1 until 9).map(i=>if(i>4) (if((i-1) % 2 == 0) io.in_d - quad_ang(i-1) else quad_ang(i) - io.in_d) else (if((i-1) % 2 == 1) io.in_d - quad_ang(i) else quad_ang(i-1) - io.in_d))

    val corrected_angle = Mux1H(cond,res)
    val quad_detector = 3.U - OHToUInt(cond)(1,0)
    val inpsign = io.in_d(bw)


    val (x0,y0,z0) = if(!v) (K_inv, zero, corrected_angle) else (one, io.in_d, zero)

    val xiw = Wire(Vec(n+1, SInt((bw+1).W)))
    val yiw = Wire(Vec(n+1, SInt((bw+1).W)))
    val ziw = Wire(Vec(n+1, SInt((bw+1).W)))

    val siw = Wire(Vec(n+1, Bool()))

    val xir = RegInit(VecInit.fill(n)(0.S((bw+1).W)))
    val yir = RegInit(VecInit.fill(n)(0.S((bw+1).W)))
    val zir = RegInit(VecInit.fill(n)(0.S((bw+1).W)))

    xiw(0) := x0
    yiw(0) := y0
    ziw(0) := z0
    siw(0) := (if(!v) z0(bw).asBool else !y0(bw).asBool)

    xiw.slice(1,n+1).zip(xir.slice(0,n)).foreach(x=> x._1 := x._2)
    yiw.slice(1,n+1).zip(yir.slice(0,n)).foreach(x=> x._1 := x._2)
    ziw.slice(1,n+1).zip(zir.slice(0,n)).foreach(x=> x._1 := x._2)
    siw.slice(1,n+1).zip(if(!v) zir.slice(0,n) else yir.slice(0,n)).foreach(x=> x._1 := (if(!v) x._2(bw).asBool else !x._2(bw).asBool))

    when(io.in_en) {
      for (i <- 0 until n) {
        xir(i) := xiw(i) + Mux(siw(i), yiw(i) >> i, -yiw(i) >> i).asSInt
        yir(i) := yiw(i) + Mux(siw(i), -xiw(i) >> i, xiw(i) >> i).asSInt
        zir(i) := ziw(i) + Mux(siw(i), angles(i), -angles(i))
      }
    }

    val quad_detected = ShiftRegister(quad_detector, n, io.in_en)
    val sign_detected = ShiftRegister(inpsign, n, io.in_en)

    val cos = Wire(SInt((bw+1).W))
    val sin = Wire(SInt((bw+1).W))
    cos := 0.S
    sin := 0.S

    switch(quad_detected){
      is (0.U){
        cos := xiw(n)
        sin := Mux(sign_detected, -yiw(n), yiw(n))
      }
      is (1.U){
        cos := -xiw(n)
        sin := Mux(sign_detected, -yiw(n), yiw(n))
      }
      is (2.U){
        cos := -xiw(n)
        sin := Mux(sign_detected, yiw(n), -yiw(n))
      }
      is (3.U){
        cos := xiw(n)
        sin := Mux(sign_detected, yiw(n), -yiw(n))
      }
    }

    io.out_valid := ShiftRegister(io.in_valid, n, io.in_en)
    io.out_x := (if(!v) cos else xiw(n))
    io.out_y := (if(!v) sin else yiw(n))
    io.out_z := ziw(n)
//    io.out_z := ShiftRegister(quad_detector, n, io.in_en)
  }

}
