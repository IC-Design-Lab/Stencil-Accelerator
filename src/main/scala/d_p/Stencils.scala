package Stencils

import chisel3._
import chisel3.util.{Cat, MuxCase, ShiftRegister, is, log2Ceil, switch}
import java.io._
import FPPackageMario.FP_Modules.FPUnits._



object dp {
  def main(args: Array[String]): Unit = {
    val dir = new File("verification/dut")
    dir.mkdirs()
    val sw2 = new PrintWriter(new File(dir, "sc_r3_k3_2d.sv"))
    sw2.println(circt.stage.ChiselStage.emitSystemVerilog(new SODA_2d(32, 2 ,8, 3,10,13,5,"cross")))
    sw2.close()
  }
}

object dp3 {
  def main(args: Array[String]): Unit = {
    val dir = new File("verification/dut")
    dir.mkdirs()
    val sw2 = new PrintWriter(new File(dir, "sc_r1_k2_3d.sv"))
    sw2.println(circt.stage.ChiselStage.emitSystemVerilog(new SODA_3d(32, 2,8,8,5 ,1,10,13,7,3)))
    sw2.close()
  }
}

object s_dual {
  def main(args: Array[String]): Unit = {
    val dir = new File("verification/dut")
    dir.mkdirs()
    val sw2 = new PrintWriter(new File(dir, "soda_dual.sv"))
    sw2.println(circt.stage.ChiselStage.emitSystemVerilog(new SODA_dual_core(32, 2,12,12 ,11,1,10,13,7,4)))
    sw2.close()
  }
}

class SODA_dual_core(bw: Int, k: Int, x:Int, y:Int,z:Int, r: Int, mult_pd: Int, add_pd: Int, points: Int, time_steps:Int) extends Module {
  val io = IO(new Bundle() {

    val in_matrix_d = Input(UInt((bw * k).W))
    val in_weight_d = Input(UInt((bw * points).W))
    val in_ready_d = Input(Bool())
    val out_data_d = Output(UInt((bw * k).W))
    val out_valid_d = Output(UInt(k.W))
  })

  val core_1 = Module(new SODA_3d(bw,k,x,y,z, r, mult_pd, add_pd, points, time_steps))
  val core_2 = Module(new SODA_3d(bw,k,x,y,z, r, mult_pd, add_pd, points, time_steps))

  val ADD_CC = 13
  val MULT_CC = 10
  val ADD_TREE_CC = log2Ceil(points) * ADD_CC // latency of the cascading adders except for the last one
  val DOT_CC = MULT_CC + ADD_TREE_CC // latency of the full dot product
  val counter_c = RegInit(1.U(bw.W))
  val dram_load  = RegInit(true.B)
  val count_flag = RegInit(false.B)
  val input_size = ((x*y*z)-2*(x+r))/k
  val pipeline_flush = (((x*y)-(r+x))+(x*y)+(2*(x+r)))/k
  val core1_valid_latency = (x*y) + DOT_CC
  val stage_latency = DOT_CC + pipeline_flush
  val delay_latency = input_size - (core1_valid_latency + stage_latency) + 1
  val core1_steps = (time_steps+1)/2
  val core2_steps = time_steps/2
  val core1_valid = Array.fill(core1_steps)(0)
  val core2_valid = Array.fill(core2_steps)(0)
  var wrap_cnt = 0
  val wrap_stride = 1
  core1_valid(0) = core1_valid_latency  // time step 1
  core2_valid(0) = core1_valid(0) + stage_latency + delay_latency   // time step 2
  wrap_cnt = (wrap_cnt +2)% k    // k moves by two strides for the first time step
  var core1_index = 1
  var core2_index = 1

  for(t <- 2 until time_steps) { // ping pong between the two cores
    if(t % 2 == 0){ // core1 turn
      if(wrap_cnt == 0){
        core1_valid(core1_index) = core2_valid(core2_index-1) + DOT_CC +  pipeline_flush + wrap_stride}
      else {
        core1_valid(core1_index) = core2_valid(core2_index-1)  + DOT_CC + pipeline_flush
      }
      wrap_cnt = (wrap_cnt + 1) % k
      core1_index += 1
    } else { // core2 turn
      if(wrap_cnt == 0){
        core2_valid(core2_index) = core1_valid(core1_index-1) + DOT_CC+ pipeline_flush + delay_latency + wrap_stride}
      else {
        core2_valid(core2_index) = core1_valid(core1_index-1) + DOT_CC+ pipeline_flush + delay_latency}
      wrap_cnt = (wrap_cnt + 1) % k
      core2_index += 1
    }
  }

  val first_valid =
    if (time_steps % 2 == 1) {
      core1_valid(core1_steps - 1)}
    else {
      core2_valid(core2_steps - 1)
    }
  val first_valid_unsigned = first_valid.U
  val offset = (r * time_steps).U
  val start_index = offset + offset * x.U + offset * x.U * y.U -1.U
  val last_index = (x*y*z - (r+x) - 1).U
  val grid_counters = RegInit(VecInit((0 until k).map(_ => 0.U(bw.W))))
  val lane_offset = (k - (time_steps % k)) % k
  when(counter_c === first_valid_unsigned-1.U ) { // grid count initilization
    for(i <- 0 until k) {
      grid_counters(i) := start_index + i.U + ((i+lane_offset) % k).U
    }
  }.elsewhen(counter_c > first_valid_unsigned -1.U) { // grid count until last point
    for(i <- 0 until k) {
      grid_counters(i) := grid_counters(i) + k.U
    }
  }

  val ix = Wire(Vec(k, UInt(log2Ceil(x).W)))
  val iy = Wire(Vec(k, UInt(log2Ceil(y).W)))
  val iz = Wire(Vec(k, UInt(log2Ceil(z).W)))
  for(i <- 0 until k) {
    ix(i) := grid_counters(i) % x.U
    iy(i) := (grid_counters(i)/x.U) % y.U
    iz(i) := grid_counters(i)/(x.U * y.U)
  }

  val lower_x = (r.asUInt*time_steps.asUInt)
  val upper_x = (x.asUInt - 1.U) - (r.asUInt*time_steps.asUInt)
  val lower_y = (r.asUInt*time_steps.asUInt)
  val upper_y = (y.asUInt - 1.U) - (r.asUInt*time_steps.asUInt)
  val lower_z = (r.asUInt*time_steps.asUInt)
  val upper_z = (z.asUInt - 1.U) - (r.asUInt*time_steps.asUInt)
  val valid_vec = Wire(Vec(k, Bool()))

  for(i <- 0 until k) {
    valid_vec(i) :=
      ix(i) >= lower_x && ix(i) <= upper_x &&
        iy(i) >= lower_y && iy(i) <= upper_y &&
        iz(i) >= lower_z && iz(i) <= upper_z
  }

  val done_matrix = grid_counters(k-1) >= last_index
  val done_matrix_d = RegNext(done_matrix, false.B)
  when(done_matrix_d) {
    grid_counters := VecInit(Seq.fill(k)(0.U(bw.W)))
    counter_c := 0.U
  }

  when(io.in_ready_d){count_flag := true.B}
  when(count_flag){counter_c := counter_c + 1.U}
  when(counter_c <= input_size.U-2.U){
    dram_load := true.B
  }.otherwise{
    dram_load := false.B
  }

  val input_counter = RegInit(0.U(bw.W))
  val dram_active   = RegInit(true.B)
  when(io.in_ready_d && dram_active){input_counter := input_counter + 1.U}
  when(input_counter === input_size.U){dram_active := false.B}
  val feedback_valid = core_2.io.out_valid
  val core1_input = Mux(dram_active, io.in_matrix_d, core_2.io.out_data)
  val core1_valid_flag = Mux(dram_active, io.in_ready_d, feedback_valid)

  core_1.io.in_matrix := core1_input
  core_1.io.in_ready  := core1_valid_flag
  core_1.io.in_weight := io.in_weight_d
  core_2.io.in_matrix := ShiftRegister(core_1.io.out_data,delay_latency)
  core_2.io.in_ready  := core_1.io.out_valid
  core_2.io.in_weight := io.in_weight_d
  io.out_data_d:= core_1.io.out_data
  io.out_valid_d := valid_vec.asUInt
}

class FIFO_r(val bw: Int, val depth: Int) extends Module {
  val io = IO(new Bundle {
    val in  = Input(UInt(bw.W))
    val out = Output(UInt(bw.W))
  })

  val regs = RegInit(VecInit(Seq.fill(depth)(0.U(bw.W))))
  regs(0) := io.in
  for(i <- 1 until depth){
    regs(i) := regs(i-1)
  }
  io.out := regs.last
}

class SODA_3d(bw: Int, k: Int, x:Int, y:Int,z:Int, r: Int, mult_pd: Int, add_pd: Int, points: Int, time_steps:Int) extends Module {
  val io = IO(new Bundle() {

    val in_matrix = Input(UInt((bw *  k).W))
    val in_weight = Input(UInt((bw *points).W))
    val in_ready = Input(Bool())
    val out_data = Output(UInt((bw * k).W))
    val out_valid = Output(Bool())
  })

  val ADD_CC = 13
  val MULT_CC = 10
  val ADD_TREE_CC = log2Ceil(points) * ADD_CC // latency of the cascading adders except for the last one
  val DOT_CC = MULT_CC + ADD_TREE_CC // latency of the full dot product
  val W_SIZE = points
  val init_g = x+r
  val t = RegInit(1.U(bw.W))
  io.out_valid:= ShiftRegister(io.in_ready,DOT_CC+(x*y))
  val weights = Reg(Vec(W_SIZE, UInt(bw.W)))
  val length = x // this is the length of the spatial block, it is assumed x = y = z for each block
  for(i <- 0 until (W_SIZE)){
    weights(i) := io.in_weight(W_SIZE*bw-(i*bw)-1, (W_SIZE*bw-(bw*(i+1))))
  }
  val counter = RegInit(0.U(bw.W))
  val count_f = RegInit(false.B)
  when(io.in_ready){count_f := true.B}
  when(count_f){counter := counter + 1.U}

  val scs = Seq.fill(k) {Module(new dot(bw, points, mult_pd, add_pd))}
  // gen starting sequence
  val a: Seq[Int] = Seq(-x*y, -x,-1, 0, 1,x,x*y)
  // expand set based on k
  val a_k = (for {shift <- 0 until k
                  point <- a} yield point + shift).distinct.sorted
  // gen the chains
  val chains = for (i <- 0 until k)
    yield a_k.iterator.filter(point => ((point % k + k) % k) == i).toSeq
  val fifo_depths_reversed: Seq[Seq[Int]] = chains.map(_ .sliding(2).map { case Seq(a,b) => (b-a)/k}.toSeq.reverse)
  val a_k_trimmed = a_k.dropRight(k)
  // map fifos to offsets
  val offset_map: Seq[Seq[Int]] = chains.map {chain =>chain.reverse.filter(a_k_trimmed.contains)}
  val fifo_offset_map = scala.collection.mutable.Map[Int,FIFO_r]()
  val fifos: Seq[Seq[FIFO_r]]=
    fifo_depths_reversed.zipWithIndex.map {case (depths,chainid) =>
      depths.zipWithIndex.map { case (depth, i) =>
        val offsetForName = offset_map(chainid)(i)
        val fifo = Module(new FIFO_r(bw, depth))
        fifo.suggestName(s"chain${chainid}_fifo${offsetForName}")
        fifo_offset_map(offsetForName) = fifo
        fifo
      }
    }

  val fifo_map: Map[Int, FIFO_r] = fifo_offset_map.toMap
  scs.zipWithIndex.foreach { case (sc,sc_id) =>
    // shifted offsets for each sc
    val sc_offsets = a.map(_ +sc_id)
    val fifo_offsets = sc_offsets.dropRight(1)
    val chain_offset = sc_offsets.last // last
    fifo_offsets.zipWithIndex.foreach { case (offset, in) =>
      sc.io.in_matrix(in) := fifo_map(offset).io.out
      sc.io.in_weight(in) := weights(in)
    }
    val last_input = fifo_offsets.length
    sc.io.in_matrix(last_input) := io.in_matrix(k*bw-(sc_id*bw)-1,(k*bw-(bw*(sc_id+1))))
    sc.io.in_weight(last_input) := weights(last_input)
  }

  val sc_outputs = Wire(Vec(k, UInt(bw.W)))

  for (i <- fifos.indices) { // i index of chain
    for (j <- fifos(i).indices) { // j index of fifo inside chain
      if (j == 0) {
        fifos(i)(j).io.in := io.in_matrix(k*bw-(i*bw)-1, (k*bw - (bw*(i+1))))
      }
      else {
        fifos(i)(j).io.in := fifos(i)(j - 1).io.out
      }
    }
  }

  for (i <- scs.indices) {sc_outputs(math.abs(i-(k-1))) := scs(i).io.out_data}
  io.out_data := sc_outputs.asUInt
}

class SODA_2d(bw: Int, k: Int, m:Int, r:Int, mult_pd: Int, add_pd: Int, points: Int, shape: String) extends Module {
  val io = IO(new Bundle() {
    val in_matrix = Input(UInt((bw * k).W))
    val in_weight = Input(UInt((bw *points).W))
    val in_ready = Input(Bool())
    val out_data = Output(UInt((bw * k).W))
    val out_valid = Output(Bool())
  })

  val d = 2*r+1
  val ADD_CC = 13
  val MULT_CC = 10
  val ADD_TREE_CC = log2Ceil(points - 1) * ADD_CC // latency of the cascading adders except for the last one
  val DOT_CC = MULT_CC + ADD_TREE_CC + ADD_CC // latency of the full dot product
  val LATENCY = ((d-1)*(m/k))+DOT_CC
  val W_SIZE = points
  val weights = Reg(Vec(W_SIZE, UInt(bw.W)))
  for(i <- 0 until (W_SIZE)){
    weights(i) := io.in_weight(W_SIZE * bw - (i*bw) - 1, (W_SIZE * bw - (bw*(i+1))))
  }
  io.out_valid := ShiftRegister(io.in_ready, LATENCY)
  val scs = Seq.fill(k) {
    Module(new dot(bw, points, mult_pd, add_pd))
  }
  // gen starting sequence
  val a: Seq[Int] = Seq(-m, -1, 0, 1, m) // cross
  val a_k = (for {shift <- 0 until k
                  point <- a} yield point + shift).distinct.sorted
  val chains =
    for (i <- 0 until k)
      yield a_k.iterator.filter(point => ((point % k + k) % k) == i).toSeq
  val fifo_depths_reversed: Seq[Seq[Int]] =
    chains.map { chain =>
      chain.sliding(2).map { case Seq(a, b) => (b - a) / k }.toSeq.reverse}
  val a_k_trimmed =  a_k.dropRight(k)
  val offset_map: Seq[Seq[Int]] = chains.map { chain => chain.reverse.filter(a_k_trimmed.contains) }
  val fifo_offset_map = scala.collection.mutable.Map[Int, FIFO_r]()
  val fifos: Seq[Seq[FIFO_r]] =
    fifo_depths_reversed.zipWithIndex.map { case (depths, chainid) =>
      depths.zipWithIndex.map { case (depth, i) =>
        val offsetForName = offset_map(chainid)(i)
        val fifo = Module(new FIFO_r(bw, depth))
        fifo.suggestName(s"chain${chainid}_fifo${offsetForName}")
        fifo_offset_map(offsetForName) = fifo
        fifo
      }
    }

  val fifo_map: Map[Int, FIFO_r] = fifo_offset_map.toMap
  scs.zipWithIndex.foreach { case (sc, sc_id) =>
    val sc_offsets = a.map(_ + sc_id)
    val fifo_offsets = sc_offsets.dropRight(1).filter(a_k_trimmed.contains)
    val chain_offset = sc_offsets.last // last one
    fifo_offsets.zipWithIndex.foreach { case (offset, in) =>
      sc.io.in_matrix(in) := fifo_map(offset).io.out
      sc.io.in_weight(in) := weights(in)
    }
    val last_input = fifo_offsets.length
    sc.io.in_matrix(last_input) := io.in_matrix(k * bw - (sc_id * bw) - 1, (k * bw - (bw * (sc_id + 1))))
    sc.io.in_weight(last_input) := weights(last_input)
  }

  val sc_outputs = Wire(Vec(k, UInt(bw.W)))

  for (i <- fifos.indices) { // i index of chain
    for (j <- fifos(i).indices) { // j index of fifo inside chain
      if (j == 0) {
        fifos(i)(j).io.in := io.in_matrix(k*bw-(i*bw) - 1, (k*bw - (bw*(i+1))))
      }
      else {
        fifos(i)(j).io.in := fifos(i)(j - 1).io.out
      }
    }
  }



  for (i <- scs.indices) {
    sc_outputs(math.abs(i-(k-1))) := scs(i).io.out_data
  }

  io.out_data := sc_outputs.asUInt

}

class dot(bw: Int, sw: Int, mult_pd: Int, add_pd: Int ) extends Module{
  val io = IO(new Bundle(){
    val in_matrix = Input(Vec(sw,UInt((bw.W))))
    val in_weight = Input(Vec(sw,UInt((bw.W))))
    val out_data = Output(UInt(bw.W))
  })
  val ADD_CC = 13
  val MULT_CC = 10
  val ADD_TREE_CC = log2Ceil(sw)*ADD_CC
  val DOT_CC = MULT_CC + ADD_TREE_CC

  val matrix = (0 until sw).map(i=> Cat(io.in_matrix).asUInt(sw*bw-(i*bw)-1,(sw * bw - (bw*(i+1)))))
  val weights = (0 until sw).map(i=> Cat(io.in_weight).asUInt(sw*bw-(i*bw)-1,(sw * bw - (bw*(i+1)))))
  val terms = (0 until sw).map{ i =>

    val mult = Module(new FP_mult(bw,mult_pd)).io
    mult.in_valid:= true.B
    mult.in_en:=true.B
    mult.in_a:= matrix(i)
    mult.in_b:= weights(i)
    mult.out_s
  }

  def Redux_Tree(inputs:Seq[UInt],depth:Int = 0): UInt = {
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
          ShiftRegister(single, ADD_CC)}
      Redux_Tree(next)
    }
  }
  io.out_data := Redux_Tree(terms)
}