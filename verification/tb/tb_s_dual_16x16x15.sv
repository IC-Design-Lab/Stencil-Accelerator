`include "../dut/define.v"
`define ERROR_THRESHOLD 5  //5% error tolerace
`define INPUT_NO (((`ROW*`COL*`DEPTH)-(2*(`RADIUS+`COL)))/`ST)
`define OUTPUT_NO ((`DEPTH/2*`RADIUS+1)*(`COL-2*`RADIUS)*(`ROW-2*`RADIUS))
`define POINTS 7
`define ROW 16
`define COL 16
`define DEPTH 15
`define ST 2
`define BW 32
`define RADIUS 1

module tb_sc();
//---------------------------------------------------------
//--- wire and reg declaration 
//---------------------------------------------------------
reg                     clock    ;
reg                     reset    ;
reg                    in_ready_d;
reg  [`ST*`BW-1:0]     in_matrix_d      ;
reg  [`POINTS*`BW-1:0]     in_weight_d   ;
wire [`ST*`BW-1:0]     out_data_d     ;
wire [`ST-1:0]               out_valid_d ;

reg [`ST*`BW-1:0]     input_ram[0:(`INPUT_NO)-1];
reg [`ST*`BW-1:0]    output_ram[0:(`OUTPUT_NO)-1];

reg [`POINTS*`BW-1:0] weights_ram[0:1];


initial begin
 $readmemh("../golden/test_3d_n16_m16_p15_r1_k2_shape_cross/input_ieee754.txt" , input_ram);
  $readmemh("../golden/test_3d_n16_m16_p15_r1_k2_shape_cross/output_ieee754.txt", output_ram);
  $readmemh("../golden/test_3d_n16_m16_p15_r1_k2_shape_cross/weights_ieee754.txt", weights_ram);

end

`ifdef SC_R1_K2_3D
SODA_dual_core u_dual (.clock         (clock    ),
                           .reset         (reset    ),
                           .io_in_ready_d     (in_ready_d),
                           .io_in_matrix_d           (in_matrix_d      ),
                           .io_in_weight_d      (in_weight_d ),
                           .io_out_data_d        (out_data_d   ),
                          .io_out_valid_d         (out_valid_d    ));
`endif
		  
//---------------------------------------------------------------------
//------- BFM
//---------------------------------------------------------------------
integer i,j, output_index, j_out, start;
logic [`ST*`BW-1:0] row_data;
logic [(`ST)*`BW-1:0] row_data_out;
integer report, report_ieee754;
always #5 clock = ~clock;
initial begin
  clock           = 1'b0;
  reset           = 1'b1;
  in_ready_d       = 1'b0;
  in_matrix_d             = `BW*`ST'h0;
  in_weight_d          = `BW*`POINTS'h0;             ;

repeat (5) @(posedge clock);

 @(posedge clock);
//  #100
  reset = 1'b0;
//  #16
    
repeat (10)  @(posedge clock );
 for(i=0; i<(`INPUT_NO);i=i+1) begin
// @(posedge clock);
 in_ready_d = 1'b1;
 in_matrix_d = input_ram[i];
 in_weight_d = weights_ram[0];
 @(posedge clock);
//  end
end
@(posedge clock);
  in_ready_d        = 1'b0;
 // repeat(10) @(posedge clock);
end

// -------------------------------------------
// --Monitor: Simulation Log -----------------
// -------------------------------------------

`include "functions/signal_declare.sv"
`include "functions/tb_func.sv"

 reg  [`ST*`BW-1:0] golden_result;
 reg  [`ST*`BW-1:0] dut_result   ;
initial begin
  output_index    = 0;
  start = 0;
  wait(~reset);
  `ifdef SC_R1_K2_3D
      report=$fopen("./sc_r1_k2_3d/n16_m16_p15/report.log", "w");
      report_ieee754=$fopen("./sc_r1_k2_3d/n16_m16_p15/report_ieee754.log", "w");
  `endif

  wait(out_valid_d[0] ||out_valid_d[1]  );
  for(output_index=0; output_index<`OUTPUT_NO;) begin
	 @(negedge clock);
       if(out_valid_d[0] == 1) begin 
      golden_result=output_ram[output_index][63:32];

      dut_result   =out_data_d[63:32];
      `include "functions/error_percent_abs_cal.sv"
      `include "functions/comp_abs.sv"
      `include "functions/comp_ieee754.sv"
     // output_index=output_index+1;
    end
    if(out_valid_d[1] == 1) begin 
      golden_result=output_ram[output_index][31:0];

      dut_result   =out_data_d[31:0];
      `include "functions/error_percent_abs_cal.sv"
      `include "functions/comp_abs.sv"
      `include "functions/comp_ieee754.sv"
	output_index=output_index+1;
      
         end

  end

  `ifdef SC_R1_K2_3D
      $fclose(report);
      $fclose(report_ieee754);
  `endif
end







endmodule
