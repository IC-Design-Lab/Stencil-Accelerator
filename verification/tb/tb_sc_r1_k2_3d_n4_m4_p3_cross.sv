`include "../dut/define.v"
`define ERROR_THRESHOLD 5  //5% error tolerace
`define INPUT_NO (((`ROW*`COL*`DEPTH)-(2*(`RADIUS+`COL)))/`ST)
`define OUTPUT_NO ((`DEPTH/2*`RADIUS+1)*(`COL-2*`RADIUS)*(`ROW-2*`RADIUS))
`define POINTS 7
`define ROW 4
`define COL 4
`define DEPTH 3
`define ST 2
`define BW 32
`define RADIUS 1

module tb_sc();
//---------------------------------------------------------
//--- wire and reg declaration 
//---------------------------------------------------------
reg                     clock    ;
reg                     reset    ;
reg                    in_ready;
reg  [`ST*`BW-1:0]     in_matrix      ;
reg  [`POINTS*`BW-1:0]     in_weight   ;
wire [`ST*`BW-1:0]     out_data     ;
wire                   out_valid ;

reg [`ST*`BW-1:0]     input_ram[0:(`INPUT_NO)-1];
reg [`ST*`BW-1:0]    output_ram[0:(`OUTPUT_NO)-1];

reg [`POINTS*`BW-1:0] weights_ram[0:1];


initial begin
 $readmemh("../golden/test_3d_n4_m4_p3_r1_k2_shape_cross/input_ieee754.txt" , input_ram);
  $readmemh("../golden/test_3d_n4_m4_p3_r1_k2_shape_cross/output_ieee754.txt", output_ram);
  $readmemh("../golden/test_3d_n4_m4_p3_r1_k2_shape_cross/weights_ieee754.txt", weights_ram);

end

`ifdef SC_R1_K2_3D
SODA_3d u_sc_r1_k2_3d (.clock         (clock    ),
                           .reset         (reset    ),
                           .io_in_ready     (in_ready),
                           .io_in_matrix           (in_matrix      ),
                           .io_in_weight      (in_weight ),
                           .io_out_data        (out_data   ),
                          .io_out_valid         (out_valid    ));
`endif
		  
//---------------------------------------------------------------------
//------- BFM
//---------------------------------------------------------------------
integer i,j, output_index, j_out;
logic [`ST*`BW-1:0] row_data;
logic [(`ST)*`BW-1:0] row_data_out;
integer report, report_ieee754;
always #5 clock = ~clock;
initial begin
  clock           = 1'b0;
  reset           = 1'b1;
  in_ready       = 1'b0;
  in_matrix             = `BW*`ST'h0;
  in_weight          = `BW*`POINTS'h0;             ;
  @(posedge clock );
 reset           = 1'b0;
 
  #10;
  @(posedge clock );
 
 for(i=0; i<(`INPUT_NO);i=i+1) begin
//	  row_data = input_ram[i];
//	  for(j =(`COL); j >=`ST-1 ; j=j-`ST) begin

 in_ready = 1'b1;
 in_matrix = input_ram[i];
 in_weight = weights_ram[0];
 @(posedge clock);
//  end
end
  in_ready        = 1'b0;
  repeat(10) @(posedge clock);
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
  wait(~reset);
  `ifdef SC_R1_K2_3D
      report=$fopen("./sc_r1_k2_3d/n4_m4_p3/report.log", "w");
      report_ieee754=$fopen("./sc_r1_k2_3d/n4_m4_p3/report_ieee754.log", "w");
  `endif

  for(output_index=0; output_index<`OUTPUT_NO;) begin
	  
    @(negedge clock);
    if(out_valid) begin 
      golden_result=output_ram[output_index];

      dut_result   =out_data;
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
