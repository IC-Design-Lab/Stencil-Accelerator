`include "../dut/define.v"
`define ERROR_THRESHOLD 5  //5% error tolerace
`define INPUT_NO (`ROW*`COL/`ST-1)
`define OUTPUT_NO ((`ROW-2)*`COL/`ST)
module tb_sc();
//---------------------------------------------------------
//--- wire and reg declaration 
//---------------------------------------------------------
reg                     clock    ;
reg                     reset    ;
reg                    in_ready;
reg  [`ST*`BW-1:0]     in_matrix      ;
reg  [`ST*`ST*`BW-1:0]     in_weight   ;
wire [`ST*`BW-1:0]     out_data     ;
wire                   out_valid ;

reg [`COL*`BW-1:0]     input_ram[0:`ROW-1];
reg [(`COL-2)*`BW-1:0]    output_ram[0:(`ROW-2)-1];
reg [`ST*`ST*`BW-1:0] weights_ram[0:1];


initial begin
  $readmemh("../golden/input_ieee754.txt" , input_ram);
  $readmemh("../golden/output_ieee754.txt", output_ram);
  $readmemh("../golden/weights_ieee754.txt", weights_ram);

end

`ifdef SC_R3_K3_2D
SODA_2d u_sc_r3_k3_2d (.clock         (clock    ),
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
integer i,j, i_out, j_out;
logic [`COL*`BW-1:0] row_data;
logic [(`COL-2)*`BW-1:0] row_data_out;
integer report, report_ieee754;
always #5 clock = ~clock;
initial begin
  clock           = 1'b0;
  reset           = 1'b1;
  in_ready       = 1'b0;
  in_matrix             = `BW*`ST'h0;
  in_weight          = `BW*`ST*`ST'h0;             ;
  @(posedge clock );
 
  reset           = 1'b0;
  #10;
  @(posedge clock );

  for(i=0; i< `ROW;i=i+1) begin
	  row_data = input_ram[i];
	  for(j =(`COL); j >=`ST-1 ; j=j-`ST) begin



    in_ready   = 1'b1;
    in_matrix    =  row_data[(`BW*j)-1 -: (`BW*`ST)];
    in_weight      =weights_ram[0];  
    @(posedge clock);
  end
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
 // output_index    = 0;
  wait(~reset);
  `ifdef SC_R3_K3_2D
      report=$fopen("./sc_r3_k3_2d/report.log", "w");
      report_ieee754=$fopen("./sc_r3_k3_2d/report_ieee754.log", "w");
  `endif

  for(i_out=0; i_out<`ROW;i_out=i_out+1) begin
	   row_data_out = output_ram[i];

	  for(j_out =`COL-2; j_out >=`ST-1 ; j_out=j_out-`ST) begin

    @(negedge clock);
    if(out_valid) begin 
      golden_result=row_data_out[(`BW*j)-1 -: (`BW*`ST)];

      dut_result   =out_data;
      `include "functions/error_percent_abs_cal.sv"
      `include "functions/comp_abs.sv"
      `include "functions/comp_ieee754.sv"
   //   output_index=output_index+1;
    end
  end
end
  `ifdef SC_R3_K3_2D
      $fclose(report);
      $fclose(report_ieee754);
  `endif
end

endmodule
