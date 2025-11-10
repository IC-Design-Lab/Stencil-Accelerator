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
reg                     din_ready;
reg  [`ST*`BW-1:0]      din      ;
reg  [`ST*`BW-1:0]      din_wt   ;
wire [`ST*`BW-1:0]      dout     ;
wire                    dout_vld ;

reg [`ST*`BW-1:0]     input_ram[0:`INPUT_NO-1];
reg [`ST*`BW-1:0]    output_ram[0:`OUTPUT_NO-1];

initial begin
  $readmemh("../golden/input.txt" , input_ram);
  $readmemh("../golden/output.txt", output_ram);
end

`ifdef SC_R3_K3_2D
sc_r3_k3_2d u_sc_r3_k3_2d (.clock         (clock    ),
                           .reset         (reset    ),
                           .din_ready     (din_ready),
                           .din           (din      ),
                           .dout_vld      (dout_vld ),
                           .din_wt        (din_wt   ),
                           .dout          (dout     ));
`endif
		  
//---------------------------------------------------------------------
//------- BFM
//---------------------------------------------------------------------
integer input_index, output_index;
integer report, report_ieee754;
always #5 clock = ~clock;
initial begin
  clock           = 1'b0;
  reset           = 1'b1;
  din_ready       = 1'b0;
  din             = `BW*`ST'h0;
  din_wt          = `BW*`ST'h0;             ;
  @(posedge clock );
 
  reset           = 1'b0;
  #10;
  @(posedge clock );

  for(input_index=0; input_index<`INPUT_NO;input_index=input_index+1) begin
    din_ready   = 1'b1;
    din         = input_ram[input_index];
    din_wt      =160'h3F8000003F8000003F8000003F8000003F800000 ;  
    @(posedge clock);
  end

  din_ready        = 1'b0;
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
  `ifdef SC_R3_K3_2D
      report=$fopen("./sc_r3_k3_2d/report.log", "w");
      report_ieee754=$fopen("./sc_r3_k3_2d/report_ieee754.log", "w");
  `endif

  for(output_index=0; output_index<`OUTPUT_NO; output_index=output_index+1) begin
    @(negedge clock);
    if(dout_vld) begin 
      golden_result=output_ram[output_index];
      dut_result   =dout;
      `include "functions/error_percent_abs_cal.sv"
      `include "functions/comp_abs.sv"
      `include "functions/comp_ieee754.sv"
      output_index=output_index+1;
    end
  end

  `ifdef SC_R3_K3_2D
      $fclose(report);
      $fclose(report_ieee754);
  `endif
end

endmodule
