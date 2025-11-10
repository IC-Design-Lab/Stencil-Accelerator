`include "define.v"
module sc_r3_k3_2d      (input                 clock    ,
                         input                 reset    ,
                         input                 din_ready,
                         input  [`ST*`BW-1:0]  din      ,
                         input  [`ST*`BW-1:0]  din_wt   ,
                         output reg [`ST*`BW-1 :0] dout     ,
                         output reg                dout_vld );

always @(posedge clock) 
  if(reset) begin
    dout_vld <= 1'b0;
    dout     <= `ST*`BW'h0;
  end else if (din_ready) begin
    dout_vld <= 1'b1;
    dout     <= `ST*`BW'h0123456789abcdef01234567;
  end
endmodule
