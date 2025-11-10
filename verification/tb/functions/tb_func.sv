
//**************************************************************************************//
//---------------- Testbench Functions   ---------------------------------------------////
//---------------- Author: Xiaokun Yang  ---------------------------------------------////
//---------------- Lawrence Berkeley Lab ---------------------------------------------////
//---------------- Date: 5/16/2023  --------------------------------------------------//// 
//------ Version 1: Testbench Functions  ---------------------------------------------////
//------ Date: 5/16/2023 -------------------------------------------------------------////
//*************************************************************************************//
//----- ieee754_to_fp: converts input (ieee754 format) into sp fp number                //
//----- error_percent: calculates error percentage between two inputs (ieee754 format)  //
//*********************************************************************************************//

// ------------------------------------------
// --Convert IEEE754 to FP ------------------
// ------------------------------------------
function real ieee754_to_fp (input [31:0] ieee754_data);
reg sign ;
reg [7:0] exponent ;
reg [22:0] mantissa;

integer int_exp   ;
real mantissa_val ; // Divide by 2^23
real fp_output    ; 

// Extracting sign, exponent, and mantissa
sign     = ieee754_data[31];
exponent = ieee754_data[30:23];
mantissa = ieee754_data[22:0];

//$display("debug task, ieee754_data input is %h", ieee754_data);
//$display("debug task, sign input is %h", sign);
//$display("debug task, exponent input is %h", exponent);
//$display("debug task, mantissa input is %h", mantissa);

// Calculating floating-point value
int_exp      = exponent - 127;
mantissa_val = 1.0 + (mantissa / 8388608.0); // Divide by 2^23
fp_output    = (sign ? -1 : 1) * mantissa_val * (2.0 ** int_exp);
//$display("debug task, int exp is %d", int_exp);
//$display("debug task, mantissa val is %f", mantissa_val);
//$display("debug task, fp_output is %f", fp_output);
return fp_output;
endfunction

/*
// ------------------------------------------
// --Caculate Error Percentage --------------
// ------------------------------------------
function real error_percent(input [31:0] golden_result, input [31:0] dut_result);
error_percent = (ieee754_to_fp(golden_result)-ieee754_to_fp(dut_result))/ieee754_to_fp(golden_result)*100;
return error_percent;
endfunction

// ------------------------------------------
// --Compare two numbers in IEEE754 Format---
// --Set 5 Percent as the boundary ---------- 
// ------------------------------------------
function ieee754_golden_dut_comp(input [31:0] golden_result, input [31:0] dut_result, output error_flag);
real error_percent;
error_percent = (ieee754_to_fp(golden_result)-ieee754_to_fp(dut_result))/ieee754_to_fp(golden_result)*100;
if(error_percent<0) begin
  error_percent=-error_percent;
end

if(error_percent<5) begin
  error_flag=0;
end else begin
  error_flag=1;
end
return error_flag;
endfunction

// ------------------------------------------
// --Compare two numbers in FP Format i------
// --Set 5 Percent as the boundary ---------- 
// ------------------------------------------
function real_golden_dut_comp(real golden_result, real dut_result, output error_flag);
real error_percent;
error_percent = (golden_result-dut_result)/golden_result*100;
if(error_percent<0) begin
  error_percent=-error_percent;
end

if(error_percent<5) begin
  error_flag=0;
  $display ("Comparison Pass! golden result: %f, dut result: %f", golden_result, dut_result);
  $display ("error percentage: %f%%", error_percent);
end else begin
  error_flag=1;
  $display ("Comparison FAIL! golden result: %f, dut result: %f", golden_result, dut_result);
  $display ("error percentage: %f%%", error_percent);
end
return error_flag;
endfunction
*/
