`ifdef SC_R3_K3_2D        
golden_item0   = golden_result[(`ST-0)*32-1:(`ST-0)*32-32];    
golden_item1   = golden_result[(`ST-1)*32-1:(`ST-1)*32-32];    
golden_item2   = golden_result[(`ST-2)*32-1:(`ST-2)*32-32];    
dut_item0      =    dut_result[(`ST-0)*32-1:(`ST-0)*32-32];    
dut_item1      =    dut_result[(`ST-1)*32-1:(`ST-1)*32-32];    
dut_item2      =    dut_result[(`ST-2)*32-1:(`ST-2)*32-32];    

golden_real_item0   =ieee754_to_fp(golden_item0   );      
golden_real_item1   =ieee754_to_fp(golden_item1   );      
golden_real_item2   =ieee754_to_fp(golden_item2   );      
dut_real_item0      =ieee754_to_fp(dut_item0   );      
dut_real_item1      =ieee754_to_fp(dut_item1   );      
dut_real_item2      =ieee754_to_fp(dut_item2   );      

error_percent0   = ((dut_real_item0  -golden_real_item0  )/golden_real_item0  )*100;
error_percent1   = ((dut_real_item1  -golden_real_item1  )/golden_real_item1  )*100;
error_percent2   = ((dut_real_item2  -golden_real_item2  )/golden_real_item2  )*100;

if(error_percent0  <0) error_percent0   = -error_percent0  ; else error_percent0   = error_percent0  ;
if(error_percent1  <0) error_percent1   = -error_percent1  ; else error_percent1   = error_percent1  ;
if(error_percent2  <0) error_percent2   = -error_percent2  ; else error_percent2   = error_percent2  ;
`endif // SC_R3_K3_2D        
