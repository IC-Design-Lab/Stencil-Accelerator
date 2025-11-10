`ifdef SC_R3_K3_2D
$fwrite(report_ieee754, "Item0   error percent: %f%%, golden result: %h, dut result: %h\n", error_percent0  , golden_item0  , dut_item0  );
$fwrite(report_ieee754, "Item1   error percent: %f%%, golden result: %h, dut result: %h\n", error_percent1  , golden_item1  , dut_item1  );
$fwrite(report_ieee754, "Item2   error percent: %f%%, golden result: %h, dut result: %h\n", error_percent2  , golden_item2  , dut_item2  );
`endif // ST_WIDTH_INF_16
