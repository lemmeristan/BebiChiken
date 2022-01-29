
`timescale 1 ns / 1 ps
module dp16k_wrapper (DataInA, DataInB, AddressA, AddressB, 
    ClockA, ClockB, ClockEnA, ClockEnB, WrA, WrB, ResetA, ResetB, QA, QB, CSA, CSB)/* synthesis NGD_DRC_MASK=1 */;
    input wire [17:0] DataInA;
    input wire [17:0] DataInB;
    input wire [13:0] AddressA;
    input wire [13:0] AddressB;
    input wire ClockA;
    input wire ClockB;
    input wire ClockEnA;
    input wire ClockEnB;
    input wire WrA;
    input wire WrB;
    input wire ResetA;
    input wire ResetB;
    output wire [17:0] QA;
    output wire [17:0] QB;

    input wire [2:0] CSA;
    input wire [2:0] CSB;



//    defparam i_dp16kd.INIT_DATA = "STATIC" ;
    // defparam i_dp16kd.ASYNC_RESET_RELEASE = "SYNC" ;
    // defparam i_dp16kd.INITVAL_3F = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_3E = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_3D = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_3C = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_3B = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_3A = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_39 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_38 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_37 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_36 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_35 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_34 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_33 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_32 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_31 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_30 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_2F = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_2E = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_2D = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_2C = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_2B = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_2A = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_29 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_28 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_27 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_26 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_25 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_24 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_23 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_22 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_21 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_20 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_1F = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_1E = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_1D = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_1C = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_1B = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_1A = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_19 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_18 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_17 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_16 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_15 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_14 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_13 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_12 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_11 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_10 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_0F = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_0E = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_0D = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_0C = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_0B = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_0A = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_09 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_08 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_07 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_06 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_05 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_04 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_03 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_02 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_01 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.INITVAL_00 = "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000" ;
    // defparam i_dp16kd.CSDECODE_B = "0b000" ;
    // defparam i_dp16kd.CSDECODE_A = "0b000" ;
    // defparam i_dp16kd.WRITEMODE_B = "WRITETHROUGH" ;
    // defparam i_dp16kd.WRITEMODE_A = "WRITETHROUGH" ;
    // defparam i_dp16kd.GSR = "ENABLED" ;
    // defparam i_dp16kd.RESETMODE = "SYNC" ;
    // defparam i_dp16kd.REGMODE_B = "NOREG" ;
    // defparam i_dp16kd.REGMODE_A = "NOREG" ;
    // defparam i_dp16kd.DATA_WIDTH_B = 18 ;
    // defparam i_dp16kd.DATA_WIDTH_A = 18 ;
    DP16KD i_dp16kd (
        .DIA17(DataInA[17]), .DIA16(DataInA[16]), 
        .DIA15(DataInA[15]), .DIA14(DataInA[14]), .DIA13(DataInA[13]), .DIA12(DataInA[12]), 
        .DIA11(DataInA[11]), .DIA10(DataInA[10]), .DIA9(DataInA[9]), .DIA8(DataInA[8]), 
        .DIA7(DataInA[7]), .DIA6(DataInA[6]), .DIA5(DataInA[5]), .DIA4(DataInA[4]), 
        .DIA3(DataInA[3]), .DIA2(DataInA[2]), .DIA1(DataInA[1]), .DIA0(DataInA[0]),

        .ADA13(AddressA[13]), .ADA12(AddressA[12]), .ADA11(AddressA[11]), .ADA10(AddressA[10]), 
        .ADA9(AddressA[9]), .ADA8(AddressA[8]), .ADA7(AddressA[7]), .ADA6(AddressA[6]), 
        .ADA5(AddressA[5]), .ADA4(AddressA[4]), .ADA3(AddressA[3]), .ADA2(AddressA[2]), 
        .ADA1(AddressA[1]), .ADA0(AddressA[0]), 

        .DOA17(QA[17]), .DOA16(QA[16]), .DOA15(QA[15]), .DOA14(QA[14]), 
        .DOA13(QA[13]), .DOA12(QA[12]), .DOA11(QA[11]), .DOA10(QA[10]), 
        .DOA9(QA[9]), .DOA8(QA[8]), .DOA7(QA[7]), .DOA6(QA[6]), .DOA5(QA[5]), 
        .DOA4(QA[4]), .DOA3(QA[3]), .DOA2(QA[2]), .DOA1(QA[1]), .DOA0(QA[0]), 
        
        .CEA(ClockEnA), .OCEA(ClockEnA), 
        .CLKA(ClockA), .WEA(WrA), 
        
        .CSA2(CSA[2]), .CSA1(CSA[1]), .CSA0(CSA[0]), 

        .RSTA(ResetA), 
        
        .DIB17(DataInB[17]), .DIB16(DataInB[16]), .DIB15(DataInB[15]), 
        .DIB14(DataInB[14]), .DIB13(DataInB[13]), .DIB12(DataInB[12]), .DIB11(DataInB[11]), 
        .DIB10(DataInB[10]), .DIB9(DataInB[9]), .DIB8(DataInB[8]), .DIB7(DataInB[7]), 
        .DIB6(DataInB[6]), .DIB5(DataInB[5]), .DIB4(DataInB[4]), .DIB3(DataInB[3]), 
        .DIB2(DataInB[2]), .DIB1(DataInB[1]), .DIB0(DataInB[0]), 
        
        .ADB13(AddressB[13]), .ADB12(AddressB[12]), .ADB11(AddressB[11]), .ADB10(AddressB[10]), 
        .ADB9(AddressB[9]), .ADB8(AddressB[8]), .ADB7(AddressB[7]), .ADB6(AddressB[6]), 
        .ADB5(AddressB[5]), .ADB4(AddressB[4]), .ADB3(AddressB[3]), .ADB2(AddressB[2]), 
        .ADB1(AddressB[1]), .ADB0(AddressB[0]), 
        
        .CEB(ClockEnB), .OCEB(ClockEnB), .CLKB(ClockB), 
        .WEB(WrB), .CSB2(CSB[2]), 
        .CSB1(CSB[1]), .CSB0(CSB[0]), 
        .RSTB(ResetB), 

        .DOB17(QB[17]), .DOB16(QB[16]), .DOB15(QB[15]), .DOB14(QB[14]), 
        .DOB13(QB[13]), .DOB12(QB[12]), .DOB11(QB[11]), .DOB10(QB[10]), 
        .DOB9(QB[9]), .DOB8(QB[8]), .DOB7(QB[7]), .DOB6(QB[6]), .DOB5(QB[5]), 
        .DOB4(QB[4]), .DOB3(QB[3]), .DOB2(QB[2]), .DOB1(QB[1]), .DOB0(QB[0]))
                     /* synthesis MEM_LPC_FILE="dpram_regfile_lattice.lpc" */
             /* synthesis MEM_INIT_FILE="INIT_ALL_0s" */;



    // exemplar begin
    // exemplar attribute dpram_regfile_lattice_0_0_1 MEM_LPC_FILE dpram_regfile_lattice.lpc
    // exemplar attribute dpram_regfile_lattice_0_0_1 MEM_INIT_FILE INIT_ALL_0s
    // exemplar attribute dpram_regfile_lattice_0_1_0 MEM_LPC_FILE dpram_regfile_lattice.lpc
    // exemplar attribute dpram_regfile_lattice_0_1_0 MEM_INIT_FILE INIT_ALL_0s
    // exemplar end



        endmodule