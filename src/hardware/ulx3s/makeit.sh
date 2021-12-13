PROJECTNAME=bebichiken

HDMI_VERILOGS=() #(HDMI_clock.v HDMI_test_hires.v TMDS_encoder.v) # 
SDRAM_VERILOGS=() #(dualport_ram.v) #
SPIFLASH_VERILOGS=(simple_dualport_ram_8k_lattice.v)
PERIPHERALS_VERILOGS=("${HDMI_VERILOGS[@]/#/peripherals/hdmi/} ${SDRAM_VERILOGS[@]/#/peripherals/sdram/} ${SPIFLASH_VERILOGS[@]/#/peripherals/spiflash/}")


SDRAM_VHDLS=() #(sdram_cache.vhd)
SPIFLASH_VHDLS=(quadflash_cache.vhd)
UART_VHDLS=(uart.vhd)
PERIPHERALS_VHDLS=("${UART_VHDLS[@]/#/peripherals/uart/} ${SDRAM_VHDLS[@]/#/peripherals/sdram/} ${SPIFLASH_VHDLS[@]/#/peripherals/spiflash/}")



VERILOGS="${PERIPHERALS_VERILOGS[@]}" # HDMI_test_hires.v HDMI_clock.v TMDS_encoder.v"
VHDLS="datatypes.vhd top.vhd registerfile.vhd cpu/cpu.vhd mmu.vhd ${PERIPHERALS_VHDLS[@]}" #  gpio.vhd timebase.vhd hdmi.vhd ram.vhd "

if [ $1 == "clean" ]; then
  rm -f *.bit *.json *.config *.svf *~
  exit
fi

if [ $1 == "prog" ]; then
  fujprog-v48-mac-x64 $PROJECTNAME.bit
  exit
fi


yosys -p "ghdl --std=08 -fsynopsys $VHDLS -e; synth_ecp5 -abc9 -top top -json $PROJECTNAME.json" $VERILOGS || exit
nextpnr-ecp5 --force --timing-allow-fail --json $PROJECTNAME.json --lpf ulx3s_v20.lpf --textcfg $PROJECTNAME.config --85k --freq 25 --package CABGA381 || exit
ecppack --compress --svf-rowsize 100000 --svf $PROJECTNAME.svf $PROJECTNAME.config $PROJECTNAME.bit || exit
fujprog-v48-mac-x64 $PROJECTNAME.bit || exit
# To flash permanently, use instead:
#   Use ujprog -j FLASH $PROJECTNAME.bit 