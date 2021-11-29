PROJECTNAME=bebichiken

VERILOGS="HDMI_test_hires.v HDMI_clock.v TMDS_encoder.v"
VHDLS="top.vhd"

if [ $1 == "clean" ]; then
  rm -f *.bit *.json *.config *.svf *~
  exit
fi

yosys -p "ghdl --std=08 -fsynopsys bebichiken.vhd ram.vhd uart.vhd gpio.vhd rom.vhd registerfile.vhd cpu.vhd timebase.vhd hdmi.vhd -e; synth_ecp5 -abc9 -top bebichiken -json $PROJECTNAME.json" $VERILOGS || exit
nextpnr-ecp5 --force --timing-allow-fail --json $PROJECTNAME.json --lpf ulx3s_v20.lpf --textcfg $PROJECTNAME.config --85k --freq 25 --package CABGA381 || exit
ecppack --compress --svf-rowsize 100000 --svf $PROJECTNAME.svf $PROJECTNAME.config $PROJECTNAME.bit || exit
fujprog-v48-mac-x64 $PROJECTNAME.bit || exit
# To flash permanently, use instead:
#   Use ujprog -j FLASH $PROJECTNAME.bit 