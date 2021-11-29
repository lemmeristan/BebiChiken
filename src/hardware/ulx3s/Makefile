.PHONY: all
.DELETE_ON_ERROR:
TOPMOD  := bebichiken
VLOGFIL := $(TOPMOD).vhd
BINFILE := $(TOPMOD).bin

## 
.PHONY: clean
clean:
	rm -rf bebichiken.json ulx3s_out.config ulx3s.bit

## Include any of these submodules in the Makefile
## ... but only if we are not building the "clean" target
## which would (oops) try to build those dependencies again
##
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(DEPS),)
include $(DEPS)
endif
endif


ulx3s.bit: ulx3s_out.config
	ecppack ulx3s_out.config ulx3s.bit

ulx3s_out.config: bebichiken.json
	nextpnr-ecp5 --85k --json bebichiken.json \
		--lpf ulx3s_v20.lpf \
		--package CABGA381 \
		--textcfg ulx3s_out.config 

bebichiken.json: bebichiken.ys bebichiken.vhd ram.vhd uart.vhd gpio.vhd rom.vhd registerfile.vhd cpu.vhd timebase.vhd hdmi.vhd HDMI_clock.v HDMI_test_hires.v
	yosys bebichiken.ys 

prog: ulx3s.bit
	fujprog-v48-mac-x64 ulx3s.bit

analysis: bebichiken.vhd ram.vhd uart.vhd gpio.vhd rom.vhd registerfile.vhd timebase.vhd hdmi.vhd
	ghdl -a --ieee=synopsys -fexplicit bebichiken.vhd ram.vhd uart.vhd gpio.vhd rom.vhd registerfile.vhd cpu.vhd timebase.vhd hdmi.vhd