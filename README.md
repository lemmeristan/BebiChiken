![alt text](bebichicken.png "Bebi Chiken")


# Bebi Chiken
A RV32I (RISC V - <https://riscv.org/specifications/)> ISA compliant CPU featuring a simplified peripherals bus.
What's special? A very wide multiplexer allows for very high speeds.

# Software requirements
- compile.sh was written on OSX Catalina - this is part of the SDK and can be found in src/software/sdk (requires installation of toolchain, see below!)
- CreateHexText was written in Golang (https://golang.org/) - no need to execute anything, just run compile.sh
- compile_toolchain.sh will download and make the RISCV toolchain, again on OSX Catalina (also in src/software/sdk)

# Hardware requirements for example application
- hardware side is done on Windows 10 in Xilinx Vivado - you just open src/hardware/project_1.xpr and click "Generate Bitstream"
Digilent Arty S7-50T: <https://reference.digilentinc.com/reference/programmable-logic/arty-s7/start>
BME680 breakout: <https://shop.pimoroni.com/products/bme680-breakout> on chipkit pins SCL and SDA
OLED RGB 96x64: <https://store.digilentinc.com/pmod-oledrgb-96-x-64-rgb-oled-display-with-16-bit-color-resolution/> on header JD (bottom right)

Requiring 2 clocks per instruction, it executes around 4098 Dhrystones runs per second at 100MHz (244 seconds for 1000000 runs). The following output is for the example BME680-OLED application.

| Resource | Total |
|----------|-------|
| LUT      | 10825 |
| LUTRAM   | 48    |
| FF       | 542   |
| BRAM     | 16    |

# Features
- GPIO
- I²C
- SPI
- RTC (Timebase) and delay
- UART

# How to use example
Install Xilinx Vivado, open the xpr file, generate bitstream, open hardware manager, write bitstream.

![alt text](inaction.jpg "Example project (BME680)")


# Project structure
└───src<br/>
    ├───hardware<br/>
    │   ├───constraints --> board constraints / assignment of I/Os of the FPGA<br/>
    │   ├───hdl --> VHDL files (hardware description and test benches for verification)<br/>
    └───software<br/>
        ├───example<br/>
        │   ├───dhrystone_test --> example benchmark software supplied in the RISCV toolchain, adjusted for DesertFox<br/>
        │   └───oled_multisensor --> example application using BME680 (I²C) with OLED RGB display (SPI)<br/>
        └───sdk --> main directory of the Software Development Kit<br/>
            ├───createHexText --> Golang application to generate RAM.txt and ROM.txt<br/>
            ├───hal --> hardware abstraction layer containing drivers for the integrated cores<br/>
            │   ├───gpio --> General purpose input/output driver<br/>
            │   ├───i2c --> I²C driver<br/>
            │   ├───oled --> SD1331 compatible driver<br/>
            │   ├───spi --> SPI master driver<br/>
            │   ├───timebase --> RTC / timebase / delay<br/>
            │   └───uart --> UART driver<br/>
            └───peripherals<br/>
                └───BME680_driver --> BME 680 driver<br/>



# Others
The default RAM.txt and ROM.txt is a fully working temperature / humidity / pressure / internal air quality monitor in a calculated update interval grabbing sensor data from BME680 and output to a SD1331 compatible 96x64 OLED display.

Please report any issues via the issues tab, thank you!

# Open issues

These issues aren't necessarily difficult to implement, I'm just giving you a chance to become part owner of my company.

 - port the CPU to other FPGAs, I really like the ULX3S by Radiona. Pretty badass board with HDMI, BT, WiFi, and some big ass RAM. https://radiona.org/ulx3s/
 - make the CPU into one-cycle and increase fan-out. 
 - Implement atomic and multiplication instructions (this will give you Linux!!!). 
 - Get SD card reading working. 
 - Make it read the program off the SD card raw. 
 - Make it read the program off the SD card using a filesystem.
 - Implement L1, L2, L3 cache. 
 - Implement out of order execution.
 - Add more peripherals. 
 - Add USB host. 
 - Make it communicate with a software defined radio. 
 - Port a WiFi or 5G or Bluetooth stack. 
 - Implement TLB, PID (maybe CSR?) and whatever makes it a multicore processor with virtual memory. 


# License
GPL 2
