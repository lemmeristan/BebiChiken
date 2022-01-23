# https://hackaday.io/project/162397/logs?sort=oldest

riscv64-unknown-elf-gcc -N -g -march=rv32i -mabi=ilp32 -ffreestanding -O5 -Wl,--gc-sections  \
    -nostartfiles -nostdlib -nodefaultlibs -Tdefault.ld \
    -DDEBUG_UART -DDEBUG_OLED \
    -DOPTIMIZE_STACK \
    startup.S \
    hal/i2c/i2c.c hal/spi/spi.c hal/oled/oled.c hal/uart/uart.c hal/timebase/timebase.c hal/gpio/gpio.c \
    peripherals/BME680_driver/bme680.c \
    main.c printf/printf/printf.c libgcc.a -e_start -o main.out

# Generate disassembly file
riscv64-unknown-elf-objdump --source -d main.out > disassembled.S

# Generate RAM.txt and ROM.txt
#riscv64-unknown-elf-objcopy -S -O binary main.out content.bin
riscv64-unknown-elf-objcopy --dump-section .text=blah.bin main.out
fujprog-v48-mac-x64 -j flash -f 0x200000 blah.bin

#go run ./createHexText/createHexText.go

# Now you get RAM.txt and ROM.txt, which you put with the .vhd files and generate the bitstream 

#riscv64-unknown-elf-objcopy --dump-section .text=blah.bin main.out
#fujprog-v48-mac-x64 -j flash -f 0x400000 blah.bin
#0 0x200000 User Bitstream
#1 0x340000 Saxonsoc fw_jump
#2 0x380000 Saxonsoc u-boot
#3 0x400000 User Data
#4 0x800000 User Data
#5 0x000000 Bootloader Bitstream