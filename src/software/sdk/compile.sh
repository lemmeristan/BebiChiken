# https://hackaday.io/project/162397/logs?sort=oldest

riscv64-unknown-elf-gcc  -g -march=rv32i -mabi=ilp32 -ffreestanding -O5 -Wl,--gc-sections  \
    -nostartfiles -nostdlib -nodefaultlibs -Tdefault.ld \
    -DDEBUG_UART -DDEBUG_OLED \
    startup.S \
    hal/i2c/i2c.c hal/spi/spi.c hal/oled/oled.c hal/uart/uart.c hal/timebase/timebase.c hal/gpio/gpio.c \
    peripherals/BME680_driver/bme680.c \
    main.c printf/printf/printf.c libgcc.a -e_start -o main.out

# Generate disassembly file
riscv64-unknown-elf-objdump --source -d main.out > disassembled.S

# Generate RAM.txt and ROM.txt
riscv64-unknown-elf-objcopy -O binary main.out content.bin
go run ./createHexText/createHexText.go

# Now you get RAM.txt and ROM.txt, which you put with the .vhd files and generate the bitstream 
