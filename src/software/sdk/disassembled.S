
main.out:     file format elf32-littleriscv


Disassembly of section .text:

80010000 <_start-0x204>:
#include "sdkdefs.h"

.align 7
j _start 
80010000:	2040006f          	j	80010204 <_start>
80010004:	00000013          	nop
80010008:	00000013          	nop
8001000c:	00000013          	nop
80010010:	00000013          	nop
80010014:	00000013          	nop
80010018:	00000013          	nop
8001001c:	00000013          	nop
80010020:	00000013          	nop
80010024:	00000013          	nop
80010028:	00000013          	nop
8001002c:	00000013          	nop
80010030:	00000013          	nop
80010034:	00000013          	nop
80010038:	00000013          	nop
8001003c:	00000013          	nop
80010040:	00000013          	nop
80010044:	00000013          	nop
80010048:	00000013          	nop
8001004c:	00000013          	nop
80010050:	00000013          	nop
80010054:	00000013          	nop
80010058:	00000013          	nop
8001005c:	00000013          	nop
80010060:	00000013          	nop
80010064:	00000013          	nop
80010068:	00000013          	nop
8001006c:	00000013          	nop
80010070:	00000013          	nop
80010074:	00000013          	nop
80010078:	00000013          	nop
8001007c:	00000013          	nop
80010080:	00000013          	nop
80010084:	00000013          	nop
80010088:	00000013          	nop
8001008c:	00000013          	nop
80010090:	00000013          	nop
80010094:	00000013          	nop
80010098:	00000013          	nop
8001009c:	00000013          	nop
800100a0:	00000013          	nop
800100a4:	00000013          	nop
800100a8:	00000013          	nop
800100ac:	00000013          	nop
800100b0:	00000013          	nop
800100b4:	00000013          	nop
800100b8:	00000013          	nop
800100bc:	00000013          	nop
800100c0:	00000013          	nop
800100c4:	00000013          	nop
800100c8:	00000013          	nop
800100cc:	00000013          	nop
800100d0:	00000013          	nop
800100d4:	00000013          	nop
800100d8:	00000013          	nop
800100dc:	00000013          	nop
800100e0:	00000013          	nop
800100e4:	00000013          	nop
800100e8:	00000013          	nop
800100ec:	00000013          	nop
800100f0:	00000013          	nop
800100f4:	00000013          	nop
800100f8:	00000013          	nop
800100fc:	00000013          	nop
.align 8
j _external_interrupt
80010100:	7c50206f          	j	800130c4 <_external_interrupt>
80010104:	00000013          	nop
80010108:	00000013          	nop
8001010c:	00000013          	nop
80010110:	00000013          	nop
80010114:	00000013          	nop
80010118:	00000013          	nop
8001011c:	00000013          	nop
80010120:	00000013          	nop
80010124:	00000013          	nop
80010128:	00000013          	nop
8001012c:	00000013          	nop
80010130:	00000013          	nop
80010134:	00000013          	nop
80010138:	00000013          	nop
8001013c:	00000013          	nop
80010140:	00000013          	nop
80010144:	00000013          	nop
80010148:	00000013          	nop
8001014c:	00000013          	nop
80010150:	00000013          	nop
80010154:	00000013          	nop
80010158:	00000013          	nop
8001015c:	00000013          	nop
80010160:	00000013          	nop
80010164:	00000013          	nop
80010168:	00000013          	nop
8001016c:	00000013          	nop
80010170:	00000013          	nop
80010174:	00000013          	nop
80010178:	00000013          	nop
8001017c:	00000013          	nop
80010180:	00000013          	nop
80010184:	00000013          	nop
80010188:	00000013          	nop
8001018c:	00000013          	nop
80010190:	00000013          	nop
80010194:	00000013          	nop
80010198:	00000013          	nop
8001019c:	00000013          	nop
800101a0:	00000013          	nop
800101a4:	00000013          	nop
800101a8:	00000013          	nop
800101ac:	00000013          	nop
800101b0:	00000013          	nop
800101b4:	00000013          	nop
800101b8:	00000013          	nop
800101bc:	00000013          	nop
800101c0:	00000013          	nop
800101c4:	00000013          	nop
800101c8:	00000013          	nop
800101cc:	00000013          	nop
800101d0:	00000013          	nop
800101d4:	00000013          	nop
800101d8:	00000013          	nop
800101dc:	00000013          	nop
800101e0:	00000013          	nop
800101e4:	00000013          	nop
800101e8:	00000013          	nop
800101ec:	00000013          	nop
800101f0:	00000013          	nop
800101f4:	00000013          	nop
800101f8:	00000013          	nop
800101fc:	00000013          	nop
.align 8
j _misaligned_instruction
80010200:	6c90206f          	j	800130c8 <_misaligned_instruction>

80010204 <_start>:

.globl _start
_start:

la sp, __stack_top
80010204:	00000117          	auipc	sp,0x0
80010208:	dfc10113          	addi	sp,sp,-516 # 80010000 <__stack_top>
#ifdef CONTINUOUS_RESTART
la ra, _start
#else
la ra, exec_done
8001020c:	00000097          	auipc	ra,0x0
80010210:	00c08093          	addi	ra,ra,12 # 80010218 <exec_done>
#endif
j main
80010214:	3d00306f          	j	800135e4 <main>

80010218 <exec_done>:

exec_done:
80010218:	0000006f          	j	80010218 <exec_done>
	...

80010284 <HAL_I2C_WriteByte>:
#include "i2c.h"

#ifndef OPTIMIZE_STACK
    void HAL_I2C_WriteByte(uint8_t value){
        *PTR_I2C_DATA = value;
80010284:	c00047b7          	lui	a5,0xc0004
80010288:	00a78023          	sb	a0,0(a5) # c0004000 <__clzsi2+0x3ffeb36c>
    }
8001028c:	00008067          	ret

80010290 <HAL_I2C_ReadByte>:

    uint8_t HAL_I2C_ReadByte(){
        return *PTR_I2C_DATA;
80010290:	c00047b7          	lui	a5,0xc0004
80010294:	0007c503          	lbu	a0,0(a5) # c0004000 <__clzsi2+0x3ffeb36c>
    }
80010298:	00008067          	ret

8001029c <HAL_I2C_SetDivider>:

    void HAL_I2C_SetDivider(uint16_t value){
        *PTR_I2C_DIVIDER = value;
8001029c:	c00047b7          	lui	a5,0xc0004
800102a0:	00a79223          	sh	a0,4(a5) # c0004004 <__clzsi2+0x3ffeb370>
    }
800102a4:	00008067          	ret

800102a8 <HAL_I2C_ReadACK>:

    uint8_t HAL_I2C_ReadACK(){
        return *PTR_I2C_ACK;
800102a8:	c00047b7          	lui	a5,0xc0004
800102ac:	0017c503          	lbu	a0,1(a5) # c0004001 <__clzsi2+0x3ffeb36d>
    }
800102b0:	00008067          	ret

800102b4 <HAL_I2C_WriteACK>:
    void HAL_I2C_WriteACK(bool value){
        *PTR_I2C_ACK = (value==true)?0:1;
800102b4:	00154513          	xori	a0,a0,1
800102b8:	c00047b7          	lui	a5,0xc0004
800102bc:	00a780a3          	sb	a0,1(a5) # c0004001 <__clzsi2+0x3ffeb36d>
    }
800102c0:	00008067          	ret

800102c4 <HAL_I2C_SetPins>:
    void HAL_I2C_SetPins(uint8_t value){
        *PTR_I2C_PINS = value;
800102c4:	c00047b7          	lui	a5,0xc0004
800102c8:	00a78123          	sb	a0,2(a5) # c0004002 <__clzsi2+0x3ffeb36e>
    }
800102cc:	00008067          	ret

800102d0 <HAL_I2C_GetPins>:
    uint8_t HAL_I2C_GetPins(){
        return *PTR_I2C_PINS;
800102d0:	c00047b7          	lui	a5,0xc0004
800102d4:	0027c503          	lbu	a0,2(a5) # c0004002 <__clzsi2+0x3ffeb36e>
    }
800102d8:	00008067          	ret

800102dc <HAL_SPI_WriteByte>:
#include "spi.h"

#ifndef OPTIMIZE_STACK
void HAL_SPI_WriteByte(uint8_t value){
    *PTR_SPI_DATA = value;
800102dc:	c00037b7          	lui	a5,0xc0003
800102e0:	00a78023          	sb	a0,0(a5) # c0003000 <__clzsi2+0x3ffea36c>
}
800102e4:	00008067          	ret

800102e8 <HAL_SPI_ReadByte>:

uint8_t HAL_SPI_ReadByte(){
    return *PTR_SPI_DATA;
800102e8:	c00037b7          	lui	a5,0xc0003
800102ec:	0007c503          	lbu	a0,0(a5) # c0003000 <__clzsi2+0x3ffea36c>
}
800102f0:	00008067          	ret

800102f4 <HAL_SPI_ReadByteWithAck>:

uint8_t HAL_SPI_ReadByteWithAck() {
    return *PTR_SPI_DATA_WITH_ACK;
800102f4:	c00037b7          	lui	a5,0xc0003
800102f8:	0037c503          	lbu	a0,3(a5) # c0003003 <__clzsi2+0x3ffea36f>
}
800102fc:	00008067          	ret

80010300 <HAL_SPI_SetDivider>:

void HAL_SPI_SetDivider(uint8_t value){
    *PTR_SPI_DIVIDER = value;
80010300:	c00037b7          	lui	a5,0xc0003
80010304:	00a780a3          	sb	a0,1(a5) # c0003001 <__clzsi2+0x3ffea36d>
}
80010308:	00008067          	ret

8001030c <HAL_SPI_SetConfig>:
void HAL_SPI_SetConfig(bool open_drain_sck, bool open_drain_mosi, bool sck_idle_level, bool mosi_idle_level, bool passthrough_sck){
    *PTR_SPI_CONFIG = (open_drain_sck?FLAG_OPEN_DRAIN_SCK:0) 
    | (open_drain_mosi?FLAG_OPEN_DRAIN_MOSI:0)
    | (sck_idle_level?FLAG_IDLE_SCK:0)
    | (mosi_idle_level?FLAG_IDLE_MOSI:0)
    | (passthrough_sck?FLAG_PASSTHROUGH_SCK:0);
8001030c:	00571713          	slli	a4,a4,0x5
    | (mosi_idle_level?FLAG_IDLE_MOSI:0)
80010310:	00469693          	slli	a3,a3,0x4
    | (passthrough_sck?FLAG_PASSTHROUGH_SCK:0);
80010314:	00d76733          	or	a4,a4,a3
    | (sck_idle_level?FLAG_IDLE_SCK:0)
80010318:	00361613          	slli	a2,a2,0x3
    | (passthrough_sck?FLAG_PASSTHROUGH_SCK:0);
8001031c:	00c76733          	or	a4,a4,a2
    | (open_drain_mosi?FLAG_OPEN_DRAIN_MOSI:0)
80010320:	00259593          	slli	a1,a1,0x2
    | (passthrough_sck?FLAG_PASSTHROUGH_SCK:0);
80010324:	00b76733          	or	a4,a4,a1
    | (open_drain_mosi?FLAG_OPEN_DRAIN_MOSI:0)
80010328:	00151513          	slli	a0,a0,0x1
    | (passthrough_sck?FLAG_PASSTHROUGH_SCK:0);
8001032c:	00a76733          	or	a4,a4,a0
80010330:	0ff77713          	zext.b	a4,a4
    *PTR_SPI_CONFIG = (open_drain_sck?FLAG_OPEN_DRAIN_SCK:0) 
80010334:	c00037b7          	lui	a5,0xc0003
80010338:	00e78123          	sb	a4,2(a5) # c0003002 <__clzsi2+0x3ffea36e>
}
8001033c:	00008067          	ret

80010340 <FontSizeConvert>:

static uint16_t gfxbuffer[height + 1][width + 1];

void FontSizeConvert(int *lpx, int *lpy)
{
    switch (chr_size)
80010340:	800047b7          	lui	a5,0x80004
80010344:	8717c783          	lbu	a5,-1935(a5) # 80003871 <__clzsi2+0xfffeabdd>
80010348:	00300713          	li	a4,3
8001034c:	06e78463          	beq	a5,a4,800103b4 <FontSizeConvert+0x74>
80010350:	02f76463          	bltu	a4,a5,80010378 <FontSizeConvert+0x38>
80010354:	00100713          	li	a4,1
80010358:	02e78e63          	beq	a5,a4,80010394 <FontSizeConvert+0x54>
8001035c:	00200713          	li	a4,2
80010360:	04e79463          	bne	a5,a4,800103a8 <FontSizeConvert+0x68>
80010364:	00100713          	li	a4,1
80010368:	00200793          	li	a5,2
    {
    case WIDE:
        *lpx = 2;
8001036c:	00e52023          	sw	a4,0(a0)
        *lpy = 1;
80010370:	00f5a023          	sw	a5,0(a1)
    default:
        *lpx = 1;
        *lpy = 1;
        break;
    }
}
80010374:	00008067          	ret
    switch (chr_size)
80010378:	00400713          	li	a4,4
8001037c:	02e79663          	bne	a5,a4,800103a8 <FontSizeConvert+0x68>
80010380:	00600713          	li	a4,6
80010384:	00600793          	li	a5,6
        *lpx = 2;
80010388:	00e52023          	sw	a4,0(a0)
        *lpy = 1;
8001038c:	00f5a023          	sw	a5,0(a1)
}
80010390:	00008067          	ret
    switch (chr_size)
80010394:	00200713          	li	a4,2
80010398:	00100793          	li	a5,1
        *lpx = 2;
8001039c:	00e52023          	sw	a4,0(a0)
        *lpy = 1;
800103a0:	00f5a023          	sw	a5,0(a1)
}
800103a4:	00008067          	ret
    switch (chr_size)
800103a8:	00100713          	li	a4,1
800103ac:	00100793          	li	a5,1
800103b0:	fbdff06f          	j	8001036c <FontSizeConvert+0x2c>
800103b4:	00200713          	li	a4,2
800103b8:	00200793          	li	a5,2
800103bc:	fb1ff06f          	j	8001036c <FontSizeConvert+0x2c>

800103c0 <SetFontSize>:

void SetFontSize(uint8_t Csize)
{
    chr_size = Csize;
800103c0:	800047b7          	lui	a5,0x80004
800103c4:	86a788a3          	sb	a0,-1935(a5) # 80003871 <__clzsi2+0xfffeabdd>
}
800103c8:	00008067          	ret

800103cc <SetFontColor>:

void SetFontColor(uint16_t foreground, uint16_t background, uint16_t border)
{
    Char_Color = foreground;
800103cc:	800047b7          	lui	a5,0x80004
800103d0:	86a79623          	sh	a0,-1940(a5) # 8000386c <__clzsi2+0xfffeabd8>
    BGround_Color = background;
800103d4:	800047b7          	lui	a5,0x80004
800103d8:	86b79523          	sh	a1,-1942(a5) # 8000386a <__clzsi2+0xfffeabd6>
    TBorder_Color = border;
800103dc:	800047b7          	lui	a5,0x80004
800103e0:	86c79423          	sh	a2,-1944(a5) # 80003868 <__clzsi2+0xfffeabd4>
}
800103e4:	00008067          	ret

800103e8 <HAL_OLED_DrawGFXBuffer>:

void HAL_OLED_DrawGFXBuffer()
{
800103e8:	fe010113          	addi	sp,sp,-32
    HAL_GPIO_SetOutputs(OLED_RES | OLED_PMODEN | OLED_VCCEN);
800103ec:	01c00513          	li	a0,28
{
800103f0:	00812c23          	sw	s0,24(sp)
800103f4:	00912a23          	sw	s1,20(sp)
800103f8:	00112e23          	sw	ra,28(sp)
800103fc:	01212823          	sw	s2,16(sp)
    HAL_GPIO_SetOutputs(OLED_RES | OLED_PMODEN | OLED_VCCEN);
80010400:	2f1000ef          	jal	ra,80010ef0 <HAL_GPIO_SetOutputs>

    uint8_t spicommands[6] = {Set_Column_Address, 0, width, Set_Row_Address, 0, height};
80010404:	755f07b7          	lui	a5,0x755f0
80010408:	01578793          	addi	a5,a5,21 # 755f0015 <font6x8-0xaa10017>
8001040c:	00f12423          	sw	a5,8(sp)
80010410:	000047b7          	lui	a5,0x4
80010414:	f0078793          	addi	a5,a5,-256 # 3f00 <font6x8-0x7fffc12c>
80010418:	00f11623          	sh	a5,12(sp)
    for (uint8_t i = 0; i < 6; i++)
8001041c:	00810413          	addi	s0,sp,8
80010420:	00e10493          	addi	s1,sp,14
    {
        HAL_SPI_WriteByte(spicommands[i]);
80010424:	00044503          	lbu	a0,0(s0)
    for (uint8_t i = 0; i < 6; i++)
80010428:	00140413          	addi	s0,s0,1
        HAL_SPI_WriteByte(spicommands[i]);
8001042c:	eb1ff0ef          	jal	ra,800102dc <HAL_SPI_WriteByte>
    for (uint8_t i = 0; i < 6; i++)
80010430:	fe941ae3          	bne	s0,s1,80010424 <HAL_OLED_DrawGFXBuffer+0x3c>
    }

    HAL_GPIO_SetOutputs(OLED_DC | OLED_RES | OLED_PMODEN | OLED_VCCEN);
80010434:	01e00513          	li	a0,30
80010438:	2b9000ef          	jal	ra,80010ef0 <HAL_GPIO_SetOutputs>

    for (uint32_t y = 0; y <= height; y++)
8001043c:	80001937          	lui	s2,0x80001
80010440:	000037b7          	lui	a5,0x3
80010444:	86890913          	addi	s2,s2,-1944 # 80000868 <__clzsi2+0xfffe7bd4>
80010448:	0c078793          	addi	a5,a5,192 # 30c0 <font6x8-0x7fffcf6c>
8001044c:	0c090493          	addi	s1,s2,192
80010450:	00f90933          	add	s2,s2,a5
        for (uint32_t x = 0; x <= width; x++)
80010454:	f4048413          	addi	s0,s1,-192
        {
            HAL_SPI_WriteByte(gfxbuffer[y][x] >> 8);
80010458:	00045503          	lhu	a0,0(s0)
        for (uint32_t x = 0; x <= width; x++)
8001045c:	00240413          	addi	s0,s0,2
            HAL_SPI_WriteByte(gfxbuffer[y][x] >> 8);
80010460:	00855513          	srli	a0,a0,0x8
80010464:	e79ff0ef          	jal	ra,800102dc <HAL_SPI_WriteByte>
            HAL_SPI_WriteByte(gfxbuffer[y][x] >> 0);
80010468:	ffe44503          	lbu	a0,-2(s0)
8001046c:	e71ff0ef          	jal	ra,800102dc <HAL_SPI_WriteByte>
        for (uint32_t x = 0; x <= width; x++)
80010470:	fe9414e3          	bne	s0,s1,80010458 <HAL_OLED_DrawGFXBuffer+0x70>
    for (uint32_t y = 0; y <= height; y++)
80010474:	0c040493          	addi	s1,s0,192
80010478:	fd249ee3          	bne	s1,s2,80010454 <HAL_OLED_DrawGFXBuffer+0x6c>
        }
    HAL_GPIO_SetOutputs(OLED_CS | OLED_RES | OLED_PMODEN | OLED_VCCEN);
8001047c:	01d00513          	li	a0,29
80010480:	271000ef          	jal	ra,80010ef0 <HAL_GPIO_SetOutputs>
    HAL_Timebase_DelayMS(5);
}
80010484:	01812403          	lw	s0,24(sp)
80010488:	01c12083          	lw	ra,28(sp)
8001048c:	01412483          	lw	s1,20(sp)
80010490:	01012903          	lw	s2,16(sp)
    HAL_Timebase_DelayMS(5);
80010494:	00500513          	li	a0,5
}
80010498:	02010113          	addi	sp,sp,32
    HAL_Timebase_DelayMS(5);
8001049c:	2210006f          	j	80010ebc <HAL_Timebase_DelayMS>

800104a0 <HAL_OLED_ClearScreen>:

void HAL_OLED_ClearScreen()
{
    for (uint32_t x = 0; x <= width; x++)
800104a0:	800016b7          	lui	a3,0x80001
800104a4:	00003637          	lui	a2,0x3
{
800104a8:	fe010113          	addi	sp,sp,-32
800104ac:	86868693          	addi	a3,a3,-1944 # 80000868 <__clzsi2+0xfffe7bd4>
800104b0:	0c060613          	addi	a2,a2,192 # 30c0 <font6x8-0x7fffcf6c>
800104b4:	00112e23          	sw	ra,28(sp)
800104b8:	00812c23          	sw	s0,24(sp)
800104bc:	00912a23          	sw	s1,20(sp)
800104c0:	0c068713          	addi	a4,a3,192
800104c4:	00c68633          	add	a2,a3,a2
800104c8:	00068793          	mv	a5,a3
        for (uint32_t y = 0; y <= height; y++)
            gfxbuffer[y][x] = 0; // black
800104cc:	0007a023          	sw	zero,0(a5)
    for (uint32_t x = 0; x <= width; x++)
800104d0:	00478793          	addi	a5,a5,4
800104d4:	fee79ce3          	bne	a5,a4,800104cc <HAL_OLED_ClearScreen+0x2c>
800104d8:	0c078713          	addi	a4,a5,192
800104dc:	0c068693          	addi	a3,a3,192
800104e0:	fec714e3          	bne	a4,a2,800104c8 <HAL_OLED_ClearScreen+0x28>
    HAL_OLED_ClearScreen();
}

void HAL_OLED_DrawBitmap(uint8_t col_start, uint8_t row_start, uint8_t col_end, uint8_t row_end, uint8_t *data)
{
    HAL_GPIO_SetOutputs(OLED_RES | OLED_PMODEN | OLED_VCCEN);
800104e4:	01c00513          	li	a0,28
800104e8:	209000ef          	jal	ra,80010ef0 <HAL_GPIO_SetOutputs>

    uint8_t spicommands[6] = {0x15, col_start, col_end, 0x75, row_start, row_end};
800104ec:	755f07b7          	lui	a5,0x755f0
800104f0:	01578793          	addi	a5,a5,21 # 755f0015 <font6x8-0xaa10017>
800104f4:	00f12423          	sw	a5,8(sp)
800104f8:	000047b7          	lui	a5,0x4
800104fc:	f0078793          	addi	a5,a5,-256 # 3f00 <font6x8-0x7fffc12c>
80010500:	00f11623          	sh	a5,12(sp)
    for (uint8_t i = 0; i < 6; i++)
80010504:	00810413          	addi	s0,sp,8
80010508:	00e10493          	addi	s1,sp,14
    {
        HAL_SPI_WriteByte(spicommands[i]);
8001050c:	00044503          	lbu	a0,0(s0)
    for (uint8_t i = 0; i < 6; i++)
80010510:	00140413          	addi	s0,s0,1
        HAL_SPI_WriteByte(spicommands[i]);
80010514:	dc9ff0ef          	jal	ra,800102dc <HAL_SPI_WriteByte>
    for (uint8_t i = 0; i < 6; i++)
80010518:	fe941ae3          	bne	s0,s1,8001050c <HAL_OLED_ClearScreen+0x6c>
    }

    HAL_GPIO_SetOutputs(OLED_DC | OLED_RES | OLED_PMODEN | OLED_VCCEN);
8001051c:	01e00513          	li	a0,30
80010520:	1d1000ef          	jal	ra,80010ef0 <HAL_GPIO_SetOutputs>
80010524:	00003437          	lui	s0,0x3
80010528:	800044b7          	lui	s1,0x80004

    for (uint32_t i = 0; i < (((col_end - col_start + 1) * (row_end - row_start + 1)) << 1); i++)
    {
        HAL_SPI_WriteByte(BGround_Color); // HAL_SPI_WriteByte(*(data+i));
8001052c:	86a4c503          	lbu	a0,-1942(s1) # 8000386a <__clzsi2+0xfffeabd6>
    for (uint32_t i = 0; i < (((col_end - col_start + 1) * (row_end - row_start + 1)) << 1); i++)
80010530:	fff40413          	addi	s0,s0,-1 # 2fff <font6x8-0x7fffd02d>
        HAL_SPI_WriteByte(BGround_Color); // HAL_SPI_WriteByte(*(data+i));
80010534:	da9ff0ef          	jal	ra,800102dc <HAL_SPI_WriteByte>
    for (uint32_t i = 0; i < (((col_end - col_start + 1) * (row_end - row_start + 1)) << 1); i++)
80010538:	fe041ae3          	bnez	s0,8001052c <HAL_OLED_ClearScreen+0x8c>
    }
    HAL_GPIO_SetOutputs(OLED_CS | OLED_RES | OLED_PMODEN | OLED_VCCEN);
8001053c:	01d00513          	li	a0,29
80010540:	1b1000ef          	jal	ra,80010ef0 <HAL_GPIO_SetOutputs>

    HAL_Timebase_DelayMS(5);
80010544:	00500513          	li	a0,5
80010548:	175000ef          	jal	ra,80010ebc <HAL_Timebase_DelayMS>
}
8001054c:	01c12083          	lw	ra,28(sp)
80010550:	01812403          	lw	s0,24(sp)
    char_x = x_offset;
80010554:	800047b7          	lui	a5,0x80004
80010558:	860789a3          	sb	zero,-1933(a5) # 80003873 <__clzsi2+0xfffeabdf>
    char_y = y_offset;
8001055c:	800047b7          	lui	a5,0x80004
80010560:	86078923          	sb	zero,-1934(a5) # 80003872 <__clzsi2+0xfffeabde>
}
80010564:	01412483          	lw	s1,20(sp)
80010568:	02010113          	addi	sp,sp,32
8001056c:	00008067          	ret

80010570 <HAL_OLED_Init>:
{
80010570:	fc010113          	addi	sp,sp,-64
    HAL_SPI_SetDivider(100); // 1 MHz
80010574:	06400513          	li	a0,100
{
80010578:	02112e23          	sw	ra,60(sp)
8001057c:	02812c23          	sw	s0,56(sp)
80010580:	02912a23          	sw	s1,52(sp)
    HAL_SPI_SetDivider(100); // 1 MHz
80010584:	d7dff0ef          	jal	ra,80010300 <HAL_SPI_SetDivider>
    HAL_SPI_SetConfig(false, false, true, false, false);
80010588:	00000713          	li	a4,0
8001058c:	00000693          	li	a3,0
80010590:	00100613          	li	a2,1
80010594:	00000593          	li	a1,0
80010598:	00000513          	li	a0,0
8001059c:	d71ff0ef          	jal	ra,8001030c <HAL_SPI_SetConfig>
    HAL_GPIO_SetOutputs(OLED_CS | OLED_RES | OLED_PMODEN);
800105a0:	01500513          	li	a0,21
800105a4:	14d000ef          	jal	ra,80010ef0 <HAL_GPIO_SetOutputs>
    HAL_GPIO_SetDirections(OLED_CS | OLED_DC | OLED_RES | OLED_VCCEN | OLED_PMODEN);
800105a8:	01f00513          	li	a0,31
800105ac:	139000ef          	jal	ra,80010ee4 <HAL_GPIO_SetDirections>
    HAL_Timebase_DelayMS(20);
800105b0:	01400513          	li	a0,20
800105b4:	109000ef          	jal	ra,80010ebc <HAL_Timebase_DelayMS>
    HAL_GPIO_SetOutputs(OLED_CS | OLED_PMODEN);
800105b8:	01100513          	li	a0,17
800105bc:	135000ef          	jal	ra,80010ef0 <HAL_GPIO_SetOutputs>
    HAL_Timebase_DelayUS(3);
800105c0:	00300513          	li	a0,3
800105c4:	0d5000ef          	jal	ra,80010e98 <HAL_Timebase_DelayUS>
    HAL_GPIO_SetOutputs(OLED_CS | OLED_RES | OLED_PMODEN);
800105c8:	01500513          	li	a0,21
800105cc:	125000ef          	jal	ra,80010ef0 <HAL_GPIO_SetOutputs>
    HAL_Timebase_DelayUS(3);
800105d0:	00300513          	li	a0,3
800105d4:	0c5000ef          	jal	ra,80010e98 <HAL_Timebase_DelayUS>
    HAL_GPIO_SetOutputs(OLED_RES | OLED_PMODEN);
800105d8:	01400513          	li	a0,20
800105dc:	115000ef          	jal	ra,80010ef0 <HAL_GPIO_SetOutputs>
    const uint8_t spicommands[44] = {
800105e0:	800007b7          	lui	a5,0x80000
800105e4:	00078793          	mv	a5,a5
800105e8:	0007ae83          	lw	t4,0(a5) # 80000000 <__clzsi2+0xfffe736c>
800105ec:	0047ae03          	lw	t3,4(a5)
800105f0:	0087a303          	lw	t1,8(a5)
800105f4:	00c7a883          	lw	a7,12(a5)
800105f8:	0107a803          	lw	a6,16(a5)
800105fc:	0147a503          	lw	a0,20(a5)
80010600:	0187a583          	lw	a1,24(a5)
80010604:	01c7a603          	lw	a2,28(a5)
80010608:	0207a683          	lw	a3,32(a5)
8001060c:	0247a703          	lw	a4,36(a5)
80010610:	0287a783          	lw	a5,40(a5)
80010614:	01d12223          	sw	t4,4(sp)
80010618:	01c12423          	sw	t3,8(sp)
8001061c:	00612623          	sw	t1,12(sp)
80010620:	01112823          	sw	a7,16(sp)
80010624:	01012a23          	sw	a6,20(sp)
80010628:	00a12c23          	sw	a0,24(sp)
8001062c:	00b12e23          	sw	a1,28(sp)
80010630:	02c12023          	sw	a2,32(sp)
80010634:	02d12223          	sw	a3,36(sp)
80010638:	02e12423          	sw	a4,40(sp)
8001063c:	02f12623          	sw	a5,44(sp)
    for (uint8_t i = 0; i < 44; i++)
80010640:	00410413          	addi	s0,sp,4
80010644:	03010493          	addi	s1,sp,48
        HAL_SPI_WriteByte(spicommands[i]);
80010648:	00044503          	lbu	a0,0(s0)
    for (uint8_t i = 0; i < 44; i++)
8001064c:	00140413          	addi	s0,s0,1
        HAL_SPI_WriteByte(spicommands[i]);
80010650:	c8dff0ef          	jal	ra,800102dc <HAL_SPI_WriteByte>
    for (uint8_t i = 0; i < 44; i++)
80010654:	fe941ae3          	bne	s0,s1,80010648 <HAL_OLED_Init+0xd8>
    HAL_GPIO_SetOutputs(OLED_RES | OLED_PMODEN | OLED_VCCEN);
80010658:	01c00513          	li	a0,28
8001065c:	095000ef          	jal	ra,80010ef0 <HAL_GPIO_SetOutputs>
    HAL_Timebase_DelayMS(25);
80010660:	01900513          	li	a0,25
80010664:	059000ef          	jal	ra,80010ebc <HAL_Timebase_DelayMS>
    HAL_SPI_WriteByte(0xAF); // turn display on
80010668:	0af00513          	li	a0,175
8001066c:	c71ff0ef          	jal	ra,800102dc <HAL_SPI_WriteByte>
    HAL_Timebase_DelayMS(100);
80010670:	06400513          	li	a0,100
80010674:	049000ef          	jal	ra,80010ebc <HAL_Timebase_DelayMS>
    HAL_GPIO_SetOutputs(OLED_CS | OLED_RES | OLED_PMODEN | OLED_VCCEN);
80010678:	01d00513          	li	a0,29
8001067c:	075000ef          	jal	ra,80010ef0 <HAL_GPIO_SetOutputs>
    chr_size = Csize;
80010680:	800047b7          	lui	a5,0x80004
80010684:	860788a3          	sb	zero,-1935(a5) # 80003871 <__clzsi2+0xfffeabdd>
    Char_Color = foreground;
80010688:	fff00713          	li	a4,-1
8001068c:	800047b7          	lui	a5,0x80004
}
80010690:	03812403          	lw	s0,56(sp)
    Char_Color = foreground;
80010694:	86e79623          	sh	a4,-1940(a5) # 8000386c <__clzsi2+0xfffeabd8>
    BGround_Color = background;
80010698:	800047b7          	lui	a5,0x80004
}
8001069c:	03c12083          	lw	ra,60(sp)
800106a0:	03412483          	lw	s1,52(sp)
    BGround_Color = background;
800106a4:	86079523          	sh	zero,-1942(a5) # 8000386a <__clzsi2+0xfffeabd6>
    TBorder_Color = border;
800106a8:	800047b7          	lui	a5,0x80004
800106ac:	86079423          	sh	zero,-1944(a5) # 80003868 <__clzsi2+0xfffeabd4>
}
800106b0:	04010113          	addi	sp,sp,64
    HAL_OLED_ClearScreen();
800106b4:	dedff06f          	j	800104a0 <HAL_OLED_ClearScreen>

800106b8 <HAL_OLED_DrawBitmap>:
{
800106b8:	fd010113          	addi	sp,sp,-48
800106bc:	01512a23          	sw	s5,20(sp)
800106c0:	00050a93          	mv	s5,a0
    HAL_GPIO_SetOutputs(OLED_RES | OLED_PMODEN | OLED_VCCEN);
800106c4:	01c00513          	li	a0,28
{
800106c8:	02812423          	sw	s0,40(sp)
800106cc:	02912223          	sw	s1,36(sp)
800106d0:	03212023          	sw	s2,32(sp)
800106d4:	01312e23          	sw	s3,28(sp)
800106d8:	01412c23          	sw	s4,24(sp)
800106dc:	00060493          	mv	s1,a2
800106e0:	00058a13          	mv	s4,a1
800106e4:	00068913          	mv	s2,a3
800106e8:	02112623          	sw	ra,44(sp)
    HAL_GPIO_SetOutputs(OLED_RES | OLED_PMODEN | OLED_VCCEN);
800106ec:	005000ef          	jal	ra,80010ef0 <HAL_GPIO_SetOutputs>
    uint8_t spicommands[6] = {0x15, col_start, col_end, 0x75, row_start, row_end};
800106f0:	01500793          	li	a5,21
800106f4:	00f10423          	sb	a5,8(sp)
800106f8:	07500793          	li	a5,117
800106fc:	015104a3          	sb	s5,9(sp)
80010700:	00910523          	sb	s1,10(sp)
80010704:	00f105a3          	sb	a5,11(sp)
80010708:	01410623          	sb	s4,12(sp)
8001070c:	012106a3          	sb	s2,13(sp)
    for (uint8_t i = 0; i < 6; i++)
80010710:	00810413          	addi	s0,sp,8
80010714:	00e10993          	addi	s3,sp,14
        HAL_SPI_WriteByte(spicommands[i]);
80010718:	00044503          	lbu	a0,0(s0)
    for (uint8_t i = 0; i < 6; i++)
8001071c:	00140413          	addi	s0,s0,1
        HAL_SPI_WriteByte(spicommands[i]);
80010720:	bbdff0ef          	jal	ra,800102dc <HAL_SPI_WriteByte>
    for (uint8_t i = 0; i < 6; i++)
80010724:	ff341ae3          	bne	s0,s3,80010718 <HAL_OLED_DrawBitmap+0x60>
    HAL_GPIO_SetOutputs(OLED_DC | OLED_RES | OLED_PMODEN | OLED_VCCEN);
80010728:	01e00513          	li	a0,30
8001072c:	7c4000ef          	jal	ra,80010ef0 <HAL_GPIO_SetOutputs>
    for (uint32_t i = 0; i < (((col_end - col_start + 1) * (row_end - row_start + 1)) << 1); i++)
80010730:	41548533          	sub	a0,s1,s5
80010734:	414905b3          	sub	a1,s2,s4
80010738:	00158593          	addi	a1,a1,1
8001073c:	00150513          	addi	a0,a0,1
80010740:	3ec080ef          	jal	ra,80018b2c <__mulsi3>
80010744:	00151493          	slli	s1,a0,0x1
80010748:	00048e63          	beqz	s1,80010764 <HAL_OLED_DrawBitmap+0xac>
8001074c:	00000413          	li	s0,0
80010750:	80004937          	lui	s2,0x80004
        HAL_SPI_WriteByte(BGround_Color); // HAL_SPI_WriteByte(*(data+i));
80010754:	86a94503          	lbu	a0,-1942(s2) # 8000386a <__clzsi2+0xfffeabd6>
    for (uint32_t i = 0; i < (((col_end - col_start + 1) * (row_end - row_start + 1)) << 1); i++)
80010758:	00140413          	addi	s0,s0,1
        HAL_SPI_WriteByte(BGround_Color); // HAL_SPI_WriteByte(*(data+i));
8001075c:	b81ff0ef          	jal	ra,800102dc <HAL_SPI_WriteByte>
    for (uint32_t i = 0; i < (((col_end - col_start + 1) * (row_end - row_start + 1)) << 1); i++)
80010760:	fe941ae3          	bne	s0,s1,80010754 <HAL_OLED_DrawBitmap+0x9c>
    HAL_GPIO_SetOutputs(OLED_CS | OLED_RES | OLED_PMODEN | OLED_VCCEN);
80010764:	01d00513          	li	a0,29
80010768:	788000ef          	jal	ra,80010ef0 <HAL_GPIO_SetOutputs>
}
8001076c:	02812403          	lw	s0,40(sp)
80010770:	02c12083          	lw	ra,44(sp)
80010774:	02412483          	lw	s1,36(sp)
80010778:	02012903          	lw	s2,32(sp)
8001077c:	01c12983          	lw	s3,28(sp)
80010780:	01812a03          	lw	s4,24(sp)
80010784:	01412a83          	lw	s5,20(sp)
    HAL_Timebase_DelayMS(5);
80010788:	00500513          	li	a0,5
}
8001078c:	03010113          	addi	sp,sp,48
    HAL_Timebase_DelayMS(5);
80010790:	72c0006f          	j	80010ebc <HAL_Timebase_DelayMS>

80010794 <HAL_OLED_SetPixel>:
    HAL_SPI_WriteByte(color & 0xFF);
    HAL_GPIO_SetOutputs(OLED_CS | OLED_RES | OLED_PMODEN | OLED_VCCEN);
    */

    uint8_t temp = row >> 1;
    if ((row & 1) == 0)
80010794:	0015f793          	andi	a5,a1,1
        gfxbuffer[fixrow(row)][col] = color;
80010798:	0015d713          	srli	a4,a1,0x1
    if ((row & 1) == 0)
8001079c:	02079663          	bnez	a5,800107c8 <HAL_OLED_SetPixel+0x34>
        gfxbuffer[fixrow(row)][col] = color;
800107a0:	00171793          	slli	a5,a4,0x1
800107a4:	00e787b3          	add	a5,a5,a4
    else
        gfxbuffer[fixrow(row)][col] = color;
800107a8:	00579793          	slli	a5,a5,0x5
800107ac:	00a78533          	add	a0,a5,a0
800107b0:	80001737          	lui	a4,0x80001
800107b4:	00151793          	slli	a5,a0,0x1
800107b8:	86870713          	addi	a4,a4,-1944 # 80000868 <__clzsi2+0xfffe7bd4>
800107bc:	00f707b3          	add	a5,a4,a5
800107c0:	00c79023          	sh	a2,0(a5)

    //     gfxbuffer[row][col] = color;
}
800107c4:	00008067          	ret
        gfxbuffer[fixrow(row)][col] = color;
800107c8:	01f58593          	addi	a1,a1,31
800107cc:	40e585b3          	sub	a1,a1,a4
800107d0:	00159793          	slli	a5,a1,0x1
800107d4:	00b787b3          	add	a5,a5,a1
800107d8:	00579793          	slli	a5,a5,0x5
800107dc:	00a78533          	add	a0,a5,a0
800107e0:	80001737          	lui	a4,0x80001
800107e4:	00151793          	slli	a5,a0,0x1
800107e8:	86870713          	addi	a4,a4,-1944 # 80000868 <__clzsi2+0xfffe7bd4>
800107ec:	00f707b3          	add	a5,a4,a5
800107f0:	00c79023          	sh	a2,0(a5)
}
800107f4:	00008067          	ret

800107f8 <HAL_OLED_PutChar>:
    }*/

    if ((value == '\n') || (char_x > width))
    { // new line, carriage return
        char_x = x_offset;
        char_y = char_y + Y_height;
800107f8:	80004737          	lui	a4,0x80004
    if ((value == '\n') || (char_x > width))
800107fc:	00a00693          	li	a3,10
        char_y = char_y + Y_height;
80010800:	87274783          	lbu	a5,-1934(a4) # 80003872 <__clzsi2+0xfffeabde>
80010804:	800042b7          	lui	t0,0x80004
    if ((value == '\n') || (char_x > width))
80010808:	5cd50a63          	beq	a0,a3,80010ddc <HAL_OLED_PutChar+0x5e4>
8001080c:	8732c803          	lbu	a6,-1933(t0) # 80003873 <__clzsi2+0xfffeabdf>
80010810:	05f00693          	li	a3,95
80010814:	5d06e463          	bltu	a3,a6,80010ddc <HAL_OLED_PutChar+0x5e4>
    }

    if (char_y > height)
80010818:	03f00693          	li	a3,63
8001081c:	00f6f663          	bgeu	a3,a5,80010828 <HAL_OLED_PutChar+0x30>
        char_y = y_offset; // return to line 0
80010820:	86070923          	sb	zero,-1934(a4)
80010824:	00000793          	li	a5,0

    if ((value < 31) || (value > 127))
80010828:	fe150713          	addi	a4,a0,-31
8001082c:	0ff77713          	zext.b	a4,a4
80010830:	06000693          	li	a3,96
80010834:	5ce6e663          	bltu	a3,a4,80010e00 <HAL_OLED_PutChar+0x608>
{
80010838:	fd010113          	addi	sp,sp,-48
8001083c:	02812623          	sw	s0,44(sp)
80010840:	02912423          	sw	s1,40(sp)
80010844:	03212223          	sw	s2,36(sp)
80010848:	03312023          	sw	s3,32(sp)
8001084c:	01412e23          	sw	s4,28(sp)
80010850:	01512c23          	sw	s5,24(sp)
80010854:	01612a23          	sw	s6,20(sp)
80010858:	01712823          	sw	s7,16(sp)
8001085c:	01812623          	sw	s8,12(sp)
80010860:	01912423          	sw	s9,8(sp)
80010864:	01a12223          	sw	s10,4(sp)
80010868:	01b12023          	sw	s11,0(sp)
8001086c:	0017f693          	andi	a3,a5,1
        return; // test char range

    for (uint8_t i = 0; i < X_width; i++)
        for (uint8_t j = 0; j < Y_height; j++)
            gfxbuffer[fixrow(char_y + j)][char_x + i] = BGround_Color;
80010870:	4017df93          	srai	t6,a5,0x1
80010874:	00068663          	beqz	a3,80010880 <HAL_OLED_PutChar+0x88>
80010878:	01f78713          	addi	a4,a5,31
8001087c:	41f70fb3          	sub	t6,a4,t6
80010880:	00178313          	addi	t1,a5,1
80010884:	00137613          	andi	a2,t1,1
80010888:	80004737          	lui	a4,0x80004
8001088c:	86a75703          	lhu	a4,-1942(a4) # 8000386a <__clzsi2+0xfffeabd6>
80010890:	40135313          	srai	t1,t1,0x1
80010894:	00060663          	beqz	a2,800108a0 <HAL_OLED_PutChar+0xa8>
80010898:	02078613          	addi	a2,a5,32
8001089c:	40660333          	sub	t1,a2,t1
800108a0:	00278f13          	addi	t5,a5,2
800108a4:	401f5f13          	srai	t5,t5,0x1
800108a8:	00068663          	beqz	a3,800108b4 <HAL_OLED_PutChar+0xbc>
800108ac:	02178613          	addi	a2,a5,33
800108b0:	41e60f33          	sub	t5,a2,t5
800108b4:	00378893          	addi	a7,a5,3
800108b8:	0018f613          	andi	a2,a7,1
800108bc:	4018d893          	srai	a7,a7,0x1
800108c0:	00060663          	beqz	a2,800108cc <HAL_OLED_PutChar+0xd4>
800108c4:	02278613          	addi	a2,a5,34
800108c8:	411608b3          	sub	a7,a2,a7
800108cc:	00478e93          	addi	t4,a5,4
800108d0:	401ede93          	srai	t4,t4,0x1
800108d4:	00068663          	beqz	a3,800108e0 <HAL_OLED_PutChar+0xe8>
800108d8:	02378613          	addi	a2,a5,35
800108dc:	41d60eb3          	sub	t4,a2,t4
800108e0:	00578593          	addi	a1,a5,5
800108e4:	0015f613          	andi	a2,a1,1
800108e8:	4015d593          	srai	a1,a1,0x1
800108ec:	00060663          	beqz	a2,800108f8 <HAL_OLED_PutChar+0x100>
800108f0:	02478613          	addi	a2,a5,36
800108f4:	40b605b3          	sub	a1,a2,a1
800108f8:	00678e13          	addi	t3,a5,6
800108fc:	401e5e13          	srai	t3,t3,0x1
80010900:	00068663          	beqz	a3,8001090c <HAL_OLED_PutChar+0x114>
80010904:	02578693          	addi	a3,a5,37
80010908:	41c68e33          	sub	t3,a3,t3
8001090c:	00778613          	addi	a2,a5,7
80010910:	00167693          	andi	a3,a2,1
80010914:	40165613          	srai	a2,a2,0x1
80010918:	4c069e63          	bnez	a3,80010df4 <HAL_OLED_PutChar+0x5fc>
8001091c:	001f9993          	slli	s3,t6,0x1
80010920:	01f989b3          	add	s3,s3,t6
80010924:	00131913          	slli	s2,t1,0x1
80010928:	00599c13          	slli	s8,s3,0x5
8001092c:	00690933          	add	s2,s2,t1
80010930:	001f1493          	slli	s1,t5,0x1
80010934:	00591b93          	slli	s7,s2,0x5
80010938:	01e484b3          	add	s1,s1,t5
8001093c:	010c0333          	add	t1,s8,a6
80010940:	00189413          	slli	s0,a7,0x1
80010944:	800017b7          	lui	a5,0x80001
80010948:	86878793          	addi	a5,a5,-1944 # 80000868 <__clzsi2+0xfffe7bd4>
8001094c:	00549b13          	slli	s6,s1,0x5
80010950:	01140433          	add	s0,s0,a7
80010954:	010b86b3          	add	a3,s7,a6
80010958:	001e9393          	slli	t2,t4,0x1
8001095c:	00131313          	slli	t1,t1,0x1
80010960:	00541a93          	slli	s5,s0,0x5
80010964:	01d383b3          	add	t2,t2,t4
80010968:	00678333          	add	t1,a5,t1
8001096c:	010b08b3          	add	a7,s6,a6
80010970:	00159f93          	slli	t6,a1,0x1
80010974:	00169693          	slli	a3,a3,0x1
80010978:	00e31023          	sh	a4,0(t1)
8001097c:	00539a13          	slli	s4,t2,0x5
80010980:	00bf8fb3          	add	t6,t6,a1
80010984:	00d786b3          	add	a3,a5,a3
80010988:	010a85b3          	add	a1,s5,a6
8001098c:	001e1f13          	slli	t5,t3,0x1
80010990:	00189893          	slli	a7,a7,0x1
80010994:	00e69023          	sh	a4,0(a3)
80010998:	005f9e93          	slli	t4,t6,0x5
8001099c:	011788b3          	add	a7,a5,a7
800109a0:	01cf0f33          	add	t5,t5,t3
800109a4:	010a06b3          	add	a3,s4,a6
800109a8:	00161313          	slli	t1,a2,0x1
800109ac:	00159593          	slli	a1,a1,0x1
800109b0:	00e89023          	sh	a4,0(a7)
800109b4:	005f1e13          	slli	t3,t5,0x5
800109b8:	00b785b3          	add	a1,a5,a1
800109bc:	010e8cb3          	add	s9,t4,a6
800109c0:	00c30333          	add	t1,t1,a2
800109c4:	00169693          	slli	a3,a3,0x1
800109c8:	00e59023          	sh	a4,0(a1)
800109cc:	00531313          	slli	t1,t1,0x5
800109d0:	00d786b3          	add	a3,a5,a3
800109d4:	010e05b3          	add	a1,t3,a6
800109d8:	001c9c93          	slli	s9,s9,0x1
800109dc:	00e69023          	sh	a4,0(a3)
800109e0:	00180893          	addi	a7,a6,1
800109e4:	01978cb3          	add	s9,a5,s9
800109e8:	010306b3          	add	a3,t1,a6
800109ec:	00159593          	slli	a1,a1,0x1
800109f0:	00ec9023          	sh	a4,0(s9)
800109f4:	00b785b3          	add	a1,a5,a1
800109f8:	011c0cb3          	add	s9,s8,a7
800109fc:	00169693          	slli	a3,a3,0x1
80010a00:	00e59023          	sh	a4,0(a1)
80010a04:	00d786b3          	add	a3,a5,a3
80010a08:	011b85b3          	add	a1,s7,a7
80010a0c:	001c9c93          	slli	s9,s9,0x1
80010a10:	00e69023          	sh	a4,0(a3)
80010a14:	01978cb3          	add	s9,a5,s9
80010a18:	011b0d33          	add	s10,s6,a7
80010a1c:	00159593          	slli	a1,a1,0x1
80010a20:	00ec9023          	sh	a4,0(s9)
80010a24:	00b785b3          	add	a1,a5,a1
80010a28:	011a8cb3          	add	s9,s5,a7
80010a2c:	001d1d13          	slli	s10,s10,0x1
80010a30:	00e59023          	sh	a4,0(a1)
80010a34:	01a78d33          	add	s10,a5,s10
80010a38:	011a05b3          	add	a1,s4,a7
80010a3c:	001c9c93          	slli	s9,s9,0x1
80010a40:	00ed1023          	sh	a4,0(s10)
80010a44:	01978cb3          	add	s9,a5,s9
80010a48:	011e8d33          	add	s10,t4,a7
80010a4c:	00159593          	slli	a1,a1,0x1
80010a50:	00ec9023          	sh	a4,0(s9)
80010a54:	00b785b3          	add	a1,a5,a1
80010a58:	011e0cb3          	add	s9,t3,a7
80010a5c:	001d1d13          	slli	s10,s10,0x1
80010a60:	00e59023          	sh	a4,0(a1)
80010a64:	01a78d33          	add	s10,a5,s10
80010a68:	00280593          	addi	a1,a6,2
80010a6c:	011308b3          	add	a7,t1,a7
80010a70:	001c9c93          	slli	s9,s9,0x1
80010a74:	00ed1023          	sh	a4,0(s10)
80010a78:	01978cb3          	add	s9,a5,s9
80010a7c:	00bc0db3          	add	s11,s8,a1
80010a80:	00189893          	slli	a7,a7,0x1
80010a84:	00ec9023          	sh	a4,0(s9)
80010a88:	011788b3          	add	a7,a5,a7
80010a8c:	00bb8cb3          	add	s9,s7,a1
80010a90:	001d9d93          	slli	s11,s11,0x1
80010a94:	00e89023          	sh	a4,0(a7)
80010a98:	01b78db3          	add	s11,a5,s11
80010a9c:	00bb0d33          	add	s10,s6,a1
80010aa0:	001c9893          	slli	a7,s9,0x1
80010aa4:	00ed9023          	sh	a4,0(s11)
80010aa8:	011788b3          	add	a7,a5,a7
80010aac:	00ba8cb3          	add	s9,s5,a1
80010ab0:	001d1d13          	slli	s10,s10,0x1
80010ab4:	00e89023          	sh	a4,0(a7)
80010ab8:	01a78d33          	add	s10,a5,s10
80010abc:	00ba08b3          	add	a7,s4,a1
80010ac0:	001c9c93          	slli	s9,s9,0x1
80010ac4:	00ed1023          	sh	a4,0(s10)
80010ac8:	01978cb3          	add	s9,a5,s9
80010acc:	00be8d33          	add	s10,t4,a1
80010ad0:	00189893          	slli	a7,a7,0x1
80010ad4:	00ec9023          	sh	a4,0(s9)
80010ad8:	011788b3          	add	a7,a5,a7
80010adc:	001d1c93          	slli	s9,s10,0x1
80010ae0:	00be0d33          	add	s10,t3,a1
80010ae4:	00e89023          	sh	a4,0(a7)
80010ae8:	01978cb3          	add	s9,a5,s9
80010aec:	00380893          	addi	a7,a6,3
80010af0:	00b305b3          	add	a1,t1,a1
80010af4:	001d1d13          	slli	s10,s10,0x1
80010af8:	00ec9023          	sh	a4,0(s9)
80010afc:	01a78d33          	add	s10,a5,s10
80010b00:	011c0cb3          	add	s9,s8,a7
80010b04:	00159593          	slli	a1,a1,0x1
80010b08:	00ed1023          	sh	a4,0(s10)
80010b0c:	00b785b3          	add	a1,a5,a1
80010b10:	011b8d33          	add	s10,s7,a7
80010b14:	001c9c93          	slli	s9,s9,0x1
80010b18:	00e59023          	sh	a4,0(a1)
80010b1c:	001d1d13          	slli	s10,s10,0x1
80010b20:	019785b3          	add	a1,a5,s9
80010b24:	011b0cb3          	add	s9,s6,a7
80010b28:	00e59023          	sh	a4,0(a1)
80010b2c:	01a78d33          	add	s10,a5,s10
80010b30:	011a85b3          	add	a1,s5,a7
80010b34:	001c9c93          	slli	s9,s9,0x1
80010b38:	00ed1023          	sh	a4,0(s10)
80010b3c:	01978cb3          	add	s9,a5,s9
80010b40:	011a0d33          	add	s10,s4,a7
80010b44:	00159593          	slli	a1,a1,0x1
80010b48:	00ec9023          	sh	a4,0(s9)
80010b4c:	00b785b3          	add	a1,a5,a1
80010b50:	011e8cb3          	add	s9,t4,a7
80010b54:	001d1d13          	slli	s10,s10,0x1
80010b58:	00e59023          	sh	a4,0(a1)
80010b5c:	01a78d33          	add	s10,a5,s10
80010b60:	011e0db3          	add	s11,t3,a7
80010b64:	001c9c93          	slli	s9,s9,0x1
80010b68:	00480593          	addi	a1,a6,4
80010b6c:	00ed1023          	sh	a4,0(s10)
80010b70:	01978cb3          	add	s9,a5,s9
80010b74:	011308b3          	add	a7,t1,a7
80010b78:	001d9d93          	slli	s11,s11,0x1
80010b7c:	00ec9023          	sh	a4,0(s9)
80010b80:	01b78db3          	add	s11,a5,s11
80010b84:	00bc0d33          	add	s10,s8,a1
80010b88:	00bb8cb3          	add	s9,s7,a1
80010b8c:	00189893          	slli	a7,a7,0x1
80010b90:	00ed9023          	sh	a4,0(s11)
80010b94:	011788b3          	add	a7,a5,a7
80010b98:	00bb0db3          	add	s11,s6,a1
80010b9c:	001c9c93          	slli	s9,s9,0x1
80010ba0:	001d1d13          	slli	s10,s10,0x1
80010ba4:	00e89023          	sh	a4,0(a7)
80010ba8:	01a78d33          	add	s10,a5,s10
80010bac:	019788b3          	add	a7,a5,s9
80010bb0:	001d9d93          	slli	s11,s11,0x1
80010bb4:	00ba8cb3          	add	s9,s5,a1
80010bb8:	00ed1023          	sh	a4,0(s10)
80010bbc:	001c9c93          	slli	s9,s9,0x1
80010bc0:	00e89023          	sh	a4,0(a7)
80010bc4:	01b78d33          	add	s10,a5,s11
80010bc8:	00ba08b3          	add	a7,s4,a1
80010bcc:	00ed1023          	sh	a4,0(s10)
80010bd0:	01978cb3          	add	s9,a5,s9
80010bd4:	00be8d33          	add	s10,t4,a1
80010bd8:	00189893          	slli	a7,a7,0x1
80010bdc:	00ec9023          	sh	a4,0(s9)
80010be0:	011788b3          	add	a7,a5,a7
80010be4:	00be0cb3          	add	s9,t3,a1
80010be8:	001d1d13          	slli	s10,s10,0x1
80010bec:	00e89023          	sh	a4,0(a7)
80010bf0:	01a78d33          	add	s10,a5,s10
80010bf4:	00580893          	addi	a7,a6,5
80010bf8:	00b305b3          	add	a1,t1,a1
80010bfc:	001c9c93          	slli	s9,s9,0x1
80010c00:	00ed1023          	sh	a4,0(s10)
80010c04:	01978cb3          	add	s9,a5,s9
80010c08:	011c0c33          	add	s8,s8,a7
80010c0c:	00159593          	slli	a1,a1,0x1
80010c10:	00ec9023          	sh	a4,0(s9)
80010c14:	00b785b3          	add	a1,a5,a1
80010c18:	011b8bb3          	add	s7,s7,a7
80010c1c:	001c1c13          	slli	s8,s8,0x1
80010c20:	00e59023          	sh	a4,0(a1)
80010c24:	011b0b33          	add	s6,s6,a7
80010c28:	018785b3          	add	a1,a5,s8
80010c2c:	001b9b93          	slli	s7,s7,0x1
80010c30:	00e59023          	sh	a4,0(a1)
80010c34:	01778bb3          	add	s7,a5,s7
80010c38:	011a85b3          	add	a1,s5,a7
80010c3c:	001b1b13          	slli	s6,s6,0x1
80010c40:	00eb9023          	sh	a4,0(s7)
80010c44:	01678ab3          	add	s5,a5,s6
80010c48:	011a0a33          	add	s4,s4,a7
80010c4c:	00159593          	slli	a1,a1,0x1
80010c50:	00ea9023          	sh	a4,0(s5)
80010c54:	00b785b3          	add	a1,a5,a1
80010c58:	011e8eb3          	add	t4,t4,a7
80010c5c:	001a1a13          	slli	s4,s4,0x1
80010c60:	00e59023          	sh	a4,0(a1)

    for (uint8_t i = 0; i < X_width; i++)
    {
        uint8_t temp = font6x8[value - 32][i];
80010c64:	fe050513          	addi	a0,a0,-32
            gfxbuffer[fixrow(char_y + j)][char_x + i] = BGround_Color;
80010c68:	01478a33          	add	s4,a5,s4
80010c6c:	001e9e93          	slli	t4,t4,0x1
80010c70:	00ea1023          	sh	a4,0(s4)
80010c74:	011e05b3          	add	a1,t3,a7
80010c78:	01d78e33          	add	t3,a5,t4
80010c7c:	00151e93          	slli	t4,a0,0x1
80010c80:	00ee1023          	sh	a4,0(t3)
80010c84:	01130333          	add	t1,t1,a7
80010c88:	00ae8e33          	add	t3,t4,a0
80010c8c:	00159593          	slli	a1,a1,0x1
80010c90:	800008b7          	lui	a7,0x80000
            //HAL_OLED_SetPixel(char_x+i, char_y+j,  (temp & 1) ? Char_Color : BGround_Color);

            uint8_t x = char_x + i, y = char_y + j;
            if (temp & 1)
            {
                gfxbuffer[fixrow(char_y + j)][char_x + i] = Char_Color;
80010c94:	80004537          	lui	a0,0x80004
            gfxbuffer[fixrow(char_y + j)][char_x + i] = BGround_Color;
80010c98:	00b785b3          	add	a1,a5,a1
80010c9c:	00088893          	mv	a7,a7
80010ca0:	00131313          	slli	t1,t1,0x1
80010ca4:	00261e93          	slli	t4,a2,0x2
                gfxbuffer[fixrow(char_y + j)][char_x + i] = Char_Color;
80010ca8:	86c55503          	lhu	a0,-1940(a0) # 8000386c <__clzsi2+0xfffeabd8>
            gfxbuffer[fixrow(char_y + j)][char_x + i] = BGround_Color;
80010cac:	00e59023          	sh	a4,0(a1)
80010cb0:	001e1e13          	slli	t3,t3,0x1
80010cb4:	02c88593          	addi	a1,a7,44 # 8000002c <__clzsi2+0xfffe7398>
80010cb8:	006787b3          	add	a5,a5,t1
80010cbc:	03288893          	addi	a7,a7,50
80010cc0:	41d60633          	sub	a2,a2,t4
80010cc4:	00e79023          	sh	a4,0(a5)
    for (uint8_t i = 0; i < X_width; i++)
80010cc8:	01c585b3          	add	a1,a1,t3
80010ccc:	00068713          	mv	a4,a3
80010cd0:	01c888b3          	add	a7,a7,t3
80010cd4:	00661693          	slli	a3,a2,0x6
80010cd8:	00699993          	slli	s3,s3,0x6
80010cdc:	00691913          	slli	s2,s2,0x6
80010ce0:	00649493          	slli	s1,s1,0x6
80010ce4:	00641413          	slli	s0,s0,0x6
80010ce8:	00639393          	slli	t2,t2,0x6
80010cec:	006f9f93          	slli	t6,t6,0x6
80010cf0:	006f1f13          	slli	t5,t5,0x6
        uint8_t temp = font6x8[value - 32][i];
80010cf4:	0005c783          	lbu	a5,0(a1)
            if (temp & 1)
80010cf8:	0017f613          	andi	a2,a5,1
80010cfc:	00060863          	beqz	a2,80010d0c <HAL_OLED_PutChar+0x514>
                gfxbuffer[fixrow(char_y + j)][char_x + i] = Char_Color;
80010d00:	00d70633          	add	a2,a4,a3
80010d04:	01360633          	add	a2,a2,s3
80010d08:	00a61023          	sh	a0,0(a2)
            if (temp & 1)
80010d0c:	0027f613          	andi	a2,a5,2
80010d10:	00060863          	beqz	a2,80010d20 <HAL_OLED_PutChar+0x528>
                gfxbuffer[fixrow(char_y + j)][char_x + i] = Char_Color;
80010d14:	00d70633          	add	a2,a4,a3
80010d18:	01260633          	add	a2,a2,s2
80010d1c:	00a61023          	sh	a0,0(a2)
            if (temp & 1)
80010d20:	0047f613          	andi	a2,a5,4
80010d24:	00060863          	beqz	a2,80010d34 <HAL_OLED_PutChar+0x53c>
                gfxbuffer[fixrow(char_y + j)][char_x + i] = Char_Color;
80010d28:	00d70633          	add	a2,a4,a3
80010d2c:	00960633          	add	a2,a2,s1
80010d30:	00a61023          	sh	a0,0(a2)
            if (temp & 1)
80010d34:	0087f613          	andi	a2,a5,8
80010d38:	00060863          	beqz	a2,80010d48 <HAL_OLED_PutChar+0x550>
                gfxbuffer[fixrow(char_y + j)][char_x + i] = Char_Color;
80010d3c:	00d70633          	add	a2,a4,a3
80010d40:	00860633          	add	a2,a2,s0
80010d44:	00a61023          	sh	a0,0(a2)
            if (temp & 1)
80010d48:	0107f613          	andi	a2,a5,16
80010d4c:	00060863          	beqz	a2,80010d5c <HAL_OLED_PutChar+0x564>
                gfxbuffer[fixrow(char_y + j)][char_x + i] = Char_Color;
80010d50:	00d70633          	add	a2,a4,a3
80010d54:	00760633          	add	a2,a2,t2
80010d58:	00a61023          	sh	a0,0(a2)
            if (temp & 1)
80010d5c:	0207f613          	andi	a2,a5,32
80010d60:	00060863          	beqz	a2,80010d70 <HAL_OLED_PutChar+0x578>
                gfxbuffer[fixrow(char_y + j)][char_x + i] = Char_Color;
80010d64:	00d70633          	add	a2,a4,a3
80010d68:	01f60633          	add	a2,a2,t6
80010d6c:	00a61023          	sh	a0,0(a2)
            if (temp & 1)
80010d70:	0407f613          	andi	a2,a5,64
80010d74:	00060863          	beqz	a2,80010d84 <HAL_OLED_PutChar+0x58c>
                gfxbuffer[fixrow(char_y + j)][char_x + i] = Char_Color;
80010d78:	00d70633          	add	a2,a4,a3
80010d7c:	01e60633          	add	a2,a2,t5
80010d80:	00a61023          	sh	a0,0(a2)
            if (temp & 1)
80010d84:	0077d793          	srli	a5,a5,0x7
80010d88:	00078463          	beqz	a5,80010d90 <HAL_OLED_PutChar+0x598>
                gfxbuffer[fixrow(char_y + j)][char_x + i] = Char_Color;
80010d8c:	00a71023          	sh	a0,0(a4)
    for (uint8_t i = 0; i < X_width; i++)
80010d90:	00158593          	addi	a1,a1,1
80010d94:	00270713          	addi	a4,a4,2
80010d98:	f5159ee3          	bne	a1,a7,80010cf4 <HAL_OLED_PutChar+0x4fc>
            temp >>= 1;
        }
    }

    char_x += X_width;
80010d9c:	02c12403          	lw	s0,44(sp)
    char_x += X_width;
80010da0:	00680813          	addi	a6,a6,6
80010da4:	870289a3          	sb	a6,-1933(t0)
80010da8:	02812483          	lw	s1,40(sp)
80010dac:	02412903          	lw	s2,36(sp)
80010db0:	02012983          	lw	s3,32(sp)
80010db4:	01c12a03          	lw	s4,28(sp)
80010db8:	01812a83          	lw	s5,24(sp)
80010dbc:	01412b03          	lw	s6,20(sp)
80010dc0:	01012b83          	lw	s7,16(sp)
80010dc4:	00c12c03          	lw	s8,12(sp)
80010dc8:	00812c83          	lw	s9,8(sp)
80010dcc:	00412d03          	lw	s10,4(sp)
80010dd0:	00012d83          	lw	s11,0(sp)
80010dd4:	03010113          	addi	sp,sp,48
80010dd8:	00008067          	ret
        char_y = char_y + Y_height;
80010ddc:	00878793          	addi	a5,a5,8
80010de0:	0ff7f793          	zext.b	a5,a5
        char_x = x_offset;
80010de4:	860289a3          	sb	zero,-1933(t0)
        char_y = char_y + Y_height;
80010de8:	86f70923          	sb	a5,-1934(a4)
80010dec:	00000813          	li	a6,0
80010df0:	a29ff06f          	j	80010818 <HAL_OLED_PutChar+0x20>
            gfxbuffer[fixrow(char_y + j)][char_x + i] = BGround_Color;
80010df4:	02678793          	addi	a5,a5,38
80010df8:	40c78633          	sub	a2,a5,a2
80010dfc:	b21ff06f          	j	8001091c <HAL_OLED_PutChar+0x124>
80010e00:	00008067          	ret

80010e04 <HAL_UART_WriteBuffer>:
#include "uart.h"

#ifndef OPTIMIZE_STACK
void HAL_UART_WriteBuffer(uint8_t *src, uint32_t length) {
    for(uint32_t i=0; i<length; i++) {
80010e04:	02058463          	beqz	a1,80010e2c <HAL_UART_WriteBuffer+0x28>
80010e08:	00b506b3          	add	a3,a0,a1
        while(*PTR_UART_STATUS & FLAG_UART_TX_FIFO_BUSY);
80010e0c:	c0001737          	lui	a4,0xc0001
80010e10:	00074783          	lbu	a5,0(a4) # c0001000 <__clzsi2+0x3ffe836c>
80010e14:	0017f793          	andi	a5,a5,1
80010e18:	fe079ce3          	bnez	a5,80010e10 <HAL_UART_WriteBuffer+0xc>
        *PTR_UART_TX_DATA = *(src+i);
80010e1c:	00054783          	lbu	a5,0(a0)
    for(uint32_t i=0; i<length; i++) {
80010e20:	00150513          	addi	a0,a0,1
        *PTR_UART_TX_DATA = *(src+i);
80010e24:	00f700a3          	sb	a5,1(a4)
    for(uint32_t i=0; i<length; i++) {
80010e28:	fed514e3          	bne	a0,a3,80010e10 <HAL_UART_WriteBuffer+0xc>
    }
}
80010e2c:	00008067          	ret

80010e30 <HAL_UART_PutChar>:

void HAL_UART_PutChar(char character)
{
    while(*PTR_UART_STATUS & FLAG_UART_TX_FIFO_BUSY);
80010e30:	c0001737          	lui	a4,0xc0001
80010e34:	00074783          	lbu	a5,0(a4) # c0001000 <__clzsi2+0x3ffe836c>
80010e38:	0017f793          	andi	a5,a5,1
80010e3c:	fe079ce3          	bnez	a5,80010e34 <HAL_UART_PutChar+0x4>
    *PTR_UART_TX_DATA = (uint8_t) character;
80010e40:	00a700a3          	sb	a0,1(a4)
}
80010e44:	00008067          	ret

80010e48 <HAL_UART_PutLine>:

void HAL_UART_PutLine(char *line) {
  while(*line != (char) 0) {
80010e48:	00054683          	lbu	a3,0(a0)
80010e4c:	02068263          	beqz	a3,80010e70 <HAL_UART_PutLine+0x28>
    while(*PTR_UART_STATUS & FLAG_UART_TX_FIFO_BUSY);
80010e50:	c0001737          	lui	a4,0xc0001
80010e54:	00074783          	lbu	a5,0(a4) # c0001000 <__clzsi2+0x3ffe836c>
80010e58:	0017f793          	andi	a5,a5,1
80010e5c:	fe079ce3          	bnez	a5,80010e54 <HAL_UART_PutLine+0xc>
    *PTR_UART_TX_DATA = (uint8_t) character;
80010e60:	00d700a3          	sb	a3,1(a4)
  while(*line != (char) 0) {
80010e64:	00154683          	lbu	a3,1(a0)
    HAL_UART_PutChar(*line);
    line++;
80010e68:	00150513          	addi	a0,a0,1
  while(*line != (char) 0) {
80010e6c:	fe0694e3          	bnez	a3,80010e54 <HAL_UART_PutLine+0xc>
  }
}
80010e70:	00008067          	ret

80010e74 <HAL_Timebase_GetSeconds>:
#include "timebase.h"

uint32_t HAL_Timebase_GetSeconds() {
    return *PTR_TIMEBASE_SEC;
80010e74:	c00007b7          	lui	a5,0xc0000
80010e78:	0047a503          	lw	a0,4(a5) # c0000004 <__clzsi2+0x3ffe7370>
}
80010e7c:	00008067          	ret

80010e80 <HAL_Timebase_GetNanos>:

uint32_t HAL_Timebase_GetNanos() {
    return *PTR_TIMEBASE_NANOS;
80010e80:	c00007b7          	lui	a5,0xc0000
80010e84:	0007a503          	lw	a0,0(a5) # c0000000 <__clzsi2+0x3ffe736c>
}
80010e88:	00008067          	ret

80010e8c <HAL_Timebase_DelayNS>:

void HAL_Timebase_DelayNS(uint32_t ns) {
    *PTR_TIMEBASE_DELAY_NS = ns;
80010e8c:	c00007b7          	lui	a5,0xc0000
80010e90:	00a7a423          	sw	a0,8(a5) # c0000008 <__clzsi2+0x3ffe7374>
}
80010e94:	00008067          	ret

80010e98 <HAL_Timebase_DelayUS>:

void HAL_Timebase_DelayUS(uint32_t us) {
    for(uint32_t i=0; i<us; i++)
80010e98:	02050063          	beqz	a0,80010eb8 <HAL_Timebase_DelayUS+0x20>
    *PTR_TIMEBASE_DELAY_NS = ns;
80010e9c:	c0000737          	lui	a4,0xc0000
    for(uint32_t i=0; i<us; i++)
80010ea0:	00000793          	li	a5,0
    *PTR_TIMEBASE_DELAY_NS = ns;
80010ea4:	00870713          	addi	a4,a4,8 # c0000008 <__clzsi2+0x3ffe7374>
80010ea8:	3e800693          	li	a3,1000
80010eac:	00d72023          	sw	a3,0(a4)
    for(uint32_t i=0; i<us; i++)
80010eb0:	00178793          	addi	a5,a5,1
80010eb4:	fef51ce3          	bne	a0,a5,80010eac <HAL_Timebase_DelayUS+0x14>
        HAL_Timebase_DelayNS(1000);
}
80010eb8:	00008067          	ret

80010ebc <HAL_Timebase_DelayMS>:

void HAL_Timebase_DelayMS(uint32_t ms) {
    for(uint32_t i=0; i<ms; i++)
80010ebc:	02050263          	beqz	a0,80010ee0 <HAL_Timebase_DelayMS+0x24>
    *PTR_TIMEBASE_DELAY_NS = ns;
80010ec0:	c00006b7          	lui	a3,0xc0000
80010ec4:	000f4737          	lui	a4,0xf4
    for(uint32_t i=0; i<ms; i++)
80010ec8:	00000793          	li	a5,0
    *PTR_TIMEBASE_DELAY_NS = ns;
80010ecc:	00868693          	addi	a3,a3,8 # c0000008 <__clzsi2+0x3ffe7374>
80010ed0:	24070713          	addi	a4,a4,576 # f4240 <font6x8-0x7ff0bdec>
80010ed4:	00e6a023          	sw	a4,0(a3)
    for(uint32_t i=0; i<ms; i++)
80010ed8:	00178793          	addi	a5,a5,1
80010edc:	fef51ce3          	bne	a0,a5,80010ed4 <HAL_Timebase_DelayMS+0x18>
        HAL_Timebase_DelayNS(1000000);
80010ee0:	00008067          	ret

80010ee4 <HAL_GPIO_SetDirections>:
#include "gpio.h"

#ifndef OPTIMIZE_STACK
void HAL_GPIO_SetDirections(uint32_t directions){
    *PTR_GPIO_DIR = directions;
80010ee4:	c00027b7          	lui	a5,0xc0002
80010ee8:	00a7a023          	sw	a0,0(a5) # c0002000 <__clzsi2+0x3ffe936c>
}
80010eec:	00008067          	ret

80010ef0 <HAL_GPIO_SetOutputs>:
void HAL_GPIO_SetOutputs(uint32_t values){
    *PTR_GPIO_DATA = values;
80010ef0:	c00027b7          	lui	a5,0xc0002
80010ef4:	00a7a223          	sw	a0,4(a5) # c0002004 <__clzsi2+0x3ffe9370>
}
80010ef8:	00008067          	ret

80010efc <HAL_GPIO_GetInputs>:
uint32_t HAL_GPIO_GetInputs(){
    return *PTR_GPIO_DATA;
80010efc:	c00027b7          	lui	a5,0xc0002
80010f00:	0047a503          	lw	a0,4(a5) # c0002004 <__clzsi2+0x3ffe9370>
}
80010f04:	00008067          	ret

80010f08 <set_mem_page>:
 */
static int8_t null_ptr_check(const struct bme680_dev *dev)
{
	int8_t rslt;

	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80010f08:	0a058e63          	beqz	a1,80010fc4 <set_mem_page+0xbc>
80010f0c:	04c5a703          	lw	a4,76(a1)
{
80010f10:	fe010113          	addi	sp,sp,-32
80010f14:	00812c23          	sw	s0,24(sp)
80010f18:	00112e23          	sw	ra,28(sp)
80010f1c:	00058413          	mv	s0,a1
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80010f20:	08070e63          	beqz	a4,80010fbc <set_mem_page+0xb4>
80010f24:	0505a783          	lw	a5,80(a1)
80010f28:	08078a63          	beqz	a5,80010fbc <set_mem_page+0xb4>
80010f2c:	0545a783          	lw	a5,84(a1)
80010f30:	08078663          	beqz	a5,80010fbc <set_mem_page+0xb4>
		if (reg_addr > 0x7f)
80010f34:	08054513          	xori	a0,a0,128
		if (mem_page != dev->mem_page) {
80010f38:	0085c683          	lbu	a3,8(a1)
		if (reg_addr > 0x7f)
80010f3c:	00755793          	srli	a5,a0,0x7
80010f40:	00479793          	slli	a5,a5,0x4
		if (mem_page != dev->mem_page) {
80010f44:	04f68e63          	beq	a3,a5,80010fa0 <set_mem_page+0x98>
			dev->com_rslt = dev->read(dev->dev_id, BME680_MEM_PAGE_ADDR | BME680_SPI_RD_MSK, &reg, 1);
80010f48:	0015c503          	lbu	a0,1(a1)
			dev->mem_page = mem_page;
80010f4c:	00f58423          	sb	a5,8(a1)
			dev->com_rslt = dev->read(dev->dev_id, BME680_MEM_PAGE_ADDR | BME680_SPI_RD_MSK, &reg, 1);
80010f50:	00100693          	li	a3,1
80010f54:	00f10613          	addi	a2,sp,15
80010f58:	0f300593          	li	a1,243
80010f5c:	000700e7          	jalr	a4
80010f60:	04a40c23          	sb	a0,88(s0)
			if (dev->com_rslt != 0)
80010f64:	04051863          	bnez	a0,80010fb4 <set_mem_page+0xac>
				reg = reg & (~BME680_MEM_PAGE_MSK);
80010f68:	00f14783          	lbu	a5,15(sp)
				reg = reg | (dev->mem_page & BME680_MEM_PAGE_MSK);
80010f6c:	00844703          	lbu	a4,8(s0)
				dev->com_rslt = dev->write(dev->dev_id, BME680_MEM_PAGE_ADDR & BME680_SPI_WR_MSK,
80010f70:	05042803          	lw	a6,80(s0)
80010f74:	00144503          	lbu	a0,1(s0)
				reg = reg | (dev->mem_page & BME680_MEM_PAGE_MSK);
80010f78:	01077713          	andi	a4,a4,16
				reg = reg & (~BME680_MEM_PAGE_MSK);
80010f7c:	fef7f793          	andi	a5,a5,-17
				reg = reg | (dev->mem_page & BME680_MEM_PAGE_MSK);
80010f80:	00e7e7b3          	or	a5,a5,a4
80010f84:	00f107a3          	sb	a5,15(sp)
				dev->com_rslt = dev->write(dev->dev_id, BME680_MEM_PAGE_ADDR & BME680_SPI_WR_MSK,
80010f88:	00100693          	li	a3,1
80010f8c:	00f10613          	addi	a2,sp,15
80010f90:	07300593          	li	a1,115
80010f94:	000800e7          	jalr	a6
80010f98:	04a40c23          	sb	a0,88(s0)
				if (dev->com_rslt != 0)
80010f9c:	00051c63          	bnez	a0,80010fb4 <set_mem_page+0xac>
		/* Device structure pointer is not valid */
		rslt = BME680_E_NULL_PTR;
	} else {
		/* Device structure is fine */
		rslt = BME680_OK;
80010fa0:	00000513          	li	a0,0
}
80010fa4:	01c12083          	lw	ra,28(sp)
80010fa8:	01812403          	lw	s0,24(sp)
80010fac:	02010113          	addi	sp,sp,32
80010fb0:	00008067          	ret
					rslt = BME680_E_COM_FAIL;
80010fb4:	ffe00513          	li	a0,-2
80010fb8:	fedff06f          	j	80010fa4 <set_mem_page+0x9c>
		rslt = BME680_E_NULL_PTR;
80010fbc:	fff00513          	li	a0,-1
80010fc0:	fe5ff06f          	j	80010fa4 <set_mem_page+0x9c>
80010fc4:	fff00513          	li	a0,-1
}
80010fc8:	00008067          	ret

80010fcc <bme680_get_regs>:
{
80010fcc:	fd010113          	addi	sp,sp,-48
80010fd0:	02812423          	sw	s0,40(sp)
80010fd4:	02112623          	sw	ra,44(sp)
80010fd8:	02912223          	sw	s1,36(sp)
80010fdc:	03212023          	sw	s2,32(sp)
80010fe0:	01312e23          	sw	s3,28(sp)
80010fe4:	00068413          	mv	s0,a3
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80010fe8:	08068863          	beqz	a3,80011078 <bme680_get_regs+0xac>
80010fec:	04c6a783          	lw	a5,76(a3)
80010ff0:	08078463          	beqz	a5,80011078 <bme680_get_regs+0xac>
80010ff4:	0506a703          	lw	a4,80(a3)
80010ff8:	08070063          	beqz	a4,80011078 <bme680_get_regs+0xac>
80010ffc:	0546a703          	lw	a4,84(a3)
80011000:	06070c63          	beqz	a4,80011078 <bme680_get_regs+0xac>
		if (dev->intf == BME680_SPI_INTF) {
80011004:	00442703          	lw	a4,4(s0)
80011008:	00050493          	mv	s1,a0
8001100c:	00058913          	mv	s2,a1
80011010:	00060693          	mv	a3,a2
		rslt = BME680_OK;
80011014:	00000993          	li	s3,0
		if (dev->intf == BME680_SPI_INTF) {
80011018:	02070e63          	beqz	a4,80011054 <bme680_get_regs+0x88>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
8001101c:	00144503          	lbu	a0,1(s0)
80011020:	00090613          	mv	a2,s2
80011024:	00048593          	mv	a1,s1
80011028:	000780e7          	jalr	a5
8001102c:	04a40c23          	sb	a0,88(s0)
		if (dev->com_rslt != 0)
80011030:	04051863          	bnez	a0,80011080 <bme680_get_regs+0xb4>
}
80011034:	02c12083          	lw	ra,44(sp)
80011038:	02812403          	lw	s0,40(sp)
8001103c:	02412483          	lw	s1,36(sp)
80011040:	02012903          	lw	s2,32(sp)
80011044:	00098513          	mv	a0,s3
80011048:	01c12983          	lw	s3,28(sp)
8001104c:	03010113          	addi	sp,sp,48
80011050:	00008067          	ret
			rslt = set_mem_page(reg_addr, dev);
80011054:	00040593          	mv	a1,s0
80011058:	00c12623          	sw	a2,12(sp)
8001105c:	eadff0ef          	jal	ra,80010f08 <set_mem_page>
			if (rslt == BME680_OK)
80011060:	00c12683          	lw	a3,12(sp)
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80011064:	04c42783          	lw	a5,76(s0)
			rslt = set_mem_page(reg_addr, dev);
80011068:	00050993          	mv	s3,a0
			if (rslt == BME680_OK)
8001106c:	fa0518e3          	bnez	a0,8001101c <bme680_get_regs+0x50>
				reg_addr = reg_addr | BME680_SPI_RD_MSK;
80011070:	0804e493          	ori	s1,s1,128
80011074:	fa9ff06f          	j	8001101c <bme680_get_regs+0x50>
		rslt = BME680_E_NULL_PTR;
80011078:	fff00993          	li	s3,-1
8001107c:	fb9ff06f          	j	80011034 <bme680_get_regs+0x68>
			rslt = BME680_E_COM_FAIL;
80011080:	ffe00993          	li	s3,-2
80011084:	fb1ff06f          	j	80011034 <bme680_get_regs+0x68>

80011088 <bme680_set_regs>:
{
80011088:	fb010113          	addi	sp,sp,-80
8001108c:	04112623          	sw	ra,76(sp)
80011090:	04812423          	sw	s0,72(sp)
80011094:	04912223          	sw	s1,68(sp)
80011098:	05212023          	sw	s2,64(sp)
8001109c:	03312e23          	sw	s3,60(sp)
800110a0:	03412c23          	sw	s4,56(sp)
800110a4:	03512a23          	sw	s5,52(sp)
800110a8:	03612823          	sw	s6,48(sp)
	uint8_t tmp_buff[BME680_TMP_BUFFER_LENGTH] = { 0 };
800110ac:	00012423          	sw	zero,8(sp)
800110b0:	00012623          	sw	zero,12(sp)
800110b4:	00012823          	sw	zero,16(sp)
800110b8:	00012a23          	sw	zero,20(sp)
800110bc:	00012c23          	sw	zero,24(sp)
800110c0:	00012e23          	sw	zero,28(sp)
800110c4:	02012023          	sw	zero,32(sp)
800110c8:	02012223          	sw	zero,36(sp)
800110cc:	02012423          	sw	zero,40(sp)
800110d0:	02012623          	sw	zero,44(sp)
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
800110d4:	1e068463          	beqz	a3,800112bc <bme680_set_regs+0x234>
800110d8:	04c6a783          	lw	a5,76(a3)
800110dc:	00068493          	mv	s1,a3
800110e0:	1c078e63          	beqz	a5,800112bc <bme680_set_regs+0x234>
800110e4:	0506a783          	lw	a5,80(a3)
800110e8:	1c078a63          	beqz	a5,800112bc <bme680_set_regs+0x234>
800110ec:	0546a783          	lw	a5,84(a3)
800110f0:	1c078663          	beqz	a5,800112bc <bme680_set_regs+0x234>
		if ((len > 0) && (len < BME680_TMP_BUFFER_LENGTH / 2)) {
800110f4:	fff60793          	addi	a5,a2,-1
800110f8:	0ff7f793          	zext.b	a5,a5
800110fc:	01200713          	li	a4,18
80011100:	1cf76c63          	bltu	a4,a5,800112d8 <bme680_set_regs+0x250>
			for (index = 0; index < len; index++) {
80011104:	01061913          	slli	s2,a2,0x10
80011108:	00050a13          	mv	s4,a0
8001110c:	00050a93          	mv	s5,a0
80011110:	00058993          	mv	s3,a1
80011114:	01095913          	srli	s2,s2,0x10
80011118:	00810b13          	addi	s6,sp,8
8001111c:	00100413          	li	s0,1
		rslt = BME680_OK;
80011120:	00000513          	li	a0,0
				if (dev->intf == BME680_SPI_INTF) {
80011124:	0044a703          	lw	a4,4(s1)
					rslt = set_mem_page(reg_addr[index], dev);
80011128:	000ac783          	lbu	a5,0(s5)
				if (dev->intf == BME680_SPI_INTF) {
8001112c:	08070a63          	beqz	a4,800111c0 <bme680_set_regs+0x138>
				tmp_buff[(2 * index) + 1] = reg_data[index];
80011130:	00898733          	add	a4,s3,s0
					tmp_buff[(2 * index)] = reg_addr[index] & BME680_SPI_WR_MSK;
80011134:	fff74583          	lbu	a1,-1(a4)
80011138:	00141613          	slli	a2,s0,0x1
			for (index = 0; index < len; index++) {
8001113c:	01041693          	slli	a3,s0,0x10
					tmp_buff[(2 * index)] = reg_addr[index] & BME680_SPI_WR_MSK;
80011140:	00859593          	slli	a1,a1,0x8
80011144:	00b7e7b3          	or	a5,a5,a1
80011148:	00fb1023          	sh	a5,0(s6)
			for (index = 0; index < len; index++) {
8001114c:	0106d693          	srli	a3,a3,0x10
80011150:	00060793          	mv	a5,a2
80011154:	00810613          	addi	a2,sp,8
80011158:	00f607b3          	add	a5,a2,a5
8001115c:	0126e663          	bltu	a3,s2,80011168 <bme680_set_regs+0xe0>
80011160:	0340006f          	j	80011194 <bme680_set_regs+0x10c>
80011164:	00898733          	add	a4,s3,s0
					tmp_buff[(2 * index)] = reg_addr[index] & BME680_SPI_WR_MSK;
80011168:	00074683          	lbu	a3,0(a4)
					rslt = set_mem_page(reg_addr[index], dev);
8001116c:	008a0733          	add	a4,s4,s0
					tmp_buff[(2 * index)] = reg_addr[index] & BME680_SPI_WR_MSK;
80011170:	00074703          	lbu	a4,0(a4)
80011174:	00869693          	slli	a3,a3,0x8
			for (index = 0; index < len; index++) {
80011178:	00140413          	addi	s0,s0,1
					tmp_buff[(2 * index)] = reg_addr[index] & BME680_SPI_WR_MSK;
8001117c:	00d76733          	or	a4,a4,a3
80011180:	00e79023          	sh	a4,0(a5)
			for (index = 0; index < len; index++) {
80011184:	01041713          	slli	a4,s0,0x10
80011188:	01075713          	srli	a4,a4,0x10
8001118c:	00278793          	addi	a5,a5,2
80011190:	fd276ae3          	bltu	a4,s2,80011164 <bme680_set_regs+0xdc>
			if (rslt == BME680_OK) {
80011194:	0e050663          	beqz	a0,80011280 <bme680_set_regs+0x1f8>
}
80011198:	04c12083          	lw	ra,76(sp)
8001119c:	04812403          	lw	s0,72(sp)
800111a0:	04412483          	lw	s1,68(sp)
800111a4:	04012903          	lw	s2,64(sp)
800111a8:	03c12983          	lw	s3,60(sp)
800111ac:	03812a03          	lw	s4,56(sp)
800111b0:	03412a83          	lw	s5,52(sp)
800111b4:	03012b03          	lw	s6,48(sp)
800111b8:	05010113          	addi	sp,sp,80
800111bc:	00008067          	ret
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
800111c0:	04c4a803          	lw	a6,76(s1)
800111c4:	0e080863          	beqz	a6,800112b4 <bme680_set_regs+0x22c>
800111c8:	0504a703          	lw	a4,80(s1)
800111cc:	0e070463          	beqz	a4,800112b4 <bme680_set_regs+0x22c>
800111d0:	0544a703          	lw	a4,84(s1)
800111d4:	0e070063          	beqz	a4,800112b4 <bme680_set_regs+0x22c>
		if (reg_addr > 0x7f)
800111d8:	0807c713          	xori	a4,a5,128
		if (mem_page != dev->mem_page) {
800111dc:	0084c683          	lbu	a3,8(s1)
		if (reg_addr > 0x7f)
800111e0:	00775713          	srli	a4,a4,0x7
800111e4:	00471713          	slli	a4,a4,0x4
		rslt = BME680_OK;
800111e8:	00000513          	li	a0,0
		if (mem_page != dev->mem_page) {
800111ec:	06e68063          	beq	a3,a4,8001124c <bme680_set_regs+0x1c4>
			dev->com_rslt = dev->read(dev->dev_id, BME680_MEM_PAGE_ADDR | BME680_SPI_RD_MSK, &reg, 1);
800111f0:	0014c503          	lbu	a0,1(s1)
			dev->mem_page = mem_page;
800111f4:	00e48423          	sb	a4,8(s1)
			dev->com_rslt = dev->read(dev->dev_id, BME680_MEM_PAGE_ADDR | BME680_SPI_RD_MSK, &reg, 1);
800111f8:	00100693          	li	a3,1
800111fc:	00710613          	addi	a2,sp,7
80011200:	0f300593          	li	a1,243
80011204:	000800e7          	jalr	a6
80011208:	04a48c23          	sb	a0,88(s1)
			if (dev->com_rslt != 0)
8001120c:	0a051c63          	bnez	a0,800112c4 <bme680_set_regs+0x23c>
				reg = reg & (~BME680_MEM_PAGE_MSK);
80011210:	00714783          	lbu	a5,7(sp)
				reg = reg | (dev->mem_page & BME680_MEM_PAGE_MSK);
80011214:	0084c703          	lbu	a4,8(s1)
				dev->com_rslt = dev->write(dev->dev_id, BME680_MEM_PAGE_ADDR & BME680_SPI_WR_MSK,
80011218:	0504a803          	lw	a6,80(s1)
8001121c:	0014c503          	lbu	a0,1(s1)
				reg = reg | (dev->mem_page & BME680_MEM_PAGE_MSK);
80011220:	01077713          	andi	a4,a4,16
				reg = reg & (~BME680_MEM_PAGE_MSK);
80011224:	fef7f793          	andi	a5,a5,-17
				reg = reg | (dev->mem_page & BME680_MEM_PAGE_MSK);
80011228:	00e7e7b3          	or	a5,a5,a4
8001122c:	00f103a3          	sb	a5,7(sp)
				dev->com_rslt = dev->write(dev->dev_id, BME680_MEM_PAGE_ADDR & BME680_SPI_WR_MSK,
80011230:	00100693          	li	a3,1
80011234:	00710613          	addi	a2,sp,7
80011238:	07300593          	li	a1,115
8001123c:	000800e7          	jalr	a6
80011240:	04a48c23          	sb	a0,88(s1)
					tmp_buff[(2 * index)] = reg_addr[index] & BME680_SPI_WR_MSK;
80011244:	000ac783          	lbu	a5,0(s5)
				if (dev->com_rslt != 0)
80011248:	08051463          	bnez	a0,800112d0 <bme680_set_regs+0x248>
				tmp_buff[(2 * index) + 1] = reg_data[index];
8001124c:	00898733          	add	a4,s3,s0
					tmp_buff[(2 * index)] = reg_addr[index] & BME680_SPI_WR_MSK;
80011250:	fff74683          	lbu	a3,-1(a4)
80011254:	07f7f793          	andi	a5,a5,127
			for (index = 0; index < len; index++) {
80011258:	01041713          	slli	a4,s0,0x10
					tmp_buff[(2 * index)] = reg_addr[index] & BME680_SPI_WR_MSK;
8001125c:	00869693          	slli	a3,a3,0x8
80011260:	00d7e7b3          	or	a5,a5,a3
			for (index = 0; index < len; index++) {
80011264:	01075713          	srli	a4,a4,0x10
					tmp_buff[(2 * index)] = reg_addr[index] & BME680_SPI_WR_MSK;
80011268:	00fb1023          	sh	a5,0(s6)
			for (index = 0; index < len; index++) {
8001126c:	f32774e3          	bgeu	a4,s2,80011194 <bme680_set_regs+0x10c>
80011270:	00140413          	addi	s0,s0,1
80011274:	001a8a93          	addi	s5,s5,1
80011278:	002b0b13          	addi	s6,s6,2
8001127c:	ea9ff06f          	j	80011124 <bme680_set_regs+0x9c>
				dev->com_rslt = dev->write(dev->dev_id, tmp_buff[0], &tmp_buff[1], (2 * len) - 1);
80011280:	00191693          	slli	a3,s2,0x1
80011284:	0504a783          	lw	a5,80(s1)
80011288:	00814583          	lbu	a1,8(sp)
8001128c:	0014c503          	lbu	a0,1(s1)
80011290:	fff68693          	addi	a3,a3,-1
80011294:	01069693          	slli	a3,a3,0x10
80011298:	0106d693          	srli	a3,a3,0x10
8001129c:	00910613          	addi	a2,sp,9
800112a0:	000780e7          	jalr	a5
800112a4:	04a48c23          	sb	a0,88(s1)
				if (dev->com_rslt != 0)
800112a8:	ee0508e3          	beqz	a0,80011198 <bme680_set_regs+0x110>
					rslt = BME680_E_COM_FAIL;
800112ac:	ffe00513          	li	a0,-2
800112b0:	ee9ff06f          	j	80011198 <bme680_set_regs+0x110>
		rslt = BME680_E_NULL_PTR;
800112b4:	fff00513          	li	a0,-1
800112b8:	f95ff06f          	j	8001124c <bme680_set_regs+0x1c4>
800112bc:	fff00513          	li	a0,-1
800112c0:	ed9ff06f          	j	80011198 <bme680_set_regs+0x110>
					tmp_buff[(2 * index)] = reg_addr[index] & BME680_SPI_WR_MSK;
800112c4:	000ac783          	lbu	a5,0(s5)
				rslt = BME680_E_COM_FAIL;
800112c8:	ffe00513          	li	a0,-2
800112cc:	f81ff06f          	j	8001124c <bme680_set_regs+0x1c4>
					rslt = BME680_E_COM_FAIL;
800112d0:	ffe00513          	li	a0,-2
800112d4:	f79ff06f          	j	8001124c <bme680_set_regs+0x1c4>
			rslt = BME680_E_INVALID_LENGTH;
800112d8:	ffc00513          	li	a0,-4
	return rslt;
800112dc:	ebdff06f          	j	80011198 <bme680_set_regs+0x110>

800112e0 <bme680_soft_reset>:
{
800112e0:	fc010113          	addi	sp,sp,-64
800112e4:	02112e23          	sw	ra,60(sp)
800112e8:	02812c23          	sw	s0,56(sp)
800112ec:	02912a23          	sw	s1,52(sp)
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
800112f0:	1a050863          	beqz	a0,800114a0 <bme680_soft_reset+0x1c0>
800112f4:	04c52703          	lw	a4,76(a0)
800112f8:	00050413          	mv	s0,a0
800112fc:	1a070263          	beqz	a4,800114a0 <bme680_soft_reset+0x1c0>
80011300:	05052783          	lw	a5,80(a0)
80011304:	18078e63          	beqz	a5,800114a0 <bme680_soft_reset+0x1c0>
80011308:	05452683          	lw	a3,84(a0)
8001130c:	18068a63          	beqz	a3,800114a0 <bme680_soft_reset+0x1c0>
		if (dev->intf == BME680_SPI_INTF)
80011310:	00452683          	lw	a3,4(a0)
80011314:	10068063          	beqz	a3,80011414 <bme680_soft_reset+0x134>
	uint8_t tmp_buff[BME680_TMP_BUFFER_LENGTH] = { 0 };
80011318:	00012423          	sw	zero,8(sp)
8001131c:	00012623          	sw	zero,12(sp)
80011320:	00012823          	sw	zero,16(sp)
80011324:	00012a23          	sw	zero,20(sp)
80011328:	00012c23          	sw	zero,24(sp)
8001132c:	00012e23          	sw	zero,28(sp)
80011330:	02012023          	sw	zero,32(sp)
80011334:	02012223          	sw	zero,36(sp)
80011338:	02012423          	sw	zero,40(sp)
8001133c:	02012623          	sw	zero,44(sp)
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80011340:	00078713          	mv	a4,a5
				if (dev->intf == BME680_SPI_INTF) {
80011344:	08068663          	beqz	a3,800113d0 <bme680_soft_reset+0xf0>
					tmp_buff[(2 * index)] = reg_addr[index];
80011348:	ffffb7b7          	lui	a5,0xffffb
8001134c:	6e078793          	addi	a5,a5,1760 # ffffb6e0 <__clzsi2+0x7ffe2a4c>
80011350:	00f11423          	sh	a5,8(sp)
80011354:	0e000593          	li	a1,224
				dev->com_rslt = dev->write(dev->dev_id, tmp_buff[0], &tmp_buff[1], (2 * len) - 1);
80011358:	00144503          	lbu	a0,1(s0)
8001135c:	00100693          	li	a3,1
80011360:	00910613          	addi	a2,sp,9
80011364:	000700e7          	jalr	a4
80011368:	00050493          	mv	s1,a0
8001136c:	04a40c23          	sb	a0,88(s0)
			dev->delay_ms(BME680_RESET_PERIOD);
80011370:	05442783          	lw	a5,84(s0)
80011374:	00a00513          	li	a0,10
				if (dev->com_rslt != 0)
80011378:	10049a63          	bnez	s1,8001148c <bme680_soft_reset+0x1ac>
			dev->delay_ms(BME680_RESET_PERIOD);
8001137c:	000780e7          	jalr	a5
				if (dev->intf == BME680_SPI_INTF)
80011380:	00442783          	lw	a5,4(s0)
80011384:	06079c63          	bnez	a5,800113fc <bme680_soft_reset+0x11c>
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80011388:	04c42783          	lw	a5,76(s0)
8001138c:	10078a63          	beqz	a5,800114a0 <bme680_soft_reset+0x1c0>
80011390:	05042703          	lw	a4,80(s0)
80011394:	10070663          	beqz	a4,800114a0 <bme680_soft_reset+0x1c0>
80011398:	05442703          	lw	a4,84(s0)
8001139c:	10070263          	beqz	a4,800114a0 <bme680_soft_reset+0x1c0>
		dev->com_rslt = dev->read(dev->dev_id, BME680_MEM_PAGE_ADDR | BME680_SPI_RD_MSK, &reg, 1);
800113a0:	00144503          	lbu	a0,1(s0)
800113a4:	00100693          	li	a3,1
800113a8:	00810613          	addi	a2,sp,8
800113ac:	0f300593          	li	a1,243
800113b0:	000780e7          	jalr	a5
800113b4:	04a40c23          	sb	a0,88(s0)
800113b8:	00050493          	mv	s1,a0
		if (dev->com_rslt != 0)
800113bc:	0c051e63          	bnez	a0,80011498 <bme680_soft_reset+0x1b8>
			dev->mem_page = reg & BME680_MEM_PAGE_MSK;
800113c0:	00814783          	lbu	a5,8(sp)
800113c4:	0107f793          	andi	a5,a5,16
800113c8:	00f40423          	sb	a5,8(s0)
800113cc:	0300006f          	j	800113fc <bme680_soft_reset+0x11c>
					rslt = set_mem_page(reg_addr[index], dev);
800113d0:	00040593          	mv	a1,s0
800113d4:	0e000513          	li	a0,224
800113d8:	b31ff0ef          	jal	ra,80010f08 <set_mem_page>
					tmp_buff[(2 * index)] = reg_addr[index] & BME680_SPI_WR_MSK;
800113dc:	ffffb7b7          	lui	a5,0xffffb
800113e0:	66078793          	addi	a5,a5,1632 # ffffb660 <__clzsi2+0x7ffe29cc>
800113e4:	00f11423          	sh	a5,8(sp)
					rslt = set_mem_page(reg_addr[index], dev);
800113e8:	00050493          	mv	s1,a0
			if (rslt == BME680_OK) {
800113ec:	08050a63          	beqz	a0,80011480 <bme680_soft_reset+0x1a0>
			dev->delay_ms(BME680_RESET_PERIOD);
800113f0:	05442783          	lw	a5,84(s0)
800113f4:	00a00513          	li	a0,10
800113f8:	000780e7          	jalr	a5
}
800113fc:	03c12083          	lw	ra,60(sp)
80011400:	03812403          	lw	s0,56(sp)
80011404:	00048513          	mv	a0,s1
80011408:	03412483          	lw	s1,52(sp)
8001140c:	04010113          	addi	sp,sp,64
80011410:	00008067          	ret
		dev->com_rslt = dev->read(dev->dev_id, BME680_MEM_PAGE_ADDR | BME680_SPI_RD_MSK, &reg, 1);
80011414:	00154503          	lbu	a0,1(a0)
80011418:	00100693          	li	a3,1
8001141c:	00810613          	addi	a2,sp,8
80011420:	0f300593          	li	a1,243
80011424:	000700e7          	jalr	a4
80011428:	04a40c23          	sb	a0,88(s0)
		if (dev->com_rslt != 0)
8001142c:	06051663          	bnez	a0,80011498 <bme680_soft_reset+0x1b8>
			dev->mem_page = reg & BME680_MEM_PAGE_MSK;
80011430:	00814783          	lbu	a5,8(sp)
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80011434:	04c42703          	lw	a4,76(s0)
	uint8_t tmp_buff[BME680_TMP_BUFFER_LENGTH] = { 0 };
80011438:	00012423          	sw	zero,8(sp)
			dev->mem_page = reg & BME680_MEM_PAGE_MSK;
8001143c:	0107f793          	andi	a5,a5,16
80011440:	00f40423          	sb	a5,8(s0)
	uint8_t tmp_buff[BME680_TMP_BUFFER_LENGTH] = { 0 };
80011444:	00012623          	sw	zero,12(sp)
80011448:	00012823          	sw	zero,16(sp)
8001144c:	00012a23          	sw	zero,20(sp)
80011450:	00012c23          	sw	zero,24(sp)
80011454:	00012e23          	sw	zero,28(sp)
80011458:	02012023          	sw	zero,32(sp)
8001145c:	02012223          	sw	zero,36(sp)
80011460:	02012423          	sw	zero,40(sp)
80011464:	02012623          	sw	zero,44(sp)
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80011468:	05442683          	lw	a3,84(s0)
8001146c:	02071e63          	bnez	a4,800114a8 <bme680_soft_reset+0x1c8>
			dev->delay_ms(BME680_RESET_PERIOD);
80011470:	00a00513          	li	a0,10
80011474:	000680e7          	jalr	a3
		rslt = BME680_E_NULL_PTR;
80011478:	fff00493          	li	s1,-1
8001147c:	f81ff06f          	j	800113fc <bme680_soft_reset+0x11c>
				dev->com_rslt = dev->write(dev->dev_id, tmp_buff[0], &tmp_buff[1], (2 * len) - 1);
80011480:	05042703          	lw	a4,80(s0)
80011484:	06000593          	li	a1,96
80011488:	ed1ff06f          	j	80011358 <bme680_soft_reset+0x78>
					rslt = BME680_E_COM_FAIL;
8001148c:	ffe00493          	li	s1,-2
			dev->delay_ms(BME680_RESET_PERIOD);
80011490:	000780e7          	jalr	a5
			if (rslt == BME680_OK) {
80011494:	f69ff06f          	j	800113fc <bme680_soft_reset+0x11c>
			rslt = BME680_E_COM_FAIL;
80011498:	ffe00493          	li	s1,-2
	return rslt;
8001149c:	f61ff06f          	j	800113fc <bme680_soft_reset+0x11c>
		rslt = BME680_E_NULL_PTR;
800114a0:	fff00493          	li	s1,-1
	return rslt;
800114a4:	f59ff06f          	j	800113fc <bme680_soft_reset+0x11c>
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
800114a8:	05042703          	lw	a4,80(s0)
800114ac:	fc0702e3          	beqz	a4,80011470 <bme680_soft_reset+0x190>
800114b0:	fc0680e3          	beqz	a3,80011470 <bme680_soft_reset+0x190>
800114b4:	00442683          	lw	a3,4(s0)
800114b8:	e8dff06f          	j	80011344 <bme680_soft_reset+0x64>

800114bc <bme680_init>:
{
800114bc:	fa010113          	addi	sp,sp,-96
800114c0:	04112e23          	sw	ra,92(sp)
800114c4:	04812c23          	sw	s0,88(sp)
800114c8:	04912a23          	sw	s1,84(sp)
800114cc:	05212823          	sw	s2,80(sp)
800114d0:	05312623          	sw	s3,76(sp)
800114d4:	05412423          	sw	s4,72(sp)
800114d8:	05512223          	sw	s5,68(sp)
800114dc:	05612023          	sw	s6,64(sp)
800114e0:	03712e23          	sw	s7,60(sp)
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
800114e4:	36050463          	beqz	a0,8001184c <bme680_init+0x390>
800114e8:	04c52783          	lw	a5,76(a0)
800114ec:	00050413          	mv	s0,a0
800114f0:	34078e63          	beqz	a5,8001184c <bme680_init+0x390>
800114f4:	05052783          	lw	a5,80(a0)
800114f8:	34078a63          	beqz	a5,8001184c <bme680_init+0x390>
800114fc:	05452783          	lw	a5,84(a0)
80011500:	34078663          	beqz	a5,8001184c <bme680_init+0x390>
		rslt = bme680_soft_reset(dev);
80011504:	dddff0ef          	jal	ra,800112e0 <bme680_soft_reset>
80011508:	00050493          	mv	s1,a0
		if (rslt == BME680_OK) {
8001150c:	02050a63          	beqz	a0,80011540 <bme680_init+0x84>
}
80011510:	05c12083          	lw	ra,92(sp)
80011514:	05812403          	lw	s0,88(sp)
80011518:	05012903          	lw	s2,80(sp)
8001151c:	04c12983          	lw	s3,76(sp)
80011520:	04812a03          	lw	s4,72(sp)
80011524:	04412a83          	lw	s5,68(sp)
80011528:	04012b03          	lw	s6,64(sp)
8001152c:	03c12b83          	lw	s7,60(sp)
80011530:	00048513          	mv	a0,s1
80011534:	05412483          	lw	s1,84(sp)
80011538:	06010113          	addi	sp,sp,96
8001153c:	00008067          	ret
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80011540:	04c42783          	lw	a5,76(s0)
80011544:	30078463          	beqz	a5,8001184c <bme680_init+0x390>
80011548:	05042703          	lw	a4,80(s0)
8001154c:	30070063          	beqz	a4,8001184c <bme680_init+0x390>
80011550:	05442703          	lw	a4,84(s0)
80011554:	2e070c63          	beqz	a4,8001184c <bme680_init+0x390>
		if (dev->intf == BME680_SPI_INTF) {
80011558:	00442703          	lw	a4,4(s0)
8001155c:	20070a63          	beqz	a4,80011770 <bme680_init+0x2b4>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80011560:	00144503          	lbu	a0,1(s0)
80011564:	00100693          	li	a3,1
80011568:	00040613          	mv	a2,s0
8001156c:	0d000593          	li	a1,208
80011570:	000780e7          	jalr	a5
80011574:	04a40c23          	sb	a0,88(s0)
		if (dev->com_rslt != 0)
80011578:	22051663          	bnez	a0,800117a4 <bme680_init+0x2e8>
				if (dev->chip_id == BME680_CHIP_ID) {
8001157c:	00044703          	lbu	a4,0(s0)
80011580:	06100793          	li	a5,97
					rslt = BME680_E_DEV_NOT_FOUND;
80011584:	ffd00493          	li	s1,-3
				if (dev->chip_id == BME680_CHIP_ID) {
80011588:	f8f714e3          	bne	a4,a5,80011510 <bme680_init+0x54>
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
8001158c:	04c42783          	lw	a5,76(s0)
	uint8_t coeff_array[BME680_COEFF_SIZE] = { 0 };
80011590:	00012223          	sw	zero,4(sp)
80011594:	00012423          	sw	zero,8(sp)
80011598:	00012623          	sw	zero,12(sp)
8001159c:	00012823          	sw	zero,16(sp)
800115a0:	00012a23          	sw	zero,20(sp)
800115a4:	00012c23          	sw	zero,24(sp)
800115a8:	00012e23          	sw	zero,28(sp)
800115ac:	02012023          	sw	zero,32(sp)
800115b0:	02012223          	sw	zero,36(sp)
800115b4:	02012423          	sw	zero,40(sp)
800115b8:	02010623          	sb	zero,44(sp)
	uint8_t temp_var = 0; /* Temporary variable */
800115bc:	000101a3          	sb	zero,3(sp)
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
800115c0:	28078663          	beqz	a5,8001184c <bme680_init+0x390>
800115c4:	05042703          	lw	a4,80(s0)
800115c8:	28070263          	beqz	a4,8001184c <bme680_init+0x390>
800115cc:	05442703          	lw	a4,84(s0)
800115d0:	26070e63          	beqz	a4,8001184c <bme680_init+0x390>
		if (dev->intf == BME680_SPI_INTF) {
800115d4:	00442703          	lw	a4,4(s0)
800115d8:	26070e63          	beqz	a4,80011854 <bme680_init+0x398>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
800115dc:	00144503          	lbu	a0,1(s0)
800115e0:	01900693          	li	a3,25
800115e4:	00410613          	addi	a2,sp,4
800115e8:	08900593          	li	a1,137
800115ec:	000780e7          	jalr	a5
800115f0:	04a40c23          	sb	a0,88(s0)
800115f4:	00050493          	mv	s1,a0
		if (dev->com_rslt != 0)
800115f8:	1a050a63          	beqz	a0,800117ac <bme680_init+0x2f0>
			rslt = BME680_E_COM_FAIL;
800115fc:	ffe00493          	li	s1,-2
		dev->calib.par_p6 = (int8_t) (coeff_array[BME680_P6_REG]);
80011600:	01414783          	lbu	a5,20(sp)
80011604:	01314303          	lbu	t1,19(sp)
		dev->calib.par_p8 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P8_MSB_REG],
80011608:	01814683          	lbu	a3,24(sp)
8001160c:	01a14703          	lbu	a4,26(sp)
80011610:	01914e03          	lbu	t3,25(sp)
80011614:	01714b03          	lbu	s6,23(sp)
		dev->calib.par_t1 = (uint16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_T1_MSB_REG],
80011618:	02614b83          	lbu	s7,38(sp)
		dev->calib.par_t2 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_T2_MSB_REG],
8001161c:	00614883          	lbu	a7,6(sp)
		dev->calib.par_p1 = (uint16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P1_MSB_REG],
80011620:	00a14803          	lbu	a6,10(sp)
		dev->calib.par_p2 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P2_MSB_REG],
80011624:	00c14503          	lbu	a0,12(sp)
		dev->calib.par_p4 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P4_MSB_REG],
80011628:	01014583          	lbu	a1,16(sp)
		dev->calib.par_p5 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P5_MSB_REG],
8001162c:	01214603          	lbu	a2,18(sp)
		dev->calib.par_p6 = (int8_t) (coeff_array[BME680_P6_REG]);
80011630:	00879793          	slli	a5,a5,0x8
80011634:	0067e7b3          	or	a5,a5,t1
		dev->calib.par_t1 = (uint16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_T1_MSB_REG],
80011638:	02514a83          	lbu	s5,37(sp)
		dev->calib.par_t2 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_T2_MSB_REG],
8001163c:	00514a03          	lbu	s4,5(sp)
		dev->calib.par_p1 = (uint16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P1_MSB_REG],
80011640:	00914983          	lbu	s3,9(sp)
		dev->calib.par_p2 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P2_MSB_REG],
80011644:	00b14903          	lbu	s2,11(sp)
		dev->calib.par_p4 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P4_MSB_REG],
80011648:	00f14383          	lbu	t2,15(sp)
		dev->calib.par_p5 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P5_MSB_REG],
8001164c:	01114283          	lbu	t0,17(sp)
		dev->calib.par_p8 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P8_MSB_REG],
80011650:	00869693          	slli	a3,a3,0x8
80011654:	00871713          	slli	a4,a4,0x8
80011658:	00010337          	lui	t1,0x10
		dev->calib.par_p3 = (int8_t) coeff_array[BME680_P3_REG];
8001165c:	00d14f03          	lbu	t5,13(sp)
		dev->calib.par_p10 = (uint8_t) (coeff_array[BME680_P10_REG]);
80011660:	01b14e83          	lbu	t4,27(sp)
		dev->calib.par_t3 = (int8_t) (coeff_array[BME680_T3_REG]);
80011664:	00714f83          	lbu	t6,7(sp)
		dev->calib.par_p8 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P8_MSB_REG],
80011668:	fff30313          	addi	t1,t1,-1 # ffff <font6x8-0x7fff002d>
8001166c:	0166eb33          	or	s6,a3,s6
80011670:	01c76733          	or	a4,a4,t3
		dev->calib.par_p6 = (int8_t) (coeff_array[BME680_P6_REG]);
80011674:	00879693          	slli	a3,a5,0x8
80011678:	0087de13          	srli	t3,a5,0x8
		dev->calib.par_p8 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P8_MSB_REG],
8001167c:	01071713          	slli	a4,a4,0x10
		dev->calib.par_t1 = (uint16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_T1_MSB_REG],
80011680:	008b9b93          	slli	s7,s7,0x8
		dev->calib.par_t2 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_T2_MSB_REG],
80011684:	00889893          	slli	a7,a7,0x8
		dev->calib.par_p1 = (uint16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P1_MSB_REG],
80011688:	00881813          	slli	a6,a6,0x8
		dev->calib.par_p2 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P2_MSB_REG],
8001168c:	00851513          	slli	a0,a0,0x8
		dev->calib.par_p4 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P4_MSB_REG],
80011690:	00859593          	slli	a1,a1,0x8
		dev->calib.par_p5 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P5_MSB_REG],
80011694:	00861613          	slli	a2,a2,0x8
		dev->calib.par_p8 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P8_MSB_REG],
80011698:	006b77b3          	and	a5,s6,t1
		dev->calib.par_p6 = (int8_t) (coeff_array[BME680_P6_REG]);
8001169c:	01c6e6b3          	or	a3,a3,t3
		dev->calib.par_p8 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P8_MSB_REG],
800116a0:	00e7e7b3          	or	a5,a5,a4
		dev->calib.par_t2 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_T2_MSB_REG],
800116a4:	0148e8b3          	or	a7,a7,s4
		dev->calib.par_p1 = (uint16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P1_MSB_REG],
800116a8:	01386833          	or	a6,a6,s3
		dev->calib.par_p2 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P2_MSB_REG],
800116ac:	01256533          	or	a0,a0,s2
		dev->calib.par_p4 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P4_MSB_REG],
800116b0:	0075e5b3          	or	a1,a1,t2
		dev->calib.par_p5 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P5_MSB_REG],
800116b4:	00566633          	or	a2,a2,t0
		dev->calib.par_t1 = (uint16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_T1_MSB_REG],
800116b8:	015beab3          	or	s5,s7,s5
		dev->calib.par_t2 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_T2_MSB_REG],
800116bc:	01141e23          	sh	a7,28(s0)
		dev->calib.par_p1 = (uint16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P1_MSB_REG],
800116c0:	03041023          	sh	a6,32(s0)
		dev->calib.par_p2 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P2_MSB_REG],
800116c4:	02a41123          	sh	a0,34(s0)
		dev->calib.par_p3 = (int8_t) coeff_array[BME680_P3_REG];
800116c8:	03e40223          	sb	t5,36(s0)
		dev->calib.par_p4 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P4_MSB_REG],
800116cc:	02b41323          	sh	a1,38(s0)
		dev->calib.par_p6 = (int8_t) (coeff_array[BME680_P6_REG]);
800116d0:	02d41523          	sh	a3,42(s0)
		dev->calib.par_p8 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P8_MSB_REG],
800116d4:	02f42623          	sw	a5,44(s0)
		dev->calib.par_p10 = (uint8_t) (coeff_array[BME680_P10_REG]);
800116d8:	03d40823          	sb	t4,48(s0)
		dev->calib.par_t1 = (uint16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_T1_MSB_REG],
800116dc:	01541d23          	sh	s5,26(s0)
		dev->calib.par_t3 = (int8_t) (coeff_array[BME680_T3_REG]);
800116e0:	01f40f23          	sb	t6,30(s0)
		dev->calib.par_p5 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_P5_MSB_REG],
800116e4:	02c41423          	sh	a2,40(s0)
			| (coeff_array[BME680_H1_LSB_REG] & BME680_BIT_H1_DATA_MSK));
800116e8:	01e14603          	lbu	a2,30(sp)
		dev->calib.par_h1 = (uint16_t) (((uint16_t) coeff_array[BME680_H1_MSB_REG] << BME680_HUM_REG_SHIFT_VAL)
800116ec:	01f14783          	lbu	a5,31(sp)
		dev->calib.par_h2 = (uint16_t) (((uint16_t) coeff_array[BME680_H2_MSB_REG] << BME680_HUM_REG_SHIFT_VAL)
800116f0:	01d14703          	lbu	a4,29(sp)
		dev->calib.par_h7 = (int8_t) coeff_array[BME680_H7_REG];
800116f4:	02914503          	lbu	a0,41(sp)
		dev->calib.par_gh2 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_GH2_MSB_REG],
800116f8:	02814683          	lbu	a3,40(sp)
			| (coeff_array[BME680_H1_LSB_REG] & BME680_BIT_H1_DATA_MSK));
800116fc:	00f67f13          	andi	t5,a2,15
		dev->calib.par_h7 = (int8_t) coeff_array[BME680_H7_REG];
80011700:	02414583          	lbu	a1,36(sp)
			| ((coeff_array[BME680_H2_LSB_REG]) >> BME680_HUM_REG_SHIFT_VAL));
80011704:	00465613          	srli	a2,a2,0x4
		dev->calib.par_gh2 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_GH2_MSB_REG],
80011708:	02714e83          	lbu	t4,39(sp)
		dev->calib.par_h1 = (uint16_t) (((uint16_t) coeff_array[BME680_H1_MSB_REG] << BME680_HUM_REG_SHIFT_VAL)
8001170c:	00479793          	slli	a5,a5,0x4
		dev->calib.par_h2 = (uint16_t) (((uint16_t) coeff_array[BME680_H2_MSB_REG] << BME680_HUM_REG_SHIFT_VAL)
80011710:	00471713          	slli	a4,a4,0x4
		dev->calib.par_h1 = (uint16_t) (((uint16_t) coeff_array[BME680_H1_MSB_REG] << BME680_HUM_REG_SHIFT_VAL)
80011714:	00c76733          	or	a4,a4,a2
		dev->calib.par_h3 = (int8_t) coeff_array[BME680_H3_REG];
80011718:	02015e03          	lhu	t3,32(sp)
		dev->calib.par_h5 = (int8_t) coeff_array[BME680_H5_REG];
8001171c:	02215883          	lhu	a7,34(sp)
		dev->calib.par_gh3 = (int8_t) coeff_array[BME680_GH3_REG];
80011720:	02a14803          	lbu	a6,42(sp)
		dev->calib.par_h1 = (uint16_t) (((uint16_t) coeff_array[BME680_H1_MSB_REG] << BME680_HUM_REG_SHIFT_VAL)
80011724:	01e7e7b3          	or	a5,a5,t5
80011728:	01071713          	slli	a4,a4,0x10
8001172c:	0067f7b3          	and	a5,a5,t1
		dev->calib.par_h7 = (int8_t) coeff_array[BME680_H7_REG];
80011730:	00851613          	slli	a2,a0,0x8
		dev->calib.par_gh2 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_GH2_MSB_REG],
80011734:	00869693          	slli	a3,a3,0x8
		dev->calib.par_h1 = (uint16_t) (((uint16_t) coeff_array[BME680_H1_MSB_REG] << BME680_HUM_REG_SHIFT_VAL)
80011738:	00e7e7b3          	or	a5,a5,a4
		dev->calib.par_h7 = (int8_t) coeff_array[BME680_H7_REG];
8001173c:	00c5e633          	or	a2,a1,a2
		dev->calib.par_gh2 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_GH2_MSB_REG],
80011740:	01d6e733          	or	a4,a3,t4
		dev->calib.par_h1 = (uint16_t) (((uint16_t) coeff_array[BME680_H1_MSB_REG] << BME680_HUM_REG_SHIFT_VAL)
80011744:	00f42623          	sw	a5,12(s0)
		dev->calib.par_h3 = (int8_t) coeff_array[BME680_H3_REG];
80011748:	01c41823          	sh	t3,16(s0)
		dev->calib.par_h5 = (int8_t) coeff_array[BME680_H5_REG];
8001174c:	01141923          	sh	a7,18(s0)
		dev->calib.par_h7 = (int8_t) coeff_array[BME680_H7_REG];
80011750:	00c41a23          	sh	a2,20(s0)
		dev->calib.par_gh2 = (int16_t) (BME680_CONCAT_BYTES(coeff_array[BME680_GH2_MSB_REG],
80011754:	00e41b23          	sh	a4,22(s0)
		dev->calib.par_gh3 = (int8_t) coeff_array[BME680_GH3_REG];
80011758:	01040c23          	sb	a6,24(s0)
		if (rslt == BME680_OK) {
8001175c:	08048863          	beqz	s1,800117ec <bme680_init+0x330>
		dev->calib.range_sw_err = ((int8_t) temp_var & (int8_t) BME680_RSERROR_MSK) / 16;
80011760:	00310783          	lb	a5,3(sp)
80011764:	4047d793          	srai	a5,a5,0x4
80011768:	02f40d23          	sb	a5,58(s0)
8001176c:	da5ff06f          	j	80011510 <bme680_init+0x54>
			rslt = set_mem_page(reg_addr, dev);
80011770:	00040593          	mv	a1,s0
80011774:	0d000513          	li	a0,208
80011778:	f90ff0ef          	jal	ra,80010f08 <set_mem_page>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
8001177c:	04c42783          	lw	a5,76(s0)
			rslt = set_mem_page(reg_addr, dev);
80011780:	00050493          	mv	s1,a0
			if (rslt == BME680_OK)
80011784:	dc050ee3          	beqz	a0,80011560 <bme680_init+0xa4>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80011788:	00144503          	lbu	a0,1(s0)
8001178c:	00100693          	li	a3,1
80011790:	00040613          	mv	a2,s0
80011794:	0d000593          	li	a1,208
80011798:	000780e7          	jalr	a5
8001179c:	04a40c23          	sb	a0,88(s0)
		if (dev->com_rslt != 0)
800117a0:	d60508e3          	beqz	a0,80011510 <bme680_init+0x54>
			rslt = BME680_E_COM_FAIL;
800117a4:	ffe00493          	li	s1,-2
800117a8:	d69ff06f          	j	80011510 <bme680_init+0x54>
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
800117ac:	04c42783          	lw	a5,76(s0)
800117b0:	22078063          	beqz	a5,800119d0 <bme680_init+0x514>
800117b4:	05042703          	lw	a4,80(s0)
800117b8:	20070c63          	beqz	a4,800119d0 <bme680_init+0x514>
800117bc:	05442703          	lw	a4,84(s0)
800117c0:	20070863          	beqz	a4,800119d0 <bme680_init+0x514>
		if (dev->intf == BME680_SPI_INTF) {
800117c4:	00442703          	lw	a4,4(s0)
800117c8:	10070a63          	beqz	a4,800118dc <bme680_init+0x420>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
800117cc:	00144503          	lbu	a0,1(s0)
800117d0:	01000693          	li	a3,16
800117d4:	01d10613          	addi	a2,sp,29
800117d8:	0e100593          	li	a1,225
800117dc:	000780e7          	jalr	a5
800117e0:	04a40c23          	sb	a0,88(s0)
		if (dev->com_rslt != 0)
800117e4:	e0050ee3          	beqz	a0,80011600 <bme680_init+0x144>
800117e8:	e15ff06f          	j	800115fc <bme680_init+0x140>
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
800117ec:	04c42783          	lw	a5,76(s0)
800117f0:	1e078463          	beqz	a5,800119d8 <bme680_init+0x51c>
800117f4:	05042703          	lw	a4,80(s0)
800117f8:	1e070063          	beqz	a4,800119d8 <bme680_init+0x51c>
800117fc:	05442703          	lw	a4,84(s0)
80011800:	1c070c63          	beqz	a4,800119d8 <bme680_init+0x51c>
		if (dev->intf == BME680_SPI_INTF) {
80011804:	00442703          	lw	a4,4(s0)
80011808:	00200913          	li	s2,2
8001180c:	0e070463          	beqz	a4,800118f4 <bme680_init+0x438>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80011810:	00144503          	lbu	a0,1(s0)
80011814:	00100693          	li	a3,1
80011818:	00310613          	addi	a2,sp,3
8001181c:	00090593          	mv	a1,s2
80011820:	000780e7          	jalr	a5
80011824:	04a40c23          	sb	a0,88(s0)
		if (dev->com_rslt != 0)
80011828:	1c051463          	bnez	a0,800119f0 <bme680_init+0x534>
			dev->calib.res_heat_range = ((temp_var & BME680_RHRANGE_MSK) / 16);
8001182c:	00314783          	lbu	a5,3(sp)
80011830:	0307f713          	andi	a4,a5,48
80011834:	40475713          	srai	a4,a4,0x4
80011838:	02e40c23          	sb	a4,56(s0)
			if (rslt == BME680_OK) {
8001183c:	04048863          	beqz	s1,8001188c <bme680_init+0x3d0>
				dev->calib.res_heat_val = (int8_t) temp_var;
80011840:	01879793          	slli	a5,a5,0x18
80011844:	4187d793          	srai	a5,a5,0x18
80011848:	f1dff06f          	j	80011764 <bme680_init+0x2a8>
		rslt = BME680_E_NULL_PTR;
8001184c:	fff00493          	li	s1,-1
	return rslt;
80011850:	cc1ff06f          	j	80011510 <bme680_init+0x54>
			rslt = set_mem_page(reg_addr, dev);
80011854:	00040593          	mv	a1,s0
80011858:	08900513          	li	a0,137
8001185c:	eacff0ef          	jal	ra,80010f08 <set_mem_page>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80011860:	04c42783          	lw	a5,76(s0)
			rslt = set_mem_page(reg_addr, dev);
80011864:	00050493          	mv	s1,a0
			if (rslt == BME680_OK)
80011868:	d6050ae3          	beqz	a0,800115dc <bme680_init+0x120>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
8001186c:	00144503          	lbu	a0,1(s0)
80011870:	01900693          	li	a3,25
80011874:	00410613          	addi	a2,sp,4
80011878:	08900593          	li	a1,137
8001187c:	000780e7          	jalr	a5
80011880:	04a40c23          	sb	a0,88(s0)
		if (dev->com_rslt != 0)
80011884:	d6050ee3          	beqz	a0,80011600 <bme680_init+0x144>
80011888:	d75ff06f          	j	800115fc <bme680_init+0x140>
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
8001188c:	04c42703          	lw	a4,76(s0)
80011890:	16070663          	beqz	a4,800119fc <bme680_init+0x540>
80011894:	05042683          	lw	a3,80(s0)
80011898:	16068263          	beqz	a3,800119fc <bme680_init+0x540>
8001189c:	05442683          	lw	a3,84(s0)
800118a0:	14068e63          	beqz	a3,800119fc <bme680_init+0x540>
		if (dev->intf == BME680_SPI_INTF) {
800118a4:	00442783          	lw	a5,4(s0)
800118a8:	00000593          	li	a1,0
800118ac:	0a078e63          	beqz	a5,80011968 <bme680_init+0x4ac>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
800118b0:	00144503          	lbu	a0,1(s0)
800118b4:	00100693          	li	a3,1
800118b8:	00310613          	addi	a2,sp,3
800118bc:	000700e7          	jalr	a4
800118c0:	04a40c23          	sb	a0,88(s0)
800118c4:	00050493          	mv	s1,a0
		if (dev->com_rslt != 0)
800118c8:	04050663          	beqz	a0,80011914 <bme680_init+0x458>
				dev->calib.res_heat_val = (int8_t) temp_var;
800118cc:	00310783          	lb	a5,3(sp)
			rslt = BME680_E_COM_FAIL;
800118d0:	ffe00493          	li	s1,-2
				dev->calib.res_heat_val = (int8_t) temp_var;
800118d4:	02f40ca3          	sb	a5,57(s0)
				if (rslt == BME680_OK)
800118d8:	e8dff06f          	j	80011764 <bme680_init+0x2a8>
			rslt = set_mem_page(reg_addr, dev);
800118dc:	00040593          	mv	a1,s0
800118e0:	0e100513          	li	a0,225
800118e4:	e24ff0ef          	jal	ra,80010f08 <set_mem_page>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
800118e8:	04c42783          	lw	a5,76(s0)
			rslt = set_mem_page(reg_addr, dev);
800118ec:	00050493          	mv	s1,a0
			if (rslt == BME680_OK)
800118f0:	eddff06f          	j	800117cc <bme680_init+0x310>
			rslt = set_mem_page(reg_addr, dev);
800118f4:	00040593          	mv	a1,s0
800118f8:	00200513          	li	a0,2
800118fc:	e0cff0ef          	jal	ra,80010f08 <set_mem_page>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80011900:	04c42783          	lw	a5,76(s0)
			rslt = set_mem_page(reg_addr, dev);
80011904:	00050493          	mv	s1,a0
			if (rslt == BME680_OK)
80011908:	f00514e3          	bnez	a0,80011810 <bme680_init+0x354>
				reg_addr = reg_addr | BME680_SPI_RD_MSK;
8001190c:	08200913          	li	s2,130
80011910:	f01ff06f          	j	80011810 <bme680_init+0x354>
				dev->calib.res_heat_val = (int8_t) temp_var;
80011914:	00310783          	lb	a5,3(sp)
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80011918:	04c42703          	lw	a4,76(s0)
				dev->calib.res_heat_val = (int8_t) temp_var;
8001191c:	02f40ca3          	sb	a5,57(s0)
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80011920:	0e070863          	beqz	a4,80011a10 <bme680_init+0x554>
80011924:	05042683          	lw	a3,80(s0)
80011928:	0e068463          	beqz	a3,80011a10 <bme680_init+0x554>
8001192c:	05442683          	lw	a3,84(s0)
80011930:	0e068063          	beqz	a3,80011a10 <bme680_init+0x554>
		if (dev->intf == BME680_SPI_INTF) {
80011934:	00442783          	lw	a5,4(s0)
80011938:	00400913          	li	s2,4
8001193c:	06078a63          	beqz	a5,800119b0 <bme680_init+0x4f4>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80011940:	00144503          	lbu	a0,1(s0)
80011944:	00100693          	li	a3,1
80011948:	00310613          	addi	a2,sp,3
8001194c:	00090593          	mv	a1,s2
80011950:	000700e7          	jalr	a4
80011954:	04a40c23          	sb	a0,88(s0)
		dev->calib.range_sw_err = ((int8_t) temp_var & (int8_t) BME680_RSERROR_MSK) / 16;
80011958:	00310783          	lb	a5,3(sp)
		if (dev->com_rslt != 0)
8001195c:	e00504e3          	beqz	a0,80011764 <bme680_init+0x2a8>
			rslt = BME680_E_COM_FAIL;
80011960:	ffe00493          	li	s1,-2
80011964:	e01ff06f          	j	80011764 <bme680_init+0x2a8>
			rslt = set_mem_page(reg_addr, dev);
80011968:	00040593          	mv	a1,s0
8001196c:	d9cff0ef          	jal	ra,80010f08 <set_mem_page>
80011970:	00050493          	mv	s1,a0
			if (rslt == BME680_OK)
80011974:	00051863          	bnez	a0,80011984 <bme680_init+0x4c8>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80011978:	04c42703          	lw	a4,76(s0)
				reg_addr = reg_addr | BME680_SPI_RD_MSK;
8001197c:	08000593          	li	a1,128
80011980:	f31ff06f          	j	800118b0 <bme680_init+0x3f4>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80011984:	04c42783          	lw	a5,76(s0)
80011988:	00144503          	lbu	a0,1(s0)
8001198c:	00100693          	li	a3,1
80011990:	00310613          	addi	a2,sp,3
80011994:	00000593          	li	a1,0
80011998:	000780e7          	jalr	a5
8001199c:	04a40c23          	sb	a0,88(s0)
		if (dev->com_rslt != 0)
800119a0:	f20516e3          	bnez	a0,800118cc <bme680_init+0x410>
				dev->calib.res_heat_val = (int8_t) temp_var;
800119a4:	00310783          	lb	a5,3(sp)
800119a8:	02f40ca3          	sb	a5,57(s0)
				if (rslt == BME680_OK)
800119ac:	db9ff06f          	j	80011764 <bme680_init+0x2a8>
			rslt = set_mem_page(reg_addr, dev);
800119b0:	00040593          	mv	a1,s0
800119b4:	00400513          	li	a0,4
800119b8:	d50ff0ef          	jal	ra,80010f08 <set_mem_page>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
800119bc:	04c42703          	lw	a4,76(s0)
			rslt = set_mem_page(reg_addr, dev);
800119c0:	00050493          	mv	s1,a0
			if (rslt == BME680_OK)
800119c4:	f6051ee3          	bnez	a0,80011940 <bme680_init+0x484>
				reg_addr = reg_addr | BME680_SPI_RD_MSK;
800119c8:	08400913          	li	s2,132
800119cc:	f75ff06f          	j	80011940 <bme680_init+0x484>
		rslt = BME680_E_NULL_PTR;
800119d0:	fff00493          	li	s1,-1
800119d4:	c2dff06f          	j	80011600 <bme680_init+0x144>
			dev->calib.res_heat_range = ((temp_var & BME680_RHRANGE_MSK) / 16);
800119d8:	00314783          	lbu	a5,3(sp)
		rslt = BME680_E_NULL_PTR;
800119dc:	fff00493          	li	s1,-1
			dev->calib.res_heat_range = ((temp_var & BME680_RHRANGE_MSK) / 16);
800119e0:	0307f713          	andi	a4,a5,48
800119e4:	40475713          	srai	a4,a4,0x4
800119e8:	02e40c23          	sb	a4,56(s0)
800119ec:	e55ff06f          	j	80011840 <bme680_init+0x384>
800119f0:	00314783          	lbu	a5,3(sp)
			rslt = BME680_E_COM_FAIL;
800119f4:	ffe00493          	li	s1,-2
800119f8:	fe9ff06f          	j	800119e0 <bme680_init+0x524>
				dev->calib.res_heat_val = (int8_t) temp_var;
800119fc:	01879793          	slli	a5,a5,0x18
80011a00:	4187d793          	srai	a5,a5,0x18
80011a04:	02f40ca3          	sb	a5,57(s0)
		rslt = BME680_E_NULL_PTR;
80011a08:	fff00493          	li	s1,-1
80011a0c:	d59ff06f          	j	80011764 <bme680_init+0x2a8>
80011a10:	fff00493          	li	s1,-1
80011a14:	d51ff06f          	j	80011764 <bme680_init+0x2a8>

80011a18 <bme680_get_sensor_settings>:
{
80011a18:	fe010113          	addi	sp,sp,-32
80011a1c:	00112e23          	sw	ra,28(sp)
80011a20:	00812c23          	sw	s0,24(sp)
80011a24:	00912a23          	sw	s1,20(sp)
80011a28:	01212823          	sw	s2,16(sp)
	uint8_t data_array[BME680_REG_BUFFER_LENGTH] = { 0 };
80011a2c:	00012423          	sw	zero,8(sp)
80011a30:	00011623          	sh	zero,12(sp)
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80011a34:	20058a63          	beqz	a1,80011c48 <bme680_get_sensor_settings+0x230>
80011a38:	04c5a783          	lw	a5,76(a1)
80011a3c:	00058413          	mv	s0,a1
80011a40:	20078463          	beqz	a5,80011c48 <bme680_get_sensor_settings+0x230>
80011a44:	0505a703          	lw	a4,80(a1)
80011a48:	20070063          	beqz	a4,80011c48 <bme680_get_sensor_settings+0x230>
80011a4c:	0545a703          	lw	a4,84(a1)
80011a50:	1e070c63          	beqz	a4,80011c48 <bme680_get_sensor_settings+0x230>
		if (dev->intf == BME680_SPI_INTF) {
80011a54:	0045a703          	lw	a4,4(a1)
80011a58:	00050493          	mv	s1,a0
80011a5c:	07000813          	li	a6,112
80011a60:	0c070463          	beqz	a4,80011b28 <bme680_get_sensor_settings+0x110>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80011a64:	00144503          	lbu	a0,1(s0)
80011a68:	00600693          	li	a3,6
80011a6c:	00810613          	addi	a2,sp,8
80011a70:	00080593          	mv	a1,a6
80011a74:	000780e7          	jalr	a5
80011a78:	04a40c23          	sb	a0,88(s0)
80011a7c:	00050913          	mv	s2,a0
		if (dev->com_rslt != 0)
80011a80:	0c051e63          	bnez	a0,80011b5c <bme680_get_sensor_settings+0x144>
			if (desired_settings & BME680_GAS_MEAS_SEL)
80011a84:	0084f793          	andi	a5,s1,8
80011a88:	0e079a63          	bnez	a5,80011b7c <bme680_get_sensor_settings+0x164>
			if (desired_settings & BME680_FILTER_SEL)
80011a8c:	0104f793          	andi	a5,s1,16
80011a90:	00078a63          	beqz	a5,80011aa4 <bme680_get_sensor_settings+0x8c>
				dev->tph_sett.filter = BME680_GET_BITS(data_array[BME680_REG_FILTER_INDEX],
80011a94:	00d14783          	lbu	a5,13(sp)
80011a98:	4027d793          	srai	a5,a5,0x2
80011a9c:	0077f793          	andi	a5,a5,7
80011aa0:	02f40fa3          	sb	a5,63(s0)
			if (desired_settings & (BME680_OST_SEL | BME680_OSP_SEL)) {
80011aa4:	0034f793          	andi	a5,s1,3
80011aa8:	00078e63          	beqz	a5,80011ac4 <bme680_get_sensor_settings+0xac>
				dev->tph_sett.os_temp = BME680_GET_BITS(data_array[BME680_REG_TEMP_INDEX], BME680_OST);
80011aac:	00c14783          	lbu	a5,12(sp)
				dev->tph_sett.os_pres = BME680_GET_BITS(data_array[BME680_REG_PRES_INDEX], BME680_OSP);
80011ab0:	4027d713          	srai	a4,a5,0x2
80011ab4:	00777713          	andi	a4,a4,7
				dev->tph_sett.os_temp = BME680_GET_BITS(data_array[BME680_REG_TEMP_INDEX], BME680_OST);
80011ab8:	0057d793          	srli	a5,a5,0x5
80011abc:	02f40ea3          	sb	a5,61(s0)
				dev->tph_sett.os_pres = BME680_GET_BITS(data_array[BME680_REG_PRES_INDEX], BME680_OSP);
80011ac0:	02e40f23          	sb	a4,62(s0)
			if (desired_settings & BME680_OSH_SEL)
80011ac4:	0044f793          	andi	a5,s1,4
80011ac8:	00078863          	beqz	a5,80011ad8 <bme680_get_sensor_settings+0xc0>
				dev->tph_sett.os_hum = BME680_GET_BITS_POS_0(data_array[BME680_REG_HUM_INDEX],
80011acc:	00a14783          	lbu	a5,10(sp)
80011ad0:	0077f793          	andi	a5,a5,7
80011ad4:	02f40e23          	sb	a5,60(s0)
			if (desired_settings & BME680_HCNTRL_SEL)
80011ad8:	0204f793          	andi	a5,s1,32
80011adc:	00078863          	beqz	a5,80011aec <bme680_get_sensor_settings+0xd4>
				dev->gas_sett.heatr_ctrl = BME680_GET_BITS_POS_0(data_array[BME680_REG_HCTRL_INDEX],
80011ae0:	00814783          	lbu	a5,8(sp)
80011ae4:	0087f793          	andi	a5,a5,8
80011ae8:	04f400a3          	sb	a5,65(s0)
			if (desired_settings & (BME680_RUN_GAS_SEL | BME680_NBCONV_SEL)) {
80011aec:	0c04f493          	andi	s1,s1,192
80011af0:	00048e63          	beqz	s1,80011b0c <bme680_get_sensor_settings+0xf4>
				dev->gas_sett.nb_conv = BME680_GET_BITS_POS_0(data_array[BME680_REG_NBCONV_INDEX],
80011af4:	00914783          	lbu	a5,9(sp)
				dev->gas_sett.run_gas = BME680_GET_BITS(data_array[BME680_REG_RUN_GAS_INDEX],
80011af8:	4047d713          	srai	a4,a5,0x4
80011afc:	00177713          	andi	a4,a4,1
				dev->gas_sett.nb_conv = BME680_GET_BITS_POS_0(data_array[BME680_REG_NBCONV_INDEX],
80011b00:	00f7f793          	andi	a5,a5,15
80011b04:	04f40023          	sb	a5,64(s0)
				dev->gas_sett.run_gas = BME680_GET_BITS(data_array[BME680_REG_RUN_GAS_INDEX],
80011b08:	04e40123          	sb	a4,66(s0)
}
80011b0c:	01c12083          	lw	ra,28(sp)
80011b10:	01812403          	lw	s0,24(sp)
80011b14:	01412483          	lw	s1,20(sp)
80011b18:	00090513          	mv	a0,s2
80011b1c:	01012903          	lw	s2,16(sp)
80011b20:	02010113          	addi	sp,sp,32
80011b24:	00008067          	ret
			rslt = set_mem_page(reg_addr, dev);
80011b28:	07000513          	li	a0,112
80011b2c:	bdcff0ef          	jal	ra,80010f08 <set_mem_page>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80011b30:	04c42783          	lw	a5,76(s0)
			rslt = set_mem_page(reg_addr, dev);
80011b34:	00050913          	mv	s2,a0
				reg_addr = reg_addr | BME680_SPI_RD_MSK;
80011b38:	0f000813          	li	a6,240
			if (rslt == BME680_OK)
80011b3c:	f20504e3          	beqz	a0,80011a64 <bme680_get_sensor_settings+0x4c>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80011b40:	00144503          	lbu	a0,1(s0)
80011b44:	00600693          	li	a3,6
80011b48:	00810613          	addi	a2,sp,8
80011b4c:	07000593          	li	a1,112
80011b50:	000780e7          	jalr	a5
80011b54:	04a40c23          	sb	a0,88(s0)
		if (dev->com_rslt != 0)
80011b58:	fa050ae3          	beqz	a0,80011b0c <bme680_get_sensor_settings+0xf4>
}
80011b5c:	01c12083          	lw	ra,28(sp)
80011b60:	01812403          	lw	s0,24(sp)
			rslt = BME680_E_COM_FAIL;
80011b64:	ffe00913          	li	s2,-2
}
80011b68:	01412483          	lw	s1,20(sp)
80011b6c:	00090513          	mv	a0,s2
80011b70:	01012903          	lw	s2,16(sp)
80011b74:	02010113          	addi	sp,sp,32
80011b78:	00008067          	ret
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80011b7c:	04c42783          	lw	a5,76(s0)
	uint8_t reg_data = 0;
80011b80:	000103a3          	sb	zero,7(sp)
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80011b84:	06078463          	beqz	a5,80011bec <bme680_get_sensor_settings+0x1d4>
80011b88:	05042703          	lw	a4,80(s0)
80011b8c:	06070063          	beqz	a4,80011bec <bme680_get_sensor_settings+0x1d4>
80011b90:	05442703          	lw	a4,84(s0)
80011b94:	04070c63          	beqz	a4,80011bec <bme680_get_sensor_settings+0x1d4>
		if (BME680_SPI_INTF == dev->intf) {
80011b98:	00442683          	lw	a3,4(s0)
80011b9c:	02068a63          	beqz	a3,80011bd0 <bme680_get_sensor_settings+0x1b8>
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80011ba0:	04070663          	beqz	a4,80011bec <bme680_get_sensor_settings+0x1d4>
		if (dev->intf == BME680_SPI_INTF) {
80011ba4:	00442703          	lw	a4,4(s0)
80011ba8:	05a00593          	li	a1,90
80011bac:	0a070263          	beqz	a4,80011c50 <bme680_get_sensor_settings+0x238>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80011bb0:	00144503          	lbu	a0,1(s0)
80011bb4:	00100693          	li	a3,1
80011bb8:	00710613          	addi	a2,sp,7
80011bbc:	000780e7          	jalr	a5
80011bc0:	04a40c23          	sb	a0,88(s0)
		if (dev->com_rslt != 0)
80011bc4:	02050863          	beqz	a0,80011bf4 <bme680_get_sensor_settings+0x1dc>
			rslt = BME680_E_COM_FAIL;
80011bc8:	ffe00913          	li	s2,-2
80011bcc:	ec1ff06f          	j	80011a8c <bme680_get_sensor_settings+0x74>
			rslt = set_mem_page(reg_addr1, dev);
80011bd0:	00040593          	mv	a1,s0
80011bd4:	05a00513          	li	a0,90
80011bd8:	b30ff0ef          	jal	ra,80010f08 <set_mem_page>
80011bdc:	00050913          	mv	s2,a0
		if (rslt == BME680_OK) {
80011be0:	ea0516e3          	bnez	a0,80011a8c <bme680_get_sensor_settings+0x74>
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80011be4:	04c42783          	lw	a5,76(s0)
80011be8:	0e079463          	bnez	a5,80011cd0 <bme680_get_sensor_settings+0x2b8>
		rslt = BME680_E_NULL_PTR;
80011bec:	fff00913          	li	s2,-1
	return rslt;
80011bf0:	e9dff06f          	j	80011a8c <bme680_get_sensor_settings+0x74>
				dev->gas_sett.heatr_temp = reg_data;
80011bf4:	00714703          	lbu	a4,7(sp)
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80011bf8:	04c42783          	lw	a5,76(s0)
				dev->gas_sett.heatr_temp = reg_data;
80011bfc:	04e41223          	sh	a4,68(s0)
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80011c00:	fe0786e3          	beqz	a5,80011bec <bme680_get_sensor_settings+0x1d4>
80011c04:	05042703          	lw	a4,80(s0)
80011c08:	fe0702e3          	beqz	a4,80011bec <bme680_get_sensor_settings+0x1d4>
80011c0c:	05442703          	lw	a4,84(s0)
80011c10:	fc070ee3          	beqz	a4,80011bec <bme680_get_sensor_settings+0x1d4>
		if (dev->intf == BME680_SPI_INTF) {
80011c14:	00442703          	lw	a4,4(s0)
80011c18:	06400593          	li	a1,100
80011c1c:	06070a63          	beqz	a4,80011c90 <bme680_get_sensor_settings+0x278>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80011c20:	00144503          	lbu	a0,1(s0)
80011c24:	00100693          	li	a3,1
80011c28:	00710613          	addi	a2,sp,7
80011c2c:	000780e7          	jalr	a5
80011c30:	04a40c23          	sb	a0,88(s0)
80011c34:	00050913          	mv	s2,a0
		if (dev->com_rslt != 0)
80011c38:	f80518e3          	bnez	a0,80011bc8 <bme680_get_sensor_settings+0x1b0>
					dev->gas_sett.heatr_dur = reg_data;
80011c3c:	00714783          	lbu	a5,7(sp)
80011c40:	04f41323          	sh	a5,70(s0)
80011c44:	e49ff06f          	j	80011a8c <bme680_get_sensor_settings+0x74>
		rslt = BME680_E_NULL_PTR;
80011c48:	fff00913          	li	s2,-1
80011c4c:	ec1ff06f          	j	80011b0c <bme680_get_sensor_settings+0xf4>
			rslt = set_mem_page(reg_addr, dev);
80011c50:	00040593          	mv	a1,s0
80011c54:	05a00513          	li	a0,90
80011c58:	ab0ff0ef          	jal	ra,80010f08 <set_mem_page>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80011c5c:	04c42783          	lw	a5,76(s0)
			rslt = set_mem_page(reg_addr, dev);
80011c60:	00050913          	mv	s2,a0
				reg_addr = reg_addr | BME680_SPI_RD_MSK;
80011c64:	0da00593          	li	a1,218
			if (rslt == BME680_OK)
80011c68:	f40504e3          	beqz	a0,80011bb0 <bme680_get_sensor_settings+0x198>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80011c6c:	00144503          	lbu	a0,1(s0)
80011c70:	00100693          	li	a3,1
80011c74:	00710613          	addi	a2,sp,7
80011c78:	05a00593          	li	a1,90
80011c7c:	000780e7          	jalr	a5
80011c80:	04a40c23          	sb	a0,88(s0)
		if (dev->com_rslt != 0)
80011c84:	e00504e3          	beqz	a0,80011a8c <bme680_get_sensor_settings+0x74>
			rslt = BME680_E_COM_FAIL;
80011c88:	ffe00913          	li	s2,-2
80011c8c:	e01ff06f          	j	80011a8c <bme680_get_sensor_settings+0x74>
			rslt = set_mem_page(reg_addr, dev);
80011c90:	00040593          	mv	a1,s0
80011c94:	06400513          	li	a0,100
80011c98:	a70ff0ef          	jal	ra,80010f08 <set_mem_page>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80011c9c:	04c42783          	lw	a5,76(s0)
			rslt = set_mem_page(reg_addr, dev);
80011ca0:	00050913          	mv	s2,a0
				reg_addr = reg_addr | BME680_SPI_RD_MSK;
80011ca4:	0e400593          	li	a1,228
			if (rslt == BME680_OK)
80011ca8:	f6050ce3          	beqz	a0,80011c20 <bme680_get_sensor_settings+0x208>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80011cac:	00144503          	lbu	a0,1(s0)
80011cb0:	00100693          	li	a3,1
80011cb4:	00710613          	addi	a2,sp,7
80011cb8:	06400593          	li	a1,100
80011cbc:	000780e7          	jalr	a5
80011cc0:	04a40c23          	sb	a0,88(s0)
		if (dev->com_rslt != 0)
80011cc4:	dc0504e3          	beqz	a0,80011a8c <bme680_get_sensor_settings+0x74>
			rslt = BME680_E_COM_FAIL;
80011cc8:	ffe00913          	li	s2,-2
80011ccc:	dc1ff06f          	j	80011a8c <bme680_get_sensor_settings+0x74>
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80011cd0:	05042703          	lw	a4,80(s0)
80011cd4:	f0070ce3          	beqz	a4,80011bec <bme680_get_sensor_settings+0x1d4>
80011cd8:	05442703          	lw	a4,84(s0)
80011cdc:	ec5ff06f          	j	80011ba0 <bme680_get_sensor_settings+0x188>

80011ce0 <bme680_set_sensor_mode>:
80011ce0:	1c050a63          	beqz	a0,80011eb4 <bme680_set_sensor_mode+0x1d4>
80011ce4:	04c52783          	lw	a5,76(a0)
{
80011ce8:	fe010113          	addi	sp,sp,-32
80011cec:	00812c23          	sw	s0,24(sp)
80011cf0:	00112e23          	sw	ra,28(sp)
80011cf4:	00912a23          	sw	s1,20(sp)
80011cf8:	01212823          	sw	s2,16(sp)
80011cfc:	00050413          	mv	s0,a0
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80011d00:	1a078663          	beqz	a5,80011eac <bme680_set_sensor_mode+0x1cc>
80011d04:	05052703          	lw	a4,80(a0)
80011d08:	1a070263          	beqz	a4,80011eac <bme680_set_sensor_mode+0x1cc>
80011d0c:	05452703          	lw	a4,84(a0)
80011d10:	18070e63          	beqz	a4,80011eac <bme680_set_sensor_mode+0x1cc>
80011d14:	00000493          	li	s1,0
		if (mem_page != dev->mem_page) {
80011d18:	01000913          	li	s2,16
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80011d1c:	14078e63          	beqz	a5,80011e78 <bme680_set_sensor_mode+0x198>
80011d20:	05042703          	lw	a4,80(s0)
80011d24:	14070a63          	beqz	a4,80011e78 <bme680_set_sensor_mode+0x198>
80011d28:	05442703          	lw	a4,84(s0)
80011d2c:	14070663          	beqz	a4,80011e78 <bme680_set_sensor_mode+0x198>
		if (dev->intf == BME680_SPI_INTF) {
80011d30:	00442703          	lw	a4,4(s0)
			dev->com_rslt = dev->read(dev->dev_id, BME680_MEM_PAGE_ADDR | BME680_SPI_RD_MSK, &reg, 1);
80011d34:	00144503          	lbu	a0,1(s0)
80011d38:	07400593          	li	a1,116
		if (dev->intf == BME680_SPI_INTF) {
80011d3c:	04070c63          	beqz	a4,80011d94 <bme680_set_sensor_mode+0xb4>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80011d40:	00100693          	li	a3,1
80011d44:	00e10613          	addi	a2,sp,14
80011d48:	000780e7          	jalr	a5
80011d4c:	04a40c23          	sb	a0,88(s0)
		if (dev->com_rslt != 0)
80011d50:	0c051063          	bnez	a0,80011e10 <bme680_set_sensor_mode+0x130>
				pow_mode = (tmp_pow_mode & BME680_MODE_MSK);
80011d54:	00e14783          	lbu	a5,14(sp)
80011d58:	0037f493          	andi	s1,a5,3
				if (pow_mode != BME680_SLEEP_MODE) {
80011d5c:	0c048a63          	beqz	s1,80011e30 <bme680_set_sensor_mode+0x150>
					rslt = dev->write(1, reg_addr, &tmp_pow_mode, 1);
80011d60:	05042703          	lw	a4,80(s0)
					tmp_pow_mode = tmp_pow_mode & (~BME680_MODE_MSK); /* Set to sleep */
80011d64:	ffc7f793          	andi	a5,a5,-4
80011d68:	00f10723          	sb	a5,14(sp)
					rslt = dev->write(1, reg_addr, &tmp_pow_mode, 1);
80011d6c:	00100693          	li	a3,1
80011d70:	00e10613          	addi	a2,sp,14
80011d74:	07400593          	li	a1,116
80011d78:	00100513          	li	a0,1
80011d7c:	000700e7          	jalr	a4
					dev->delay_ms(BME680_POLL_PERIOD_MS);
80011d80:	05442783          	lw	a5,84(s0)
80011d84:	00a00513          	li	a0,10
80011d88:	000780e7          	jalr	a5
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80011d8c:	04c42783          	lw	a5,76(s0)
80011d90:	f8dff06f          	j	80011d1c <bme680_set_sensor_mode+0x3c>
		if (mem_page != dev->mem_page) {
80011d94:	00844703          	lbu	a4,8(s0)
80011d98:	07270063          	beq	a4,s2,80011df8 <bme680_set_sensor_mode+0x118>
			dev->mem_page = mem_page;
80011d9c:	01240423          	sb	s2,8(s0)
			dev->com_rslt = dev->read(dev->dev_id, BME680_MEM_PAGE_ADDR | BME680_SPI_RD_MSK, &reg, 1);
80011da0:	00100693          	li	a3,1
80011da4:	00f10613          	addi	a2,sp,15
80011da8:	0f300593          	li	a1,243
80011dac:	000780e7          	jalr	a5
80011db0:	04a40c23          	sb	a0,88(s0)
			if (dev->com_rslt != 0)
80011db4:	0c051863          	bnez	a0,80011e84 <bme680_set_sensor_mode+0x1a4>
				reg = reg & (~BME680_MEM_PAGE_MSK);
80011db8:	00f14783          	lbu	a5,15(sp)
				reg = reg | (dev->mem_page & BME680_MEM_PAGE_MSK);
80011dbc:	00844703          	lbu	a4,8(s0)
				dev->com_rslt = dev->write(dev->dev_id, BME680_MEM_PAGE_ADDR & BME680_SPI_WR_MSK,
80011dc0:	05042803          	lw	a6,80(s0)
80011dc4:	00144503          	lbu	a0,1(s0)
				reg = reg | (dev->mem_page & BME680_MEM_PAGE_MSK);
80011dc8:	01077713          	andi	a4,a4,16
				reg = reg & (~BME680_MEM_PAGE_MSK);
80011dcc:	fef7f793          	andi	a5,a5,-17
				reg = reg | (dev->mem_page & BME680_MEM_PAGE_MSK);
80011dd0:	00e7e7b3          	or	a5,a5,a4
80011dd4:	00f107a3          	sb	a5,15(sp)
				dev->com_rslt = dev->write(dev->dev_id, BME680_MEM_PAGE_ADDR & BME680_SPI_WR_MSK,
80011dd8:	00100693          	li	a3,1
80011ddc:	00f10613          	addi	a2,sp,15
80011de0:	07300593          	li	a1,115
80011de4:	000800e7          	jalr	a6
80011de8:	04a40c23          	sb	a0,88(s0)
				if (dev->com_rslt != 0)
80011dec:	08051c63          	bnez	a0,80011e84 <bme680_set_sensor_mode+0x1a4>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80011df0:	04c42783          	lw	a5,76(s0)
80011df4:	00144503          	lbu	a0,1(s0)
				reg_addr = reg_addr | BME680_SPI_RD_MSK;
80011df8:	0f400593          	li	a1,244
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80011dfc:	00100693          	li	a3,1
80011e00:	00e10613          	addi	a2,sp,14
80011e04:	000780e7          	jalr	a5
80011e08:	04a40c23          	sb	a0,88(s0)
		if (dev->com_rslt != 0)
80011e0c:	f40504e3          	beqz	a0,80011d54 <bme680_set_sensor_mode+0x74>
			rslt = BME680_E_COM_FAIL;
80011e10:	ffe00513          	li	a0,-2
		} while (pow_mode != BME680_SLEEP_MODE);
80011e14:	f6049ce3          	bnez	s1,80011d8c <bme680_set_sensor_mode+0xac>
}
80011e18:	01c12083          	lw	ra,28(sp)
80011e1c:	01812403          	lw	s0,24(sp)
80011e20:	01412483          	lw	s1,20(sp)
80011e24:	01012903          	lw	s2,16(sp)
80011e28:	02010113          	addi	sp,sp,32
80011e2c:	00008067          	ret
		if (dev->power_mode != BME680_SLEEP_MODE) {
80011e30:	04844703          	lbu	a4,72(s0)
80011e34:	fe0702e3          	beqz	a4,80011e18 <bme680_set_sensor_mode+0x138>
				rslt = dev->write(1, reg_addr, &tmp_pow_mode, 1);
80011e38:	05042803          	lw	a6,80(s0)
			tmp_pow_mode = (tmp_pow_mode & ~BME680_MODE_MSK) | (dev->power_mode & BME680_MODE_MSK);
80011e3c:	00377713          	andi	a4,a4,3
80011e40:	ffc7f793          	andi	a5,a5,-4
80011e44:	00e7e7b3          	or	a5,a5,a4
80011e48:	00f10723          	sb	a5,14(sp)
				rslt = dev->write(1, reg_addr, &tmp_pow_mode, 1);
80011e4c:	00e10613          	addi	a2,sp,14
80011e50:	00100693          	li	a3,1
80011e54:	07400593          	li	a1,116
80011e58:	00100513          	li	a0,1
80011e5c:	000800e7          	jalr	a6
}
80011e60:	01c12083          	lw	ra,28(sp)
80011e64:	01812403          	lw	s0,24(sp)
80011e68:	01412483          	lw	s1,20(sp)
80011e6c:	01012903          	lw	s2,16(sp)
80011e70:	02010113          	addi	sp,sp,32
80011e74:	00008067          	ret
		rslt = BME680_E_NULL_PTR;
80011e78:	fff00513          	li	a0,-1
		} while (pow_mode != BME680_SLEEP_MODE);
80011e7c:	f00498e3          	bnez	s1,80011d8c <bme680_set_sensor_mode+0xac>
80011e80:	f99ff06f          	j	80011e18 <bme680_set_sensor_mode+0x138>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80011e84:	04c42783          	lw	a5,76(s0)
80011e88:	00144503          	lbu	a0,1(s0)
80011e8c:	00100693          	li	a3,1
80011e90:	00e10613          	addi	a2,sp,14
80011e94:	07400593          	li	a1,116
80011e98:	000780e7          	jalr	a5
80011e9c:	04a40c23          	sb	a0,88(s0)
80011ea0:	ffe00513          	li	a0,-2
		} while (pow_mode != BME680_SLEEP_MODE);
80011ea4:	ee0494e3          	bnez	s1,80011d8c <bme680_set_sensor_mode+0xac>
80011ea8:	f71ff06f          	j	80011e18 <bme680_set_sensor_mode+0x138>
		rslt = BME680_E_NULL_PTR;
80011eac:	fff00513          	li	a0,-1
80011eb0:	f69ff06f          	j	80011e18 <bme680_set_sensor_mode+0x138>
80011eb4:	fff00513          	li	a0,-1
}
80011eb8:	00008067          	ret

80011ebc <bme680_set_sensor_settings>:
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80011ebc:	04c5a783          	lw	a5,76(a1)
{
80011ec0:	fb010113          	addi	sp,sp,-80
80011ec4:	04112623          	sw	ra,76(sp)
80011ec8:	04812423          	sw	s0,72(sp)
80011ecc:	04912223          	sw	s1,68(sp)
80011ed0:	05212023          	sw	s2,64(sp)
80011ed4:	03312e23          	sw	s3,60(sp)
80011ed8:	03412c23          	sw	s4,56(sp)
80011edc:	03512a23          	sw	s5,52(sp)
	uint8_t data = 0;
80011ee0:	000103a3          	sb	zero,7(sp)
	uint8_t intended_power_mode = dev->power_mode; /* Save intended power mode */
80011ee4:	0485ca03          	lbu	s4,72(a1)
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80011ee8:	66078a63          	beqz	a5,8001255c <bme680_set_sensor_settings+0x6a0>
80011eec:	0505a903          	lw	s2,80(a1)
80011ef0:	00058413          	mv	s0,a1
80011ef4:	66090463          	beqz	s2,8001255c <bme680_set_sensor_settings+0x6a0>
80011ef8:	0545a783          	lw	a5,84(a1)
80011efc:	66078063          	beqz	a5,8001255c <bme680_set_sensor_settings+0x6a0>
		if (desired_settings & BME680_GAS_MEAS_SEL)
80011f00:	00857793          	andi	a5,a0,8
80011f04:	00050493          	mv	s1,a0
80011f08:	3c079863          	bnez	a5,800122d8 <bme680_set_sensor_settings+0x41c>
		dev->power_mode = BME680_SLEEP_MODE;
80011f0c:	04040423          	sb	zero,72(s0)
			rslt = bme680_set_sensor_mode(dev);
80011f10:	00040513          	mv	a0,s0
80011f14:	dcdff0ef          	jal	ra,80011ce0 <bme680_set_sensor_mode>
80011f18:	00050913          	mv	s2,a0
		if (desired_settings & BME680_FILTER_SEL) {
80011f1c:	0104f793          	andi	a5,s1,16
80011f20:	0a078063          	beqz	a5,80011fc0 <bme680_set_sensor_settings+0x104>
		if (*value < min) {
80011f24:	03f44783          	lbu	a5,63(s0)
		if (*value > max) {
80011f28:	00700713          	li	a4,7
80011f2c:	00f77c63          	bgeu	a4,a5,80011f44 <bme680_set_sensor_settings+0x88>
			dev->info_msg |= BME680_I_MAX_CORRECTION;
80011f30:	04a44683          	lbu	a3,74(s0)
			*value = max;
80011f34:	02e40fa3          	sb	a4,63(s0)
			dev->info_msg |= BME680_I_MAX_CORRECTION;
80011f38:	00700793          	li	a5,7
80011f3c:	0026e713          	ori	a4,a3,2
80011f40:	04e40523          	sb	a4,74(s0)
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80011f44:	04c42703          	lw	a4,76(s0)
80011f48:	05042803          	lw	a6,80(s0)
80011f4c:	62070263          	beqz	a4,80012570 <bme680_set_sensor_settings+0x6b4>
80011f50:	62080063          	beqz	a6,80012570 <bme680_set_sensor_settings+0x6b4>
80011f54:	05442683          	lw	a3,84(s0)
80011f58:	60068c63          	beqz	a3,80012570 <bme680_set_sensor_settings+0x6b4>
		if (dev->intf == BME680_SPI_INTF) {
80011f5c:	00442783          	lw	a5,4(s0)
		rslt = BME680_OK;
80011f60:	00000913          	li	s2,0
80011f64:	07500a93          	li	s5,117
		if (dev->intf == BME680_SPI_INTF) {
80011f68:	3a078663          	beqz	a5,80012314 <bme680_set_sensor_settings+0x458>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80011f6c:	00144503          	lbu	a0,1(s0)
80011f70:	00710993          	addi	s3,sp,7
80011f74:	00100693          	li	a3,1
80011f78:	00098613          	mv	a2,s3
80011f7c:	000a8593          	mv	a1,s5
80011f80:	000700e7          	jalr	a4
80011f84:	04a40c23          	sb	a0,88(s0)
				data = BME680_SET_BITS(data, BME680_FILTER, dev->tph_sett.filter);
80011f88:	03f44783          	lbu	a5,63(s0)
			dev->write(1, reg_addr, &data, 1);
80011f8c:	05042803          	lw	a6,80(s0)
		if (dev->com_rslt != 0)
80011f90:	60051e63          	bnez	a0,800125ac <bme680_set_sensor_settings+0x6f0>
				data = BME680_SET_BITS(data, BME680_FILTER, dev->tph_sett.filter);
80011f94:	00714703          	lbu	a4,7(sp)
80011f98:	00279793          	slli	a5,a5,0x2
80011f9c:	01c7f793          	andi	a5,a5,28
80011fa0:	fe377713          	andi	a4,a4,-29
80011fa4:	00f767b3          	or	a5,a4,a5
80011fa8:	00f103a3          	sb	a5,7(sp)
			dev->write(1, reg_addr, &data, 1);
80011fac:	00100693          	li	a3,1
80011fb0:	00098613          	mv	a2,s3
80011fb4:	07500593          	li	a1,117
80011fb8:	00100513          	li	a0,1
80011fbc:	000800e7          	jalr	a6
		if (desired_settings & BME680_HCNTRL_SEL) {
80011fc0:	0204f793          	andi	a5,s1,32
80011fc4:	08078e63          	beqz	a5,80012060 <bme680_set_sensor_settings+0x1a4>
		if (*value < min) {
80011fc8:	04144783          	lbu	a5,65(s0)
		if (*value > max) {
80011fcc:	00800713          	li	a4,8
80011fd0:	00f77c63          	bgeu	a4,a5,80011fe8 <bme680_set_sensor_settings+0x12c>
			dev->info_msg |= BME680_I_MAX_CORRECTION;
80011fd4:	04a44683          	lbu	a3,74(s0)
			*value = max;
80011fd8:	04e400a3          	sb	a4,65(s0)
			dev->info_msg |= BME680_I_MAX_CORRECTION;
80011fdc:	00800793          	li	a5,8
80011fe0:	0026e713          	ori	a4,a3,2
80011fe4:	04e40523          	sb	a4,74(s0)
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80011fe8:	04c42703          	lw	a4,76(s0)
80011fec:	05042803          	lw	a6,80(s0)
80011ff0:	56070a63          	beqz	a4,80012564 <bme680_set_sensor_settings+0x6a8>
80011ff4:	56080863          	beqz	a6,80012564 <bme680_set_sensor_settings+0x6a8>
80011ff8:	05442683          	lw	a3,84(s0)
80011ffc:	56068463          	beqz	a3,80012564 <bme680_set_sensor_settings+0x6a8>
		if (dev->intf == BME680_SPI_INTF) {
80012000:	00442783          	lw	a5,4(s0)
		rslt = BME680_OK;
80012004:	00000913          	li	s2,0
80012008:	07000a93          	li	s5,112
		if (dev->intf == BME680_SPI_INTF) {
8001200c:	32078463          	beqz	a5,80012334 <bme680_set_sensor_settings+0x478>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80012010:	00144503          	lbu	a0,1(s0)
80012014:	00710993          	addi	s3,sp,7
80012018:	00100693          	li	a3,1
8001201c:	00098613          	mv	a2,s3
80012020:	000a8593          	mv	a1,s5
80012024:	000700e7          	jalr	a4
80012028:	04a40c23          	sb	a0,88(s0)
			data = BME680_SET_BITS_POS_0(data, BME680_HCTRL, dev->gas_sett.heatr_ctrl);
8001202c:	04144783          	lbu	a5,65(s0)
			dev->write(1, reg_addr, &data, 1);
80012030:	05042803          	lw	a6,80(s0)
		if (dev->com_rslt != 0)
80012034:	58051463          	bnez	a0,800125bc <bme680_set_sensor_settings+0x700>
			data = BME680_SET_BITS_POS_0(data, BME680_HCTRL, dev->gas_sett.heatr_ctrl);
80012038:	00714703          	lbu	a4,7(sp)
8001203c:	0087f793          	andi	a5,a5,8
			dev->write(1, reg_addr, &data, 1);
80012040:	00100693          	li	a3,1
			data = BME680_SET_BITS_POS_0(data, BME680_HCTRL, dev->gas_sett.heatr_ctrl);
80012044:	ff777713          	andi	a4,a4,-9
80012048:	00f767b3          	or	a5,a4,a5
8001204c:	00f103a3          	sb	a5,7(sp)
			dev->write(1, reg_addr, &data, 1);
80012050:	00098613          	mv	a2,s3
80012054:	07000593          	li	a1,112
80012058:	00100513          	li	a0,1
8001205c:	000800e7          	jalr	a6
		if (desired_settings & (BME680_OST_SEL | BME680_OSP_SEL)) {
80012060:	0034f793          	andi	a5,s1,3
80012064:	0c078263          	beqz	a5,80012128 <bme680_set_sensor_settings+0x26c>
		if (*value > max) {
80012068:	03d44703          	lbu	a4,61(s0)
8001206c:	00500793          	li	a5,5
80012070:	00e7fa63          	bgeu	a5,a4,80012084 <bme680_set_sensor_settings+0x1c8>
			dev->info_msg |= BME680_I_MAX_CORRECTION;
80012074:	04a44703          	lbu	a4,74(s0)
			*value = max;
80012078:	02f40ea3          	sb	a5,61(s0)
			dev->info_msg |= BME680_I_MAX_CORRECTION;
8001207c:	00276793          	ori	a5,a4,2
80012080:	04f40523          	sb	a5,74(s0)
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80012084:	04c42783          	lw	a5,76(s0)
80012088:	05042703          	lw	a4,80(s0)
8001208c:	4e078e63          	beqz	a5,80012588 <bme680_set_sensor_settings+0x6cc>
80012090:	4e070c63          	beqz	a4,80012588 <bme680_set_sensor_settings+0x6cc>
80012094:	05442683          	lw	a3,84(s0)
80012098:	4e068863          	beqz	a3,80012588 <bme680_set_sensor_settings+0x6cc>
		if (dev->intf == BME680_SPI_INTF) {
8001209c:	00442703          	lw	a4,4(s0)
		rslt = BME680_OK;
800120a0:	00000913          	li	s2,0
800120a4:	07400a93          	li	s5,116
		if (dev->intf == BME680_SPI_INTF) {
800120a8:	2a070663          	beqz	a4,80012354 <bme680_set_sensor_settings+0x498>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
800120ac:	00144503          	lbu	a0,1(s0)
800120b0:	00710993          	addi	s3,sp,7
800120b4:	00100693          	li	a3,1
800120b8:	00098613          	mv	a2,s3
800120bc:	000a8593          	mv	a1,s5
800120c0:	000780e7          	jalr	a5
800120c4:	04a40c23          	sb	a0,88(s0)
			dev->write(1, reg_addr, &data, 1);
800120c8:	05042703          	lw	a4,80(s0)
		if (dev->com_rslt != 0)
800120cc:	4e051463          	bnez	a0,800125b4 <bme680_set_sensor_settings+0x6f8>
			if (desired_settings & BME680_OST_SEL)
800120d0:	0014f793          	andi	a5,s1,1
800120d4:	00078e63          	beqz	a5,800120f0 <bme680_set_sensor_settings+0x234>
				data = BME680_SET_BITS(data, BME680_OST, dev->tph_sett.os_temp);
800120d8:	00714783          	lbu	a5,7(sp)
800120dc:	03d44683          	lbu	a3,61(s0)
800120e0:	01f7f793          	andi	a5,a5,31
800120e4:	00569693          	slli	a3,a3,0x5
800120e8:	00d7e7b3          	or	a5,a5,a3
800120ec:	00f103a3          	sb	a5,7(sp)
			if (desired_settings & BME680_OSP_SEL)
800120f0:	0024f793          	andi	a5,s1,2
800120f4:	02078063          	beqz	a5,80012114 <bme680_set_sensor_settings+0x258>
				data = BME680_SET_BITS(data, BME680_OSP, dev->tph_sett.os_pres);
800120f8:	03e44783          	lbu	a5,62(s0)
800120fc:	00714683          	lbu	a3,7(sp)
80012100:	00279793          	slli	a5,a5,0x2
80012104:	01c7f793          	andi	a5,a5,28
80012108:	fe36f693          	andi	a3,a3,-29
8001210c:	00d7e7b3          	or	a5,a5,a3
80012110:	00f103a3          	sb	a5,7(sp)
			dev->write(1, reg_addr, &data, 1);
80012114:	00100693          	li	a3,1
80012118:	00098613          	mv	a2,s3
8001211c:	07400593          	li	a1,116
80012120:	00100513          	li	a0,1
80012124:	000700e7          	jalr	a4
		if (desired_settings & BME680_OSH_SEL) {
80012128:	0044f793          	andi	a5,s1,4
8001212c:	08078e63          	beqz	a5,800121c8 <bme680_set_sensor_settings+0x30c>
		if (*value < min) {
80012130:	03c44783          	lbu	a5,60(s0)
		if (*value > max) {
80012134:	00500713          	li	a4,5
80012138:	00f77c63          	bgeu	a4,a5,80012150 <bme680_set_sensor_settings+0x294>
			dev->info_msg |= BME680_I_MAX_CORRECTION;
8001213c:	04a44683          	lbu	a3,74(s0)
			*value = max;
80012140:	02e40e23          	sb	a4,60(s0)
			dev->info_msg |= BME680_I_MAX_CORRECTION;
80012144:	00500793          	li	a5,5
80012148:	0026e713          	ori	a4,a3,2
8001214c:	04e40523          	sb	a4,74(s0)
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80012150:	04c42703          	lw	a4,76(s0)
80012154:	05042803          	lw	a6,80(s0)
80012158:	42070263          	beqz	a4,8001257c <bme680_set_sensor_settings+0x6c0>
8001215c:	42080063          	beqz	a6,8001257c <bme680_set_sensor_settings+0x6c0>
80012160:	05442683          	lw	a3,84(s0)
80012164:	40068c63          	beqz	a3,8001257c <bme680_set_sensor_settings+0x6c0>
		if (dev->intf == BME680_SPI_INTF) {
80012168:	00442783          	lw	a5,4(s0)
		rslt = BME680_OK;
8001216c:	00000913          	li	s2,0
80012170:	07200a93          	li	s5,114
		if (dev->intf == BME680_SPI_INTF) {
80012174:	20078063          	beqz	a5,80012374 <bme680_set_sensor_settings+0x4b8>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80012178:	00144503          	lbu	a0,1(s0)
8001217c:	00710993          	addi	s3,sp,7
80012180:	00100693          	li	a3,1
80012184:	00098613          	mv	a2,s3
80012188:	000a8593          	mv	a1,s5
8001218c:	000700e7          	jalr	a4
80012190:	04a40c23          	sb	a0,88(s0)
			data = BME680_SET_BITS_POS_0(data, BME680_OSH, dev->tph_sett.os_hum);
80012194:	03c44783          	lbu	a5,60(s0)
			dev->write(1, reg_addr, &data, 1);
80012198:	05042803          	lw	a6,80(s0)
		if (dev->com_rslt != 0)
8001219c:	42051863          	bnez	a0,800125cc <bme680_set_sensor_settings+0x710>
			data = BME680_SET_BITS_POS_0(data, BME680_OSH, dev->tph_sett.os_hum);
800121a0:	00714703          	lbu	a4,7(sp)
800121a4:	0077f793          	andi	a5,a5,7
			dev->write(1, reg_addr, &data, 1);
800121a8:	00100693          	li	a3,1
			data = BME680_SET_BITS_POS_0(data, BME680_OSH, dev->tph_sett.os_hum);
800121ac:	ff877713          	andi	a4,a4,-8
800121b0:	00f767b3          	or	a5,a4,a5
800121b4:	00f103a3          	sb	a5,7(sp)
			dev->write(1, reg_addr, &data, 1);
800121b8:	00098613          	mv	a2,s3
800121bc:	07200593          	li	a1,114
800121c0:	00100513          	li	a0,1
800121c4:	000800e7          	jalr	a6
		if (desired_settings & (BME680_RUN_GAS_SEL | BME680_NBCONV_SEL)) {
800121c8:	0c04f793          	andi	a5,s1,192
800121cc:	0e078063          	beqz	a5,800122ac <bme680_set_sensor_settings+0x3f0>
		if (*value > max) {
800121d0:	04244703          	lbu	a4,66(s0)
800121d4:	00100793          	li	a5,1
800121d8:	00e7fa63          	bgeu	a5,a4,800121ec <bme680_set_sensor_settings+0x330>
			dev->info_msg |= BME680_I_MAX_CORRECTION;
800121dc:	04a44703          	lbu	a4,74(s0)
			*value = max;
800121e0:	04f40123          	sb	a5,66(s0)
			dev->info_msg |= BME680_I_MAX_CORRECTION;
800121e4:	00276793          	ori	a5,a4,2
800121e8:	04f40523          	sb	a5,74(s0)
		if (*value > max) {
800121ec:	04044703          	lbu	a4,64(s0)
800121f0:	00a00793          	li	a5,10
800121f4:	00e7fa63          	bgeu	a5,a4,80012208 <bme680_set_sensor_settings+0x34c>
			dev->info_msg |= BME680_I_MAX_CORRECTION;
800121f8:	04a44703          	lbu	a4,74(s0)
			*value = max;
800121fc:	04f40023          	sb	a5,64(s0)
			dev->info_msg |= BME680_I_MAX_CORRECTION;
80012200:	00276793          	ori	a5,a4,2
80012204:	04f40523          	sb	a5,74(s0)
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80012208:	04c42783          	lw	a5,76(s0)
8001220c:	05042703          	lw	a4,80(s0)
80012210:	38078263          	beqz	a5,80012594 <bme680_set_sensor_settings+0x6d8>
80012214:	38070063          	beqz	a4,80012594 <bme680_set_sensor_settings+0x6d8>
80012218:	05442683          	lw	a3,84(s0)
8001221c:	36068c63          	beqz	a3,80012594 <bme680_set_sensor_settings+0x6d8>
		if (dev->intf == BME680_SPI_INTF) {
80012220:	00442703          	lw	a4,4(s0)
		rslt = BME680_OK;
80012224:	00000913          	li	s2,0
80012228:	07100a93          	li	s5,113
		if (dev->intf == BME680_SPI_INTF) {
8001222c:	0c070463          	beqz	a4,800122f4 <bme680_set_sensor_settings+0x438>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80012230:	00144503          	lbu	a0,1(s0)
80012234:	00710993          	addi	s3,sp,7
80012238:	00100693          	li	a3,1
8001223c:	00098613          	mv	a2,s3
80012240:	000a8593          	mv	a1,s5
80012244:	000780e7          	jalr	a5
80012248:	04a40c23          	sb	a0,88(s0)
			dev->write(1, reg_addr, &data, 1);
8001224c:	05042703          	lw	a4,80(s0)
		if (dev->com_rslt != 0)
80012250:	36051a63          	bnez	a0,800125c4 <bme680_set_sensor_settings+0x708>
			if (desired_settings & BME680_RUN_GAS_SEL)
80012254:	0404f793          	andi	a5,s1,64
80012258:	02078063          	beqz	a5,80012278 <bme680_set_sensor_settings+0x3bc>
				data = BME680_SET_BITS(data, BME680_RUN_GAS, dev->gas_sett.run_gas);
8001225c:	04244783          	lbu	a5,66(s0)
80012260:	00714683          	lbu	a3,7(sp)
80012264:	00479793          	slli	a5,a5,0x4
80012268:	0107f793          	andi	a5,a5,16
8001226c:	fef6f693          	andi	a3,a3,-17
80012270:	00d7e7b3          	or	a5,a5,a3
80012274:	00f103a3          	sb	a5,7(sp)
			if (desired_settings & BME680_NBCONV_SEL)
80012278:	0804f493          	andi	s1,s1,128
8001227c:	00048e63          	beqz	s1,80012298 <bme680_set_sensor_settings+0x3dc>
				data = BME680_SET_BITS_POS_0(data, BME680_NBCONV, dev->gas_sett.nb_conv);
80012280:	00714783          	lbu	a5,7(sp)
80012284:	04044683          	lbu	a3,64(s0)
80012288:	ff07f793          	andi	a5,a5,-16
8001228c:	00f6f693          	andi	a3,a3,15
80012290:	00d7e7b3          	or	a5,a5,a3
80012294:	00f103a3          	sb	a5,7(sp)
			dev->write(1, reg_addr, &data, 1);
80012298:	00100693          	li	a3,1
8001229c:	00098613          	mv	a2,s3
800122a0:	07100593          	li	a1,113
800122a4:	00100513          	li	a0,1
800122a8:	000700e7          	jalr	a4
		dev->power_mode = intended_power_mode;
800122ac:	05440423          	sb	s4,72(s0)
}
800122b0:	04c12083          	lw	ra,76(sp)
800122b4:	04812403          	lw	s0,72(sp)
800122b8:	04412483          	lw	s1,68(sp)
800122bc:	03c12983          	lw	s3,60(sp)
800122c0:	03812a03          	lw	s4,56(sp)
800122c4:	03412a83          	lw	s5,52(sp)
800122c8:	00090513          	mv	a0,s2
800122cc:	04012903          	lw	s2,64(sp)
800122d0:	05010113          	addi	sp,sp,80
800122d4:	00008067          	ret
		if (dev->power_mode == BME680_FORCED_MODE) {
800122d8:	00100793          	li	a5,1
800122dc:	0afa0c63          	beq	s4,a5,80012394 <bme680_set_sensor_settings+0x4d8>
		dev->power_mode = BME680_SLEEP_MODE;
800122e0:	04058423          	sb	zero,72(a1)
		if (desired_settings & BME680_FILTER_SEL) {
800122e4:	0104f793          	andi	a5,s1,16
			rslt = BME680_W_DEFINE_PWR_MODE;
800122e8:	00100913          	li	s2,1
		if (desired_settings & BME680_FILTER_SEL) {
800122ec:	cc078ae3          	beqz	a5,80011fc0 <bme680_set_sensor_settings+0x104>
800122f0:	c35ff06f          	j	80011f24 <bme680_set_sensor_settings+0x68>
			rslt = set_mem_page(reg_addr, dev);
800122f4:	00040593          	mv	a1,s0
800122f8:	07100513          	li	a0,113
800122fc:	c0dfe0ef          	jal	ra,80010f08 <set_mem_page>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80012300:	04c42783          	lw	a5,76(s0)
			rslt = set_mem_page(reg_addr, dev);
80012304:	00050913          	mv	s2,a0
			if (rslt == BME680_OK)
80012308:	f20514e3          	bnez	a0,80012230 <bme680_set_sensor_settings+0x374>
				reg_addr = reg_addr | BME680_SPI_RD_MSK;
8001230c:	0f100a93          	li	s5,241
80012310:	f21ff06f          	j	80012230 <bme680_set_sensor_settings+0x374>
			rslt = set_mem_page(reg_addr, dev);
80012314:	00040593          	mv	a1,s0
80012318:	07500513          	li	a0,117
8001231c:	bedfe0ef          	jal	ra,80010f08 <set_mem_page>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80012320:	04c42703          	lw	a4,76(s0)
			rslt = set_mem_page(reg_addr, dev);
80012324:	00050913          	mv	s2,a0
			if (rslt == BME680_OK)
80012328:	c40512e3          	bnez	a0,80011f6c <bme680_set_sensor_settings+0xb0>
				reg_addr = reg_addr | BME680_SPI_RD_MSK;
8001232c:	0f500a93          	li	s5,245
80012330:	c3dff06f          	j	80011f6c <bme680_set_sensor_settings+0xb0>
			rslt = set_mem_page(reg_addr, dev);
80012334:	00040593          	mv	a1,s0
80012338:	07000513          	li	a0,112
8001233c:	bcdfe0ef          	jal	ra,80010f08 <set_mem_page>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80012340:	04c42703          	lw	a4,76(s0)
			rslt = set_mem_page(reg_addr, dev);
80012344:	00050913          	mv	s2,a0
			if (rslt == BME680_OK)
80012348:	cc0514e3          	bnez	a0,80012010 <bme680_set_sensor_settings+0x154>
				reg_addr = reg_addr | BME680_SPI_RD_MSK;
8001234c:	0f000a93          	li	s5,240
80012350:	cc1ff06f          	j	80012010 <bme680_set_sensor_settings+0x154>
			rslt = set_mem_page(reg_addr, dev);
80012354:	00040593          	mv	a1,s0
80012358:	07400513          	li	a0,116
8001235c:	badfe0ef          	jal	ra,80010f08 <set_mem_page>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80012360:	04c42783          	lw	a5,76(s0)
			rslt = set_mem_page(reg_addr, dev);
80012364:	00050913          	mv	s2,a0
			if (rslt == BME680_OK)
80012368:	d40512e3          	bnez	a0,800120ac <bme680_set_sensor_settings+0x1f0>
				reg_addr = reg_addr | BME680_SPI_RD_MSK;
8001236c:	0f400a93          	li	s5,244
80012370:	d3dff06f          	j	800120ac <bme680_set_sensor_settings+0x1f0>
			rslt = set_mem_page(reg_addr, dev);
80012374:	00040593          	mv	a1,s0
80012378:	07200513          	li	a0,114
8001237c:	b8dfe0ef          	jal	ra,80010f08 <set_mem_page>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80012380:	04c42703          	lw	a4,76(s0)
			rslt = set_mem_page(reg_addr, dev);
80012384:	00050913          	mv	s2,a0
			if (rslt == BME680_OK)
80012388:	de0518e3          	bnez	a0,80012178 <bme680_set_sensor_settings+0x2bc>
				reg_addr = reg_addr | BME680_SPI_RD_MSK;
8001238c:	0f200a93          	li	s5,242
80012390:	de9ff06f          	j	80012178 <bme680_set_sensor_settings+0x2bc>
	var2 = (dev->calib.par_gh1 + 784) * (((((dev->calib.par_gh2 + 154009) * temp * 5) / 100) + 3276800) / 10);
80012394:	01659503          	lh	a0,22(a1)
80012398:	0445d703          	lhu	a4,68(a1)
8001239c:	000267b7          	lui	a5,0x26
800123a0:	99978793          	addi	a5,a5,-1639 # 25999 <font6x8-0x7ffda693>
800123a4:	19000693          	li	a3,400
800123a8:	00f50533          	add	a0,a0,a5
800123ac:	00070593          	mv	a1,a4
800123b0:	00e6f463          	bgeu	a3,a4,800123b8 <bme680_set_sensor_settings+0x4fc>
800123b4:	19000593          	li	a1,400
800123b8:	01059593          	slli	a1,a1,0x10
800123bc:	0105d593          	srli	a1,a1,0x10
800123c0:	76c060ef          	jal	ra,80018b2c <__mulsi3>
800123c4:	01400593          	li	a1,20
800123c8:	019060ef          	jal	ra,80018be0 <__divsi3>
800123cc:	003207b7          	lui	a5,0x320
800123d0:	00f50533          	add	a0,a0,a5
800123d4:	00a00593          	li	a1,10
800123d8:	009060ef          	jal	ra,80018be0 <__divsi3>
800123dc:	01540583          	lb	a1,21(s0)
		durval = 0xff; /* Max duration*/
800123e0:	0ff00993          	li	s3,255
	var2 = (dev->calib.par_gh1 + 784) * (((((dev->calib.par_gh2 + 154009) * temp * 5) / 100) + 3276800) / 10);
800123e4:	31058593          	addi	a1,a1,784
800123e8:	744060ef          	jal	ra,80018b2c <__mulsi3>
	var1 = (((int32_t) dev->amb_temp * dev->calib.par_gh3) / 1000) * 256;
800123ec:	01840583          	lb	a1,24(s0)
	var2 = (dev->calib.par_gh1 + 784) * (((((dev->calib.par_gh2 + 154009) * temp * 5) / 100) + 3276800) / 10);
800123f0:	00050a93          	mv	s5,a0
	var1 = (((int32_t) dev->amb_temp * dev->calib.par_gh3) / 1000) * 256;
800123f4:	00940503          	lb	a0,9(s0)
	var3 = var1 + (var2 / 2);
800123f8:	401ada93          	srai	s5,s5,0x1
	var1 = (((int32_t) dev->amb_temp * dev->calib.par_gh3) / 1000) * 256;
800123fc:	730060ef          	jal	ra,80018b2c <__mulsi3>
80012400:	3e800593          	li	a1,1000
80012404:	7dc060ef          	jal	ra,80018be0 <__divsi3>
	var4 = (var3 / (dev->calib.res_heat_range + 4));
80012408:	03844583          	lbu	a1,56(s0)
	var1 = (((int32_t) dev->amb_temp * dev->calib.par_gh3) / 1000) * 256;
8001240c:	00851513          	slli	a0,a0,0x8
	var4 = (var3 / (dev->calib.res_heat_range + 4));
80012410:	00aa8533          	add	a0,s5,a0
80012414:	00458593          	addi	a1,a1,4
80012418:	7c8060ef          	jal	ra,80018be0 <__divsi3>
	var5 = (131 * dev->calib.res_heat_val) + 65536;
8001241c:	03940703          	lb	a4,57(s0)
	heatr_res_x100 = (int32_t) (((var4 / var5) - 250) * 34);
80012420:	000105b7          	lui	a1,0x10
	var5 = (131 * dev->calib.res_heat_val) + 65536;
80012424:	00571793          	slli	a5,a4,0x5
80012428:	00e787b3          	add	a5,a5,a4
8001242c:	00279793          	slli	a5,a5,0x2
80012430:	40e787b3          	sub	a5,a5,a4
	heatr_res_x100 = (int32_t) (((var4 / var5) - 250) * 34);
80012434:	00b785b3          	add	a1,a5,a1
80012438:	7a8060ef          	jal	ra,80018be0 <__divsi3>
8001243c:	f0650793          	addi	a5,a0,-250
80012440:	00479513          	slli	a0,a5,0x4
80012444:	00f50533          	add	a0,a0,a5
80012448:	00151513          	slli	a0,a0,0x1
	heatr_res = (uint8_t) ((heatr_res_x100 + 50) / 100);
8001244c:	06400593          	li	a1,100
80012450:	03250513          	addi	a0,a0,50
80012454:	78c060ef          	jal	ra,80018be0 <__divsi3>
			reg_data[1] = calc_heater_dur(dev->gas_sett.heatr_dur);
80012458:	04645703          	lhu	a4,70(s0)
	if (dur >= 0xfc0) {
8001245c:	000017b7          	lui	a5,0x1
80012460:	fbf78793          	addi	a5,a5,-65 # fbf <font6x8-0x7ffff06d>
	heatr_res = (uint8_t) ((heatr_res_x100 + 50) / 100);
80012464:	0ff57a93          	zext.b	s5,a0
	if (dur >= 0xfc0) {
80012468:	02e7ea63          	bltu	a5,a4,8001249c <bme680_set_sensor_settings+0x5e0>
		while (dur > 0x3F) {
8001246c:	03f00793          	li	a5,63
80012470:	16e7f263          	bgeu	a5,a4,800125d4 <bme680_set_sensor_settings+0x718>
	uint8_t factor = 0;
80012474:	00000793          	li	a5,0
		while (dur > 0x3F) {
80012478:	03f00693          	li	a3,63
			factor += 1;
8001247c:	00178793          	addi	a5,a5,1
			dur = dur / 4;
80012480:	00275713          	srli	a4,a4,0x2
			factor += 1;
80012484:	0ff7f793          	zext.b	a5,a5
		while (dur > 0x3F) {
80012488:	fee6eae3          	bltu	a3,a4,8001247c <bme680_set_sensor_settings+0x5c0>
		durval = (uint8_t) (dur + (factor * 64));
8001248c:	00679993          	slli	s3,a5,0x6
80012490:	0ff9f993          	zext.b	s3,s3
80012494:	00e989b3          	add	s3,s3,a4
80012498:	0ff9f993          	zext.b	s3,s3
				if (dev->intf == BME680_SPI_INTF) {
8001249c:	00442783          	lw	a5,4(s0)
			dev->gas_sett.nb_conv = 0;
800124a0:	04040023          	sb	zero,64(s0)
	uint8_t tmp_buff[BME680_TMP_BUFFER_LENGTH] = { 0 };
800124a4:	00012423          	sw	zero,8(sp)
800124a8:	00012623          	sw	zero,12(sp)
800124ac:	00012823          	sw	zero,16(sp)
800124b0:	00012a23          	sw	zero,20(sp)
800124b4:	00012c23          	sw	zero,24(sp)
800124b8:	00012e23          	sw	zero,28(sp)
800124bc:	02012023          	sw	zero,32(sp)
800124c0:	02012223          	sw	zero,36(sp)
800124c4:	02012423          	sw	zero,40(sp)
800124c8:	02012623          	sw	zero,44(sp)
				if (dev->intf == BME680_SPI_INTF) {
800124cc:	04078263          	beqz	a5,80012510 <bme680_set_sensor_settings+0x654>
					tmp_buff[(2 * index)] = reg_addr[index];
800124d0:	05a00793          	li	a5,90
800124d4:	00f10423          	sb	a5,8(sp)
800124d8:	06400793          	li	a5,100
				tmp_buff[(2 * index) + 1] = reg_data[index];
800124dc:	015104a3          	sb	s5,9(sp)
					tmp_buff[(2 * index)] = reg_addr[index];
800124e0:	00f10523          	sb	a5,10(sp)
				tmp_buff[(2 * index) + 1] = reg_data[index];
800124e4:	013105a3          	sb	s3,11(sp)
800124e8:	05a00593          	li	a1,90
				dev->com_rslt = dev->write(dev->dev_id, tmp_buff[0], &tmp_buff[1], (2 * len) - 1);
800124ec:	00144503          	lbu	a0,1(s0)
800124f0:	00300693          	li	a3,3
800124f4:	00910613          	addi	a2,sp,9
800124f8:	000900e7          	jalr	s2
800124fc:	04a40c23          	sb	a0,88(s0)
				if (dev->com_rslt != 0)
80012500:	a00506e3          	beqz	a0,80011f0c <bme680_set_sensor_settings+0x50>
		dev->power_mode = BME680_SLEEP_MODE;
80012504:	04040423          	sb	zero,72(s0)
					rslt = BME680_E_COM_FAIL;
80012508:	ffe00913          	li	s2,-2
8001250c:	a11ff06f          	j	80011f1c <bme680_set_sensor_settings+0x60>
					rslt = set_mem_page(reg_addr[index], dev);
80012510:	00040593          	mv	a1,s0
80012514:	05a00513          	li	a0,90
80012518:	9f1fe0ef          	jal	ra,80010f08 <set_mem_page>
				if (dev->intf == BME680_SPI_INTF) {
8001251c:	00442783          	lw	a5,4(s0)
					tmp_buff[(2 * index)] = reg_addr[index] & BME680_SPI_WR_MSK;
80012520:	05a00713          	li	a4,90
80012524:	00e10423          	sb	a4,8(sp)
				tmp_buff[(2 * index) + 1] = reg_data[index];
80012528:	015104a3          	sb	s5,9(sp)
					rslt = set_mem_page(reg_addr[index], dev);
8001252c:	00050913          	mv	s2,a0
				if (dev->intf == BME680_SPI_INTF) {
80012530:	00079a63          	bnez	a5,80012544 <bme680_set_sensor_settings+0x688>
					rslt = set_mem_page(reg_addr[index], dev);
80012534:	00040593          	mv	a1,s0
80012538:	06400513          	li	a0,100
8001253c:	9cdfe0ef          	jal	ra,80010f08 <set_mem_page>
80012540:	00050913          	mv	s2,a0
					tmp_buff[(2 * index)] = reg_addr[index];
80012544:	06400793          	li	a5,100
80012548:	00f10523          	sb	a5,10(sp)
				tmp_buff[(2 * index) + 1] = reg_data[index];
8001254c:	013105a3          	sb	s3,11(sp)
			if (rslt == BME680_OK) {
80012550:	04090863          	beqz	s2,800125a0 <bme680_set_sensor_settings+0x6e4>
		dev->power_mode = BME680_SLEEP_MODE;
80012554:	04040423          	sb	zero,72(s0)
		if (rslt == BME680_OK)
80012558:	9c5ff06f          	j	80011f1c <bme680_set_sensor_settings+0x60>
		rslt = BME680_E_NULL_PTR;
8001255c:	fff00913          	li	s2,-1
	return rslt;
80012560:	d51ff06f          	j	800122b0 <bme680_set_sensor_settings+0x3f4>
		rslt = BME680_E_NULL_PTR;
80012564:	fff00913          	li	s2,-1
80012568:	00710993          	addi	s3,sp,7
8001256c:	acdff06f          	j	80012038 <bme680_set_sensor_settings+0x17c>
80012570:	fff00913          	li	s2,-1
80012574:	00710993          	addi	s3,sp,7
80012578:	a1dff06f          	j	80011f94 <bme680_set_sensor_settings+0xd8>
8001257c:	fff00913          	li	s2,-1
80012580:	00710993          	addi	s3,sp,7
80012584:	c1dff06f          	j	800121a0 <bme680_set_sensor_settings+0x2e4>
80012588:	fff00913          	li	s2,-1
8001258c:	00710993          	addi	s3,sp,7
80012590:	b41ff06f          	j	800120d0 <bme680_set_sensor_settings+0x214>
80012594:	fff00913          	li	s2,-1
80012598:	00710993          	addi	s3,sp,7
8001259c:	cb9ff06f          	j	80012254 <bme680_set_sensor_settings+0x398>
				dev->com_rslt = dev->write(dev->dev_id, tmp_buff[0], &tmp_buff[1], (2 * len) - 1);
800125a0:	05042903          	lw	s2,80(s0)
800125a4:	00814583          	lbu	a1,8(sp)
800125a8:	f45ff06f          	j	800124ec <bme680_set_sensor_settings+0x630>
			rslt = BME680_E_COM_FAIL;
800125ac:	ffe00913          	li	s2,-2
800125b0:	9e5ff06f          	j	80011f94 <bme680_set_sensor_settings+0xd8>
800125b4:	ffe00913          	li	s2,-2
800125b8:	b19ff06f          	j	800120d0 <bme680_set_sensor_settings+0x214>
800125bc:	ffe00913          	li	s2,-2
800125c0:	a79ff06f          	j	80012038 <bme680_set_sensor_settings+0x17c>
800125c4:	ffe00913          	li	s2,-2
800125c8:	c8dff06f          	j	80012254 <bme680_set_sensor_settings+0x398>
800125cc:	ffe00913          	li	s2,-2
800125d0:	bd1ff06f          	j	800121a0 <bme680_set_sensor_settings+0x2e4>
		while (dur > 0x3F) {
800125d4:	00000993          	li	s3,0
800125d8:	ebdff06f          	j	80012494 <bme680_set_sensor_settings+0x5d8>

800125dc <bme680_get_sensor_mode>:
{
800125dc:	fe010113          	addi	sp,sp,-32
800125e0:	00112e23          	sw	ra,28(sp)
800125e4:	00812c23          	sw	s0,24(sp)
800125e8:	00912a23          	sw	s1,20(sp)
800125ec:	01212823          	sw	s2,16(sp)
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
800125f0:	08050a63          	beqz	a0,80012684 <bme680_get_sensor_mode+0xa8>
800125f4:	04c52783          	lw	a5,76(a0)
800125f8:	00050413          	mv	s0,a0
800125fc:	08078463          	beqz	a5,80012684 <bme680_get_sensor_mode+0xa8>
80012600:	05052703          	lw	a4,80(a0)
80012604:	08070063          	beqz	a4,80012684 <bme680_get_sensor_mode+0xa8>
80012608:	05452703          	lw	a4,84(a0)
8001260c:	06070c63          	beqz	a4,80012684 <bme680_get_sensor_mode+0xa8>
		if (dev->intf == BME680_SPI_INTF) {
80012610:	00452703          	lw	a4,4(a0)
		rslt = BME680_OK;
80012614:	00000493          	li	s1,0
80012618:	07400913          	li	s2,116
		if (dev->intf == BME680_SPI_INTF) {
8001261c:	04070463          	beqz	a4,80012664 <bme680_get_sensor_mode+0x88>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80012620:	00144503          	lbu	a0,1(s0)
80012624:	00100693          	li	a3,1
80012628:	00f10613          	addi	a2,sp,15
8001262c:	00090593          	mv	a1,s2
80012630:	000780e7          	jalr	a5
80012634:	04a40c23          	sb	a0,88(s0)
		if (dev->com_rslt != 0)
80012638:	04051a63          	bnez	a0,8001268c <bme680_get_sensor_mode+0xb0>
		dev->power_mode = mode & BME680_MODE_MSK;
8001263c:	00f14783          	lbu	a5,15(sp)
80012640:	0037f793          	andi	a5,a5,3
80012644:	04f40423          	sb	a5,72(s0)
}
80012648:	01c12083          	lw	ra,28(sp)
8001264c:	01812403          	lw	s0,24(sp)
80012650:	01012903          	lw	s2,16(sp)
80012654:	00048513          	mv	a0,s1
80012658:	01412483          	lw	s1,20(sp)
8001265c:	02010113          	addi	sp,sp,32
80012660:	00008067          	ret
			rslt = set_mem_page(reg_addr, dev);
80012664:	00050593          	mv	a1,a0
80012668:	07400513          	li	a0,116
8001266c:	89dfe0ef          	jal	ra,80010f08 <set_mem_page>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
80012670:	04c42783          	lw	a5,76(s0)
			rslt = set_mem_page(reg_addr, dev);
80012674:	00050493          	mv	s1,a0
			if (rslt == BME680_OK)
80012678:	fa0514e3          	bnez	a0,80012620 <bme680_get_sensor_mode+0x44>
				reg_addr = reg_addr | BME680_SPI_RD_MSK;
8001267c:	0f400913          	li	s2,244
80012680:	fa1ff06f          	j	80012620 <bme680_get_sensor_mode+0x44>
		rslt = BME680_E_NULL_PTR;
80012684:	fff00493          	li	s1,-1
80012688:	fc1ff06f          	j	80012648 <bme680_get_sensor_mode+0x6c>
			rslt = BME680_E_COM_FAIL;
8001268c:	ffe00493          	li	s1,-2
80012690:	fadff06f          	j	8001263c <bme680_get_sensor_mode+0x60>

80012694 <bme680_set_profile_dur>:
{
80012694:	fe010113          	addi	sp,sp,-32
80012698:	00812c23          	sw	s0,24(sp)
8001269c:	00912a23          	sw	s1,20(sp)
800126a0:	00112e23          	sw	ra,28(sp)
	meas_cycles = os_to_meas_cycles[dev->tph_sett.os_temp];
800126a4:	03d5c603          	lbu	a2,61(a1) # 1003d <font6x8-0x7ffeffef>
	meas_cycles += os_to_meas_cycles[dev->tph_sett.os_pres];
800126a8:	03e5c683          	lbu	a3,62(a1)
	meas_cycles += os_to_meas_cycles[dev->tph_sett.os_hum];
800126ac:	03c5c703          	lbu	a4,60(a1)
{
800126b0:	00058413          	mv	s0,a1
	uint8_t os_to_meas_cycles[6] = {0, 1, 2, 4, 8, 16};
800126b4:	000017b7          	lui	a5,0x1
800126b8:	040205b7          	lui	a1,0x4020
800126bc:	00878813          	addi	a6,a5,8 # 1008 <font6x8-0x7ffff024>
800126c0:	10058593          	addi	a1,a1,256 # 4020100 <font6x8-0x7bfdff2c>
	meas_cycles = os_to_meas_cycles[dev->tph_sett.os_temp];
800126c4:	01060613          	addi	a2,a2,16
	meas_cycles += os_to_meas_cycles[dev->tph_sett.os_pres];
800126c8:	01068693          	addi	a3,a3,16
	uint8_t os_to_meas_cycles[6] = {0, 1, 2, 4, 8, 16};
800126cc:	00b12423          	sw	a1,8(sp)
800126d0:	01011623          	sh	a6,12(sp)
	meas_cycles = os_to_meas_cycles[dev->tph_sett.os_temp];
800126d4:	00260633          	add	a2,a2,sp
	meas_cycles += os_to_meas_cycles[dev->tph_sett.os_pres];
800126d8:	002686b3          	add	a3,a3,sp
	meas_cycles += os_to_meas_cycles[dev->tph_sett.os_hum];
800126dc:	01070713          	addi	a4,a4,16
	meas_cycles = os_to_meas_cycles[dev->tph_sett.os_temp];
800126e0:	ff864583          	lbu	a1,-8(a2)
	meas_cycles += os_to_meas_cycles[dev->tph_sett.os_hum];
800126e4:	00270733          	add	a4,a4,sp
	meas_cycles += os_to_meas_cycles[dev->tph_sett.os_pres];
800126e8:	ff86c603          	lbu	a2,-8(a3)
	meas_cycles += os_to_meas_cycles[dev->tph_sett.os_hum];
800126ec:	ff874703          	lbu	a4,-8(a4)
	tph_dur += UINT32_C(500); /* Get it to the closest whole number.*/
800126f0:	2b978693          	addi	a3,a5,697
	meas_cycles += os_to_meas_cycles[dev->tph_sett.os_pres];
800126f4:	00b60633          	add	a2,a2,a1
	meas_cycles += os_to_meas_cycles[dev->tph_sett.os_hum];
800126f8:	00c70733          	add	a4,a4,a2
	tph_dur = meas_cycles * UINT32_C(1963);
800126fc:	00571793          	slli	a5,a4,0x5
80012700:	40e787b3          	sub	a5,a5,a4
80012704:	00279793          	slli	a5,a5,0x2
80012708:	40e787b3          	sub	a5,a5,a4
8001270c:	00279793          	slli	a5,a5,0x2
80012710:	40e787b3          	sub	a5,a5,a4
80012714:	00279793          	slli	a5,a5,0x2
80012718:	40e787b3          	sub	a5,a5,a4
{
8001271c:	00050493          	mv	s1,a0
	tph_dur /= UINT32_C(1000); /* Convert to ms */
80012720:	3e800593          	li	a1,1000
80012724:	00d78533          	add	a0,a5,a3
80012728:	4c0060ef          	jal	ra,80018be8 <__udivsi3>
	tph_dur += UINT32_C(1); /* Wake up duration of 1ms */
8001272c:	00150513          	addi	a0,a0,1
	dev->gas_sett.heatr_dur = duration - (uint16_t) tph_dur;
80012730:	40a484b3          	sub	s1,s1,a0
80012734:	04941323          	sh	s1,70(s0)
}
80012738:	01c12083          	lw	ra,28(sp)
8001273c:	01812403          	lw	s0,24(sp)
80012740:	01412483          	lw	s1,20(sp)
80012744:	02010113          	addi	sp,sp,32
80012748:	00008067          	ret

8001274c <bme680_get_profile_dur>:
{
8001274c:	fe010113          	addi	sp,sp,-32
80012750:	00812c23          	sw	s0,24(sp)
80012754:	00912a23          	sw	s1,20(sp)
80012758:	00112e23          	sw	ra,28(sp)
	meas_cycles = os_to_meas_cycles[dev->tph_sett.os_temp];
8001275c:	03d5c603          	lbu	a2,61(a1)
	meas_cycles += os_to_meas_cycles[dev->tph_sett.os_pres];
80012760:	03e5c683          	lbu	a3,62(a1)
	meas_cycles += os_to_meas_cycles[dev->tph_sett.os_hum];
80012764:	03c5c703          	lbu	a4,60(a1)
	uint8_t os_to_meas_cycles[6] = {0, 1, 2, 4, 8, 16};
80012768:	000017b7          	lui	a5,0x1
{
8001276c:	00058413          	mv	s0,a1
	uint8_t os_to_meas_cycles[6] = {0, 1, 2, 4, 8, 16};
80012770:	040205b7          	lui	a1,0x4020
80012774:	00878813          	addi	a6,a5,8 # 1008 <font6x8-0x7ffff024>
80012778:	10058593          	addi	a1,a1,256 # 4020100 <font6x8-0x7bfdff2c>
	meas_cycles = os_to_meas_cycles[dev->tph_sett.os_temp];
8001277c:	01060613          	addi	a2,a2,16
	meas_cycles += os_to_meas_cycles[dev->tph_sett.os_pres];
80012780:	01068693          	addi	a3,a3,16
	uint8_t os_to_meas_cycles[6] = {0, 1, 2, 4, 8, 16};
80012784:	00b12423          	sw	a1,8(sp)
80012788:	01011623          	sh	a6,12(sp)
	meas_cycles = os_to_meas_cycles[dev->tph_sett.os_temp];
8001278c:	00260633          	add	a2,a2,sp
	meas_cycles += os_to_meas_cycles[dev->tph_sett.os_pres];
80012790:	002686b3          	add	a3,a3,sp
	meas_cycles += os_to_meas_cycles[dev->tph_sett.os_hum];
80012794:	01070713          	addi	a4,a4,16
	meas_cycles = os_to_meas_cycles[dev->tph_sett.os_temp];
80012798:	ff864583          	lbu	a1,-8(a2)
	meas_cycles += os_to_meas_cycles[dev->tph_sett.os_hum];
8001279c:	00270733          	add	a4,a4,sp
	meas_cycles += os_to_meas_cycles[dev->tph_sett.os_pres];
800127a0:	ff86c603          	lbu	a2,-8(a3)
	meas_cycles += os_to_meas_cycles[dev->tph_sett.os_hum];
800127a4:	ff874703          	lbu	a4,-8(a4)
	tph_dur += UINT32_C(500); /* Get it to the closest whole number.*/
800127a8:	2b978693          	addi	a3,a5,697
	meas_cycles += os_to_meas_cycles[dev->tph_sett.os_pres];
800127ac:	00b60633          	add	a2,a2,a1
	meas_cycles += os_to_meas_cycles[dev->tph_sett.os_hum];
800127b0:	00c70733          	add	a4,a4,a2
	tph_dur = meas_cycles * UINT32_C(1963);
800127b4:	00571793          	slli	a5,a4,0x5
800127b8:	40e787b3          	sub	a5,a5,a4
800127bc:	00279793          	slli	a5,a5,0x2
800127c0:	40e787b3          	sub	a5,a5,a4
800127c4:	00279793          	slli	a5,a5,0x2
800127c8:	40e787b3          	sub	a5,a5,a4
800127cc:	00279793          	slli	a5,a5,0x2
800127d0:	40e787b3          	sub	a5,a5,a4
{
800127d4:	00050493          	mv	s1,a0
	tph_dur /= UINT32_C(1000); /* Convert to ms */
800127d8:	3e800593          	li	a1,1000
800127dc:	00d78533          	add	a0,a5,a3
800127e0:	408060ef          	jal	ra,80018be8 <__udivsi3>
	tph_dur += UINT32_C(1); /* Wake up duration of 1ms */
800127e4:	00150793          	addi	a5,a0,1
	*duration = (uint16_t) tph_dur;
800127e8:	01079793          	slli	a5,a5,0x10
800127ec:	0107d793          	srli	a5,a5,0x10
800127f0:	00f49023          	sh	a5,0(s1)
	if (dev->gas_sett.run_gas) {
800127f4:	04244703          	lbu	a4,66(s0)
800127f8:	00070863          	beqz	a4,80012808 <bme680_get_profile_dur+0xbc>
		*duration += dev->gas_sett.heatr_dur;
800127fc:	04645703          	lhu	a4,70(s0)
80012800:	00e787b3          	add	a5,a5,a4
80012804:	00f49023          	sh	a5,0(s1)
}
80012808:	01c12083          	lw	ra,28(sp)
8001280c:	01812403          	lw	s0,24(sp)
80012810:	01412483          	lw	s1,20(sp)
80012814:	02010113          	addi	sp,sp,32
80012818:	00008067          	ret

8001281c <bme680_get_sensor_data>:
{
8001281c:	f2010113          	addi	sp,sp,-224
80012820:	0c112e23          	sw	ra,220(sp)
80012824:	0c812c23          	sw	s0,216(sp)
80012828:	0c912a23          	sw	s1,212(sp)
8001282c:	0d212823          	sw	s2,208(sp)
80012830:	0d312623          	sw	s3,204(sp)
80012834:	0d412423          	sw	s4,200(sp)
80012838:	0d512223          	sw	s5,196(sp)
8001283c:	0d612023          	sw	s6,192(sp)
80012840:	0b712e23          	sw	s7,188(sp)
80012844:	0b812c23          	sw	s8,184(sp)
80012848:	0b912a23          	sw	s9,180(sp)
8001284c:	0ba12823          	sw	s10,176(sp)
80012850:	0bb12623          	sw	s11,172(sp)
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80012854:	66058263          	beqz	a1,80012eb8 <bme680_get_sensor_data+0x69c>
80012858:	04c5a783          	lw	a5,76(a1)
8001285c:	00058413          	mv	s0,a1
80012860:	64078c63          	beqz	a5,80012eb8 <bme680_get_sensor_data+0x69c>
80012864:	0505a703          	lw	a4,80(a1)
80012868:	64070863          	beqz	a4,80012eb8 <bme680_get_sensor_data+0x69c>
8001286c:	0545a703          	lw	a4,84(a1)
80012870:	64070463          	beqz	a4,80012eb8 <bme680_get_sensor_data+0x69c>
80012874:	00050993          	mv	s3,a0
	uint8_t buff[BME680_FIELD_LENGTH] = { 0 };
80012878:	00012823          	sw	zero,16(sp)
8001287c:	00012a23          	sw	zero,20(sp)
80012880:	00012c23          	sw	zero,24(sp)
80012884:	00011e23          	sh	zero,28(sp)
80012888:	00010f23          	sb	zero,30(sp)
8001288c:	00a00913          	li	s2,10
	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
80012890:	60078c63          	beqz	a5,80012ea8 <bme680_get_sensor_data+0x68c>
80012894:	05042703          	lw	a4,80(s0)
80012898:	60070863          	beqz	a4,80012ea8 <bme680_get_sensor_data+0x68c>
8001289c:	05442703          	lw	a4,84(s0)
800128a0:	60070463          	beqz	a4,80012ea8 <bme680_get_sensor_data+0x68c>
		if (dev->intf == BME680_SPI_INTF) {
800128a4:	00442703          	lw	a4,4(s0)
		rslt = BME680_OK;
800128a8:	00000493          	li	s1,0
800128ac:	01d00a13          	li	s4,29
		if (dev->intf == BME680_SPI_INTF) {
800128b0:	0e070063          	beqz	a4,80012990 <bme680_get_sensor_data+0x174>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
800128b4:	00144503          	lbu	a0,1(s0)
800128b8:	00f00693          	li	a3,15
800128bc:	01010613          	addi	a2,sp,16
800128c0:	000a0593          	mv	a1,s4
800128c4:	000780e7          	jalr	a5
800128c8:	04a40c23          	sb	a0,88(s0)
		if (dev->com_rslt != 0)
800128cc:	5e051263          	bnez	a0,80012eb0 <bme680_get_sensor_data+0x694>
			data->status = buff[0] & BME680_NEW_DATA_MSK;
800128d0:	01014783          	lbu	a5,16(sp)
			adc_gas_res = (uint16_t) ((uint32_t) buff[13] * 4 | (((uint32_t) buff[14]) / 64));
800128d4:	01e14b03          	lbu	s6,30(sp)
			data->meas_index = buff[1];
800128d8:	01114603          	lbu	a2,17(sp)
			data->status = buff[0] & BME680_NEW_DATA_MSK;
800128dc:	0807f713          	andi	a4,a5,128
			data->status |= buff[14] & BME680_HEAT_STAB_MSK;
800128e0:	030b7a13          	andi	s4,s6,48
			data->gas_index = buff[0] & BME680_GAS_INDEX_MSK;
800128e4:	00f7f793          	andi	a5,a5,15
			data->status |= buff[14] & BME680_HEAT_STAB_MSK;
800128e8:	014766b3          	or	a3,a4,s4
800128ec:	00879793          	slli	a5,a5,0x8
800128f0:	00f6e7b3          	or	a5,a3,a5
			data->meas_index = buff[1];
800128f4:	00c98123          	sb	a2,2(s3)
			data->status |= buff[14] & BME680_HEAT_STAB_MSK;
800128f8:	00f99023          	sh	a5,0(s3)
			adc_pres = (uint32_t) (((uint32_t) buff[2] * 4096) | ((uint32_t) buff[3] * 16)
800128fc:	01214d83          	lbu	s11,18(sp)
80012900:	01314c03          	lbu	s8,19(sp)
				| ((uint32_t) buff[4] / 16));
80012904:	01414803          	lbu	a6,20(sp)
			adc_temp = (uint32_t) (((uint32_t) buff[5] * 4096) | ((uint32_t) buff[6] * 16)
80012908:	01514683          	lbu	a3,21(sp)
8001290c:	01614783          	lbu	a5,22(sp)
				| ((uint32_t) buff[7] / 16));
80012910:	01714603          	lbu	a2,23(sp)
			adc_hum = (uint16_t) (((uint32_t) buff[8] * 256) | (uint32_t) buff[9]);
80012914:	01814c83          	lbu	s9,24(sp)
80012918:	01914d03          	lbu	s10,25(sp)
			adc_gas_res = (uint16_t) ((uint32_t) buff[13] * 4 | (((uint32_t) buff[14]) / 64));
8001291c:	01d14a83          	lbu	s5,29(sp)
			data->status |= buff[14] & BME680_HEAT_STAB_MSK;
80012920:	01476a33          	or	s4,a4,s4
			if (data->status & BME680_NEW_DATA_MSK) {
80012924:	08071663          	bnez	a4,800129b0 <bme680_get_sensor_data+0x194>
			dev->delay_ms(BME680_POLL_PERIOD_MS);
80012928:	05442783          	lw	a5,84(s0)
	} while (tries);
8001292c:	fff90913          	addi	s2,s2,-1
			dev->delay_ms(BME680_POLL_PERIOD_MS);
80012930:	00a00513          	li	a0,10
	} while (tries);
80012934:	0ff97913          	zext.b	s2,s2
			dev->delay_ms(BME680_POLL_PERIOD_MS);
80012938:	000780e7          	jalr	a5
	} while (tries);
8001293c:	00090463          	beqz	s2,80012944 <bme680_get_sensor_data+0x128>
		if (rslt == BME680_OK) {
80012940:	04048463          	beqz	s1,80012988 <bme680_get_sensor_data+0x16c>
		rslt = BME680_W_NO_NEW_DATA;
80012944:	00200493          	li	s1,2
}
80012948:	0dc12083          	lw	ra,220(sp)
8001294c:	0d812403          	lw	s0,216(sp)
80012950:	0d012903          	lw	s2,208(sp)
80012954:	0cc12983          	lw	s3,204(sp)
80012958:	0c812a03          	lw	s4,200(sp)
8001295c:	0c412a83          	lw	s5,196(sp)
80012960:	0c012b03          	lw	s6,192(sp)
80012964:	0bc12b83          	lw	s7,188(sp)
80012968:	0b812c03          	lw	s8,184(sp)
8001296c:	0b412c83          	lw	s9,180(sp)
80012970:	0b012d03          	lw	s10,176(sp)
80012974:	0ac12d83          	lw	s11,172(sp)
80012978:	00048513          	mv	a0,s1
8001297c:	0d412483          	lw	s1,212(sp)
80012980:	0e010113          	addi	sp,sp,224
80012984:	00008067          	ret
80012988:	04c42783          	lw	a5,76(s0)
8001298c:	f05ff06f          	j	80012890 <bme680_get_sensor_data+0x74>
			rslt = set_mem_page(reg_addr, dev);
80012990:	00040593          	mv	a1,s0
80012994:	01d00513          	li	a0,29
80012998:	d70fe0ef          	jal	ra,80010f08 <set_mem_page>
		dev->com_rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
8001299c:	04c42783          	lw	a5,76(s0)
			rslt = set_mem_page(reg_addr, dev);
800129a0:	00050493          	mv	s1,a0
			if (rslt == BME680_OK)
800129a4:	f00518e3          	bnez	a0,800128b4 <bme680_get_sensor_data+0x98>
				reg_addr = reg_addr | BME680_SPI_RD_MSK;
800129a8:	09d00a13          	li	s4,157
800129ac:	f09ff06f          	j	800128b4 <bme680_get_sensor_data+0x98>
	var1 = ((int32_t) temp_adc >> 3) - ((int32_t) dev->calib.par_t1 << 1);
800129b0:	01a45703          	lhu	a4,26(s0)
			adc_temp = (uint32_t) (((uint32_t) buff[5] * 4096) | ((uint32_t) buff[6] * 16)
800129b4:	00479913          	slli	s2,a5,0x4
				| ((uint32_t) buff[7] / 16));
800129b8:	00465613          	srli	a2,a2,0x4
			adc_temp = (uint32_t) (((uint32_t) buff[5] * 4096) | ((uint32_t) buff[6] * 16)
800129bc:	00c96933          	or	s2,s2,a2
800129c0:	00c69693          	slli	a3,a3,0xc
800129c4:	00d96933          	or	s2,s2,a3
	var1 = ((int32_t) temp_adc >> 3) - ((int32_t) dev->calib.par_t1 << 1);
800129c8:	00171793          	slli	a5,a4,0x1
800129cc:	40395913          	srai	s2,s2,0x3
800129d0:	40f90933          	sub	s2,s2,a5
800129d4:	41f95713          	srai	a4,s2,0x1f
	var3 = ((var1 >> 1) * (var1 >> 1)) >> 12;
800129d8:	01f71793          	slli	a5,a4,0x1f
800129dc:	00195613          	srli	a2,s2,0x1
800129e0:	00c7e633          	or	a2,a5,a2
800129e4:	00070693          	mv	a3,a4
800129e8:	00070593          	mv	a1,a4
800129ec:	00060513          	mv	a0,a2
800129f0:	01012623          	sw	a6,12(sp)
800129f4:	00e12023          	sw	a4,0(sp)
800129f8:	158060ef          	jal	ra,80018b50 <__muldi3>
	var3 = ((var3) * ((int32_t) dev->calib.par_t3 << 4)) >> 14;
800129fc:	01e40683          	lb	a3,30(s0)
	var3 = ((var1 >> 1) * (var1 >> 1)) >> 12;
80012a00:	01459793          	slli	a5,a1,0x14
80012a04:	00c55513          	srli	a0,a0,0xc
	var3 = ((var3) * ((int32_t) dev->calib.par_t3 << 4)) >> 14;
80012a08:	00469613          	slli	a2,a3,0x4
80012a0c:	00a7e533          	or	a0,a5,a0
80012a10:	41f6d693          	srai	a3,a3,0x1f
80012a14:	40c5d593          	srai	a1,a1,0xc
80012a18:	138060ef          	jal	ra,80018b50 <__muldi3>
	var2 = (var1 * (int32_t) dev->calib.par_t2) >> 11;
80012a1c:	00012703          	lw	a4,0(sp)
	var3 = ((var3) * ((int32_t) dev->calib.par_t3 << 4)) >> 14;
80012a20:	00050793          	mv	a5,a0
	var2 = (var1 * (int32_t) dev->calib.par_t2) >> 11;
80012a24:	01c41503          	lh	a0,28(s0)
80012a28:	00090613          	mv	a2,s2
80012a2c:	00070693          	mv	a3,a4
	var3 = ((var3) * ((int32_t) dev->calib.par_t3 << 4)) >> 14;
80012a30:	00058b93          	mv	s7,a1
	var2 = (var1 * (int32_t) dev->calib.par_t2) >> 11;
80012a34:	41f55593          	srai	a1,a0,0x1f
	var3 = ((var3) * ((int32_t) dev->calib.par_t3 << 4)) >> 14;
80012a38:	00078913          	mv	s2,a5
	var2 = (var1 * (int32_t) dev->calib.par_t2) >> 11;
80012a3c:	114060ef          	jal	ra,80018b50 <__muldi3>
80012a40:	01559593          	slli	a1,a1,0x15
80012a44:	00b55793          	srli	a5,a0,0xb
	var3 = ((var3) * ((int32_t) dev->calib.par_t3 << 4)) >> 14;
80012a48:	012b9b93          	slli	s7,s7,0x12
80012a4c:	00e95513          	srli	a0,s2,0xe
80012a50:	00abe533          	or	a0,s7,a0
	var2 = (var1 * (int32_t) dev->calib.par_t2) >> 11;
80012a54:	00f5e7b3          	or	a5,a1,a5
	dev->calib.t_fine = (int32_t) (var2 + var3);
80012a58:	00a787b3          	add	a5,a5,a0
	calc_temp = (int16_t) (((dev->calib.t_fine * 5) + 128) >> 8);
80012a5c:	00279b93          	slli	s7,a5,0x2
	var1 = (((int32_t)dev->calib.t_fine) >> 1) - 64000;
80012a60:	ffff06b7          	lui	a3,0xffff0
80012a64:	60068693          	addi	a3,a3,1536 # ffff0600 <__clzsi2+0x7ffd796c>
	calc_temp = (int16_t) (((dev->calib.t_fine * 5) + 128) >> 8);
80012a68:	00fb8bb3          	add	s7,s7,a5
	var1 = (((int32_t)dev->calib.t_fine) >> 1) - 64000;
80012a6c:	4017d713          	srai	a4,a5,0x1
80012a70:	00d70733          	add	a4,a4,a3
	calc_temp = (int16_t) (((dev->calib.t_fine * 5) + 128) >> 8);
80012a74:	080b8b93          	addi	s7,s7,128
	var2 = ((((var1 >> 2) * (var1 >> 2)) >> 11) *
80012a78:	40275593          	srai	a1,a4,0x2
	dev->calib.t_fine = (int32_t) (var2 + var3);
80012a7c:	02f42a23          	sw	a5,52(s0)
	calc_temp = (int16_t) (((dev->calib.t_fine * 5) + 128) >> 8);
80012a80:	408bdb93          	srai	s7,s7,0x8
	var2 = ((((var1 >> 2) * (var1 >> 2)) >> 11) *
80012a84:	00058513          	mv	a0,a1
	calc_temp = (int16_t) (((dev->calib.t_fine * 5) + 128) >> 8);
80012a88:	01799223          	sh	s7,4(s3)
	var2 = ((((var1 >> 2) * (var1 >> 2)) >> 11) *
80012a8c:	00e12023          	sw	a4,0(sp)
80012a90:	09c060ef          	jal	ra,80018b2c <__mulsi3>
80012a94:	02a40583          	lb	a1,42(s0)
80012a98:	00050793          	mv	a5,a0
80012a9c:	40b55513          	srai	a0,a0,0xb
80012aa0:	00f12423          	sw	a5,8(sp)
80012aa4:	088060ef          	jal	ra,80018b2c <__mulsi3>
	var2 = var2 + ((var1 * (int32_t)dev->calib.par_p5) << 1);
80012aa8:	00012703          	lw	a4,0(sp)
	var2 = ((((var1 >> 2) * (var1 >> 2)) >> 11) *
80012aac:	00050693          	mv	a3,a0
	var2 = var2 + ((var1 * (int32_t)dev->calib.par_p5) << 1);
80012ab0:	02841503          	lh	a0,40(s0)
80012ab4:	00070593          	mv	a1,a4
	var2 = ((((var1 >> 2) * (var1 >> 2)) >> 11) *
80012ab8:	4026d693          	srai	a3,a3,0x2
	var2 = var2 + ((var1 * (int32_t)dev->calib.par_p5) << 1);
80012abc:	00e12223          	sw	a4,4(sp)
	var2 = ((((var1 >> 2) * (var1 >> 2)) >> 11) *
80012ac0:	00d12023          	sw	a3,0(sp)
	var2 = var2 + ((var1 * (int32_t)dev->calib.par_p5) << 1);
80012ac4:	068060ef          	jal	ra,80018b2c <__mulsi3>
		(((int32_t)dev->calib.par_p2 * var1) >> 1);
80012ac8:	00412703          	lw	a4,4(sp)
	var2 = var2 + ((var1 * (int32_t)dev->calib.par_p5) << 1);
80012acc:	00012683          	lw	a3,0(sp)
80012ad0:	00050913          	mv	s2,a0
		(((int32_t)dev->calib.par_p2 * var1) >> 1);
80012ad4:	02241503          	lh	a0,34(s0)
80012ad8:	00070593          	mv	a1,a4
	var2 = var2 + ((var1 * (int32_t)dev->calib.par_p5) << 1);
80012adc:	00191913          	slli	s2,s2,0x1
80012ae0:	00d90933          	add	s2,s2,a3
		(((int32_t)dev->calib.par_p2 * var1) >> 1);
80012ae4:	048060ef          	jal	ra,80018b2c <__mulsi3>
		((int32_t)dev->calib.par_p3 << 5)) >> 3) +
80012ae8:	02440683          	lb	a3,36(s0)
	var1 = (((((var1 >> 2) * (var1 >> 2)) >> 13) *
80012aec:	00812783          	lw	a5,8(sp)
		(((int32_t)dev->calib.par_p2 * var1) >> 1);
80012af0:	40155713          	srai	a4,a0,0x1
	var1 = (((((var1 >> 2) * (var1 >> 2)) >> 13) *
80012af4:	00569513          	slli	a0,a3,0x5
80012af8:	40d7d593          	srai	a1,a5,0xd
		(((int32_t)dev->calib.par_p2 * var1) >> 1);
80012afc:	00e12023          	sw	a4,0(sp)
	var1 = (((((var1 >> 2) * (var1 >> 2)) >> 13) *
80012b00:	02c060ef          	jal	ra,80018b2c <__mulsi3>
80012b04:	00012703          	lw	a4,0(sp)
		((int32_t)dev->calib.par_p3 << 5)) >> 3) +
80012b08:	40355793          	srai	a5,a0,0x3
	var1 = ((32768 + var1) * (int32_t)dev->calib.par_p1) >> 15;
80012b0c:	02045583          	lhu	a1,32(s0)
	var1 = (((((var1 >> 2) * (var1 >> 2)) >> 13) *
80012b10:	00e787b3          	add	a5,a5,a4
	var1 = var1 >> 18;
80012b14:	4127d793          	srai	a5,a5,0x12
	var1 = ((32768 + var1) * (int32_t)dev->calib.par_p1) >> 15;
80012b18:	00008537          	lui	a0,0x8
80012b1c:	00a78533          	add	a0,a5,a0
80012b20:	00c060ef          	jal	ra,80018b2c <__mulsi3>
				| ((uint32_t) buff[4] / 16));
80012b24:	00c12803          	lw	a6,12(sp)
	var2 = (var2 >> 2) + ((int32_t)dev->calib.par_p4 << 16);
80012b28:	02641683          	lh	a3,38(s0)
			adc_pres = (uint32_t) (((uint32_t) buff[2] * 4096) | ((uint32_t) buff[3] * 16)
80012b2c:	004c1793          	slli	a5,s8,0x4
				| ((uint32_t) buff[4] / 16));
80012b30:	00485813          	srli	a6,a6,0x4
	var2 = (var2 >> 2) + ((int32_t)dev->calib.par_p4 << 16);
80012b34:	40295913          	srai	s2,s2,0x2
			adc_pres = (uint32_t) (((uint32_t) buff[2] * 4096) | ((uint32_t) buff[3] * 16)
80012b38:	0107e7b3          	or	a5,a5,a6
80012b3c:	00cd9d93          	slli	s11,s11,0xc
	var2 = (var2 >> 2) + ((int32_t)dev->calib.par_p4 << 16);
80012b40:	01069693          	slli	a3,a3,0x10
			adc_pres = (uint32_t) (((uint32_t) buff[2] * 4096) | ((uint32_t) buff[3] * 16)
80012b44:	01b7e7b3          	or	a5,a5,s11
	var2 = (var2 >> 2) + ((int32_t)dev->calib.par_p4 << 16);
80012b48:	012686b3          	add	a3,a3,s2
	pressure_comp = 1048576 - pres_adc;
80012b4c:	00100737          	lui	a4,0x100
80012b50:	40f70733          	sub	a4,a4,a5
	pressure_comp = (int32_t)((pressure_comp - (var2 >> 12)) * ((uint32_t)3125));
80012b54:	40c6d793          	srai	a5,a3,0xc
80012b58:	40f70733          	sub	a4,a4,a5
80012b5c:	00171793          	slli	a5,a4,0x1
80012b60:	00e787b3          	add	a5,a5,a4
80012b64:	00679693          	slli	a3,a5,0x6
80012b68:	00d787b3          	add	a5,a5,a3
80012b6c:	00279793          	slli	a5,a5,0x2
80012b70:	00e787b3          	add	a5,a5,a4
80012b74:	00279793          	slli	a5,a5,0x2
80012b78:	00e787b3          	add	a5,a5,a4
	if (pressure_comp >= BME680_MAX_OVERFLOW_VAL)
80012b7c:	40000737          	lui	a4,0x40000
	var1 = ((32768 + var1) * (int32_t)dev->calib.par_p1) >> 15;
80012b80:	40f55593          	srai	a1,a0,0xf
	if (pressure_comp >= BME680_MAX_OVERFLOW_VAL)
80012b84:	30e7ca63          	blt	a5,a4,80012e98 <bme680_get_sensor_data+0x67c>
		pressure_comp = ((pressure_comp / var1) << 1);
80012b88:	00078513          	mv	a0,a5
80012b8c:	054060ef          	jal	ra,80018be0 <__divsi3>
80012b90:	00151c13          	slli	s8,a0,0x1
	var1 = ((int32_t)dev->calib.par_p9 * (int32_t)(((pressure_comp >> 3) *
80012b94:	403c5593          	srai	a1,s8,0x3
80012b98:	00058513          	mv	a0,a1
80012b9c:	791050ef          	jal	ra,80018b2c <__mulsi3>
80012ba0:	02e41583          	lh	a1,46(s0)
80012ba4:	40d55513          	srai	a0,a0,0xd
	var3 = ((int32_t)(pressure_comp >> 8) * (int32_t)(pressure_comp >> 8) *
80012ba8:	408c5d93          	srai	s11,s8,0x8
	var1 = ((int32_t)dev->calib.par_p9 * (int32_t)(((pressure_comp >> 3) *
80012bac:	781050ef          	jal	ra,80018b2c <__mulsi3>
	var2 = ((int32_t)(pressure_comp >> 2) *
80012bb0:	02c41583          	lh	a1,44(s0)
	var1 = ((int32_t)dev->calib.par_p9 * (int32_t)(((pressure_comp >> 3) *
80012bb4:	00050913          	mv	s2,a0
	var2 = ((int32_t)(pressure_comp >> 2) *
80012bb8:	402c5513          	srai	a0,s8,0x2
80012bbc:	771050ef          	jal	ra,80018b2c <__mulsi3>
80012bc0:	40d55793          	srai	a5,a0,0xd
	var3 = ((int32_t)(pressure_comp >> 8) * (int32_t)(pressure_comp >> 8) *
80012bc4:	000d8593          	mv	a1,s11
	var1 = ((int32_t)dev->calib.par_p9 * (int32_t)(((pressure_comp >> 3) *
80012bc8:	40c95913          	srai	s2,s2,0xc
	var3 = ((int32_t)(pressure_comp >> 8) * (int32_t)(pressure_comp >> 8) *
80012bcc:	000d8513          	mv	a0,s11
	pressure_comp = (int32_t)(pressure_comp) + ((var1 + var2 + var3 +
80012bd0:	00f90933          	add	s2,s2,a5
	var3 = ((int32_t)(pressure_comp >> 8) * (int32_t)(pressure_comp >> 8) *
80012bd4:	759050ef          	jal	ra,80018b2c <__mulsi3>
80012bd8:	000d8593          	mv	a1,s11
80012bdc:	751050ef          	jal	ra,80018b2c <__mulsi3>
		(int32_t)(pressure_comp >> 8) *
80012be0:	03044583          	lbu	a1,48(s0)
			adc_hum = (uint16_t) (((uint32_t) buff[8] * 256) | (uint32_t) buff[9]);
80012be4:	008c9c93          	slli	s9,s9,0x8
	var1 = (int32_t) (hum_adc - ((int32_t) ((int32_t) dev->calib.par_h1 * 16)))
80012be8:	01acecb3          	or	s9,s9,s10
		(int32_t)(pressure_comp >> 8) *
80012bec:	741050ef          	jal	ra,80018b2c <__mulsi3>
		((int32_t)dev->calib.par_p7 << 7)) >> 4);
80012bf0:	02b40783          	lb	a5,43(s0)
	var3 = ((int32_t)(pressure_comp >> 8) * (int32_t)(pressure_comp >> 8) *
80012bf4:	41155513          	srai	a0,a0,0x11
	pressure_comp = (int32_t)(pressure_comp) + ((var1 + var2 + var3 +
80012bf8:	00a90933          	add	s2,s2,a0
		((int32_t)dev->calib.par_p7 << 7)) >> 4);
80012bfc:	00779793          	slli	a5,a5,0x7
	pressure_comp = (int32_t)(pressure_comp) + ((var1 + var2 + var3 +
80012c00:	00f90933          	add	s2,s2,a5
		((int32_t)dev->calib.par_p7 << 7)) >> 4);
80012c04:	40495913          	srai	s2,s2,0x4
	pressure_comp = (int32_t)(pressure_comp) + ((var1 + var2 + var3 +
80012c08:	012c0c33          	add	s8,s8,s2
				data->pressure = calc_pressure(adc_pres, dev);
80012c0c:	0189a423          	sw	s8,8(s3)
			+ (((temp_scaled * ((temp_scaled * (int32_t) dev->calib.par_h5) / ((int32_t) 100))) >> 6)
80012c10:	01240503          	lb	a0,18(s0)
80012c14:	000b8593          	mv	a1,s7
80012c18:	715050ef          	jal	ra,80018b2c <__mulsi3>
80012c1c:	06400593          	li	a1,100
80012c20:	7c1050ef          	jal	ra,80018be0 <__divsi3>
80012c24:	000b8593          	mv	a1,s7
80012c28:	705050ef          	jal	ra,80018b2c <__mulsi3>
				/ ((int32_t) 100)) + (int32_t) (1 << 14))) >> 10;
80012c2c:	06400593          	li	a1,100
80012c30:	40655513          	srai	a0,a0,0x6
80012c34:	7ad050ef          	jal	ra,80018be0 <__divsi3>
80012c38:	00050913          	mv	s2,a0
		* (((temp_scaled * (int32_t) dev->calib.par_h4) / ((int32_t) 100))
80012c3c:	01140503          	lb	a0,17(s0)
80012c40:	000b8593          	mv	a1,s7
80012c44:	6e9050ef          	jal	ra,80018b2c <__mulsi3>
80012c48:	06400593          	li	a1,100
80012c4c:	795050ef          	jal	ra,80018be0 <__divsi3>
80012c50:	00e45583          	lhu	a1,14(s0)
			+ (((temp_scaled * ((temp_scaled * (int32_t) dev->calib.par_h5) / ((int32_t) 100))) >> 6)
80012c54:	00a90933          	add	s2,s2,a0
				/ ((int32_t) 100)) + (int32_t) (1 << 14))) >> 10;
80012c58:	00004537          	lui	a0,0x4
		* (((temp_scaled * (int32_t) dev->calib.par_h4) / ((int32_t) 100))
80012c5c:	00a90533          	add	a0,s2,a0
80012c60:	6cd050ef          	jal	ra,80018b2c <__mulsi3>
	var1 = (int32_t) (hum_adc - ((int32_t) ((int32_t) dev->calib.par_h1 * 16)))
80012c64:	00c45783          	lhu	a5,12(s0)
		* (((temp_scaled * (int32_t) dev->calib.par_h4) / ((int32_t) 100))
80012c68:	00050913          	mv	s2,a0
		- (((temp_scaled * (int32_t) dev->calib.par_h3) / ((int32_t) 100)) >> 1);
80012c6c:	01040503          	lb	a0,16(s0)
	var1 = (int32_t) (hum_adc - ((int32_t) ((int32_t) dev->calib.par_h1 * 16)))
80012c70:	00479793          	slli	a5,a5,0x4
		- (((temp_scaled * (int32_t) dev->calib.par_h3) / ((int32_t) 100)) >> 1);
80012c74:	000b8593          	mv	a1,s7
	var1 = (int32_t) (hum_adc - ((int32_t) ((int32_t) dev->calib.par_h1 * 16)))
80012c78:	40fc8cb3          	sub	s9,s9,a5
		- (((temp_scaled * (int32_t) dev->calib.par_h3) / ((int32_t) 100)) >> 1);
80012c7c:	6b1050ef          	jal	ra,80018b2c <__mulsi3>
80012c80:	06400593          	li	a1,100
80012c84:	75d050ef          	jal	ra,80018be0 <__divsi3>
	var2 = ((int32_t) dev->calib.par_h2
80012c88:	40a95913          	srai	s2,s2,0xa
		- (((temp_scaled * (int32_t) dev->calib.par_h3) / ((int32_t) 100)) >> 1);
80012c8c:	40155593          	srai	a1,a0,0x1
	var3 = var1 * var2;
80012c90:	40bc85b3          	sub	a1,s9,a1
80012c94:	00090513          	mv	a0,s2
80012c98:	695050ef          	jal	ra,80018b2c <__mulsi3>
80012c9c:	00050913          	mv	s2,a0
	var4 = ((var4) + ((temp_scaled * (int32_t) dev->calib.par_h7) / ((int32_t) 100))) >> 4;
80012ca0:	01440503          	lb	a0,20(s0)
80012ca4:	000b8593          	mv	a1,s7
80012ca8:	685050ef          	jal	ra,80018b2c <__mulsi3>
80012cac:	06400593          	li	a1,100
80012cb0:	731050ef          	jal	ra,80018be0 <__divsi3>
	var4 = (int32_t) dev->calib.par_h6 << 7;
80012cb4:	01344783          	lbu	a5,19(s0)
	var5 = ((var3 >> 14) * (var3 >> 14)) >> 10;
80012cb8:	40e95593          	srai	a1,s2,0xe
	var4 = ((var4) + ((temp_scaled * (int32_t) dev->calib.par_h7) / ((int32_t) 100))) >> 4;
80012cbc:	00050b93          	mv	s7,a0
	var4 = (int32_t) dev->calib.par_h6 << 7;
80012cc0:	00779793          	slli	a5,a5,0x7
	var5 = ((var3 >> 14) * (var3 >> 14)) >> 10;
80012cc4:	00058513          	mv	a0,a1
	var4 = ((var4) + ((temp_scaled * (int32_t) dev->calib.par_h7) / ((int32_t) 100))) >> 4;
80012cc8:	00fb8bb3          	add	s7,s7,a5
	var5 = ((var3 >> 14) * (var3 >> 14)) >> 10;
80012ccc:	661050ef          	jal	ra,80018b2c <__mulsi3>
	var6 = (var4 * var5) >> 1;
80012cd0:	404bd593          	srai	a1,s7,0x4
80012cd4:	40a55513          	srai	a0,a0,0xa
80012cd8:	655050ef          	jal	ra,80018b2c <__mulsi3>
80012cdc:	40155713          	srai	a4,a0,0x1
	calc_hum = (((var3 + var6) >> 10) * ((int32_t) 1000)) >> 12;
80012ce0:	01270733          	add	a4,a4,s2
80012ce4:	40a75713          	srai	a4,a4,0xa
80012ce8:	00571793          	slli	a5,a4,0x5
80012cec:	40e787b3          	sub	a5,a5,a4
80012cf0:	00279793          	slli	a5,a5,0x2
80012cf4:	00e787b3          	add	a5,a5,a4
80012cf8:	00379793          	slli	a5,a5,0x3
80012cfc:	40c7d793          	srai	a5,a5,0xc
	return (uint32_t) calc_hum;
80012d00:	fff7c713          	not	a4,a5
80012d04:	41f75713          	srai	a4,a4,0x1f
80012d08:	00e7f7b3          	and	a5,a5,a4
80012d0c:	00018737          	lui	a4,0x18
80012d10:	6a070713          	addi	a4,a4,1696 # 186a0 <font6x8-0x7ffe798c>
80012d14:	00f75463          	bge	a4,a5,80012d1c <bme680_get_sensor_data+0x500>
80012d18:	00070793          	mv	a5,a4
				data->humidity = calc_humidity(adc_hum, dev);
80012d1c:	00f9a623          	sw	a5,12(s3)
				data->gas_resistance = calc_gas_resistance(adc_gas_res, gas_range, dev);
80012d20:	03a40683          	lb	a3,58(s0)
	uint32_t lookupTable1[16] = { UINT32_C(2147483647), UINT32_C(2147483647), UINT32_C(2147483647), UINT32_C(2147483647),
80012d24:	800007b7          	lui	a5,0x80000
80012d28:	26c78613          	addi	a2,a5,620 # 8000026c <__clzsi2+0xfffe75d8>
80012d2c:	02010713          	addi	a4,sp,32
80012d30:	26c78793          	addi	a5,a5,620
80012d34:	04060313          	addi	t1,a2,64
80012d38:	0007a883          	lw	a7,0(a5)
80012d3c:	0047a803          	lw	a6,4(a5)
80012d40:	0087a503          	lw	a0,8(a5)
80012d44:	00c7a583          	lw	a1,12(a5)
80012d48:	01172023          	sw	a7,0(a4)
80012d4c:	01072223          	sw	a6,4(a4)
80012d50:	00a72423          	sw	a0,8(a4)
80012d54:	00b72623          	sw	a1,12(a4)
80012d58:	01078793          	addi	a5,a5,16
80012d5c:	01070713          	addi	a4,a4,16
80012d60:	fc679ce3          	bne	a5,t1,80012d38 <bme680_get_sensor_data+0x51c>
	uint32_t lookupTable2[16] = { UINT32_C(4096000000), UINT32_C(2048000000), UINT32_C(1024000000), UINT32_C(512000000),
80012d64:	06010713          	addi	a4,sp,96
80012d68:	08060613          	addi	a2,a2,128
80012d6c:	0007a883          	lw	a7,0(a5)
80012d70:	0047a803          	lw	a6,4(a5)
80012d74:	0087a503          	lw	a0,8(a5)
80012d78:	00c7a583          	lw	a1,12(a5)
80012d7c:	01172023          	sw	a7,0(a4)
80012d80:	01072223          	sw	a6,4(a4)
80012d84:	00a72423          	sw	a0,8(a4)
80012d88:	00b72623          	sw	a1,12(a4)
80012d8c:	01078793          	addi	a5,a5,16
80012d90:	01070713          	addi	a4,a4,16
80012d94:	fcc79ce3          	bne	a5,a2,80012d6c <bme680_get_sensor_data+0x550>
		((int64_t) lookupTable1[gas_range])) >> 16;
80012d98:	00fb7b93          	andi	s7,s6,15
	var1 = (int64_t) ((1340 + (5 * (int64_t) dev->calib.range_sw_err)) *
80012d9c:	41f6d713          	srai	a4,a3,0x1f
		((int64_t) lookupTable1[gas_range])) >> 16;
80012da0:	002b9b93          	slli	s7,s7,0x2
	var1 = (int64_t) ((1340 + (5 * (int64_t) dev->calib.range_sw_err)) *
80012da4:	00271793          	slli	a5,a4,0x2
		((int64_t) lookupTable1[gas_range])) >> 16;
80012da8:	01010813          	addi	a6,sp,16
	var1 = (int64_t) ((1340 + (5 * (int64_t) dev->calib.range_sw_err)) *
80012dac:	01e6d613          	srli	a2,a3,0x1e
80012db0:	00269593          	slli	a1,a3,0x2
		((int64_t) lookupTable1[gas_range])) >> 16;
80012db4:	090b8513          	addi	a0,s7,144
80012db8:	01050bb3          	add	s7,a0,a6
	var1 = (int64_t) ((1340 + (5 * (int64_t) dev->calib.range_sw_err)) *
80012dbc:	00d586b3          	add	a3,a1,a3
80012dc0:	00f667b3          	or	a5,a2,a5
80012dc4:	00e787b3          	add	a5,a5,a4
80012dc8:	53c68513          	addi	a0,a3,1340
80012dcc:	00b6b5b3          	sltu	a1,a3,a1
80012dd0:	f80ba603          	lw	a2,-128(s7)
80012dd4:	00f587b3          	add	a5,a1,a5
			adc_gas_res = (uint16_t) ((uint32_t) buff[13] * 4 | (((uint32_t) buff[14]) / 64));
80012dd8:	006b5b13          	srli	s6,s6,0x6
	var1 = (int64_t) ((1340 + (5 * (int64_t) dev->calib.range_sw_err)) *
80012ddc:	00d535b3          	sltu	a1,a0,a3
			adc_gas_res = (uint16_t) ((uint32_t) buff[13] * 4 | (((uint32_t) buff[14]) / 64));
80012de0:	002a9a93          	slli	s5,s5,0x2
	var1 = (int64_t) ((1340 + (5 * (int64_t) dev->calib.range_sw_err)) *
80012de4:	00000693          	li	a3,0
80012de8:	00f585b3          	add	a1,a1,a5
	var2 = (((int64_t) ((int64_t) gas_res_adc << 15) - (int64_t) (16777216)) + var1);
80012dec:	016aeab3          	or	s5,s5,s6
	var1 = (int64_t) ((1340 + (5 * (int64_t) dev->calib.range_sw_err)) *
80012df0:	561050ef          	jal	ra,80018b50 <__muldi3>
	var2 = (((int64_t) ((int64_t) gas_res_adc << 15) - (int64_t) (16777216)) + var1);
80012df4:	00fa9a93          	slli	s5,s5,0xf
80012df8:	ff000937          	lui	s2,0xff000
	var1 = (int64_t) ((1340 + (5 * (int64_t) dev->calib.range_sw_err)) *
80012dfc:	01059793          	slli	a5,a1,0x10
	var2 = (((int64_t) ((int64_t) gas_res_adc << 15) - (int64_t) (16777216)) + var1);
80012e00:	012a8933          	add	s2,s5,s2
	var1 = (int64_t) ((1340 + (5 * (int64_t) dev->calib.range_sw_err)) *
80012e04:	01055613          	srli	a2,a0,0x10
80012e08:	00c7e633          	or	a2,a5,a2
	var2 = (((int64_t) ((int64_t) gas_res_adc << 15) - (int64_t) (16777216)) + var1);
80012e0c:	01593ab3          	sltu	s5,s2,s5
	var3 = (((int64_t) lookupTable2[gas_range] * (int64_t) var1) >> 9);
80012e10:	fc0ba503          	lw	a0,-64(s7)
	var2 = (((int64_t) ((int64_t) gas_res_adc << 15) - (int64_t) (16777216)) + var1);
80012e14:	00c90b33          	add	s6,s2,a2
	var1 = (int64_t) ((1340 + (5 * (int64_t) dev->calib.range_sw_err)) *
80012e18:	4105d693          	srai	a3,a1,0x10
	var2 = (((int64_t) ((int64_t) gas_res_adc << 15) - (int64_t) (16777216)) + var1);
80012e1c:	fffa8a93          	addi	s5,s5,-1
80012e20:	00da8ab3          	add	s5,s5,a3
80012e24:	012b3933          	sltu	s2,s6,s2
	var3 = (((int64_t) lookupTable2[gas_range] * (int64_t) var1) >> 9);
80012e28:	00000593          	li	a1,0
80012e2c:	525050ef          	jal	ra,80018b50 <__muldi3>
	var2 = (((int64_t) ((int64_t) gas_res_adc << 15) - (int64_t) (16777216)) + var1);
80012e30:	01590933          	add	s2,s2,s5
	var3 = (((int64_t) lookupTable2[gas_range] * (int64_t) var1) >> 9);
80012e34:	01759693          	slli	a3,a1,0x17
80012e38:	00955793          	srli	a5,a0,0x9
	calc_gas_res = (uint32_t) ((var3 + ((int64_t) var2 >> 1)) / (int64_t) var2);
80012e3c:	01f91713          	slli	a4,s2,0x1f
80012e40:	001b5513          	srli	a0,s6,0x1
	var3 = (((int64_t) lookupTable2[gas_range] * (int64_t) var1) >> 9);
80012e44:	00f6e7b3          	or	a5,a3,a5
	calc_gas_res = (uint32_t) ((var3 + ((int64_t) var2 >> 1)) / (int64_t) var2);
80012e48:	00a76533          	or	a0,a4,a0
80012e4c:	00a78533          	add	a0,a5,a0
80012e50:	40195713          	srai	a4,s2,0x1
	var3 = (((int64_t) lookupTable2[gas_range] * (int64_t) var1) >> 9);
80012e54:	4095d593          	srai	a1,a1,0x9
	calc_gas_res = (uint32_t) ((var3 + ((int64_t) var2 >> 1)) / (int64_t) var2);
80012e58:	00f537b3          	sltu	a5,a0,a5
80012e5c:	00e585b3          	add	a1,a1,a4
80012e60:	000b0613          	mv	a2,s6
80012e64:	00090693          	mv	a3,s2
80012e68:	00b785b3          	add	a1,a5,a1
80012e6c:	175020ef          	jal	ra,800157e0 <__divdi3>
				data->gas_resistance = calc_gas_resistance(adc_gas_res, gas_range, dev);
80012e70:	00a9a823          	sw	a0,16(s3)
		if (rslt == BME680_OK) {
80012e74:	ac049ae3          	bnez	s1,80012948 <bme680_get_sensor_data+0x12c>
			if (data->status & BME680_NEW_DATA_MSK)
80012e78:	018a1a13          	slli	s4,s4,0x18
80012e7c:	418a5a13          	srai	s4,s4,0x18
80012e80:	000a4663          	bltz	s4,80012e8c <bme680_get_sensor_data+0x670>
				dev->new_fields = 0;
80012e84:	040404a3          	sb	zero,73(s0)
80012e88:	ac1ff06f          	j	80012948 <bme680_get_sensor_data+0x12c>
				dev->new_fields = 1;
80012e8c:	00100793          	li	a5,1
80012e90:	04f404a3          	sb	a5,73(s0)
80012e94:	ab5ff06f          	j	80012948 <bme680_get_sensor_data+0x12c>
		pressure_comp = ((pressure_comp << 1) / var1);
80012e98:	00179513          	slli	a0,a5,0x1
80012e9c:	545050ef          	jal	ra,80018be0 <__divsi3>
80012ea0:	00050c13          	mv	s8,a0
80012ea4:	cf1ff06f          	j	80012b94 <bme680_get_sensor_data+0x378>
		rslt = BME680_E_NULL_PTR;
80012ea8:	fff00493          	li	s1,-1
80012eac:	a25ff06f          	j	800128d0 <bme680_get_sensor_data+0xb4>
			rslt = BME680_E_COM_FAIL;
80012eb0:	ffe00493          	li	s1,-2
80012eb4:	a1dff06f          	j	800128d0 <bme680_get_sensor_data+0xb4>
		rslt = BME680_E_NULL_PTR;
80012eb8:	fff00493          	li	s1,-1
80012ebc:	a8dff06f          	j	80012948 <bme680_get_sensor_data+0x12c>

80012ec0 <user_delay_ms>:
}


void user_delay_ms(uint32_t period)
{
    HAL_Timebase_DelayMS(period);
80012ec0:	ffdfd06f          	j	80010ebc <HAL_Timebase_DelayMS>

80012ec4 <_putchar>:
{
80012ec4:	ff010113          	addi	sp,sp,-16
80012ec8:	00812423          	sw	s0,8(sp)
80012ecc:	00112623          	sw	ra,12(sp)
80012ed0:	00050413          	mv	s0,a0
    HAL_UART_PutChar(character);
80012ed4:	f5dfd0ef          	jal	ra,80010e30 <HAL_UART_PutChar>
    HAL_OLED_PutChar(character);
80012ed8:	00040513          	mv	a0,s0
}
80012edc:	00812403          	lw	s0,8(sp)
80012ee0:	00c12083          	lw	ra,12(sp)
80012ee4:	01010113          	addi	sp,sp,16
    HAL_OLED_PutChar(character);
80012ee8:	911fd06f          	j	800107f8 <HAL_OLED_PutChar>

80012eec <printf>:
{
80012eec:	ec010113          	addi	sp,sp,-320
  va_start(vl, s);
80012ef0:	12410313          	addi	t1,sp,292
{
80012ef4:	12b12223          	sw	a1,292(sp)
80012ef8:	12c12423          	sw	a2,296(sp)
80012efc:	12d12623          	sw	a3,300(sp)
  vsnprintf(out, 255, s, vl);
80012f00:	00050613          	mv	a2,a0
80012f04:	00030693          	mv	a3,t1
80012f08:	0ff00593          	li	a1,255
80012f0c:	01010513          	addi	a0,sp,16
{
80012f10:	10912a23          	sw	s1,276(sp)
80012f14:	10112e23          	sw	ra,284(sp)
80012f18:	10812c23          	sw	s0,280(sp)
80012f1c:	12e12823          	sw	a4,304(sp)
80012f20:	12f12a23          	sw	a5,308(sp)
80012f24:	13012c23          	sw	a6,312(sp)
80012f28:	13112e23          	sw	a7,316(sp)
  va_start(vl, s);
80012f2c:	00612623          	sw	t1,12(sp)
  vsnprintf(out, 255, s, vl);
80012f30:	039020ef          	jal	ra,80015768 <vsnprintf_>
  for(uint8_t i=0; out[i] != (char) 0; i++)
80012f34:	01014483          	lbu	s1,16(sp)
80012f38:	02048863          	beqz	s1,80012f68 <printf+0x7c>
80012f3c:	00000413          	li	s0,0
    HAL_UART_PutChar(character);
80012f40:	00048513          	mv	a0,s1
80012f44:	eedfd0ef          	jal	ra,80010e30 <HAL_UART_PutChar>
    HAL_OLED_PutChar(character);
80012f48:	00048513          	mv	a0,s1
  for(uint8_t i=0; out[i] != (char) 0; i++)
80012f4c:	00140413          	addi	s0,s0,1
    HAL_OLED_PutChar(character);
80012f50:	8a9fd0ef          	jal	ra,800107f8 <HAL_OLED_PutChar>
  for(uint8_t i=0; out[i] != (char) 0; i++)
80012f54:	0ff47413          	zext.b	s0,s0
80012f58:	11040793          	addi	a5,s0,272
80012f5c:	002787b3          	add	a5,a5,sp
80012f60:	f007c483          	lbu	s1,-256(a5)
80012f64:	fc049ee3          	bnez	s1,80012f40 <printf+0x54>
    HAL_OLED_DrawGFXBuffer();
80012f68:	c80fd0ef          	jal	ra,800103e8 <HAL_OLED_DrawGFXBuffer>
}
80012f6c:	11c12083          	lw	ra,284(sp)
80012f70:	11812403          	lw	s0,280(sp)
80012f74:	11412483          	lw	s1,276(sp)
80012f78:	00000513          	li	a0,0
80012f7c:	14010113          	addi	sp,sp,320
80012f80:	00008067          	ret

80012f84 <user_i2c_write>:

    return 0;
}

int8_t user_i2c_write(uint8_t dev_id, uint8_t reg_addr, uint8_t *reg_data, uint16_t len) // correct, don't change
{   
80012f84:	fe010113          	addi	sp,sp,-32
80012f88:	00112e23          	sw	ra,28(sp)
80012f8c:	00812c23          	sw	s0,24(sp)
80012f90:	00912a23          	sw	s1,20(sp)
80012f94:	01212823          	sw	s2,16(sp)
80012f98:	01312623          	sw	s3,12(sp)
80012f9c:	01412423          	sw	s4,8(sp)
    int8_t res = 0;

    while(len) {
80012fa0:	0a068863          	beqz	a3,80013050 <user_i2c_write+0xcc>
80012fa4:	00068413          	mv	s0,a3
80012fa8:	00058a13          	mv	s4,a1
80012fac:	00060913          	mv	s2,a2
    HAL_I2C_SetPins(PIN_SCL | PIN_SDA);
80012fb0:	00300513          	li	a0,3
        res = BME680_WriteRegister(reg_addr, *reg_data);
80012fb4:	00094983          	lbu	s3,0(s2) # ff000000 <__clzsi2+0x7efe736c>
    HAL_I2C_SetPins(PIN_SCL | PIN_SDA);
80012fb8:	b0cfd0ef          	jal	ra,800102c4 <HAL_I2C_SetPins>
    HAL_Timebase_DelayUS(5);
80012fbc:	00500513          	li	a0,5
80012fc0:	ed9fd0ef          	jal	ra,80010e98 <HAL_Timebase_DelayUS>
    HAL_I2C_SetPins(PIN_SCL);
80012fc4:	00100513          	li	a0,1
80012fc8:	afcfd0ef          	jal	ra,800102c4 <HAL_I2C_SetPins>
    HAL_Timebase_DelayUS(5);
80012fcc:	00500513          	li	a0,5
80012fd0:	ec9fd0ef          	jal	ra,80010e98 <HAL_Timebase_DelayUS>
    HAL_I2C_WriteByte((0x76 << 1) | 0); // write
80012fd4:	0ec00513          	li	a0,236
80012fd8:	aacfd0ef          	jal	ra,80010284 <HAL_I2C_WriteByte>
    if(HAL_I2C_ReadACK() == 1)
80012fdc:	accfd0ef          	jal	ra,800102a8 <HAL_I2C_ReadACK>
80012fe0:	00050793          	mv	a5,a0
80012fe4:	00100493          	li	s1,1
    HAL_I2C_WriteByte(reg); // register address
80012fe8:	000a0513          	mv	a0,s4
    if(HAL_I2C_ReadACK() == 1)
80012fec:	08978463          	beq	a5,s1,80013074 <user_i2c_write+0xf0>
    HAL_I2C_WriteByte(reg); // register address
80012ff0:	a94fd0ef          	jal	ra,80010284 <HAL_I2C_WriteByte>
    if(HAL_I2C_ReadACK() == 1)
80012ff4:	ab4fd0ef          	jal	ra,800102a8 <HAL_I2C_ReadACK>
80012ff8:	00050793          	mv	a5,a0
    HAL_I2C_WriteByte(value); // register data
80012ffc:	00098513          	mv	a0,s3
    if(HAL_I2C_ReadACK() == 1)
80013000:	0a978663          	beq	a5,s1,800130ac <user_i2c_write+0x128>
    HAL_I2C_WriteByte(value); // register data
80013004:	a80fd0ef          	jal	ra,80010284 <HAL_I2C_WriteByte>
    if(HAL_I2C_ReadACK() == 1)
80013008:	aa0fd0ef          	jal	ra,800102a8 <HAL_I2C_ReadACK>
8001300c:	00050793          	mv	a5,a0
    HAL_I2C_WriteACK(false);
80013010:	00000513          	li	a0,0
    if(HAL_I2C_ReadACK() == 1)
80013014:	0a978263          	beq	a5,s1,800130b8 <user_i2c_write+0x134>
    HAL_I2C_WriteACK(false);
80013018:	a9cfd0ef          	jal	ra,800102b4 <HAL_I2C_WriteACK>
    HAL_I2C_SetPins(PIN_SCL);
8001301c:	00100513          	li	a0,1
80013020:	aa4fd0ef          	jal	ra,800102c4 <HAL_I2C_SetPins>
    HAL_Timebase_DelayUS(5);
80013024:	00500513          	li	a0,5
80013028:	e71fd0ef          	jal	ra,80010e98 <HAL_Timebase_DelayUS>
    HAL_I2C_SetPins(PIN_SCL | PIN_SDA);
8001302c:	00300513          	li	a0,3
            return res;
        }

        reg_data++;
        //reg_addr++;
        len--;
80013030:	fff40413          	addi	s0,s0,-1
    HAL_I2C_SetPins(PIN_SCL | PIN_SDA);
80013034:	a90fd0ef          	jal	ra,800102c4 <HAL_I2C_SetPins>
        len--;
80013038:	01041413          	slli	s0,s0,0x10
    HAL_Timebase_DelayUS(5);
8001303c:	00500513          	li	a0,5
        len--;
80013040:	01045413          	srli	s0,s0,0x10
    HAL_Timebase_DelayUS(5);
80013044:	e55fd0ef          	jal	ra,80010e98 <HAL_Timebase_DelayUS>
        reg_data++;
80013048:	00190913          	addi	s2,s2,1
    while(len) {
8001304c:	f60412e3          	bnez	s0,80012fb0 <user_i2c_write+0x2c>
    }
}
80013050:	01c12083          	lw	ra,28(sp)
80013054:	01812403          	lw	s0,24(sp)
80013058:	01012903          	lw	s2,16(sp)
8001305c:	00c12983          	lw	s3,12(sp)
80013060:	00812a03          	lw	s4,8(sp)
80013064:	00048513          	mv	a0,s1
80013068:	01412483          	lw	s1,20(sp)
8001306c:	02010113          	addi	sp,sp,32
80013070:	00008067          	ret
80013074:	fff00993          	li	s3,-1
        return -1;
80013078:	fff00493          	li	s1,-1
            printf("\n\r\n\rWrite to addr %x (length %d)", reg_addr, len);
8001307c:	80000537          	lui	a0,0x80000
80013080:	00040613          	mv	a2,s0
80013084:	000a0593          	mv	a1,s4
80013088:	2ec50513          	addi	a0,a0,748 # 800002ec <__clzsi2+0xfffe7658>
8001308c:	e61ff0ef          	jal	ra,80012eec <printf>
            printf("\n\rWrite error: reg_addr = %02x, data = %02x, error = %d", reg_addr, *reg_data, res);
80013090:	00094603          	lbu	a2,0(s2)
80013094:	80000537          	lui	a0,0x80000
80013098:	00098693          	mv	a3,s3
8001309c:	000a0593          	mv	a1,s4
800130a0:	31050513          	addi	a0,a0,784 # 80000310 <__clzsi2+0xfffe767c>
800130a4:	e49ff0ef          	jal	ra,80012eec <printf>
            return res;
800130a8:	fa9ff06f          	j	80013050 <user_i2c_write+0xcc>
800130ac:	ffe00993          	li	s3,-2
        return -2;
800130b0:	ffe00493          	li	s1,-2
800130b4:	fc9ff06f          	j	8001307c <user_i2c_write+0xf8>
800130b8:	ffd00993          	li	s3,-3
        return -3;
800130bc:	ffd00493          	li	s1,-3
800130c0:	fbdff06f          	j	8001307c <user_i2c_write+0xf8>

800130c4 <_external_interrupt>:
}
800130c4:	00008067          	ret

800130c8 <_misaligned_instruction>:
800130c8:	00008067          	ret

800130cc <memset>:
    for(int i=0; i<len; i++)
800130cc:	0e060063          	beqz	a2,800131ac <memset+0xe0>
800130d0:	40a007b3          	neg	a5,a0
800130d4:	fff60693          	addi	a3,a2,-1
800130d8:	00500813          	li	a6,5
        *(uint8_t*) (dest+i) = c;
800130dc:	0ff5f713          	zext.b	a4,a1
800130e0:	0037f793          	andi	a5,a5,3
800130e4:	0ed87a63          	bgeu	a6,a3,800131d8 <memset+0x10c>
800130e8:	0c078a63          	beqz	a5,800131bc <memset+0xf0>
800130ec:	00e50023          	sb	a4,0(a0)
    for(int i=0; i<len; i++)
800130f0:	00100693          	li	a3,1
800130f4:	0ad78e63          	beq	a5,a3,800131b0 <memset+0xe4>
        *(uint8_t*) (dest+i) = c;
800130f8:	00e500a3          	sb	a4,1(a0)
    for(int i=0; i<len; i++)
800130fc:	00300693          	li	a3,3
80013100:	0cd79663          	bne	a5,a3,800131cc <memset+0x100>
        *(uint8_t*) (dest+i) = c;
80013104:	00e50123          	sb	a4,2(a0)
    for(int i=0; i<len; i++)
80013108:	00300893          	li	a7,3
8001310c:	00300813          	li	a6,3
80013110:	00871693          	slli	a3,a4,0x8
80013114:	40f60333          	sub	t1,a2,a5
80013118:	01071593          	slli	a1,a4,0x10
8001311c:	00d766b3          	or	a3,a4,a3
80013120:	00b6e6b3          	or	a3,a3,a1
80013124:	01871e13          	slli	t3,a4,0x18
80013128:	00f507b3          	add	a5,a0,a5
8001312c:	ffc37593          	andi	a1,t1,-4
80013130:	01c6e6b3          	or	a3,a3,t3
80013134:	00f585b3          	add	a1,a1,a5
        *(uint8_t*) (dest+i) = c;
80013138:	00d7a023          	sw	a3,0(a5)
    for(int i=0; i<len; i++)
8001313c:	00478793          	addi	a5,a5,4
80013140:	feb79ce3          	bne	a5,a1,80013138 <memset+0x6c>
80013144:	ffc37693          	andi	a3,t1,-4
80013148:	00d888b3          	add	a7,a7,a3
8001314c:	010687b3          	add	a5,a3,a6
80013150:	06d30c63          	beq	t1,a3,800131c8 <memset+0xfc>
        *(uint8_t*) (dest+i) = c;
80013154:	011508b3          	add	a7,a0,a7
80013158:	00e88023          	sb	a4,0(a7)
    for(int i=0; i<len; i++)
8001315c:	00178693          	addi	a3,a5,1
80013160:	04c6f663          	bgeu	a3,a2,800131ac <memset+0xe0>
        *(uint8_t*) (dest+i) = c;
80013164:	00d506b3          	add	a3,a0,a3
80013168:	00e68023          	sb	a4,0(a3)
    for(int i=0; i<len; i++)
8001316c:	00278693          	addi	a3,a5,2
80013170:	02c6fe63          	bgeu	a3,a2,800131ac <memset+0xe0>
        *(uint8_t*) (dest+i) = c;
80013174:	00d506b3          	add	a3,a0,a3
80013178:	00e68023          	sb	a4,0(a3)
    for(int i=0; i<len; i++)
8001317c:	00378693          	addi	a3,a5,3
80013180:	02c6f663          	bgeu	a3,a2,800131ac <memset+0xe0>
        *(uint8_t*) (dest+i) = c;
80013184:	00d506b3          	add	a3,a0,a3
80013188:	00e68023          	sb	a4,0(a3)
    for(int i=0; i<len; i++)
8001318c:	00478693          	addi	a3,a5,4
80013190:	00c6fe63          	bgeu	a3,a2,800131ac <memset+0xe0>
        *(uint8_t*) (dest+i) = c;
80013194:	00d506b3          	add	a3,a0,a3
80013198:	00e68023          	sb	a4,0(a3)
    for(int i=0; i<len; i++)
8001319c:	00578793          	addi	a5,a5,5
800131a0:	00c7f663          	bgeu	a5,a2,800131ac <memset+0xe0>
        *(uint8_t*) (dest+i) = c;
800131a4:	00f50533          	add	a0,a0,a5
800131a8:	00e50023          	sb	a4,0(a0)
}
800131ac:	00008067          	ret
    for(int i=0; i<len; i++)
800131b0:	00100893          	li	a7,1
800131b4:	00100813          	li	a6,1
800131b8:	f59ff06f          	j	80013110 <memset+0x44>
800131bc:	00000893          	li	a7,0
800131c0:	00000813          	li	a6,0
800131c4:	f4dff06f          	j	80013110 <memset+0x44>
800131c8:	00008067          	ret
800131cc:	00200813          	li	a6,2
800131d0:	00200893          	li	a7,2
800131d4:	f3dff06f          	j	80013110 <memset+0x44>
800131d8:	00000793          	li	a5,0
800131dc:	00000893          	li	a7,0
800131e0:	f75ff06f          	j	80013154 <memset+0x88>

800131e4 <memcpy>:
    for(size_t i=0; i<len; i++)
800131e4:	00060c63          	beqz	a2,800131fc <memcpy+0x18>
800131e8:	00000793          	li	a5,0
        *(uint8_t *) dest = *(uint8_t *) src;
800131ec:	0005c703          	lbu	a4,0(a1)
    for(size_t i=0; i<len; i++)
800131f0:	00178793          	addi	a5,a5,1
        *(uint8_t *) dest = *(uint8_t *) src;
800131f4:	00e50023          	sb	a4,0(a0)
    for(size_t i=0; i<len; i++)
800131f8:	fef61ae3          	bne	a2,a5,800131ec <memcpy+0x8>
}
800131fc:	00008067          	ret

80013200 <HAL_OLED_PutLine>:
void HAL_OLED_PutLine(const char* line) {
80013200:	ff010113          	addi	sp,sp,-16
80013204:	00812423          	sw	s0,8(sp)
80013208:	00112623          	sw	ra,12(sp)
8001320c:	00050413          	mv	s0,a0
  while(*line != (char) 0) {
80013210:	00054503          	lbu	a0,0(a0)
80013214:	00050a63          	beqz	a0,80013228 <HAL_OLED_PutLine+0x28>
    line++;
80013218:	00140413          	addi	s0,s0,1
    HAL_OLED_PutChar(*line);
8001321c:	ddcfd0ef          	jal	ra,800107f8 <HAL_OLED_PutChar>
  while(*line != (char) 0) {
80013220:	00044503          	lbu	a0,0(s0)
80013224:	fe051ae3          	bnez	a0,80013218 <HAL_OLED_PutLine+0x18>
}
80013228:	00c12083          	lw	ra,12(sp)
8001322c:	00812403          	lw	s0,8(sp)
80013230:	01010113          	addi	sp,sp,16
80013234:	00008067          	ret

80013238 <HAL_OLED_printf>:
{
80013238:	ec010113          	addi	sp,sp,-320
  va_start(vl, s);
8001323c:	12410313          	addi	t1,sp,292
{
80013240:	12b12223          	sw	a1,292(sp)
80013244:	12c12423          	sw	a2,296(sp)
80013248:	12d12623          	sw	a3,300(sp)
  vsnprintf(out, 255, s, vl);
8001324c:	00050613          	mv	a2,a0
80013250:	00030693          	mv	a3,t1
80013254:	0ff00593          	li	a1,255
80013258:	01010513          	addi	a0,sp,16
{
8001325c:	10112e23          	sw	ra,284(sp)
80013260:	10812c23          	sw	s0,280(sp)
80013264:	12e12823          	sw	a4,304(sp)
80013268:	12f12a23          	sw	a5,308(sp)
8001326c:	13012c23          	sw	a6,312(sp)
80013270:	13112e23          	sw	a7,316(sp)
  va_start(vl, s);
80013274:	00612623          	sw	t1,12(sp)
  vsnprintf(out, 255, s, vl);
80013278:	4f0020ef          	jal	ra,80015768 <vsnprintf_>
  while(*line != (char) 0) {
8001327c:	01014503          	lbu	a0,16(sp)
80013280:	00050c63          	beqz	a0,80013298 <HAL_OLED_printf+0x60>
80013284:	01010413          	addi	s0,sp,16
    line++;
80013288:	00140413          	addi	s0,s0,1
    HAL_OLED_PutChar(*line);
8001328c:	d6cfd0ef          	jal	ra,800107f8 <HAL_OLED_PutChar>
  while(*line != (char) 0) {
80013290:	00044503          	lbu	a0,0(s0)
80013294:	fe051ae3          	bnez	a0,80013288 <HAL_OLED_printf+0x50>
}
80013298:	11c12083          	lw	ra,284(sp)
8001329c:	11812403          	lw	s0,280(sp)
800132a0:	14010113          	addi	sp,sp,320
800132a4:	00008067          	ret

800132a8 <HAL_I2C_StartCondition>:
void HAL_I2C_StartCondition() {
800132a8:	ff010113          	addi	sp,sp,-16
    HAL_I2C_SetPins(PIN_SCL | PIN_SDA);
800132ac:	00300513          	li	a0,3
void HAL_I2C_StartCondition() {
800132b0:	00112623          	sw	ra,12(sp)
    HAL_I2C_SetPins(PIN_SCL | PIN_SDA);
800132b4:	810fd0ef          	jal	ra,800102c4 <HAL_I2C_SetPins>
    HAL_Timebase_DelayUS(5);
800132b8:	00500513          	li	a0,5
800132bc:	bddfd0ef          	jal	ra,80010e98 <HAL_Timebase_DelayUS>
    HAL_I2C_SetPins(PIN_SCL);
800132c0:	00100513          	li	a0,1
800132c4:	800fd0ef          	jal	ra,800102c4 <HAL_I2C_SetPins>
}
800132c8:	00c12083          	lw	ra,12(sp)
    HAL_Timebase_DelayUS(5);
800132cc:	00500513          	li	a0,5
}
800132d0:	01010113          	addi	sp,sp,16
    HAL_Timebase_DelayUS(5);
800132d4:	bc5fd06f          	j	80010e98 <HAL_Timebase_DelayUS>

800132d8 <HAL_I2C_StopCondition>:
void HAL_I2C_StopCondition() {
800132d8:	ff010113          	addi	sp,sp,-16
    HAL_I2C_SetPins(PIN_SCL);
800132dc:	00100513          	li	a0,1
void HAL_I2C_StopCondition() {
800132e0:	00112623          	sw	ra,12(sp)
    HAL_I2C_SetPins(PIN_SCL);
800132e4:	fe1fc0ef          	jal	ra,800102c4 <HAL_I2C_SetPins>
    HAL_Timebase_DelayUS(5);
800132e8:	00500513          	li	a0,5
800132ec:	badfd0ef          	jal	ra,80010e98 <HAL_Timebase_DelayUS>
    HAL_I2C_SetPins(PIN_SCL | PIN_SDA);
800132f0:	00300513          	li	a0,3
800132f4:	fd1fc0ef          	jal	ra,800102c4 <HAL_I2C_SetPins>
}
800132f8:	00c12083          	lw	ra,12(sp)
    HAL_Timebase_DelayUS(5);
800132fc:	00500513          	li	a0,5
}
80013300:	01010113          	addi	sp,sp,16
    HAL_Timebase_DelayUS(5);
80013304:	b95fd06f          	j	80010e98 <HAL_Timebase_DelayUS>

80013308 <BME680_ReadRegister>:
int16_t BME680_ReadRegister(uint8_t reg) {
80013308:	ff010113          	addi	sp,sp,-16
8001330c:	00812423          	sw	s0,8(sp)
80013310:	00050413          	mv	s0,a0
    HAL_I2C_SetPins(PIN_SCL | PIN_SDA);
80013314:	00300513          	li	a0,3
int16_t BME680_ReadRegister(uint8_t reg) {
80013318:	00112623          	sw	ra,12(sp)
8001331c:	00912223          	sw	s1,4(sp)
    HAL_I2C_SetPins(PIN_SCL | PIN_SDA);
80013320:	fa5fc0ef          	jal	ra,800102c4 <HAL_I2C_SetPins>
    HAL_Timebase_DelayUS(5);
80013324:	00500513          	li	a0,5
80013328:	b71fd0ef          	jal	ra,80010e98 <HAL_Timebase_DelayUS>
    HAL_I2C_SetPins(PIN_SCL);
8001332c:	00100513          	li	a0,1
80013330:	f95fc0ef          	jal	ra,800102c4 <HAL_I2C_SetPins>
    HAL_Timebase_DelayUS(5);
80013334:	00500513          	li	a0,5
80013338:	b61fd0ef          	jal	ra,80010e98 <HAL_Timebase_DelayUS>
    HAL_I2C_WriteByte((0x76 << 1) | 0); // write
8001333c:	0ec00513          	li	a0,236
80013340:	f45fc0ef          	jal	ra,80010284 <HAL_I2C_WriteByte>
    if(HAL_I2C_ReadACK() == 1)
80013344:	f65fc0ef          	jal	ra,800102a8 <HAL_I2C_ReadACK>
80013348:	00100493          	li	s1,1
8001334c:	0a950063          	beq	a0,s1,800133ec <BME680_ReadRegister+0xe4>
    HAL_I2C_WriteByte(reg); // register address
80013350:	00040513          	mv	a0,s0
80013354:	f31fc0ef          	jal	ra,80010284 <HAL_I2C_WriteByte>
    if(HAL_I2C_ReadACK() == 1)
80013358:	f51fc0ef          	jal	ra,800102a8 <HAL_I2C_ReadACK>
8001335c:	08950c63          	beq	a0,s1,800133f4 <BME680_ReadRegister+0xec>
    HAL_I2C_WriteACK(false);
80013360:	00000513          	li	a0,0
80013364:	f51fc0ef          	jal	ra,800102b4 <HAL_I2C_WriteACK>
    HAL_I2C_SetPins(PIN_SCL | PIN_SDA);
80013368:	00300513          	li	a0,3
8001336c:	f59fc0ef          	jal	ra,800102c4 <HAL_I2C_SetPins>
    HAL_Timebase_DelayUS(5);
80013370:	00500513          	li	a0,5
80013374:	b25fd0ef          	jal	ra,80010e98 <HAL_Timebase_DelayUS>
    HAL_I2C_SetPins(PIN_SCL);
80013378:	00100513          	li	a0,1
8001337c:	f49fc0ef          	jal	ra,800102c4 <HAL_I2C_SetPins>
    HAL_Timebase_DelayUS(5);
80013380:	00500513          	li	a0,5
80013384:	b15fd0ef          	jal	ra,80010e98 <HAL_Timebase_DelayUS>
    HAL_I2C_WriteByte((0x76 << 1) | 1); // read
80013388:	0ed00513          	li	a0,237
8001338c:	ef9fc0ef          	jal	ra,80010284 <HAL_I2C_WriteByte>
    if(HAL_I2C_ReadACK() == 1)
80013390:	f19fc0ef          	jal	ra,800102a8 <HAL_I2C_ReadACK>
80013394:	06950463          	beq	a0,s1,800133fc <BME680_ReadRegister+0xf4>
    uint8_t res = HAL_I2C_ReadByte();
80013398:	ef9fc0ef          	jal	ra,80010290 <HAL_I2C_ReadByte>
8001339c:	00050413          	mv	s0,a0
    HAL_I2C_WriteACK(false);
800133a0:	00000513          	li	a0,0
800133a4:	f11fc0ef          	jal	ra,800102b4 <HAL_I2C_WriteACK>
    HAL_I2C_WriteACK(true);
800133a8:	00100513          	li	a0,1
800133ac:	f09fc0ef          	jal	ra,800102b4 <HAL_I2C_WriteACK>
    HAL_I2C_SetPins(PIN_SCL);
800133b0:	00100513          	li	a0,1
800133b4:	f11fc0ef          	jal	ra,800102c4 <HAL_I2C_SetPins>
    HAL_Timebase_DelayUS(5);
800133b8:	00500513          	li	a0,5
800133bc:	addfd0ef          	jal	ra,80010e98 <HAL_Timebase_DelayUS>
    HAL_I2C_SetPins(PIN_SCL | PIN_SDA);
800133c0:	00300513          	li	a0,3
800133c4:	f01fc0ef          	jal	ra,800102c4 <HAL_I2C_SetPins>
    HAL_Timebase_DelayUS(5);
800133c8:	00500513          	li	a0,5
800133cc:	acdfd0ef          	jal	ra,80010e98 <HAL_Timebase_DelayUS>
    return res;
800133d0:	01041513          	slli	a0,s0,0x10
800133d4:	41055513          	srai	a0,a0,0x10
}
800133d8:	00c12083          	lw	ra,12(sp)
800133dc:	00812403          	lw	s0,8(sp)
800133e0:	00412483          	lw	s1,4(sp)
800133e4:	01010113          	addi	sp,sp,16
800133e8:	00008067          	ret
        return -1;
800133ec:	fff00513          	li	a0,-1
800133f0:	fe9ff06f          	j	800133d8 <BME680_ReadRegister+0xd0>
        return -2;
800133f4:	ffe00513          	li	a0,-2
800133f8:	fe1ff06f          	j	800133d8 <BME680_ReadRegister+0xd0>
        return -3;
800133fc:	ffd00513          	li	a0,-3
80013400:	fd9ff06f          	j	800133d8 <BME680_ReadRegister+0xd0>

80013404 <user_i2c_read>:
    while(len) {
80013404:	0c068a63          	beqz	a3,800134d8 <user_i2c_read+0xd4>
{
80013408:	fe010113          	addi	sp,sp,-32
8001340c:	00912a23          	sw	s1,20(sp)
80013410:	01312623          	sw	s3,12(sp)
80013414:	01412423          	sw	s4,8(sp)
80013418:	00112e23          	sw	ra,28(sp)
8001341c:	00812c23          	sw	s0,24(sp)
80013420:	01212823          	sw	s2,16(sp)
80013424:	00068493          	mv	s1,a3
80013428:	00060993          	mv	s3,a2
8001342c:	00d58a33          	add	s4,a1,a3
80013430:	0180006f          	j	80013448 <user_i2c_read+0x44>
        len--;
80013434:	01079493          	slli	s1,a5,0x10
        *reg_data = (uint8_t) res;
80013438:	00a98023          	sb	a0,0(s3)
        len--;
8001343c:	0104d493          	srli	s1,s1,0x10
        reg_data++;
80013440:	00198993          	addi	s3,s3,1
    while(len) {
80013444:	06048863          	beqz	s1,800134b4 <user_i2c_read+0xb0>
80013448:	409a0933          	sub	s2,s4,s1
8001344c:	0ff97913          	zext.b	s2,s2
        res = BME680_ReadRegister(reg_addr);
80013450:	00090513          	mv	a0,s2
80013454:	eb5ff0ef          	jal	ra,80013308 <BME680_ReadRegister>
        len--;
80013458:	fff48793          	addi	a5,s1,-1
        res = BME680_ReadRegister(reg_addr);
8001345c:	00050413          	mv	s0,a0
        if(res < 0) {
80013460:	fc055ae3          	bgez	a0,80013434 <user_i2c_read+0x30>
            printf("\n\r\n\rRead from addr %x (length %x)", reg_addr, len);
80013464:	80000537          	lui	a0,0x80000
80013468:	00048613          	mv	a2,s1
8001346c:	00090593          	mv	a1,s2
80013470:	34850513          	addi	a0,a0,840 # 80000348 <__clzsi2+0xfffe76b4>
80013474:	a79ff0ef          	jal	ra,80012eec <printf>
            printf("\n\rRead error: reg_addr = %x, result = %x", reg_addr, res);
80013478:	80000537          	lui	a0,0x80000
8001347c:	00040613          	mv	a2,s0
80013480:	00090593          	mv	a1,s2
80013484:	36c50513          	addi	a0,a0,876 # 8000036c <__clzsi2+0xfffe76d8>
80013488:	a65ff0ef          	jal	ra,80012eec <printf>
}
8001348c:	01c12083          	lw	ra,28(sp)
            return res;
80013490:	01841513          	slli	a0,s0,0x18
}
80013494:	01812403          	lw	s0,24(sp)
80013498:	01412483          	lw	s1,20(sp)
8001349c:	01012903          	lw	s2,16(sp)
800134a0:	00c12983          	lw	s3,12(sp)
800134a4:	00812a03          	lw	s4,8(sp)
            return res;
800134a8:	41855513          	srai	a0,a0,0x18
}
800134ac:	02010113          	addi	sp,sp,32
800134b0:	00008067          	ret
800134b4:	01c12083          	lw	ra,28(sp)
800134b8:	01812403          	lw	s0,24(sp)
800134bc:	01412483          	lw	s1,20(sp)
800134c0:	01012903          	lw	s2,16(sp)
800134c4:	00c12983          	lw	s3,12(sp)
800134c8:	00812a03          	lw	s4,8(sp)
    return 0;
800134cc:	00000513          	li	a0,0
}
800134d0:	02010113          	addi	sp,sp,32
800134d4:	00008067          	ret
    return 0;
800134d8:	00000513          	li	a0,0
}
800134dc:	00008067          	ret

800134e0 <BME680_WriteRegister>:
int8_t BME680_WriteRegister(uint8_t reg, uint8_t value) {
800134e0:	ff010113          	addi	sp,sp,-16
800134e4:	00812423          	sw	s0,8(sp)
800134e8:	00050413          	mv	s0,a0
    HAL_I2C_SetPins(PIN_SCL | PIN_SDA);
800134ec:	00300513          	li	a0,3
int8_t BME680_WriteRegister(uint8_t reg, uint8_t value) {
800134f0:	00112623          	sw	ra,12(sp)
800134f4:	00912223          	sw	s1,4(sp)
800134f8:	01212023          	sw	s2,0(sp)
800134fc:	00058493          	mv	s1,a1
    HAL_I2C_SetPins(PIN_SCL | PIN_SDA);
80013500:	dc5fc0ef          	jal	ra,800102c4 <HAL_I2C_SetPins>
    HAL_Timebase_DelayUS(5);
80013504:	00500513          	li	a0,5
80013508:	991fd0ef          	jal	ra,80010e98 <HAL_Timebase_DelayUS>
    HAL_I2C_SetPins(PIN_SCL);
8001350c:	00100513          	li	a0,1
80013510:	db5fc0ef          	jal	ra,800102c4 <HAL_I2C_SetPins>
    HAL_Timebase_DelayUS(5);
80013514:	00500513          	li	a0,5
80013518:	981fd0ef          	jal	ra,80010e98 <HAL_Timebase_DelayUS>
    HAL_I2C_WriteByte((0x76 << 1) | 0); // write
8001351c:	0ec00513          	li	a0,236
80013520:	d65fc0ef          	jal	ra,80010284 <HAL_I2C_WriteByte>
    if(HAL_I2C_ReadACK() == 1)
80013524:	d85fc0ef          	jal	ra,800102a8 <HAL_I2C_ReadACK>
80013528:	00100913          	li	s2,1
8001352c:	07250463          	beq	a0,s2,80013594 <BME680_WriteRegister+0xb4>
    HAL_I2C_WriteByte(reg); // register address
80013530:	00040513          	mv	a0,s0
80013534:	d51fc0ef          	jal	ra,80010284 <HAL_I2C_WriteByte>
    if(HAL_I2C_ReadACK() == 1)
80013538:	d71fc0ef          	jal	ra,800102a8 <HAL_I2C_ReadACK>
8001353c:	07250063          	beq	a0,s2,8001359c <BME680_WriteRegister+0xbc>
    HAL_I2C_WriteByte(value); // register data
80013540:	00048513          	mv	a0,s1
80013544:	d41fc0ef          	jal	ra,80010284 <HAL_I2C_WriteByte>
    if(HAL_I2C_ReadACK() == 1)
80013548:	d61fc0ef          	jal	ra,800102a8 <HAL_I2C_ReadACK>
8001354c:	05250c63          	beq	a0,s2,800135a4 <BME680_WriteRegister+0xc4>
    HAL_I2C_WriteACK(false);
80013550:	00000513          	li	a0,0
80013554:	d61fc0ef          	jal	ra,800102b4 <HAL_I2C_WriteACK>
    HAL_I2C_SetPins(PIN_SCL);
80013558:	00100513          	li	a0,1
8001355c:	d69fc0ef          	jal	ra,800102c4 <HAL_I2C_SetPins>
    HAL_Timebase_DelayUS(5);
80013560:	00500513          	li	a0,5
80013564:	935fd0ef          	jal	ra,80010e98 <HAL_Timebase_DelayUS>
    HAL_I2C_SetPins(PIN_SCL | PIN_SDA);
80013568:	00300513          	li	a0,3
8001356c:	d59fc0ef          	jal	ra,800102c4 <HAL_I2C_SetPins>
    HAL_Timebase_DelayUS(5);
80013570:	00500513          	li	a0,5
80013574:	925fd0ef          	jal	ra,80010e98 <HAL_Timebase_DelayUS>
    return 0;
80013578:	00000513          	li	a0,0
}
8001357c:	00c12083          	lw	ra,12(sp)
80013580:	00812403          	lw	s0,8(sp)
80013584:	00412483          	lw	s1,4(sp)
80013588:	00012903          	lw	s2,0(sp)
8001358c:	01010113          	addi	sp,sp,16
80013590:	00008067          	ret
        return -1;
80013594:	fff00513          	li	a0,-1
80013598:	fe5ff06f          	j	8001357c <BME680_WriteRegister+0x9c>
        return -2;
8001359c:	ffe00513          	li	a0,-2
800135a0:	fddff06f          	j	8001357c <BME680_WriteRegister+0x9c>
        return -3;
800135a4:	ffd00513          	li	a0,-3
800135a8:	fd5ff06f          	j	8001357c <BME680_WriteRegister+0x9c>

800135ac <BME680_Init>:
int8_t BME680_Init() {
800135ac:	ff010113          	addi	sp,sp,-16
    HAL_I2C_SetDivider(0xFF);
800135b0:	0ff00513          	li	a0,255
int8_t BME680_Init() {
800135b4:	00112623          	sw	ra,12(sp)
    HAL_I2C_SetDivider(0xFF);
800135b8:	ce5fc0ef          	jal	ra,8001029c <HAL_I2C_SetDivider>
    HAL_I2C_SetPins(PIN_SCL | PIN_SDA);
800135bc:	00300513          	li	a0,3
800135c0:	d05fc0ef          	jal	ra,800102c4 <HAL_I2C_SetPins>
}
800135c4:	00c12083          	lw	ra,12(sp)
800135c8:	00000513          	li	a0,0
800135cc:	01010113          	addi	sp,sp,16
800135d0:	00008067          	ret

800135d4 <user_spi_read>:
}
800135d4:	00000513          	li	a0,0
800135d8:	00008067          	ret

800135dc <user_spi_write>:
800135dc:	00000513          	li	a0,0
800135e0:	00008067          	ret

800135e4 <main>:



void main() {
800135e4:	f4010113          	addi	sp,sp,-192
800135e8:	0a112e23          	sw	ra,188(sp)
800135ec:	0a812c23          	sw	s0,184(sp)
800135f0:	0a912a23          	sw	s1,180(sp)
800135f4:	0b212823          	sw	s2,176(sp)
800135f8:	0b312623          	sw	s3,172(sp)
800135fc:	0b412423          	sw	s4,168(sp)
80013600:	0b512223          	sw	s5,164(sp)
80013604:	09812c23          	sw	s8,152(sp)
80013608:	09912a23          	sw	s9,148(sp)
8001360c:	09a12823          	sw	s10,144(sp)
80013610:	09b12623          	sw	s11,140(sp)
80013614:	0b612023          	sw	s6,160(sp)
80013618:	09712e23          	sw	s7,156(sp)
#ifdef DEBUG_OLED
    HAL_OLED_Init();
8001361c:	f55fc0ef          	jal	ra,80010570 <HAL_OLED_Init>
    printf("OLED display initialized.");
80013620:	80000537          	lui	a0,0x80000
80013624:	39850513          	addi	a0,a0,920 # 80000398 <__clzsi2+0xfffe7704>
80013628:	8c5ff0ef          	jal	ra,80012eec <printf>
    HAL_I2C_SetDivider(0xFF);
8001362c:	0ff00513          	li	a0,255
80013630:	c6dfc0ef          	jal	ra,8001029c <HAL_I2C_SetDivider>
    HAL_I2C_SetPins(PIN_SCL | PIN_SDA);
80013634:	00300513          	li	a0,3
80013638:	c8dfc0ef          	jal	ra,800102c4 <HAL_I2C_SetPins>
#endif

    if(BME680_Init() == 0) {
    struct bme680_dev gas_sensor;

    gas_sensor.dev_id = BME680_I2C_ADDR_PRIMARY;
8001363c:	07600793          	li	a5,118
80013640:	02f102a3          	sb	a5,37(sp)
    gas_sensor.intf = BME680_I2C_INTF;
    gas_sensor.read = user_i2c_read;
80013644:	800137b7          	lui	a5,0x80013
80013648:	40478793          	addi	a5,a5,1028 # 80013404 <__clzsi2+0xffffa770>
8001364c:	06f12823          	sw	a5,112(sp)
    gas_sensor.write = user_i2c_write;
80013650:	800137b7          	lui	a5,0x80013
80013654:	f8478793          	addi	a5,a5,-124 # 80012f84 <__clzsi2+0xffffa2f0>
80013658:	06f12a23          	sw	a5,116(sp)
    gas_sensor.delay_ms = user_delay_ms;
8001365c:	800137b7          	lui	a5,0x80013
80013660:	ec078793          	addi	a5,a5,-320 # 80012ec0 <__clzsi2+0xffffa22c>
    gas_sensor.intf = BME680_I2C_INTF;
80013664:	00100413          	li	s0,1
    gas_sensor.delay_ms = user_delay_ms;
80013668:	06f12c23          	sw	a5,120(sp)
     */
    gas_sensor.amb_temp = 25;


    int8_t rslt = BME680_OK;
    rslt = bme680_init(&gas_sensor);
8001366c:	02410513          	addi	a0,sp,36
    gas_sensor.amb_temp = 25;
80013670:	01900793          	li	a5,25
80013674:	02f106a3          	sb	a5,45(sp)
    gas_sensor.intf = BME680_I2C_INTF;
80013678:	02812423          	sw	s0,40(sp)
    rslt = bme680_init(&gas_sensor);
8001367c:	e41fd0ef          	jal	ra,800114bc <bme680_init>

    // set up measurement parameters
    uint8_t set_required_settings;

    /* Set the temperature, pressure and humidity settings */
    gas_sensor.tph_sett.os_hum = BME680_OS_2X;
80013680:	020307b7          	lui	a5,0x2030
80013684:	40278793          	addi	a5,a5,1026 # 2030402 <font6x8-0x7dfcfc2a>
80013688:	06f12023          	sw	a5,96(sp)
    gas_sensor.tph_sett.filter = BME680_FILTER_SIZE_3;

    /* Set the remaining gas sensor settings and link the heating profile */
    gas_sensor.gas_sett.run_gas = BME680_ENABLE_GAS_MEAS;
    /* Create a ramp heat waveform in 3 steps */
    gas_sensor.gas_sett.heatr_temp = 320; /* degree Celsius */
8001368c:	02ee07b7          	lui	a5,0x2ee0
80013690:	14078793          	addi	a5,a5,320 # 2ee0140 <font6x8-0x7d11feec>
    /* Set the required sensor settings needed */
    set_required_settings = BME680_OST_SEL | BME680_OSP_SEL | BME680_OSH_SEL | BME680_FILTER_SEL 
        | BME680_GAS_SENSOR_SEL;

    /* Set the desired sensor configuration */
    rslt = bme680_set_sensor_settings(set_required_settings,&gas_sensor);
80013694:	02410593          	addi	a1,sp,36
80013698:	0df00513          	li	a0,223
    gas_sensor.gas_sett.heatr_temp = 320; /* degree Celsius */
8001369c:	06f12423          	sw	a5,104(sp)
    gas_sensor.gas_sett.run_gas = BME680_ENABLE_GAS_MEAS;
800136a0:	06810323          	sb	s0,102(sp)
    gas_sensor.power_mode = BME680_FORCED_MODE; 
800136a4:	06810623          	sb	s0,108(sp)
    rslt = bme680_set_sensor_settings(set_required_settings,&gas_sensor);
800136a8:	815fe0ef          	jal	ra,80011ebc <bme680_set_sensor_settings>
    //printf("\n\rbme680_set_sensor_settings: %d", rslt);

    /* Set the power mode */
    rslt = bme680_set_sensor_mode(&gas_sensor);
800136ac:	02410513          	addi	a0,sp,36
800136b0:	e30fe0ef          	jal	ra,80011ce0 <bme680_set_sensor_mode>
    //printf("\n\rbme680_set_sensor_mode: %d", rslt);

        /* Get the total measurement duration so as to sleep or wait till the
     * measurement is complete */
    uint16_t meas_period;
    bme680_get_profile_dur(&meas_period, &gas_sensor);
800136b4:	02410593          	addi	a1,sp,36
800136b8:	00e10513          	addi	a0,sp,14
800136bc:	890ff0ef          	jal	ra,8001274c <bme680_get_profile_dur>
800136c0:	800009b7          	lui	s3,0x80000
800136c4:	80000937          	lui	s2,0x80000
800136c8:	800004b7          	lui	s1,0x80000
800136cc:	80000cb7          	lui	s9,0x80000
800136d0:	80000db7          	lui	s11,0x80000
        printf("\n\rHumidity: %4d %%", data.humidity / 1000);
        printf("\n\rPressu.: %3d hPa", data.pressure / 100);

        /* Avoid using measurements from an unstable heating setup */
        if(data.status & BME680_GASM_VALID_MSK)
            if(data.gas_resistance < 50)
800136d4:	03100a13          	li	s4,49
                printf("\n\rIAQ: good");
            else if(data.gas_resistance < 100)
800136d8:	06300a93          	li	s5,99
                printf("\n\rIAQ: average");
            else if(data.gas_resistance < 150)
800136dc:	09500c13          	li	s8,149
                printf("\n\rIAQ: little bad");
            else if(data.gas_resistance < 200)
800136e0:	0c700d13          	li	s10,199
                printf("\n\rIAQ: bad");
            else if(data.gas_resistance < 250)
800136e4:	0f900b13          	li	s6,249
                printf("\n\rIAQ: worse");
            else if(data.gas_resistance < 300)
800136e8:	12b00b93          	li	s7,299
            //printf("\n\rGas resistance: %d Ohm", data.gas_resistance);



        /* Trigger the next measurement if you would like to read data out continuously */
        if (gas_sensor.power_mode == BME680_FORCED_MODE) {
800136ec:	00100413          	li	s0,1
    HAL_Timebase_DelayMS(period);
800136f0:	00e15503          	lhu	a0,14(sp)
800136f4:	fc8fd0ef          	jal	ra,80010ebc <HAL_Timebase_DelayMS>
        rslt = bme680_get_sensor_data(&data, &gas_sensor);
800136f8:	02410593          	addi	a1,sp,36
800136fc:	01010513          	addi	a0,sp,16
80013700:	91cff0ef          	jal	ra,8001281c <bme680_get_sensor_data>
        HAL_OLED_ClearScreen();
80013704:	d9dfc0ef          	jal	ra,800104a0 <HAL_OLED_ClearScreen>
        printf("\n\rTemperat.: %3d C", data.temperature / 100);
80013708:	01411503          	lh	a0,20(sp)
8001370c:	06400593          	li	a1,100
80013710:	4d0050ef          	jal	ra,80018be0 <__divsi3>
80013714:	01051593          	slli	a1,a0,0x10
80013718:	4105d593          	srai	a1,a1,0x10
8001371c:	3b498513          	addi	a0,s3,948 # 800003b4 <__clzsi2+0xfffe7720>
80013720:	fccff0ef          	jal	ra,80012eec <printf>
        printf("\n\rHumidity: %4d %%", data.humidity / 1000);
80013724:	01c12503          	lw	a0,28(sp)
80013728:	3e800593          	li	a1,1000
8001372c:	4bc050ef          	jal	ra,80018be8 <__udivsi3>
80013730:	00050593          	mv	a1,a0
80013734:	3c890513          	addi	a0,s2,968 # 800003c8 <__clzsi2+0xfffe7734>
80013738:	fb4ff0ef          	jal	ra,80012eec <printf>
        printf("\n\rPressu.: %3d hPa", data.pressure / 100);
8001373c:	01812503          	lw	a0,24(sp)
80013740:	06400593          	li	a1,100
80013744:	4a4050ef          	jal	ra,80018be8 <__udivsi3>
80013748:	00050593          	mv	a1,a0
8001374c:	3dc48513          	addi	a0,s1,988 # 800003dc <__clzsi2+0xfffe7748>
80013750:	f9cff0ef          	jal	ra,80012eec <printf>
        if(data.status & BME680_GASM_VALID_MSK)
80013754:	01014783          	lbu	a5,16(sp)
80013758:	0207f793          	andi	a5,a5,32
8001375c:	02078663          	beqz	a5,80013788 <main+0x1a4>
            if(data.gas_resistance < 50)
80013760:	02012783          	lw	a5,32(sp)
80013764:	02fa7c63          	bgeu	s4,a5,8001379c <main+0x1b8>
            else if(data.gas_resistance < 100)
80013768:	04faf063          	bgeu	s5,a5,800137a8 <main+0x1c4>
            else if(data.gas_resistance < 150)
8001376c:	04fc7463          	bgeu	s8,a5,800137b4 <main+0x1d0>
            else if(data.gas_resistance < 200)
80013770:	04fd7a63          	bgeu	s10,a5,800137c4 <main+0x1e0>
            else if(data.gas_resistance < 250)
80013774:	06fb7063          	bgeu	s6,a5,800137d4 <main+0x1f0>
            else if(data.gas_resistance < 300)
80013778:	02fbf863          	bgeu	s7,a5,800137a8 <main+0x1c4>
                printf("\n\rIAQ: very bad");
8001377c:	800007b7          	lui	a5,0x80000
80013780:	43c78513          	addi	a0,a5,1084 # 8000043c <__clzsi2+0xfffe77a8>
80013784:	f68ff0ef          	jal	ra,80012eec <printf>
        if (gas_sensor.power_mode == BME680_FORCED_MODE) {
80013788:	06c14783          	lbu	a5,108(sp)
8001378c:	f68792e3          	bne	a5,s0,800136f0 <main+0x10c>
            rslt = bme680_set_sensor_mode(&gas_sensor);
80013790:	02410513          	addi	a0,sp,36
80013794:	d4cfe0ef          	jal	ra,80011ce0 <bme680_set_sensor_mode>
80013798:	f4dff06f          	j	800136e4 <main+0x100>
                printf("\n\rIAQ: good");
8001379c:	3f0c8513          	addi	a0,s9,1008 # 800003f0 <__clzsi2+0xfffe775c>
800137a0:	f4cff0ef          	jal	ra,80012eec <printf>
800137a4:	fe5ff06f          	j	80013788 <main+0x1a4>
                printf("\n\rIAQ: average");
800137a8:	3fcd8513          	addi	a0,s11,1020 # 800003fc <__clzsi2+0xfffe7768>
800137ac:	f40ff0ef          	jal	ra,80012eec <printf>
800137b0:	fd9ff06f          	j	80013788 <main+0x1a4>
                printf("\n\rIAQ: little bad");
800137b4:	800007b7          	lui	a5,0x80000
800137b8:	40c78513          	addi	a0,a5,1036 # 8000040c <__clzsi2+0xfffe7778>
800137bc:	f30ff0ef          	jal	ra,80012eec <printf>
800137c0:	fc9ff06f          	j	80013788 <main+0x1a4>
                printf("\n\rIAQ: bad");
800137c4:	800007b7          	lui	a5,0x80000
800137c8:	42078513          	addi	a0,a5,1056 # 80000420 <__clzsi2+0xfffe778c>
800137cc:	f20ff0ef          	jal	ra,80012eec <printf>
800137d0:	fb9ff06f          	j	80013788 <main+0x1a4>
                printf("\n\rIAQ: worse");
800137d4:	800007b7          	lui	a5,0x80000
800137d8:	42c78513          	addi	a0,a5,1068 # 8000042c <__clzsi2+0xfffe7798>
800137dc:	f10ff0ef          	jal	ra,80012eec <printf>
800137e0:	fa9ff06f          	j	80013788 <main+0x1a4>

800137e4 <_out_buffer>:


// internal buffer output
static inline void _out_buffer(char character, void* buffer, size_t idx, size_t maxlen)
{
  if (idx < maxlen) {
800137e4:	00d67663          	bgeu	a2,a3,800137f0 <_out_buffer+0xc>
    ((char*)buffer)[idx] = character;
800137e8:	00c585b3          	add	a1,a1,a2
800137ec:	00a58023          	sb	a0,0(a1)
  }
}
800137f0:	00008067          	ret

800137f4 <_out_null>:

// internal null output
static inline void _out_null(char character, void* buffer, size_t idx, size_t maxlen)
{
  (void)character; (void)buffer; (void)idx; (void)maxlen;
}
800137f4:	00008067          	ret

800137f8 <_out_rev>:
}


// output the specified string in reverse, taking care of any zero-padding
static size_t _out_rev(out_fct_type out, char* buffer, size_t idx, size_t maxlen, const char* buf, size_t len, unsigned int width, unsigned int flags)
{
800137f8:	fd010113          	addi	sp,sp,-48
800137fc:	02912223          	sw	s1,36(sp)
80013800:	03212023          	sw	s2,32(sp)
80013804:	01312e23          	sw	s3,28(sp)
80013808:	01412c23          	sw	s4,24(sp)
8001380c:	01512a23          	sw	s5,20(sp)
80013810:	01612823          	sw	s6,16(sp)
80013814:	00060a93          	mv	s5,a2
80013818:	01812423          	sw	s8,8(sp)
8001381c:	01912223          	sw	s9,4(sp)
80013820:	01a12023          	sw	s10,0(sp)
80013824:	02112623          	sw	ra,44(sp)
80013828:	02812423          	sw	s0,40(sp)
8001382c:	01712623          	sw	s7,12(sp)
  const size_t start_idx = idx;

  // pad spaces up to given width
  if (!(flags & FLAGS_LEFT) && !(flags & FLAGS_ZEROPAD)) {
80013830:	0038f613          	andi	a2,a7,3
{
80013834:	00088c93          	mv	s9,a7
80013838:	00050913          	mv	s2,a0
8001383c:	00058993          	mv	s3,a1
80013840:	00068a13          	mv	s4,a3
80013844:	00070b13          	mv	s6,a4
80013848:	00078d13          	mv	s10,a5
8001384c:	00080c13          	mv	s8,a6
80013850:	000a8493          	mv	s1,s5
  if (!(flags & FLAGS_LEFT) && !(flags & FLAGS_ZEROPAD)) {
80013854:	02061863          	bnez	a2,80013884 <_out_rev+0x8c>
    for (size_t i = len; i < width; i++) {
80013858:	40f804b3          	sub	s1,a6,a5
8001385c:	015484b3          	add	s1,s1,s5
80013860:	000a8413          	mv	s0,s5
80013864:	0b07fc63          	bgeu	a5,a6,8001391c <_out_rev+0x124>
      out(' ', buffer, idx++, maxlen);
80013868:	00040613          	mv	a2,s0
8001386c:	000a0693          	mv	a3,s4
80013870:	00140413          	addi	s0,s0,1
80013874:	00098593          	mv	a1,s3
80013878:	02000513          	li	a0,32
8001387c:	000900e7          	jalr	s2
    for (size_t i = len; i < width; i++) {
80013880:	fe9414e3          	bne	s0,s1,80013868 <_out_rev+0x70>
    }
  }

  // reverse string
  while (len) {
80013884:	01ab0433          	add	s0,s6,s10
80013888:	00940bb3          	add	s7,s0,s1
8001388c:	020d0263          	beqz	s10,800138b0 <_out_rev+0xb8>
    out(buf[--len], buffer, idx++, maxlen);
80013890:	fff44503          	lbu	a0,-1(s0)
80013894:	408b8633          	sub	a2,s7,s0
80013898:	000a0693          	mv	a3,s4
  while (len) {
8001389c:	fff40413          	addi	s0,s0,-1
    out(buf[--len], buffer, idx++, maxlen);
800138a0:	00098593          	mv	a1,s3
800138a4:	000900e7          	jalr	s2
  while (len) {
800138a8:	fe8b14e3          	bne	s6,s0,80013890 <_out_rev+0x98>
    out(buf[--len], buffer, idx++, maxlen);
800138ac:	01a484b3          	add	s1,s1,s10
  }

  // append pad spaces up to given width
  if (flags & FLAGS_LEFT) {
800138b0:	002cfc93          	andi	s9,s9,2
800138b4:	020c8663          	beqz	s9,800138e0 <_out_rev+0xe8>
    while (idx - start_idx < width) {
800138b8:	41548ab3          	sub	s5,s1,s5
800138bc:	038af263          	bgeu	s5,s8,800138e0 <_out_rev+0xe8>
      out(' ', buffer, idx++, maxlen);
800138c0:	00048613          	mv	a2,s1
    while (idx - start_idx < width) {
800138c4:	001a8a93          	addi	s5,s5,1
      out(' ', buffer, idx++, maxlen);
800138c8:	000a0693          	mv	a3,s4
800138cc:	00098593          	mv	a1,s3
800138d0:	02000513          	li	a0,32
800138d4:	00148493          	addi	s1,s1,1
800138d8:	000900e7          	jalr	s2
    while (idx - start_idx < width) {
800138dc:	ff8ae2e3          	bltu	s5,s8,800138c0 <_out_rev+0xc8>
    }
  }

  return idx;
}
800138e0:	02c12083          	lw	ra,44(sp)
800138e4:	02812403          	lw	s0,40(sp)
800138e8:	02012903          	lw	s2,32(sp)
800138ec:	01c12983          	lw	s3,28(sp)
800138f0:	01812a03          	lw	s4,24(sp)
800138f4:	01412a83          	lw	s5,20(sp)
800138f8:	01012b03          	lw	s6,16(sp)
800138fc:	00c12b83          	lw	s7,12(sp)
80013900:	00812c03          	lw	s8,8(sp)
80013904:	00412c83          	lw	s9,4(sp)
80013908:	00012d03          	lw	s10,0(sp)
8001390c:	00048513          	mv	a0,s1
80013910:	02412483          	lw	s1,36(sp)
80013914:	03010113          	addi	sp,sp,48
80013918:	00008067          	ret
    for (size_t i = len; i < width; i++) {
8001391c:	000a8493          	mv	s1,s5
80013920:	f65ff06f          	j	80013884 <_out_rev+0x8c>

80013924 <_ntoa_format>:


// internal itoa format
static size_t _ntoa_format(out_fct_type out, char* buffer, size_t idx, size_t maxlen, char* buf, size_t len, bool negative, unsigned int base, unsigned int prec, unsigned int width, unsigned int flags)
{
80013924:	00812383          	lw	t2,8(sp)
80013928:	00012283          	lw	t0,0(sp)
8001392c:	00412f83          	lw	t6,4(sp)
  // pad leading zeros
  if (!(flags & FLAGS_LEFT)) {
80013930:	0023f313          	andi	t1,t2,2
{
80013934:	00080f13          	mv	t5,a6
  if (!(flags & FLAGS_LEFT)) {
80013938:	06031463          	bnez	t1,800139a0 <_ntoa_format+0x7c>
    if (width && (flags & FLAGS_ZEROPAD) && (negative || (flags & (FLAGS_PLUS | FLAGS_SPACE)))) {
8001393c:	0013f813          	andi	a6,t2,1
80013940:	0e0f9e63          	bnez	t6,80013a3c <_ntoa_format+0x118>
      width--;
    }
    while ((len < prec) && (len < PRINTF_NTOA_BUFFER_SIZE)) {
80013944:	0257f663          	bgeu	a5,t0,80013970 <_ntoa_format+0x4c>
80013948:	01f00313          	li	t1,31
8001394c:	22f36c63          	bltu	t1,a5,80013b84 <_ntoa_format+0x260>
      buf[len++] = '0';
80013950:	03000e13          	li	t3,48
    while ((len < prec) && (len < PRINTF_NTOA_BUFFER_SIZE)) {
80013954:	02000e93          	li	t4,32
80013958:	0080006f          	j	80013960 <_ntoa_format+0x3c>
8001395c:	01d78a63          	beq	a5,t4,80013970 <_ntoa_format+0x4c>
      buf[len++] = '0';
80013960:	00178793          	addi	a5,a5,1
80013964:	00f70333          	add	t1,a4,a5
80013968:	ffc30fa3          	sb	t3,-1(t1)
    while ((len < prec) && (len < PRINTF_NTOA_BUFFER_SIZE)) {
8001396c:	fe57e8e3          	bltu	a5,t0,8001395c <_ntoa_format+0x38>
    }
    while ((flags & FLAGS_ZEROPAD) && (len < width) && (len < PRINTF_NTOA_BUFFER_SIZE)) {
80013970:	02080863          	beqz	a6,800139a0 <_ntoa_format+0x7c>
80013974:	03f7f663          	bgeu	a5,t6,800139a0 <_ntoa_format+0x7c>
80013978:	02000813          	li	a6,32
      buf[len++] = '0';
8001397c:	03000e13          	li	t3,48
    while ((flags & FLAGS_ZEROPAD) && (len < width) && (len < PRINTF_NTOA_BUFFER_SIZE)) {
80013980:	02000e93          	li	t4,32
80013984:	01079663          	bne	a5,a6,80013990 <_ntoa_format+0x6c>
80013988:	0d80006f          	j	80013a60 <_ntoa_format+0x13c>
8001398c:	0dd78a63          	beq	a5,t4,80013a60 <_ntoa_format+0x13c>
      buf[len++] = '0';
80013990:	00178793          	addi	a5,a5,1
80013994:	00f70333          	add	t1,a4,a5
80013998:	ffc30fa3          	sb	t3,-1(t1)
    while ((flags & FLAGS_ZEROPAD) && (len < width) && (len < PRINTF_NTOA_BUFFER_SIZE)) {
8001399c:	fff798e3          	bne	a5,t6,8001398c <_ntoa_format+0x68>
    }
  }

  // handle hash
  if (flags & FLAGS_HASH) {
800139a0:	0103f813          	andi	a6,t2,16
800139a4:	06080463          	beqz	a6,80013a0c <_ntoa_format+0xe8>
    if (!(flags & FLAGS_PRECISION) && len && ((len == prec) || (len == width))) {
800139a8:	4003f813          	andi	a6,t2,1024
800139ac:	0c081c63          	bnez	a6,80013a84 <_ntoa_format+0x160>
800139b0:	0c079663          	bnez	a5,80013a7c <_ntoa_format+0x158>
      len--;
      if (len && (base == 16U)) {
        len--;
      }
    }
    if ((base == 16U) && !(flags & FLAGS_UPPERCASE) && (len < PRINTF_NTOA_BUFFER_SIZE)) {
800139b4:	01000793          	li	a5,16
800139b8:	18f88e63          	beq	a7,a5,80013b54 <_ntoa_format+0x230>
      buf[len++] = 'x';
    }
    else if ((base == 16U) && (flags & FLAGS_UPPERCASE) && (len < PRINTF_NTOA_BUFFER_SIZE)) {
      buf[len++] = 'X';
    }
    else if ((base == 2U) && (len < PRINTF_NTOA_BUFFER_SIZE)) {
800139bc:	00200793          	li	a5,2
800139c0:	02f88863          	beq	a7,a5,800139f0 <_ntoa_format+0xcc>
      buf[len++] = 'b';
    }
    if (len < PRINTF_NTOA_BUFFER_SIZE) {
      buf[len++] = '0';
800139c4:	03000793          	li	a5,48
800139c8:	00f70023          	sb	a5,0(a4)
800139cc:	00100793          	li	a5,1
    }
  }

  if (len < PRINTF_NTOA_BUFFER_SIZE) {
    if (negative) {
800139d0:	040f0463          	beqz	t5,80013a18 <_ntoa_format+0xf4>
      buf[len++] = '-';
800139d4:	00f70833          	add	a6,a4,a5
800139d8:	02d00893          	li	a7,45
800139dc:	01180023          	sb	a7,0(a6)
800139e0:	00178793          	addi	a5,a5,1
    else if (flags & FLAGS_SPACE) {
      buf[len++] = ' ';
    }
  }

  return _out_rev(out, buffer, idx, maxlen, buf, len, width, flags);
800139e4:	00038893          	mv	a7,t2
800139e8:	000f8813          	mv	a6,t6
800139ec:	e0dff06f          	j	800137f8 <_out_rev>
      buf[len++] = 'b';
800139f0:	06200793          	li	a5,98
800139f4:	00f70023          	sb	a5,0(a4)
800139f8:	00100793          	li	a5,1
      buf[len++] = '0';
800139fc:	00f70833          	add	a6,a4,a5
80013a00:	03000893          	li	a7,48
80013a04:	01180023          	sb	a7,0(a6)
80013a08:	00178793          	addi	a5,a5,1
  if (len < PRINTF_NTOA_BUFFER_SIZE) {
80013a0c:	02000813          	li	a6,32
80013a10:	fd078ae3          	beq	a5,a6,800139e4 <_ntoa_format+0xc0>
    if (negative) {
80013a14:	fc0f10e3          	bnez	t5,800139d4 <_ntoa_format+0xb0>
    else if (flags & FLAGS_PLUS) {
80013a18:	0043f813          	andi	a6,t2,4
80013a1c:	0a081063          	bnez	a6,80013abc <_ntoa_format+0x198>
    else if (flags & FLAGS_SPACE) {
80013a20:	0083f813          	andi	a6,t2,8
80013a24:	fc0800e3          	beqz	a6,800139e4 <_ntoa_format+0xc0>
      buf[len++] = ' ';
80013a28:	00f70833          	add	a6,a4,a5
80013a2c:	02000893          	li	a7,32
80013a30:	01180023          	sb	a7,0(a6)
80013a34:	00178793          	addi	a5,a5,1
80013a38:	fadff06f          	j	800139e4 <_ntoa_format+0xc0>
    if (width && (flags & FLAGS_ZEROPAD) && (negative || (flags & (FLAGS_PLUS | FLAGS_SPACE)))) {
80013a3c:	06080663          	beqz	a6,80013aa8 <_ntoa_format+0x184>
80013a40:	0a0f1663          	bnez	t5,80013aec <_ntoa_format+0x1c8>
80013a44:	00c3f313          	andi	t1,t2,12
80013a48:	0a031263          	bnez	t1,80013aec <_ntoa_format+0x1c8>
    while ((len < prec) && (len < PRINTF_NTOA_BUFFER_SIZE)) {
80013a4c:	f257f4e3          	bgeu	a5,t0,80013974 <_ntoa_format+0x50>
80013a50:	01f00313          	li	t1,31
80013a54:	eef37ee3          	bgeu	t1,a5,80013950 <_ntoa_format+0x2c>
    while ((flags & FLAGS_ZEROPAD) && (len < width) && (len < PRINTF_NTOA_BUFFER_SIZE)) {
80013a58:	02000793          	li	a5,32
80013a5c:	f5f7f2e3          	bgeu	a5,t6,800139a0 <_ntoa_format+0x7c>
  if (flags & FLAGS_HASH) {
80013a60:	0103f793          	andi	a5,t2,16
80013a64:	00079663          	bnez	a5,80013a70 <_ntoa_format+0x14c>
80013a68:	02000793          	li	a5,32
80013a6c:	f79ff06f          	j	800139e4 <_ntoa_format+0xc0>
    if (!(flags & FLAGS_PRECISION) && len && ((len == prec) || (len == width))) {
80013a70:	4003f793          	andi	a5,t2,1024
80013a74:	0e079c63          	bnez	a5,80013b6c <_ntoa_format+0x248>
80013a78:	02000793          	li	a5,32
80013a7c:	06f28c63          	beq	t0,a5,80013af4 <_ntoa_format+0x1d0>
80013a80:	07f78a63          	beq	a5,t6,80013af4 <_ntoa_format+0x1d0>
    if ((base == 16U) && !(flags & FLAGS_UPPERCASE) && (len < PRINTF_NTOA_BUFFER_SIZE)) {
80013a84:	01000813          	li	a6,16
80013a88:	09088663          	beq	a7,a6,80013b14 <_ntoa_format+0x1f0>
    else if ((base == 2U) && (len < PRINTF_NTOA_BUFFER_SIZE)) {
80013a8c:	00200813          	li	a6,2
80013a90:	05088463          	beq	a7,a6,80013ad8 <_ntoa_format+0x1b4>
    if (len < PRINTF_NTOA_BUFFER_SIZE) {
80013a94:	02000813          	li	a6,32
80013a98:	f70792e3          	bne	a5,a6,800139fc <_ntoa_format+0xd8>
  return _out_rev(out, buffer, idx, maxlen, buf, len, width, flags);
80013a9c:	00038893          	mv	a7,t2
80013aa0:	000f8813          	mv	a6,t6
80013aa4:	d55ff06f          	j	800137f8 <_out_rev>
    while ((len < prec) && (len < PRINTF_NTOA_BUFFER_SIZE)) {
80013aa8:	ee57fce3          	bgeu	a5,t0,800139a0 <_ntoa_format+0x7c>
80013aac:	01f00313          	li	t1,31
80013ab0:	eaf370e3          	bgeu	t1,a5,80013950 <_ntoa_format+0x2c>
80013ab4:	02000793          	li	a5,32
80013ab8:	ee9ff06f          	j	800139a0 <_ntoa_format+0x7c>
      buf[len++] = '+';  // ignore the space if the '+' exists
80013abc:	00f70833          	add	a6,a4,a5
80013ac0:	02b00893          	li	a7,43
80013ac4:	01180023          	sb	a7,0(a6)
80013ac8:	00178793          	addi	a5,a5,1
  return _out_rev(out, buffer, idx, maxlen, buf, len, width, flags);
80013acc:	00038893          	mv	a7,t2
80013ad0:	000f8813          	mv	a6,t6
80013ad4:	d25ff06f          	j	800137f8 <_out_rev>
    else if ((base == 2U) && (len < PRINTF_NTOA_BUFFER_SIZE)) {
80013ad8:	02000813          	li	a6,32
80013adc:	f10784e3          	beq	a5,a6,800139e4 <_ntoa_format+0xc0>
      buf[len++] = 'b';
80013ae0:	00f70833          	add	a6,a4,a5
80013ae4:	00178793          	addi	a5,a5,1
80013ae8:	0c40006f          	j	80013bac <_ntoa_format+0x288>
      width--;
80013aec:	ffff8f93          	addi	t6,t6,-1
80013af0:	f5dff06f          	j	80013a4c <_ntoa_format+0x128>
      len--;
80013af4:	fff78813          	addi	a6,a5,-1
      if (len && (base == 16U)) {
80013af8:	ea080ee3          	beqz	a6,800139b4 <_ntoa_format+0x90>
80013afc:	01000313          	li	t1,16
80013b00:	08688863          	beq	a7,t1,80013b90 <_ntoa_format+0x26c>
    else if ((base == 2U) && (len < PRINTF_NTOA_BUFFER_SIZE)) {
80013b04:	00200313          	li	t1,2
80013b08:	0a688063          	beq	a7,t1,80013ba8 <_ntoa_format+0x284>
80013b0c:	00080793          	mv	a5,a6
80013b10:	eedff06f          	j	800139fc <_ntoa_format+0xd8>
    if ((base == 16U) && !(flags & FLAGS_UPPERCASE) && (len < PRINTF_NTOA_BUFFER_SIZE)) {
80013b14:	0203f813          	andi	a6,t2,32
80013b18:	02081063          	bnez	a6,80013b38 <_ntoa_format+0x214>
80013b1c:	02000813          	li	a6,32
80013b20:	ed0782e3          	beq	a5,a6,800139e4 <_ntoa_format+0xc0>
      buf[len++] = 'b';
80013b24:	00f708b3          	add	a7,a4,a5
80013b28:	00178793          	addi	a5,a5,1
      buf[len++] = 'x';
80013b2c:	07800813          	li	a6,120
80013b30:	01088023          	sb	a6,0(a7)
80013b34:	f61ff06f          	j	80013a94 <_ntoa_format+0x170>
    else if ((base == 16U) && (flags & FLAGS_UPPERCASE) && (len < PRINTF_NTOA_BUFFER_SIZE)) {
80013b38:	02000813          	li	a6,32
80013b3c:	eb0784e3          	beq	a5,a6,800139e4 <_ntoa_format+0xc0>
      buf[len++] = 'X';
80013b40:	00f70833          	add	a6,a4,a5
80013b44:	05800893          	li	a7,88
80013b48:	01180023          	sb	a7,0(a6)
80013b4c:	00178793          	addi	a5,a5,1
80013b50:	f45ff06f          	j	80013a94 <_ntoa_format+0x170>
    if ((base == 16U) && !(flags & FLAGS_UPPERCASE) && (len < PRINTF_NTOA_BUFFER_SIZE)) {
80013b54:	0203f793          	andi	a5,t2,32
80013b58:	00079e63          	bnez	a5,80013b74 <_ntoa_format+0x250>
      buf[len++] = 'x';
80013b5c:	07800793          	li	a5,120
80013b60:	00f70023          	sb	a5,0(a4)
80013b64:	00100793          	li	a5,1
80013b68:	e95ff06f          	j	800139fc <_ntoa_format+0xd8>
    if (!(flags & FLAGS_PRECISION) && len && ((len == prec) || (len == width))) {
80013b6c:	02000793          	li	a5,32
80013b70:	f15ff06f          	j	80013a84 <_ntoa_format+0x160>
      buf[len++] = 'X';
80013b74:	05800793          	li	a5,88
80013b78:	00f70023          	sb	a5,0(a4)
80013b7c:	00100793          	li	a5,1
80013b80:	e7dff06f          	j	800139fc <_ntoa_format+0xd8>
    while ((len < prec) && (len < PRINTF_NTOA_BUFFER_SIZE)) {
80013b84:	02000793          	li	a5,32
    while ((flags & FLAGS_ZEROPAD) && (len < width) && (len < PRINTF_NTOA_BUFFER_SIZE)) {
80013b88:	de0816e3          	bnez	a6,80013974 <_ntoa_format+0x50>
80013b8c:	e15ff06f          	j	800139a0 <_ntoa_format+0x7c>
    if ((base == 16U) && !(flags & FLAGS_UPPERCASE) && (len < PRINTF_NTOA_BUFFER_SIZE)) {
80013b90:	0203f893          	andi	a7,t2,32
        len--;
80013b94:	ffe78793          	addi	a5,a5,-2
    if ((base == 16U) && !(flags & FLAGS_UPPERCASE) && (len < PRINTF_NTOA_BUFFER_SIZE)) {
80013b98:	fa0894e3          	bnez	a7,80013b40 <_ntoa_format+0x21c>
      buf[len++] = 'x';
80013b9c:	00f708b3          	add	a7,a4,a5
80013ba0:	00080793          	mv	a5,a6
80013ba4:	f89ff06f          	j	80013b2c <_ntoa_format+0x208>
      buf[len++] = 'b';
80013ba8:	01070833          	add	a6,a4,a6
80013bac:	06200893          	li	a7,98
80013bb0:	01180023          	sb	a7,0(a6)
80013bb4:	ee1ff06f          	j	80013a94 <_ntoa_format+0x170>

80013bb8 <_out_char>:
  if (character) {
80013bb8:	00051463          	bnez	a0,80013bc0 <_out_char+0x8>
}
80013bbc:	00008067          	ret
    _putchar(character);
80013bc0:	b04ff06f          	j	80012ec4 <_putchar>

80013bc4 <_out_fct>:
  if (character) {
80013bc4:	00050863          	beqz	a0,80013bd4 <_out_fct+0x10>
    ((out_fct_wrap_type*)buffer)->fct(character, ((out_fct_wrap_type*)buffer)->arg);
80013bc8:	0005a783          	lw	a5,0(a1)
80013bcc:	0045a583          	lw	a1,4(a1)
80013bd0:	00078067          	jr	a5
}
80013bd4:	00008067          	ret

80013bd8 <_etoa>:


#if defined(PRINTF_SUPPORT_EXPONENTIAL)
// internal ftoa variant for exponential floating-point type, contributed by Martijn Jasperse <m.jasperse@gmail.com>
static size_t _etoa(out_fct_type out, char* buffer, size_t idx, size_t maxlen, double value, unsigned int prec, unsigned int width, unsigned int flags)
{
80013bd8:	f7010113          	addi	sp,sp,-144
80013bdc:	09212023          	sw	s2,128(sp)
80013be0:	07312e23          	sw	s3,124(sp)
80013be4:	07412c23          	sw	s4,120(sp)
80013be8:	07512a23          	sw	s5,116(sp)
80013bec:	00050993          	mv	s3,a0
80013bf0:	00058a13          	mv	s4,a1
80013bf4:	00060913          	mv	s2,a2
80013bf8:	00068a93          	mv	s5,a3
  // check for NaN and special values
  if ((value != value) || (value > DBL_MAX) || (value < -DBL_MAX)) {
80013bfc:	00070613          	mv	a2,a4
80013c00:	00078693          	mv	a3,a5
80013c04:	00070513          	mv	a0,a4
80013c08:	00078593          	mv	a1,a5
{
80013c0c:	08812423          	sw	s0,136(sp)
80013c10:	08912223          	sw	s1,132(sp)
80013c14:	07612823          	sw	s6,112(sp)
80013c18:	07812423          	sw	s8,104(sp)
80013c1c:	07912223          	sw	s9,100(sp)
80013c20:	08112623          	sw	ra,140(sp)
80013c24:	07712623          	sw	s7,108(sp)
80013c28:	07a12023          	sw	s10,96(sp)
80013c2c:	05b12e23          	sw	s11,92(sp)
80013c30:	00070413          	mv	s0,a4
80013c34:	00078493          	mv	s1,a5
80013c38:	00080c13          	mv	s8,a6
80013c3c:	00088b13          	mv	s6,a7
  if ((value != value) || (value > DBL_MAX) || (value < -DBL_MAX)) {
80013c40:	42d030ef          	jal	ra,8001786c <__eqdf2>
{
80013c44:	09012c83          	lw	s9,144(sp)
  if ((value != value) || (value > DBL_MAX) || (value < -DBL_MAX)) {
80013c48:	56051263          	bnez	a0,800141ac <_etoa+0x5d4>
80013c4c:	800007b7          	lui	a5,0x80000
80013c50:	7d07a603          	lw	a2,2000(a5) # 800007d0 <__clzsi2+0xfffe7b3c>
80013c54:	7d47a683          	lw	a3,2004(a5)
80013c58:	00040513          	mv	a0,s0
80013c5c:	00048593          	mv	a1,s1
80013c60:	499030ef          	jal	ra,800178f8 <__gedf2>
80013c64:	54a04463          	bgtz	a0,800141ac <_etoa+0x5d4>
80013c68:	800007b7          	lui	a5,0x80000
80013c6c:	7d87a603          	lw	a2,2008(a5) # 800007d8 <__clzsi2+0xfffe7b44>
80013c70:	7dc7a683          	lw	a3,2012(a5)
80013c74:	00040513          	mv	a0,s0
80013c78:	00048593          	mv	a1,s1
80013c7c:	561030ef          	jal	ra,800179dc <__ledf2>
80013c80:	52054663          	bltz	a0,800141ac <_etoa+0x5d4>
    return _ftoa(out, buffer, idx, maxlen, value, prec, width, flags);
  }

  // determine the sign
  const bool negative = value < 0;
  if (negative) {
80013c84:	00040513          	mv	a0,s0
80013c88:	00048593          	mv	a1,s1
80013c8c:	00000613          	li	a2,0
80013c90:	00000693          	li	a3,0
80013c94:	549030ef          	jal	ra,800179dc <__ledf2>
    value = -value;
80013c98:	80000bb7          	lui	s7,0x80000
80013c9c:	00812a23          	sw	s0,20(sp)
80013ca0:	009bcbb3          	xor	s7,s7,s1
  if (negative) {
80013ca4:	00054463          	bltz	a0,80013cac <_etoa+0xd4>
80013ca8:	00048b93          	mv	s7,s1
  }

  // default precision
  if (!(flags & FLAGS_PRECISION)) {
80013cac:	400cf793          	andi	a5,s9,1024
80013cb0:	02f12623          	sw	a5,44(sp)
80013cb4:	00079463          	bnez	a5,80013cbc <_etoa+0xe4>
    prec = PRINTF_DEFAULT_FLOAT_PRECISION;
80013cb8:	00600c13          	li	s8,6
    uint64_t U;
    double   F;
  } conv;

  conv.F = value;
  int exp2 = (int)((conv.U >> 52U) & 0x07FFU) - 1023;           // effectively log2
80013cbc:	014bd513          	srli	a0,s7,0x14
80013cc0:	7ff57513          	andi	a0,a0,2047
  conv.U = (conv.U & ((1ULL << 52U) - 1U)) | (1023ULL << 52U);  // drop the exponent so conv.F is now in [1,2)
  // now approximate log10 from the log2 integer part and an expansion of ln around 1.5
  int expval = (int)(0.1760912590558 + exp2 * 0.301029995663981 + (conv.F - 1.5) * 0.289529654602168);
80013cc4:	c0150513          	addi	a0,a0,-1023
80013cc8:	53d040ef          	jal	ra,80018a04 <__floatsidf>
80013ccc:	800007b7          	lui	a5,0x80000
80013cd0:	7e07a603          	lw	a2,2016(a5) # 800007e0 <__clzsi2+0xfffe7b4c>
80013cd4:	7e47a683          	lw	a3,2020(a5)
80013cd8:	5e9030ef          	jal	ra,80017ac0 <__muldf3>
80013cdc:	800007b7          	lui	a5,0x80000
80013ce0:	7e87a603          	lw	a2,2024(a5) # 800007e8 <__clzsi2+0xfffe7b54>
80013ce4:	7ec7a683          	lw	a3,2028(a5)
80013ce8:	3f1020ef          	jal	ra,800168d8 <__adddf3>
80013cec:	800007b7          	lui	a5,0x80000
  conv.U = (conv.U & ((1ULL << 52U) - 1U)) | (1023ULL << 52U);  // drop the exponent so conv.F is now in [1,2)
80013cf0:	00cb9713          	slli	a4,s7,0xc
80013cf4:	00c75713          	srli	a4,a4,0xc
  int expval = (int)(0.1760912590558 + exp2 * 0.301029995663981 + (conv.F - 1.5) * 0.289529654602168);
80013cf8:	7f07a603          	lw	a2,2032(a5) # 800007f0 <__clzsi2+0xfffe7b5c>
80013cfc:	7f47a683          	lw	a3,2036(a5)
80013d00:	3ff007b7          	lui	a5,0x3ff00
80013d04:	00e7e7b3          	or	a5,a5,a4
80013d08:	00050d93          	mv	s11,a0
80013d0c:	00058d13          	mv	s10,a1
80013d10:	00040513          	mv	a0,s0
80013d14:	00078593          	mv	a1,a5
80013d18:	428040ef          	jal	ra,80018140 <__subdf3>
80013d1c:	800007b7          	lui	a5,0x80000
80013d20:	7f87a603          	lw	a2,2040(a5) # 800007f8 <__clzsi2+0xfffe7b64>
80013d24:	7fc7a683          	lw	a3,2044(a5)
80013d28:	599030ef          	jal	ra,80017ac0 <__muldf3>
80013d2c:	00050613          	mv	a2,a0
80013d30:	00058693          	mv	a3,a1
80013d34:	000d8513          	mv	a0,s11
80013d38:	000d0593          	mv	a1,s10
80013d3c:	39d020ef          	jal	ra,800168d8 <__adddf3>
80013d40:	3c9040ef          	jal	ra,80018908 <__fixdfsi>
80013d44:	00050d13          	mv	s10,a0
  // now we want to compute 10^expval but we want to be sure it won't overflow
  exp2 = (int)(expval * 3.321928094887362 + 0.5);
80013d48:	4bd040ef          	jal	ra,80018a04 <__floatsidf>
80013d4c:	800017b7          	lui	a5,0x80001
80013d50:	8007a603          	lw	a2,-2048(a5) # 80000800 <__clzsi2+0xfffe7b6c>
80013d54:	8047a683          	lw	a3,-2044(a5)
80013d58:	00058d93          	mv	s11,a1
80013d5c:	00a12823          	sw	a0,16(sp)
80013d60:	561030ef          	jal	ra,80017ac0 <__muldf3>
80013d64:	80001737          	lui	a4,0x80001
80013d68:	80872603          	lw	a2,-2040(a4) # 80000808 <__clzsi2+0xfffe7b74>
80013d6c:	80c72683          	lw	a3,-2036(a4)
80013d70:	369020ef          	jal	ra,800168d8 <__adddf3>
80013d74:	395040ef          	jal	ra,80018908 <__fixdfsi>
  const double z  = expval * 2.302585092994046 - exp2 * 0.6931471805599453;
80013d78:	01012783          	lw	a5,16(sp)
80013d7c:	800016b7          	lui	a3,0x80001
80013d80:	8106a603          	lw	a2,-2032(a3) # 80000810 <__clzsi2+0xfffe7b7c>
80013d84:	8146a683          	lw	a3,-2028(a3)
  exp2 = (int)(expval * 3.321928094887362 + 0.5);
80013d88:	00050713          	mv	a4,a0
  const double z  = expval * 2.302585092994046 - exp2 * 0.6931471805599453;
80013d8c:	000d8593          	mv	a1,s11
80013d90:	00078513          	mv	a0,a5
  exp2 = (int)(expval * 3.321928094887362 + 0.5);
80013d94:	00070d93          	mv	s11,a4
  const double z  = expval * 2.302585092994046 - exp2 * 0.6931471805599453;
80013d98:	529030ef          	jal	ra,80017ac0 <__muldf3>
80013d9c:	00050793          	mv	a5,a0
80013da0:	000d8513          	mv	a0,s11
80013da4:	00f12823          	sw	a5,16(sp)
80013da8:	01b12c23          	sw	s11,24(sp)
80013dac:	00058d93          	mv	s11,a1
80013db0:	455040ef          	jal	ra,80018a04 <__floatsidf>
80013db4:	80001737          	lui	a4,0x80001
80013db8:	81872603          	lw	a2,-2024(a4) # 80000818 <__clzsi2+0xfffe7b84>
80013dbc:	81c72683          	lw	a3,-2020(a4)
80013dc0:	501030ef          	jal	ra,80017ac0 <__muldf3>
80013dc4:	01012783          	lw	a5,16(sp)
80013dc8:	00050613          	mv	a2,a0
80013dcc:	00058693          	mv	a3,a1
80013dd0:	00078513          	mv	a0,a5
80013dd4:	000d8593          	mv	a1,s11
80013dd8:	368040ef          	jal	ra,80018140 <__subdf3>
  const double z2 = z * z;
80013ddc:	00050613          	mv	a2,a0
80013de0:	00058693          	mv	a3,a1
80013de4:	00a12e23          	sw	a0,28(sp)
80013de8:	00b12823          	sw	a1,16(sp)
80013dec:	4d5030ef          	jal	ra,80017ac0 <__muldf3>
  conv.U = (uint64_t)(exp2 + 1023) << 52U;
  // compute exp(z) using continued fractions, see https://en.wikipedia.org/wiki/Exponential_function#Continued_fractions_for_ex
  conv.F *= 1 + 2 * z / (2 - z + (z2 / (6 + (z2 / (10 + z2 / 14)))));
80013df0:	01012783          	lw	a5,16(sp)
80013df4:	01c12703          	lw	a4,28(sp)
  const double z2 = z * z;
80013df8:	00050d93          	mv	s11,a0
  conv.F *= 1 + 2 * z / (2 - z + (z2 / (6 + (z2 / (10 + z2 / 14)))));
80013dfc:	00078693          	mv	a3,a5
  const double z2 = z * z;
80013e00:	00058813          	mv	a6,a1
  conv.F *= 1 + 2 * z / (2 - z + (z2 / (6 + (z2 / (10 + z2 / 14)))));
80013e04:	00070613          	mv	a2,a4
80013e08:	00078593          	mv	a1,a5
80013e0c:	00070513          	mv	a0,a4
80013e10:	02f12223          	sw	a5,36(sp)
80013e14:	02e12423          	sw	a4,40(sp)
  const double z2 = z * z;
80013e18:	01012823          	sw	a6,16(sp)
  conv.F *= 1 + 2 * z / (2 - z + (z2 / (6 + (z2 / (10 + z2 / 14)))));
80013e1c:	2bd020ef          	jal	ra,800168d8 <__adddf3>
80013e20:	800016b7          	lui	a3,0x80001
80013e24:	00058813          	mv	a6,a1
80013e28:	8206a603          	lw	a2,-2016(a3) # 80000820 <__clzsi2+0xfffe7b8c>
80013e2c:	01012583          	lw	a1,16(sp)
80013e30:	8246a683          	lw	a3,-2012(a3)
80013e34:	00050893          	mv	a7,a0
80013e38:	000d8513          	mv	a0,s11
80013e3c:	03112023          	sw	a7,32(sp)
80013e40:	01012e23          	sw	a6,28(sp)
80013e44:	250030ef          	jal	ra,80017094 <__divdf3>
80013e48:	80001737          	lui	a4,0x80001
80013e4c:	82872603          	lw	a2,-2008(a4) # 80000828 <__clzsi2+0xfffe7b94>
80013e50:	82c72683          	lw	a3,-2004(a4)
80013e54:	285020ef          	jal	ra,800168d8 <__adddf3>
80013e58:	00058693          	mv	a3,a1
80013e5c:	01012583          	lw	a1,16(sp)
80013e60:	00050613          	mv	a2,a0
80013e64:	000d8513          	mv	a0,s11
80013e68:	22c030ef          	jal	ra,80017094 <__divdf3>
80013e6c:	800016b7          	lui	a3,0x80001
80013e70:	8306a603          	lw	a2,-2000(a3) # 80000830 <__clzsi2+0xfffe7b9c>
80013e74:	8346a683          	lw	a3,-1996(a3)
80013e78:	261020ef          	jal	ra,800168d8 <__adddf3>
80013e7c:	00058693          	mv	a3,a1
80013e80:	01012583          	lw	a1,16(sp)
80013e84:	00050613          	mv	a2,a0
80013e88:	000d8513          	mv	a0,s11
80013e8c:	208030ef          	jal	ra,80017094 <__divdf3>
80013e90:	02812703          	lw	a4,40(sp)
80013e94:	02412783          	lw	a5,36(sp)
80013e98:	800016b7          	lui	a3,0x80001
80013e9c:	00050313          	mv	t1,a0
80013ea0:	00058d93          	mv	s11,a1
80013ea4:	8386a503          	lw	a0,-1992(a3) # 80000838 <__clzsi2+0xfffe7ba4>
80013ea8:	83c6a583          	lw	a1,-1988(a3)
80013eac:	00070613          	mv	a2,a4
80013eb0:	00078693          	mv	a3,a5
80013eb4:	00612823          	sw	t1,16(sp)
80013eb8:	288040ef          	jal	ra,80018140 <__subdf3>
80013ebc:	01012303          	lw	t1,16(sp)
80013ec0:	00050613          	mv	a2,a0
80013ec4:	00058693          	mv	a3,a1
80013ec8:	00030513          	mv	a0,t1
80013ecc:	000d8593          	mv	a1,s11
80013ed0:	209020ef          	jal	ra,800168d8 <__adddf3>
80013ed4:	02012883          	lw	a7,32(sp)
80013ed8:	01c12803          	lw	a6,28(sp)
80013edc:	00050613          	mv	a2,a0
80013ee0:	00058693          	mv	a3,a1
80013ee4:	00088513          	mv	a0,a7
80013ee8:	00080593          	mv	a1,a6
80013eec:	1a8030ef          	jal	ra,80017094 <__divdf3>
80013ef0:	800017b7          	lui	a5,0x80001
80013ef4:	8407a603          	lw	a2,-1984(a5) # 80000840 <__clzsi2+0xfffe7bac>
80013ef8:	8447a683          	lw	a3,-1980(a5)
80013efc:	1dd020ef          	jal	ra,800168d8 <__adddf3>
  conv.U = (uint64_t)(exp2 + 1023) << 52U;
80013f00:	01812783          	lw	a5,24(sp)
  conv.F *= 1 + 2 * z / (2 - z + (z2 / (6 + (z2 / (10 + z2 / 14)))));
80013f04:	00000613          	li	a2,0
  conv.U = (uint64_t)(exp2 + 1023) << 52U;
80013f08:	3ff78793          	addi	a5,a5,1023
  conv.F *= 1 + 2 * z / (2 - z + (z2 / (6 + (z2 / (10 + z2 / 14)))));
80013f0c:	01479793          	slli	a5,a5,0x14
80013f10:	00078693          	mv	a3,a5
80013f14:	3ad030ef          	jal	ra,80017ac0 <__muldf3>
  // correct for rounding errors
  if (value < conv.F) {
80013f18:	00050613          	mv	a2,a0
80013f1c:	00a12c23          	sw	a0,24(sp)
80013f20:	00058693          	mv	a3,a1
80013f24:	00b12823          	sw	a1,16(sp)
80013f28:	00040513          	mv	a0,s0
80013f2c:	000b8593          	mv	a1,s7
80013f30:	2ad030ef          	jal	ra,800179dc <__ledf2>
80013f34:	01012783          	lw	a5,16(sp)
80013f38:	01812703          	lw	a4,24(sp)
80013f3c:	02055463          	bgez	a0,80013f64 <_etoa+0x38c>
    expval--;
    conv.F /= 10;
80013f40:	800016b7          	lui	a3,0x80001
80013f44:	8286a603          	lw	a2,-2008(a3) # 80000828 <__clzsi2+0xfffe7b94>
80013f48:	82c6a683          	lw	a3,-2004(a3)
80013f4c:	00070513          	mv	a0,a4
80013f50:	00078593          	mv	a1,a5
80013f54:	140030ef          	jal	ra,80017094 <__divdf3>
    expval--;
80013f58:	fffd0d13          	addi	s10,s10,-1
    conv.F /= 10;
80013f5c:	00050713          	mv	a4,a0
80013f60:	00058793          	mv	a5,a1
  }

  // the exponent format is "%+03d" and largest value is "307", so set aside 4-5 characters
  unsigned int minwidth = ((expval < 100) && (expval > -100)) ? 4U : 5U;
80013f64:	063d0d93          	addi	s11,s10,99

  // in "%g" mode, "prec" is the number of *significant figures* not decimals
  if (flags & FLAGS_ADAPT_EXP) {
80013f68:	00bcd693          	srli	a3,s9,0xb
  unsigned int minwidth = ((expval < 100) && (expval > -100)) ? 4U : 5U;
80013f6c:	0c600613          	li	a2,198
80013f70:	01b63db3          	sltu	s11,a2,s11
  if (flags & FLAGS_ADAPT_EXP) {
80013f74:	0016f693          	andi	a3,a3,1
  unsigned int minwidth = ((expval < 100) && (expval > -100)) ? 4U : 5U;
80013f78:	004d8d93          	addi	s11,s11,4
  if (flags & FLAGS_ADAPT_EXP) {
80013f7c:	06068663          	beqz	a3,80013fe8 <_etoa+0x410>
    // do we want to fall-back to "%f" mode?
    if ((value >= 1e-4) && (value < 1e6)) {
80013f80:	800016b7          	lui	a3,0x80001
80013f84:	8486a603          	lw	a2,-1976(a3) # 80000848 <__clzsi2+0xfffe7bb4>
80013f88:	84c6a683          	lw	a3,-1972(a3)
80013f8c:	00040513          	mv	a0,s0
80013f90:	000b8593          	mv	a1,s7
80013f94:	00f12c23          	sw	a5,24(sp)
80013f98:	00e12823          	sw	a4,16(sp)
80013f9c:	15d030ef          	jal	ra,800178f8 <__gedf2>
80013fa0:	01012703          	lw	a4,16(sp)
80013fa4:	01812783          	lw	a5,24(sp)
80013fa8:	28054663          	bltz	a0,80014234 <_etoa+0x65c>
80013fac:	800016b7          	lui	a3,0x80001
80013fb0:	8506a603          	lw	a2,-1968(a3) # 80000850 <__clzsi2+0xfffe7bbc>
80013fb4:	8546a683          	lw	a3,-1964(a3)
80013fb8:	00040513          	mv	a0,s0
80013fbc:	000b8593          	mv	a1,s7
80013fc0:	21d030ef          	jal	ra,800179dc <__ledf2>
80013fc4:	01012703          	lw	a4,16(sp)
80013fc8:	01812783          	lw	a5,24(sp)
80013fcc:	26055463          	bgez	a0,80014234 <_etoa+0x65c>
      if ((int)prec > expval) {
80013fd0:	278d5c63          	bge	s10,s8,80014248 <_etoa+0x670>
        prec = (unsigned)((int)prec - expval - 1);
80013fd4:	41ac0d33          	sub	s10,s8,s10
80013fd8:	fffd0c13          	addi	s8,s10,-1
      }
      else {
        prec = 0;
      }
      flags |= FLAGS_PRECISION;   // make sure _ftoa respects precision
80013fdc:	400cec93          	ori	s9,s9,1024
      // no characters in exponent
      minwidth = 0U;
80013fe0:	00000d93          	li	s11,0
      expval   = 0;
80013fe4:	00000d13          	li	s10,0
  if (width > minwidth) {
    // we didn't fall-back so subtract the characters required for the exponent
    fwidth -= minwidth;
  } else {
    // not enough characters, so go back to default sizing
    fwidth = 0U;
80013fe8:	00000893          	li	a7,0
  if (width > minwidth) {
80013fec:	016df463          	bgeu	s11,s6,80013ff4 <_etoa+0x41c>
    fwidth -= minwidth;
80013ff0:	41bb08b3          	sub	a7,s6,s11
  }
  if ((flags & FLAGS_LEFT) && minwidth) {
80013ff4:	002cf693          	andi	a3,s9,2
80013ff8:	00d12c23          	sw	a3,24(sp)
80013ffc:	00068863          	beqz	a3,8001400c <_etoa+0x434>
    // if we're padding on the right, DON'T pad the floating part
    fwidth = 0U;
80014000:	001db693          	seqz	a3,s11
80014004:	40d006b3          	neg	a3,a3
80014008:	00d8f8b3          	and	a7,a7,a3
  }

  // rescale the float value
  if (expval) {
8001400c:	200d1063          	bnez	s10,8001420c <_etoa+0x634>
    value /= conv.F;
  }

  // output the floating part
  const size_t start_idx = idx;
  idx = _ftoa(out, buffer, idx, maxlen, negative ? -value : value, prec, fwidth, flags & ~FLAGS_ADAPT_EXP);
80014010:	00040513          	mv	a0,s0
80014014:	00048593          	mv	a1,s1
80014018:	00000613          	li	a2,0
8001401c:	00000693          	li	a3,0
80014020:	01112823          	sw	a7,16(sp)
80014024:	1b9030ef          	jal	ra,800179dc <__ledf2>
80014028:	01012883          	lw	a7,16(sp)
8001402c:	00055663          	bgez	a0,80014038 <_etoa+0x460>
80014030:	800007b7          	lui	a5,0x80000
80014034:	0177cbb3          	xor	s7,a5,s7
80014038:	01412783          	lw	a5,20(sp)
8001403c:	000c0813          	mv	a6,s8
80014040:	000a8693          	mv	a3,s5
80014044:	00078613          	mv	a2,a5
80014048:	fffff7b7          	lui	a5,0xfffff
8001404c:	7ff78793          	addi	a5,a5,2047 # fffff7ff <__clzsi2+0x7ffe6b6b>
80014050:	00fcf7b3          	and	a5,s9,a5
80014054:	00f12023          	sw	a5,0(sp)
80014058:	00060713          	mv	a4,a2
8001405c:	000b8793          	mv	a5,s7
80014060:	00090613          	mv	a2,s2
80014064:	000a0593          	mv	a1,s4
80014068:	00098513          	mv	a0,s3
8001406c:	1e4000ef          	jal	ra,80014250 <_ftoa>
80014070:	00050493          	mv	s1,a0

  // output the exponent part
  if (minwidth) {
80014074:	0e0d8c63          	beqz	s11,8001416c <_etoa+0x594>
    // output the exponential symbol
    out((flags & FLAGS_UPPERCASE) ? 'E' : 'e', buffer, idx++, maxlen);
80014078:	01ac9513          	slli	a0,s9,0x1a
8001407c:	41f55513          	srai	a0,a0,0x1f
80014080:	fe057513          	andi	a0,a0,-32
80014084:	00048613          	mv	a2,s1
80014088:	000a8693          	mv	a3,s5
8001408c:	000a0593          	mv	a1,s4
80014090:	06550513          	addi	a0,a0,101
80014094:	000980e7          	jalr	s3
    // output the exponent value
    idx = _ntoa_long(out, buffer, idx, maxlen, (expval < 0) ? -expval : expval, expval < 0, 10, 0, minwidth-1, FLAGS_ZEROPAD | FLAGS_PLUS);
80014098:	41fd5793          	srai	a5,s10,0x1f
8001409c:	01a7c433          	xor	s0,a5,s10
800140a0:	40f40433          	sub	s0,s0,a5
800140a4:	fffd8793          	addi	a5,s11,-1
    out((flags & FLAGS_UPPERCASE) ? 'E' : 'e', buffer, idx++, maxlen);
800140a8:	00148493          	addi	s1,s1,1
    idx = _ntoa_long(out, buffer, idx, maxlen, (expval < 0) ? -expval : expval, expval < 0, 10, 0, minwidth-1, FLAGS_ZEROPAD | FLAGS_PLUS);
800140ac:	01fd5d13          	srli	s10,s10,0x1f
800140b0:	00f12823          	sw	a5,16(sp)
800140b4:	00000c93          	li	s9,0
    } while (value && (len < PRINTF_NTOA_BUFFER_SIZE));
800140b8:	00900b93          	li	s7,9
800140bc:	02000c13          	li	s8,32
800140c0:	0080006f          	j	800140c8 <_etoa+0x4f0>
800140c4:	038c8e63          	beq	s9,s8,80014100 <_etoa+0x528>
      const char digit = (char)(value % base);
800140c8:	00a00593          	li	a1,10
800140cc:	00040513          	mv	a0,s0
800140d0:	361040ef          	jal	ra,80018c30 <__umodsi3>
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
800140d4:	001c8c93          	addi	s9,s9,1
800140d8:	03010713          	addi	a4,sp,48
800140dc:	03050793          	addi	a5,a0,48
800140e0:	01970733          	add	a4,a4,s9
      value /= base;
800140e4:	00040513          	mv	a0,s0
800140e8:	00a00593          	li	a1,10
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
800140ec:	fef70fa3          	sb	a5,-1(a4)
      value /= base;
800140f0:	00040d93          	mv	s11,s0
800140f4:	2f5040ef          	jal	ra,80018be8 <__udivsi3>
800140f8:	00050413          	mv	s0,a0
    } while (value && (len < PRINTF_NTOA_BUFFER_SIZE));
800140fc:	fdbbe4e3          	bltu	s7,s11,800140c4 <_etoa+0x4ec>
  return _ntoa_format(out, buffer, idx, maxlen, buf, len, negative, (unsigned int)base, prec, width, flags);
80014100:	00500793          	li	a5,5
80014104:	00f12423          	sw	a5,8(sp)
80014108:	01012783          	lw	a5,16(sp)
8001410c:	00048613          	mv	a2,s1
80014110:	00012023          	sw	zero,0(sp)
80014114:	00f12223          	sw	a5,4(sp)
80014118:	00a00893          	li	a7,10
8001411c:	000c8793          	mv	a5,s9
80014120:	000d0813          	mv	a6,s10
80014124:	03010713          	addi	a4,sp,48
80014128:	000a8693          	mv	a3,s5
8001412c:	000a0593          	mv	a1,s4
80014130:	00098513          	mv	a0,s3
80014134:	ff0ff0ef          	jal	ra,80013924 <_ntoa_format>
    // might need to right-pad spaces
    if (flags & FLAGS_LEFT) {
80014138:	01812783          	lw	a5,24(sp)
  return _ntoa_format(out, buffer, idx, maxlen, buf, len, negative, (unsigned int)base, prec, width, flags);
8001413c:	00050493          	mv	s1,a0
    if (flags & FLAGS_LEFT) {
80014140:	02078663          	beqz	a5,8001416c <_etoa+0x594>
      while (idx - start_idx < width) out(' ', buffer, idx++, maxlen);
80014144:	41250933          	sub	s2,a0,s2
80014148:	03697263          	bgeu	s2,s6,8001416c <_etoa+0x594>
8001414c:	00048613          	mv	a2,s1
80014150:	00190913          	addi	s2,s2,1
80014154:	000a8693          	mv	a3,s5
80014158:	000a0593          	mv	a1,s4
8001415c:	02000513          	li	a0,32
80014160:	00148493          	addi	s1,s1,1
80014164:	000980e7          	jalr	s3
80014168:	ff6962e3          	bltu	s2,s6,8001414c <_etoa+0x574>
    }
  }
  return idx;
}
8001416c:	08c12083          	lw	ra,140(sp)
80014170:	08812403          	lw	s0,136(sp)
80014174:	08012903          	lw	s2,128(sp)
80014178:	07c12983          	lw	s3,124(sp)
8001417c:	07812a03          	lw	s4,120(sp)
80014180:	07412a83          	lw	s5,116(sp)
80014184:	07012b03          	lw	s6,112(sp)
80014188:	06c12b83          	lw	s7,108(sp)
8001418c:	06812c03          	lw	s8,104(sp)
80014190:	06412c83          	lw	s9,100(sp)
80014194:	06012d03          	lw	s10,96(sp)
80014198:	05c12d83          	lw	s11,92(sp)
8001419c:	00048513          	mv	a0,s1
800141a0:	08412483          	lw	s1,132(sp)
800141a4:	09010113          	addi	sp,sp,144
800141a8:	00008067          	ret
    return _ftoa(out, buffer, idx, maxlen, value, prec, width, flags);
800141ac:	00040713          	mv	a4,s0
}
800141b0:	08812403          	lw	s0,136(sp)
    return _ftoa(out, buffer, idx, maxlen, value, prec, width, flags);
800141b4:	09912823          	sw	s9,144(sp)
}
800141b8:	08c12083          	lw	ra,140(sp)
800141bc:	06c12b83          	lw	s7,108(sp)
800141c0:	06412c83          	lw	s9,100(sp)
800141c4:	06012d03          	lw	s10,96(sp)
800141c8:	05c12d83          	lw	s11,92(sp)
    return _ftoa(out, buffer, idx, maxlen, value, prec, width, flags);
800141cc:	00048793          	mv	a5,s1
800141d0:	000b0893          	mv	a7,s6
}
800141d4:	08412483          	lw	s1,132(sp)
800141d8:	07012b03          	lw	s6,112(sp)
    return _ftoa(out, buffer, idx, maxlen, value, prec, width, flags);
800141dc:	000c0813          	mv	a6,s8
800141e0:	000a8693          	mv	a3,s5
}
800141e4:	06812c03          	lw	s8,104(sp)
800141e8:	07412a83          	lw	s5,116(sp)
    return _ftoa(out, buffer, idx, maxlen, value, prec, width, flags);
800141ec:	00090613          	mv	a2,s2
800141f0:	000a0593          	mv	a1,s4
}
800141f4:	08012903          	lw	s2,128(sp)
800141f8:	07812a03          	lw	s4,120(sp)
    return _ftoa(out, buffer, idx, maxlen, value, prec, width, flags);
800141fc:	00098513          	mv	a0,s3
}
80014200:	07c12983          	lw	s3,124(sp)
80014204:	09010113          	addi	sp,sp,144
    return _ftoa(out, buffer, idx, maxlen, value, prec, width, flags);
80014208:	0480006f          	j	80014250 <_ftoa>
    value /= conv.F;
8001420c:	000b8593          	mv	a1,s7
80014210:	00070613          	mv	a2,a4
80014214:	00040513          	mv	a0,s0
80014218:	00078693          	mv	a3,a5
8001421c:	01112823          	sw	a7,16(sp)
80014220:	675020ef          	jal	ra,80017094 <__divdf3>
80014224:	01012883          	lw	a7,16(sp)
80014228:	00a12a23          	sw	a0,20(sp)
8001422c:	00058b93          	mv	s7,a1
80014230:	de1ff06f          	j	80014010 <_etoa+0x438>
      if ((prec > 0) && (flags & FLAGS_PRECISION)) {
80014234:	da0c0ae3          	beqz	s8,80013fe8 <_etoa+0x410>
        --prec;
80014238:	02c12683          	lw	a3,44(sp)
8001423c:	00d036b3          	snez	a3,a3
80014240:	40dc0c33          	sub	s8,s8,a3
80014244:	da5ff06f          	j	80013fe8 <_etoa+0x410>
        prec = 0;
80014248:	00000c13          	li	s8,0
8001424c:	d91ff06f          	j	80013fdc <_etoa+0x404>

80014250 <_ftoa>:
{
80014250:	f8010113          	addi	sp,sp,-128
80014254:	07312623          	sw	s3,108(sp)
80014258:	07412423          	sw	s4,104(sp)
8001425c:	07512223          	sw	s5,100(sp)
80014260:	05812c23          	sw	s8,88(sp)
80014264:	00050993          	mv	s3,a0
80014268:	00058a13          	mv	s4,a1
8001426c:	00060a93          	mv	s5,a2
80014270:	00068c13          	mv	s8,a3
  if (value != value)
80014274:	00070613          	mv	a2,a4
80014278:	00078693          	mv	a3,a5
8001427c:	00070513          	mv	a0,a4
80014280:	00078593          	mv	a1,a5
{
80014284:	06812c23          	sw	s0,120(sp)
80014288:	06912a23          	sw	s1,116(sp)
8001428c:	07212823          	sw	s2,112(sp)
80014290:	07612023          	sw	s6,96(sp)
80014294:	05912a23          	sw	s9,84(sp)
80014298:	06112e23          	sw	ra,124(sp)
8001429c:	05712e23          	sw	s7,92(sp)
800142a0:	05a12823          	sw	s10,80(sp)
800142a4:	05b12623          	sw	s11,76(sp)
800142a8:	00070493          	mv	s1,a4
800142ac:	00078413          	mv	s0,a5
800142b0:	00080b13          	mv	s6,a6
800142b4:	00088913          	mv	s2,a7
  if (value != value)
800142b8:	5b4030ef          	jal	ra,8001786c <__eqdf2>
{
800142bc:	08012c83          	lw	s9,128(sp)
  if (value != value)
800142c0:	2a051263          	bnez	a0,80014564 <_ftoa+0x314>
  if (value < -DBL_MAX)
800142c4:	800007b7          	lui	a5,0x80000
800142c8:	7d87a603          	lw	a2,2008(a5) # 800007d8 <__clzsi2+0xfffe7b44>
800142cc:	7dc7a683          	lw	a3,2012(a5)
800142d0:	00048513          	mv	a0,s1
800142d4:	00040593          	mv	a1,s0
800142d8:	704030ef          	jal	ra,800179dc <__ledf2>
800142dc:	38054063          	bltz	a0,8001465c <_ftoa+0x40c>
  if (value > DBL_MAX)
800142e0:	800007b7          	lui	a5,0x80000
800142e4:	7d07a603          	lw	a2,2000(a5) # 800007d0 <__clzsi2+0xfffe7b3c>
800142e8:	7d47a683          	lw	a3,2004(a5)
800142ec:	00048513          	mv	a0,s1
800142f0:	00040593          	mv	a1,s0
800142f4:	604030ef          	jal	ra,800178f8 <__gedf2>
800142f8:	24a04663          	bgtz	a0,80014544 <_ftoa+0x2f4>
  if ((value > PRINTF_MAX_FLOAT) || (value < -PRINTF_MAX_FLOAT)) {
800142fc:	800017b7          	lui	a5,0x80001
80014300:	8587a603          	lw	a2,-1960(a5) # 80000858 <__clzsi2+0xfffe7bc4>
80014304:	85c7a683          	lw	a3,-1956(a5)
80014308:	00048513          	mv	a0,s1
8001430c:	00040593          	mv	a1,s0
80014310:	5e8030ef          	jal	ra,800178f8 <__gedf2>
80014314:	2ea04463          	bgtz	a0,800145fc <_ftoa+0x3ac>
80014318:	800017b7          	lui	a5,0x80001
8001431c:	8607a603          	lw	a2,-1952(a5) # 80000860 <__clzsi2+0xfffe7bcc>
80014320:	8647a683          	lw	a3,-1948(a5)
80014324:	00048513          	mv	a0,s1
80014328:	00040593          	mv	a1,s0
8001432c:	6b0030ef          	jal	ra,800179dc <__ledf2>
80014330:	2c054663          	bltz	a0,800145fc <_ftoa+0x3ac>
  if (value < 0) {
80014334:	00048513          	mv	a0,s1
80014338:	00040593          	mv	a1,s0
8001433c:	00000613          	li	a2,0
80014340:	00000693          	li	a3,0
80014344:	698030ef          	jal	ra,800179dc <__ledf2>
80014348:	28054663          	bltz	a0,800145d4 <_ftoa+0x384>
  bool negative = false;
8001434c:	00012e23          	sw	zero,28(sp)
  if (!(flags & FLAGS_PRECISION)) {
80014350:	400cf793          	andi	a5,s9,1024
80014354:	00079463          	bnez	a5,8001435c <_ftoa+0x10c>
    prec = PRINTF_DEFAULT_FLOAT_PRECISION;
80014358:	00600b13          	li	s6,6
8001435c:	02010693          	addi	a3,sp,32
80014360:	fe0b0513          	addi	a0,s6,-32
80014364:	000b0b93          	mv	s7,s6
  while ((len < PRINTF_FTOA_BUFFER_SIZE) && (prec > 9U)) {
80014368:	00900613          	li	a2,9
    buf[len++] = '0';
8001436c:	03000593          	li	a1,48
80014370:	0140006f          	j	80014384 <_ftoa+0x134>
80014374:	00b68023          	sb	a1,0(a3)
    prec--;
80014378:	fffb8b93          	addi	s7,s7,-1 # 7fffffff <__clzsi2+0xfffe736b>
  while ((len < PRINTF_FTOA_BUFFER_SIZE) && (prec > 9U)) {
8001437c:	00168693          	addi	a3,a3,1
80014380:	3eab8863          	beq	s7,a0,80014770 <_ftoa+0x520>
    buf[len++] = '0';
80014384:	417b0db3          	sub	s11,s6,s7
  while ((len < PRINTF_FTOA_BUFFER_SIZE) && (prec > 9U)) {
80014388:	ff7666e3          	bltu	a2,s7,80014374 <_ftoa+0x124>
  int whole = (int)value;
8001438c:	00040593          	mv	a1,s0
80014390:	00048513          	mv	a0,s1
80014394:	574040ef          	jal	ra,80018908 <__fixdfsi>
  double tmp = (value - whole) * pow10[prec];
80014398:	800006b7          	lui	a3,0x80000
8001439c:	003b9613          	slli	a2,s7,0x3
800143a0:	60868693          	addi	a3,a3,1544 # 80000608 <__clzsi2+0xfffe7974>
800143a4:	00c686b3          	add	a3,a3,a2
800143a8:	0046a803          	lw	a6,4(a3)
800143ac:	0006a783          	lw	a5,0(a3)
  int whole = (int)value;
800143b0:	00050d13          	mv	s10,a0
  double tmp = (value - whole) * pow10[prec];
800143b4:	01012623          	sw	a6,12(sp)
800143b8:	00f12423          	sw	a5,8(sp)
800143bc:	648040ef          	jal	ra,80018a04 <__floatsidf>
800143c0:	00050613          	mv	a2,a0
800143c4:	00058693          	mv	a3,a1
800143c8:	00048513          	mv	a0,s1
800143cc:	00040593          	mv	a1,s0
800143d0:	571030ef          	jal	ra,80018140 <__subdf3>
800143d4:	00812603          	lw	a2,8(sp)
800143d8:	00c12683          	lw	a3,12(sp)
800143dc:	6e4030ef          	jal	ra,80017ac0 <__muldf3>
800143e0:	00b12a23          	sw	a1,20(sp)
800143e4:	00a12c23          	sw	a0,24(sp)
  unsigned long frac = (unsigned long)tmp;
800143e8:	5a0040ef          	jal	ra,80018988 <__fixunsdfsi>
800143ec:	00050b13          	mv	s6,a0
  diff = tmp - frac;
800143f0:	6bc040ef          	jal	ra,80018aac <__floatunsidf>
800143f4:	01412783          	lw	a5,20(sp)
800143f8:	01812303          	lw	t1,24(sp)
800143fc:	00050613          	mv	a2,a0
80014400:	00058693          	mv	a3,a1
80014404:	00030513          	mv	a0,t1
80014408:	00078593          	mv	a1,a5
8001440c:	535030ef          	jal	ra,80018140 <__subdf3>
  if (diff > 0.5) {
80014410:	80001737          	lui	a4,0x80001
80014414:	80872303          	lw	t1,-2040(a4) # 80000808 <__clzsi2+0xfffe7b74>
80014418:	80c72383          	lw	t2,-2036(a4)
  diff = tmp - frac;
8001441c:	00a12a23          	sw	a0,20(sp)
  if (diff > 0.5) {
80014420:	00030613          	mv	a2,t1
80014424:	00038693          	mv	a3,t2
  diff = tmp - frac;
80014428:	00b12c23          	sw	a1,24(sp)
  if (diff > 0.5) {
8001442c:	4cc030ef          	jal	ra,800178f8 <__gedf2>
80014430:	800017b7          	lui	a5,0x80001
80014434:	8087a303          	lw	t1,-2040(a5) # 80000808 <__clzsi2+0xfffe7b74>
80014438:	80c7a383          	lw	t2,-2036(a5)
8001443c:	30a05863          	blez	a0,8001474c <_ftoa+0x4fc>
    ++frac;
80014440:	001b0b13          	addi	s6,s6,1
    if (frac >= pow10[prec]) {
80014444:	000b0513          	mv	a0,s6
80014448:	664040ef          	jal	ra,80018aac <__floatunsidf>
8001444c:	00050613          	mv	a2,a0
80014450:	00058693          	mv	a3,a1
80014454:	00812503          	lw	a0,8(sp)
80014458:	00c12583          	lw	a1,12(sp)
8001445c:	580030ef          	jal	ra,800179dc <__ledf2>
80014460:	00a04663          	bgtz	a0,8001446c <_ftoa+0x21c>
      ++whole;
80014464:	001d0d13          	addi	s10,s10,1
      frac = 0;
80014468:	00000b13          	li	s6,0
  if (prec == 0U) {
8001446c:	200b8463          	beqz	s7,80014674 <_ftoa+0x424>
    while (len < PRINTF_FTOA_BUFFER_SIZE) {
80014470:	02000413          	li	s0,32
      if (!(frac /= 10U)) {
80014474:	00900493          	li	s1,9
80014478:	0380006f          	j	800144b0 <_ftoa+0x260>
      buf[len++] = (char)(48U + (frac % 10U));
8001447c:	7b4040ef          	jal	ra,80018c30 <__umodsi3>
80014480:	001d8d93          	addi	s11,s11,1
80014484:	020d8793          	addi	a5,s11,32
80014488:	03050693          	addi	a3,a0,48
8001448c:	00278633          	add	a2,a5,sp
80014490:	fed60fa3          	sb	a3,-1(a2)
      if (!(frac /= 10U)) {
80014494:	000b0513          	mv	a0,s6
80014498:	00a00593          	li	a1,10
8001449c:	74c040ef          	jal	ra,80018be8 <__udivsi3>
      --count;
800144a0:	fffb8693          	addi	a3,s7,-1
      if (!(frac /= 10U)) {
800144a4:	3164f063          	bgeu	s1,s6,800147a4 <_ftoa+0x554>
      --count;
800144a8:	00068b93          	mv	s7,a3
      if (!(frac /= 10U)) {
800144ac:	00050b13          	mv	s6,a0
      buf[len++] = (char)(48U + (frac % 10U));
800144b0:	00a00593          	li	a1,10
800144b4:	000b0513          	mv	a0,s6
    while (len < PRINTF_FTOA_BUFFER_SIZE) {
800144b8:	fc8d92e3          	bne	s11,s0,8001447c <_ftoa+0x22c>
  if (!(flags & FLAGS_LEFT) && (flags & FLAGS_ZEROPAD)) {
800144bc:	003cf793          	andi	a5,s9,3
800144c0:	00100713          	li	a4,1
800144c4:	36e79063          	bne	a5,a4,80014824 <_ftoa+0x5d4>
    if (width && (negative || (flags & (FLAGS_PLUS | FLAGS_SPACE)))) {
800144c8:	02000793          	li	a5,32
800144cc:	00090e63          	beqz	s2,800144e8 <_ftoa+0x298>
800144d0:	01c12703          	lw	a4,28(sp)
800144d4:	34071c63          	bnez	a4,8001482c <_ftoa+0x5dc>
800144d8:	00ccf713          	andi	a4,s9,12
800144dc:	34070c63          	beqz	a4,80014834 <_ftoa+0x5e4>
      width--;
800144e0:	fff90913          	addi	s2,s2,-1
    while ((len < width) && (len < PRINTF_FTOA_BUFFER_SIZE)) {
800144e4:	3527f063          	bgeu	a5,s2,80014824 <_ftoa+0x5d4>
  return _out_rev(out, buffer, idx, maxlen, buf, len, width, flags);
800144e8:	000c8893          	mv	a7,s9
800144ec:	00090813          	mv	a6,s2
800144f0:	02010713          	addi	a4,sp,32
800144f4:	000c0693          	mv	a3,s8
800144f8:	000a8613          	mv	a2,s5
800144fc:	000a0593          	mv	a1,s4
80014500:	00098513          	mv	a0,s3
80014504:	af4ff0ef          	jal	ra,800137f8 <_out_rev>
}
80014508:	07c12083          	lw	ra,124(sp)
8001450c:	07812403          	lw	s0,120(sp)
80014510:	07412483          	lw	s1,116(sp)
80014514:	07012903          	lw	s2,112(sp)
80014518:	06c12983          	lw	s3,108(sp)
8001451c:	06812a03          	lw	s4,104(sp)
80014520:	06412a83          	lw	s5,100(sp)
80014524:	06012b03          	lw	s6,96(sp)
80014528:	05c12b83          	lw	s7,92(sp)
8001452c:	05812c03          	lw	s8,88(sp)
80014530:	05412c83          	lw	s9,84(sp)
80014534:	05012d03          	lw	s10,80(sp)
80014538:	04c12d83          	lw	s11,76(sp)
8001453c:	08010113          	addi	sp,sp,128
80014540:	00008067          	ret
    return _out_rev(out, buffer, idx, maxlen, (flags & FLAGS_PLUS) ? "fni+" : "fni", (flags & FLAGS_PLUS) ? 4U : 3U, width, flags);
80014544:	004cf793          	andi	a5,s9,4
80014548:	06079e63          	bnez	a5,800145c4 <_ftoa+0x374>
8001454c:	80000737          	lui	a4,0x80000
80014550:	45470713          	addi	a4,a4,1108 # 80000454 <__clzsi2+0xfffe77c0>
80014554:	00300793          	li	a5,3
80014558:	000c8893          	mv	a7,s9
8001455c:	00090813          	mv	a6,s2
80014560:	0180006f          	j	80014578 <_ftoa+0x328>
    return _out_rev(out, buffer, idx, maxlen, "nan", 3, width, flags);
80014564:	80000737          	lui	a4,0x80000
80014568:	000c8893          	mv	a7,s9
8001456c:	00090813          	mv	a6,s2
80014570:	00300793          	li	a5,3
80014574:	45870713          	addi	a4,a4,1112 # 80000458 <__clzsi2+0xfffe77c4>
}
80014578:	07812403          	lw	s0,120(sp)
8001457c:	07c12083          	lw	ra,124(sp)
80014580:	07412483          	lw	s1,116(sp)
80014584:	07012903          	lw	s2,112(sp)
80014588:	06012b03          	lw	s6,96(sp)
8001458c:	05c12b83          	lw	s7,92(sp)
80014590:	05412c83          	lw	s9,84(sp)
80014594:	05012d03          	lw	s10,80(sp)
80014598:	04c12d83          	lw	s11,76(sp)
    return _out_rev(out, buffer, idx, maxlen, "fni-", 4, width, flags);
8001459c:	000c0693          	mv	a3,s8
800145a0:	000a8613          	mv	a2,s5
}
800145a4:	05812c03          	lw	s8,88(sp)
800145a8:	06412a83          	lw	s5,100(sp)
    return _out_rev(out, buffer, idx, maxlen, "fni-", 4, width, flags);
800145ac:	000a0593          	mv	a1,s4
800145b0:	00098513          	mv	a0,s3
}
800145b4:	06812a03          	lw	s4,104(sp)
800145b8:	06c12983          	lw	s3,108(sp)
800145bc:	08010113          	addi	sp,sp,128
    return _out_rev(out, buffer, idx, maxlen, "fni-", 4, width, flags);
800145c0:	a38ff06f          	j	800137f8 <_out_rev>
    return _out_rev(out, buffer, idx, maxlen, (flags & FLAGS_PLUS) ? "fni+" : "fni", (flags & FLAGS_PLUS) ? 4U : 3U, width, flags);
800145c4:	80000737          	lui	a4,0x80000
800145c8:	44c70713          	addi	a4,a4,1100 # 8000044c <__clzsi2+0xfffe77b8>
800145cc:	00400793          	li	a5,4
800145d0:	f89ff06f          	j	80014558 <_ftoa+0x308>
    value = 0 - value;
800145d4:	00048613          	mv	a2,s1
800145d8:	00040693          	mv	a3,s0
800145dc:	00000513          	li	a0,0
800145e0:	00000593          	li	a1,0
800145e4:	35d030ef          	jal	ra,80018140 <__subdf3>
    negative = true;
800145e8:	00100793          	li	a5,1
    value = 0 - value;
800145ec:	00050493          	mv	s1,a0
800145f0:	00058413          	mv	s0,a1
    negative = true;
800145f4:	00f12e23          	sw	a5,28(sp)
800145f8:	d59ff06f          	j	80014350 <_ftoa+0x100>
    return _etoa(out, buffer, idx, maxlen, value, prec, width, flags);
800145fc:	00040793          	mv	a5,s0
}
80014600:	07812403          	lw	s0,120(sp)
    return _etoa(out, buffer, idx, maxlen, value, prec, width, flags);
80014604:	09912023          	sw	s9,128(sp)
}
80014608:	07c12083          	lw	ra,124(sp)
8001460c:	05c12b83          	lw	s7,92(sp)
80014610:	05412c83          	lw	s9,84(sp)
80014614:	05012d03          	lw	s10,80(sp)
80014618:	04c12d83          	lw	s11,76(sp)
    return _etoa(out, buffer, idx, maxlen, value, prec, width, flags);
8001461c:	00048713          	mv	a4,s1
80014620:	00090893          	mv	a7,s2
}
80014624:	07412483          	lw	s1,116(sp)
80014628:	07012903          	lw	s2,112(sp)
    return _etoa(out, buffer, idx, maxlen, value, prec, width, flags);
8001462c:	000b0813          	mv	a6,s6
80014630:	000c0693          	mv	a3,s8
}
80014634:	06012b03          	lw	s6,96(sp)
80014638:	05812c03          	lw	s8,88(sp)
    return _etoa(out, buffer, idx, maxlen, value, prec, width, flags);
8001463c:	000a8613          	mv	a2,s5
80014640:	000a0593          	mv	a1,s4
}
80014644:	06412a83          	lw	s5,100(sp)
80014648:	06812a03          	lw	s4,104(sp)
    return _etoa(out, buffer, idx, maxlen, value, prec, width, flags);
8001464c:	00098513          	mv	a0,s3
}
80014650:	06c12983          	lw	s3,108(sp)
80014654:	08010113          	addi	sp,sp,128
    return _etoa(out, buffer, idx, maxlen, value, prec, width, flags);
80014658:	d80ff06f          	j	80013bd8 <_etoa>
    return _out_rev(out, buffer, idx, maxlen, "fni-", 4, width, flags);
8001465c:	80000737          	lui	a4,0x80000
80014660:	000c8893          	mv	a7,s9
80014664:	00090813          	mv	a6,s2
80014668:	00400793          	li	a5,4
8001466c:	45c70713          	addi	a4,a4,1116 # 8000045c <__clzsi2+0xfffe77c8>
80014670:	f09ff06f          	j	80014578 <_ftoa+0x328>
    diff = value - (double)whole;
80014674:	000d0513          	mv	a0,s10
80014678:	38c040ef          	jal	ra,80018a04 <__floatsidf>
8001467c:	00050613          	mv	a2,a0
80014680:	00058693          	mv	a3,a1
80014684:	00048513          	mv	a0,s1
80014688:	00040593          	mv	a1,s0
8001468c:	2b5030ef          	jal	ra,80018140 <__subdf3>
    if ((!(diff < 0.5) || (diff > 0.5)) && (whole & 1)) {
80014690:	800017b7          	lui	a5,0x80001
80014694:	8087ab03          	lw	s6,-2040(a5) # 80000808 <__clzsi2+0xfffe7b74>
80014698:	80c7ab83          	lw	s7,-2036(a5)
    diff = value - (double)whole;
8001469c:	00050493          	mv	s1,a0
    if ((!(diff < 0.5) || (diff > 0.5)) && (whole & 1)) {
800146a0:	000b0613          	mv	a2,s6
800146a4:	000b8693          	mv	a3,s7
    diff = value - (double)whole;
800146a8:	00058413          	mv	s0,a1
    if ((!(diff < 0.5) || (diff > 0.5)) && (whole & 1)) {
800146ac:	330030ef          	jal	ra,800179dc <__ledf2>
800146b0:	08055863          	bgez	a0,80014740 <_ftoa+0x4f0>
800146b4:	00048513          	mv	a0,s1
800146b8:	00040593          	mv	a1,s0
800146bc:	000b0613          	mv	a2,s6
800146c0:	000b8693          	mv	a3,s7
800146c4:	234030ef          	jal	ra,800178f8 <__gedf2>
800146c8:	06a04c63          	bgtz	a0,80014740 <_ftoa+0x4f0>
  while (len < PRINTF_FTOA_BUFFER_SIZE) {
800146cc:	02000413          	li	s0,32
    buf[len++] = (char)(48 + (whole % 10));
800146d0:	00a00593          	li	a1,10
800146d4:	000d0513          	mv	a0,s10
  while (len < PRINTF_FTOA_BUFFER_SIZE) {
800146d8:	de8d82e3          	beq	s11,s0,800144bc <_ftoa+0x26c>
    buf[len++] = (char)(48 + (whole % 10));
800146dc:	588040ef          	jal	ra,80018c64 <__modsi3>
800146e0:	001d8d93          	addi	s11,s11,1
800146e4:	020d8713          	addi	a4,s11,32
800146e8:	03050793          	addi	a5,a0,48
800146ec:	002706b3          	add	a3,a4,sp
    if (!(whole /= 10)) {
800146f0:	000d0513          	mv	a0,s10
800146f4:	00a00593          	li	a1,10
    buf[len++] = (char)(48 + (whole % 10));
800146f8:	fef68fa3          	sb	a5,-1(a3)
    if (!(whole /= 10)) {
800146fc:	4e4040ef          	jal	ra,80018be0 <__divsi3>
80014700:	00050d13          	mv	s10,a0
80014704:	fc0516e3          	bnez	a0,800146d0 <_ftoa+0x480>
  if (!(flags & FLAGS_LEFT) && (flags & FLAGS_ZEROPAD)) {
80014708:	003cf793          	andi	a5,s9,3
8001470c:	00100693          	li	a3,1
80014710:	12d78863          	beq	a5,a3,80014840 <_ftoa+0x5f0>
  if (len < PRINTF_FTOA_BUFFER_SIZE) {
80014714:	02000693          	li	a3,32
80014718:	10dd8663          	beq	s11,a3,80014824 <_ftoa+0x5d4>
    if (negative) {
8001471c:	01c12783          	lw	a5,28(sp)
80014720:	04078c63          	beqz	a5,80014778 <_ftoa+0x528>
      buf[len++] = '-';
80014724:	020d8793          	addi	a5,s11,32
80014728:	02010713          	addi	a4,sp,32
8001472c:	00e786b3          	add	a3,a5,a4
80014730:	02d00713          	li	a4,45
80014734:	001d8793          	addi	a5,s11,1
80014738:	fee68023          	sb	a4,-32(a3)
8001473c:	dadff06f          	j	800144e8 <_ftoa+0x298>
      ++whole;
80014740:	001d7793          	andi	a5,s10,1
80014744:	00fd0d33          	add	s10,s10,a5
80014748:	f85ff06f          	j	800146cc <_ftoa+0x47c>
  else if (diff < 0.5) {
8001474c:	01412503          	lw	a0,20(sp)
80014750:	01812583          	lw	a1,24(sp)
80014754:	00030613          	mv	a2,t1
80014758:	00038693          	mv	a3,t2
8001475c:	280030ef          	jal	ra,800179dc <__ledf2>
80014760:	d00546e3          	bltz	a0,8001446c <_ftoa+0x21c>
  else if ((frac == 0U) || (frac & 1U)) {
80014764:	0a0b1863          	bnez	s6,80014814 <_ftoa+0x5c4>
    ++frac;
80014768:	001b0b13          	addi	s6,s6,1
8001476c:	d01ff06f          	j	8001446c <_ftoa+0x21c>
80014770:	02000d93          	li	s11,32
80014774:	c19ff06f          	j	8001438c <_ftoa+0x13c>
    else if (flags & FLAGS_PLUS) {
80014778:	004cf793          	andi	a5,s9,4
8001477c:	06079e63          	bnez	a5,800147f8 <_ftoa+0x5a8>
    else if (flags & FLAGS_SPACE) {
80014780:	008cf613          	andi	a2,s9,8
80014784:	000d8793          	mv	a5,s11
80014788:	d60600e3          	beqz	a2,800144e8 <_ftoa+0x298>
      buf[len++] = ' ';
8001478c:	020d8713          	addi	a4,s11,32
80014790:	02010613          	addi	a2,sp,32
80014794:	00c70733          	add	a4,a4,a2
80014798:	001d8793          	addi	a5,s11,1
8001479c:	fed70023          	sb	a3,-32(a4)
800147a0:	d49ff06f          	j	800144e8 <_ftoa+0x298>
    while ((len < PRINTF_FTOA_BUFFER_SIZE) && (count-- > 0U)) {
800147a4:	d08d8ce3          	beq	s11,s0,800144bc <_ftoa+0x26c>
800147a8:	000d8613          	mv	a2,s11
      buf[len++] = '0';
800147ac:	03000893          	li	a7,48
    while ((len < PRINTF_FTOA_BUFFER_SIZE) && (count-- > 0U)) {
800147b0:	02000513          	li	a0,32
800147b4:	02068463          	beqz	a3,800147dc <_ftoa+0x58c>
      buf[len++] = '0';
800147b8:	00160613          	addi	a2,a2,1
800147bc:	02060793          	addi	a5,a2,32
800147c0:	40cb86b3          	sub	a3,s7,a2
800147c4:	002785b3          	add	a1,a5,sp
800147c8:	fff68693          	addi	a3,a3,-1
800147cc:	ff158fa3          	sb	a7,-1(a1)
    while ((len < PRINTF_FTOA_BUFFER_SIZE) && (count-- > 0U)) {
800147d0:	00dd86b3          	add	a3,s11,a3
800147d4:	cea604e3          	beq	a2,a0,800144bc <_ftoa+0x26c>
800147d8:	fe0690e3          	bnez	a3,800147b8 <_ftoa+0x568>
      buf[len++] = '.';
800147dc:	02060793          	addi	a5,a2,32
800147e0:	02010713          	addi	a4,sp,32
800147e4:	00e787b3          	add	a5,a5,a4
800147e8:	02e00693          	li	a3,46
800147ec:	00160d93          	addi	s11,a2,1
800147f0:	fed78023          	sb	a3,-32(a5)
800147f4:	ed9ff06f          	j	800146cc <_ftoa+0x47c>
      buf[len++] = '+';  // ignore the space if the '+' exists
800147f8:	020d8793          	addi	a5,s11,32
800147fc:	02010713          	addi	a4,sp,32
80014800:	00e786b3          	add	a3,a5,a4
80014804:	02b00713          	li	a4,43
80014808:	001d8793          	addi	a5,s11,1
8001480c:	fee68023          	sb	a4,-32(a3)
80014810:	cd9ff06f          	j	800144e8 <_ftoa+0x298>
  else if ((frac == 0U) || (frac & 1U)) {
80014814:	001b7693          	andi	a3,s6,1
80014818:	c4068ae3          	beqz	a3,8001446c <_ftoa+0x21c>
    ++frac;
8001481c:	001b0b13          	addi	s6,s6,1
80014820:	c4dff06f          	j	8001446c <_ftoa+0x21c>
  if (!(flags & FLAGS_LEFT) && (flags & FLAGS_ZEROPAD)) {
80014824:	02000793          	li	a5,32
80014828:	cc1ff06f          	j	800144e8 <_ftoa+0x298>
      width--;
8001482c:	fff90913          	addi	s2,s2,-1
    while ((len < width) && (len < PRINTF_FTOA_BUFFER_SIZE)) {
80014830:	cb9ff06f          	j	800144e8 <_ftoa+0x298>
80014834:	cb27eae3          	bltu	a5,s2,800144e8 <_ftoa+0x298>
80014838:	02000d93          	li	s11,32
8001483c:	ed9ff06f          	j	80014714 <_ftoa+0x4c4>
    if (width && (negative || (flags & (FLAGS_PLUS | FLAGS_SPACE)))) {
80014840:	ec090ae3          	beqz	s2,80014714 <_ftoa+0x4c4>
80014844:	01c12783          	lw	a5,28(sp)
80014848:	04079063          	bnez	a5,80014888 <_ftoa+0x638>
8001484c:	00ccf793          	andi	a5,s9,12
80014850:	02079c63          	bnez	a5,80014888 <_ftoa+0x638>
    while ((len < width) && (len < PRINTF_FTOA_BUFFER_SIZE)) {
80014854:	ed2df0e3          	bgeu	s11,s2,80014714 <_ftoa+0x4c4>
80014858:	02000593          	li	a1,32
8001485c:	000d8793          	mv	a5,s11
      buf[len++] = '0';
80014860:	03000613          	li	a2,48
    while ((len < width) && (len < PRINTF_FTOA_BUFFER_SIZE)) {
80014864:	02000693          	li	a3,32
80014868:	c8bd80e3          	beq	s11,a1,800144e8 <_ftoa+0x298>
      buf[len++] = '0';
8001486c:	00178793          	addi	a5,a5,1
80014870:	02078713          	addi	a4,a5,32
80014874:	00270733          	add	a4,a4,sp
80014878:	fec70fa3          	sb	a2,-1(a4)
    while ((len < width) && (len < PRINTF_FTOA_BUFFER_SIZE)) {
8001487c:	01278a63          	beq	a5,s2,80014890 <_ftoa+0x640>
80014880:	fed796e3          	bne	a5,a3,8001486c <_ftoa+0x61c>
80014884:	c65ff06f          	j	800144e8 <_ftoa+0x298>
      width--;
80014888:	fff90913          	addi	s2,s2,-1
8001488c:	fc9ff06f          	j	80014854 <_ftoa+0x604>
80014890:	00090d93          	mv	s11,s2
80014894:	e81ff06f          	j	80014714 <_ftoa+0x4c4>

80014898 <_vsnprintf>:
#endif  // PRINTF_SUPPORT_FLOAT


// internal vsnprintf
static int _vsnprintf(out_fct_type out, char* buffer, const size_t maxlen, const char* format, va_list va)
{
80014898:	f7010113          	addi	sp,sp,-144
8001489c:	08912223          	sw	s1,132(sp)
800148a0:	09212023          	sw	s2,128(sp)
800148a4:	07312e23          	sw	s3,124(sp)
800148a8:	07512a23          	sw	s5,116(sp)
800148ac:	07712623          	sw	s7,108(sp)
800148b0:	08112623          	sw	ra,140(sp)
800148b4:	08812423          	sw	s0,136(sp)
800148b8:	07412c23          	sw	s4,120(sp)
800148bc:	07612823          	sw	s6,112(sp)
800148c0:	07812423          	sw	s8,104(sp)
800148c4:	07912223          	sw	s9,100(sp)
800148c8:	07a12023          	sw	s10,96(sp)
800148cc:	05b12e23          	sw	s11,92(sp)
800148d0:	00058913          	mv	s2,a1
800148d4:	00060493          	mv	s1,a2
800148d8:	00068b93          	mv	s7,a3
800148dc:	00070a93          	mv	s5,a4
800148e0:	00050993          	mv	s3,a0
  unsigned int flags, width, precision, n;
  size_t idx = 0U;

  if (!buffer) {
800148e4:	40058c63          	beqz	a1,80014cfc <_vsnprintf+0x464>
    // use null output function
    out = _out_null;
  }

  while (*format)
800148e8:	000bc503          	lbu	a0,0(s7)
800148ec:	00000c13          	li	s8,0
800148f0:	42050063          	beqz	a0,80014d10 <_vsnprintf+0x478>
      default :
        break;
    }

    // evaluate specifier
    switch (*format) {
800148f4:	800007b7          	lui	a5,0x80000
800148f8:	4f878793          	addi	a5,a5,1272 # 800004f8 <__clzsi2+0xfffe7864>
800148fc:	00f12e23          	sw	a5,28(sp)
    switch (*format) {
80014900:	800007b7          	lui	a5,0x80000
      switch (*format) {
80014904:	80000a37          	lui	s4,0x80000
    switch (*format) {
80014908:	4ac78793          	addi	a5,a5,1196 # 800004ac <__clzsi2+0xfffe7818>
      switch (*format) {
8001490c:	468a0a13          	addi	s4,s4,1128 # 80000468 <__clzsi2+0xfffe77d4>
    switch (*format) {
80014910:	00f12c23          	sw	a5,24(sp)
80014914:	0200006f          	j	80014934 <_vsnprintf+0x9c>
        out('%', buffer, idx++, maxlen);
        format++;
        break;

      default :
        out(*format, buffer, idx++, maxlen);
80014918:	000c0613          	mv	a2,s8
8001491c:	00048693          	mv	a3,s1
80014920:	00090593          	mv	a1,s2
80014924:	001c0c13          	addi	s8,s8,1
80014928:	000980e7          	jalr	s3
  while (*format)
8001492c:	000bc503          	lbu	a0,0(s7)
80014930:	1c050863          	beqz	a0,80014b00 <_vsnprintf+0x268>
    if (*format != '%') {
80014934:	02500793          	li	a5,37
      format++;
80014938:	001b8b93          	addi	s7,s7,1
    if (*format != '%') {
8001493c:	fcf51ee3          	bne	a0,a5,80014918 <_vsnprintf+0x80>
    flags = 0U;
80014940:	00000413          	li	s0,0
      switch (*format) {
80014944:	01000693          	li	a3,16
80014948:	000bc503          	lbu	a0,0(s7)
      format++;
8001494c:	001b8713          	addi	a4,s7,1
      switch (*format) {
80014950:	fe050793          	addi	a5,a0,-32
80014954:	0ff7f793          	zext.b	a5,a5
80014958:	00f6ea63          	bltu	a3,a5,8001496c <_vsnprintf+0xd4>
8001495c:	00279793          	slli	a5,a5,0x2
80014960:	014787b3          	add	a5,a5,s4
80014964:	0007a783          	lw	a5,0(a5)
80014968:	00078067          	jr	a5
  return (ch >= '0') && (ch <= '9');
8001496c:	fd050793          	addi	a5,a0,-48
    if (_is_digit(*format)) {
80014970:	0ff7f793          	zext.b	a5,a5
80014974:	00900693          	li	a3,9
80014978:	1ef6f463          	bgeu	a3,a5,80014b60 <_vsnprintf+0x2c8>
    else if (*format == '*') {
8001497c:	02a00793          	li	a5,42
    width = 0U;
80014980:	00000d13          	li	s10,0
    else if (*format == '*') {
80014984:	24f50663          	beq	a0,a5,80014bd0 <_vsnprintf+0x338>
    if (*format == '.') {
80014988:	02e00793          	li	a5,46
    precision = 0U;
8001498c:	00000b13          	li	s6,0
    if (*format == '.') {
80014990:	20f50863          	beq	a0,a5,80014ba0 <_vsnprintf+0x308>
    switch (*format) {
80014994:	f9850793          	addi	a5,a0,-104
80014998:	0ff7f793          	zext.b	a5,a5
8001499c:	01200693          	li	a3,18
800149a0:	06f6e063          	bltu	a3,a5,80014a00 <_vsnprintf+0x168>
800149a4:	01812683          	lw	a3,24(sp)
800149a8:	00279793          	slli	a5,a5,0x2
800149ac:	00d787b3          	add	a5,a5,a3
800149b0:	0007a783          	lw	a5,0(a5)
800149b4:	00078067          	jr	a5
        case '0': flags |= FLAGS_ZEROPAD; format++; n = 1U; break;
800149b8:	00146413          	ori	s0,s0,1
      format++;
800149bc:	00070b93          	mv	s7,a4
800149c0:	f89ff06f          	j	80014948 <_vsnprintf+0xb0>
        case '-': flags |= FLAGS_LEFT;    format++; n = 1U; break;
800149c4:	00246413          	ori	s0,s0,2
      format++;
800149c8:	00070b93          	mv	s7,a4
800149cc:	f7dff06f          	j	80014948 <_vsnprintf+0xb0>
        case '+': flags |= FLAGS_PLUS;    format++; n = 1U; break;
800149d0:	00446413          	ori	s0,s0,4
      format++;
800149d4:	00070b93          	mv	s7,a4
800149d8:	f71ff06f          	j	80014948 <_vsnprintf+0xb0>
        case '#': flags |= FLAGS_HASH;    format++; n = 1U; break;
800149dc:	01046413          	ori	s0,s0,16
      format++;
800149e0:	00070b93          	mv	s7,a4
800149e4:	f65ff06f          	j	80014948 <_vsnprintf+0xb0>
        case ' ': flags |= FLAGS_SPACE;   format++; n = 1U; break;
800149e8:	00846413          	ori	s0,s0,8
      format++;
800149ec:	00070b93          	mv	s7,a4
800149f0:	f59ff06f          	j	80014948 <_vsnprintf+0xb0>
        if (*format == 'l') {
800149f4:	001bc503          	lbu	a0,1(s7)
        flags |= (sizeof(size_t) == sizeof(long) ? FLAGS_LONG : FLAGS_LONG_LONG);
800149f8:	10046413          	ori	s0,s0,256
        break;
800149fc:	00170713          	addi	a4,a4,1
    switch (*format) {
80014a00:	06700793          	li	a5,103
        format++;
80014a04:	00070b93          	mv	s7,a4
    switch (*format) {
80014a08:	02a7e863          	bltu	a5,a0,80014a38 <_vsnprintf+0x1a0>
80014a0c:	02400793          	li	a5,36
80014a10:	f0a7f4e3          	bgeu	a5,a0,80014918 <_vsnprintf+0x80>
80014a14:	fdb50793          	addi	a5,a0,-37
80014a18:	0ff7f793          	zext.b	a5,a5
80014a1c:	04200713          	li	a4,66
80014a20:	eef76ce3          	bltu	a4,a5,80014918 <_vsnprintf+0x80>
80014a24:	01c12703          	lw	a4,28(sp)
80014a28:	00279793          	slli	a5,a5,0x2
80014a2c:	00e787b3          	add	a5,a5,a4
80014a30:	0007a783          	lw	a5,0(a5)
80014a34:	00078067          	jr	a5
80014a38:	f9750793          	addi	a5,a0,-105
80014a3c:	0ff7f793          	zext.b	a5,a5
80014a40:	00f00713          	li	a4,15
80014a44:	ecf76ae3          	bltu	a4,a5,80014918 <_vsnprintf+0x80>
80014a48:	00100d93          	li	s11,1
80014a4c:	00009737          	lui	a4,0x9
80014a50:	00fd9db3          	sll	s11,s11,a5
80014a54:	04170713          	addi	a4,a4,65 # 9041 <font6x8-0x7fff6feb>
80014a58:	00edfdb3          	and	s11,s11,a4
80014a5c:	520d9a63          	bnez	s11,80014f90 <_vsnprintf+0x6f8>
80014a60:	00a00713          	li	a4,10
80014a64:	2ae78c63          	beq	a5,a4,80014d1c <_vsnprintf+0x484>
80014a68:	00700713          	li	a4,7
80014a6c:	eae796e3          	bne	a5,a4,80014918 <_vsnprintf+0x80>
          idx = _ntoa_long(out, buffer, idx, maxlen, (unsigned long)((uintptr_t)va_arg(va, void*)), false, 16U, precision, width, flags);
80014a70:	000aa783          	lw	a5,0(s5)
        flags |= FLAGS_ZEROPAD | FLAGS_UPPERCASE;
80014a74:	02146e13          	ori	t3,s0,33
          idx = _ntoa_long(out, buffer, idx, maxlen, (unsigned long)((uintptr_t)va_arg(va, void*)), false, 16U, precision, width, flags);
80014a78:	004a8a93          	addi	s5,s5,4
  if (!value) {
80014a7c:	3a078063          	beqz	a5,80014e1c <_vsnprintf+0x584>
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
80014a80:	00900513          	li	a0,9
    } while (value && (len < PRINTF_NTOA_BUFFER_SIZE));
80014a84:	00f00593          	li	a1,15
80014a88:	02000893          	li	a7,32
80014a8c:	00c0006f          	j	80014a98 <_vsnprintf+0x200>
80014a90:	031d8863          	beq	s11,a7,80014ac0 <_vsnprintf+0x228>
      value /= base;
80014a94:	00070793          	mv	a5,a4
      const char digit = (char)(value % base);
80014a98:	00f7f613          	andi	a2,a5,15
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
80014a9c:	03760693          	addi	a3,a2,55
80014aa0:	00c56463          	bltu	a0,a2,80014aa8 <_vsnprintf+0x210>
80014aa4:	03060693          	addi	a3,a2,48
80014aa8:	001d8d93          	addi	s11,s11,1
80014aac:	03010713          	addi	a4,sp,48
80014ab0:	01b70733          	add	a4,a4,s11
80014ab4:	fed70fa3          	sb	a3,-1(a4)
      value /= base;
80014ab8:	0047d713          	srli	a4,a5,0x4
    } while (value && (len < PRINTF_NTOA_BUFFER_SIZE));
80014abc:	fcf5eae3          	bltu	a1,a5,80014a90 <_vsnprintf+0x1f8>
  return _ntoa_format(out, buffer, idx, maxlen, buf, len, negative, (unsigned int)base, prec, width, flags);
80014ac0:	00800793          	li	a5,8
80014ac4:	00f12223          	sw	a5,4(sp)
80014ac8:	000c0613          	mv	a2,s8
80014acc:	01c12423          	sw	t3,8(sp)
80014ad0:	01612023          	sw	s6,0(sp)
80014ad4:	01000893          	li	a7,16
80014ad8:	00000813          	li	a6,0
80014adc:	000d8793          	mv	a5,s11
80014ae0:	03010713          	addi	a4,sp,48
80014ae4:	00048693          	mv	a3,s1
80014ae8:	00090593          	mv	a1,s2
80014aec:	00098513          	mv	a0,s3
80014af0:	e35fe0ef          	jal	ra,80013924 <_ntoa_format>
80014af4:	00050c13          	mv	s8,a0
  while (*format)
80014af8:	000bc503          	lbu	a0,0(s7)
80014afc:	e2051ce3          	bnez	a0,80014934 <_vsnprintf+0x9c>

  // termination
  out((char)0, buffer, idx < maxlen ? idx : maxlen - 1U, maxlen);

  // return written chars without terminating \0
  return (int)idx;
80014b00:	000c0413          	mv	s0,s8
  out((char)0, buffer, idx < maxlen ? idx : maxlen - 1U, maxlen);
80014b04:	009c6463          	bltu	s8,s1,80014b0c <_vsnprintf+0x274>
80014b08:	fff48c13          	addi	s8,s1,-1
80014b0c:	00048693          	mv	a3,s1
80014b10:	000c0613          	mv	a2,s8
80014b14:	00090593          	mv	a1,s2
80014b18:	00000513          	li	a0,0
80014b1c:	000980e7          	jalr	s3
}
80014b20:	08c12083          	lw	ra,140(sp)
80014b24:	00040513          	mv	a0,s0
80014b28:	08812403          	lw	s0,136(sp)
80014b2c:	08412483          	lw	s1,132(sp)
80014b30:	08012903          	lw	s2,128(sp)
80014b34:	07c12983          	lw	s3,124(sp)
80014b38:	07812a03          	lw	s4,120(sp)
80014b3c:	07412a83          	lw	s5,116(sp)
80014b40:	07012b03          	lw	s6,112(sp)
80014b44:	06c12b83          	lw	s7,108(sp)
80014b48:	06812c03          	lw	s8,104(sp)
80014b4c:	06412c83          	lw	s9,100(sp)
80014b50:	06012d03          	lw	s10,96(sp)
80014b54:	05c12d83          	lw	s11,92(sp)
80014b58:	09010113          	addi	sp,sp,144
80014b5c:	00008067          	ret
  unsigned int i = 0U;
80014b60:	00000d13          	li	s10,0
  while (_is_digit(**str)) {
80014b64:	00900693          	li	a3,9
    i = i * 10U + (unsigned int)(*((*str)++) - '0');
80014b68:	002d1793          	slli	a5,s10,0x2
80014b6c:	01a787b3          	add	a5,a5,s10
80014b70:	00179793          	slli	a5,a5,0x1
80014b74:	00a787b3          	add	a5,a5,a0
  while (_is_digit(**str)) {
80014b78:	00074503          	lbu	a0,0(a4)
    i = i * 10U + (unsigned int)(*((*str)++) - '0');
80014b7c:	fd078d13          	addi	s10,a5,-48
80014b80:	00070b93          	mv	s7,a4
  return (ch >= '0') && (ch <= '9');
80014b84:	fd050793          	addi	a5,a0,-48
  while (_is_digit(**str)) {
80014b88:	0ff7f793          	zext.b	a5,a5
80014b8c:	00170713          	addi	a4,a4,1
80014b90:	fcf6fce3          	bgeu	a3,a5,80014b68 <_vsnprintf+0x2d0>
    if (*format == '.') {
80014b94:	02e00793          	li	a5,46
    precision = 0U;
80014b98:	00000b13          	li	s6,0
    if (*format == '.') {
80014b9c:	def51ce3          	bne	a0,a5,80014994 <_vsnprintf+0xfc>
      if (_is_digit(*format)) {
80014ba0:	001bc503          	lbu	a0,1(s7)
80014ba4:	00900613          	li	a2,9
      format++;
80014ba8:	00070693          	mv	a3,a4
  return (ch >= '0') && (ch <= '9');
80014bac:	fd050793          	addi	a5,a0,-48
      if (_is_digit(*format)) {
80014bb0:	0ff7f793          	zext.b	a5,a5
      flags |= FLAGS_PRECISION;
80014bb4:	40046413          	ori	s0,s0,1024
      if (_is_digit(*format)) {
80014bb8:	1ef67e63          	bgeu	a2,a5,80014db4 <_vsnprintf+0x51c>
      else if (*format == '*') {
80014bbc:	02a00793          	li	a5,42
80014bc0:	22f50663          	beq	a0,a5,80014dec <_vsnprintf+0x554>
      format++;
80014bc4:	00070b93          	mv	s7,a4
80014bc8:	00170713          	addi	a4,a4,1
80014bcc:	dc9ff06f          	j	80014994 <_vsnprintf+0xfc>
      const int w = va_arg(va, int);
80014bd0:	000aad03          	lw	s10,0(s5)
80014bd4:	004a8a93          	addi	s5,s5,4
      if (w < 0) {
80014bd8:	000d4a63          	bltz	s10,80014bec <_vsnprintf+0x354>
    if (*format == '.') {
80014bdc:	001bc503          	lbu	a0,1(s7)
      format++;
80014be0:	00070b93          	mv	s7,a4
80014be4:	00170713          	addi	a4,a4,1
80014be8:	da1ff06f          	j	80014988 <_vsnprintf+0xf0>
    if (*format == '.') {
80014bec:	001bc503          	lbu	a0,1(s7)
        flags |= FLAGS_LEFT;    // reverse padding
80014bf0:	00246413          	ori	s0,s0,2
      format++;
80014bf4:	00070b93          	mv	s7,a4
        width = (unsigned int)-w;
80014bf8:	41a00d33          	neg	s10,s10
      format++;
80014bfc:	00170713          	addi	a4,a4,1
80014c00:	d89ff06f          	j	80014988 <_vsnprintf+0xf0>
        if (*format == 'l') {
80014c04:	001bc503          	lbu	a0,1(s7)
        flags |= (sizeof(intmax_t) == sizeof(long) ? FLAGS_LONG : FLAGS_LONG_LONG);
80014c08:	20046413          	ori	s0,s0,512
        break;
80014c0c:	00170713          	addi	a4,a4,1
80014c10:	df1ff06f          	j	80014a00 <_vsnprintf+0x168>
        if (*format == 'h') {
80014c14:	001bc503          	lbu	a0,1(s7)
80014c18:	06800793          	li	a5,104
80014c1c:	4af50063          	beq	a0,a5,800150bc <_vsnprintf+0x824>
        flags |= FLAGS_SHORT;
80014c20:	08046413          	ori	s0,s0,128
80014c24:	00170713          	addi	a4,a4,1
80014c28:	dd9ff06f          	j	80014a00 <_vsnprintf+0x168>
        if (*format == 'l') {
80014c2c:	001bc503          	lbu	a0,1(s7)
80014c30:	06c00793          	li	a5,108
80014c34:	dcf512e3          	bne	a0,a5,800149f8 <_vsnprintf+0x160>
    switch (*format) {
80014c38:	002bc503          	lbu	a0,2(s7)
          flags |= FLAGS_LONG_LONG;
80014c3c:	30046413          	ori	s0,s0,768
          format++;
80014c40:	003b8713          	addi	a4,s7,3
80014c44:	dbdff06f          	j	80014a00 <_vsnprintf+0x168>
        if (*format == 'F') flags |= FLAGS_UPPERCASE;
80014c48:	04600793          	li	a5,70
80014c4c:	32f50e63          	beq	a0,a5,80014f88 <_vsnprintf+0x6f0>
        idx = _ftoa(out, buffer, idx, maxlen, va_arg(va, double), precision, width, flags);
80014c50:	007a8a93          	addi	s5,s5,7
80014c54:	ff8afa93          	andi	s5,s5,-8
80014c58:	000aa703          	lw	a4,0(s5)
80014c5c:	004aa783          	lw	a5,4(s5)
80014c60:	000c0613          	mv	a2,s8
80014c64:	00812023          	sw	s0,0(sp)
80014c68:	000d0893          	mv	a7,s10
80014c6c:	000b0813          	mv	a6,s6
80014c70:	00048693          	mv	a3,s1
80014c74:	00090593          	mv	a1,s2
80014c78:	00098513          	mv	a0,s3
80014c7c:	dd4ff0ef          	jal	ra,80014250 <_ftoa>
80014c80:	008a8a93          	addi	s5,s5,8
80014c84:	00050c13          	mv	s8,a0
        break;
80014c88:	ca5ff06f          	j	8001492c <_vsnprintf+0x94>
        out('%', buffer, idx++, maxlen);
80014c8c:	000c0613          	mv	a2,s8
80014c90:	00048693          	mv	a3,s1
80014c94:	00090593          	mv	a1,s2
80014c98:	02500513          	li	a0,37
80014c9c:	001c0c13          	addi	s8,s8,1
80014ca0:	000980e7          	jalr	s3
        break;
80014ca4:	c89ff06f          	j	8001492c <_vsnprintf+0x94>
        if (!(flags & FLAGS_LEFT)) {
80014ca8:	00247413          	andi	s0,s0,2
            idx = _ntoa_long(out, buffer, idx, maxlen, va_arg(va, unsigned long), false, base, precision, width, flags);
80014cac:	004a8b13          	addi	s6,s5,4
        if (!(flags & FLAGS_LEFT)) {
80014cb0:	46040e63          	beqz	s0,8001512c <_vsnprintf+0x894>
        out((char)va_arg(va, int), buffer, idx++, maxlen);
80014cb4:	000ac503          	lbu	a0,0(s5)
80014cb8:	000c0613          	mv	a2,s8
80014cbc:	00048693          	mv	a3,s1
80014cc0:	00090593          	mv	a1,s2
80014cc4:	000980e7          	jalr	s3
          while (l++ < width) {
80014cc8:	00100793          	li	a5,1
      out(*format, buffer, idx++, maxlen);
80014ccc:	001c0413          	addi	s0,s8,1
          while (l++ < width) {
80014cd0:	01ac0c33          	add	s8,s8,s10
80014cd4:	61a7f263          	bgeu	a5,s10,800152d8 <_vsnprintf+0xa40>
            out(' ', buffer, idx++, maxlen);
80014cd8:	00040613          	mv	a2,s0
80014cdc:	00048693          	mv	a3,s1
80014ce0:	00140413          	addi	s0,s0,1
80014ce4:	00090593          	mv	a1,s2
80014ce8:	02000513          	li	a0,32
80014cec:	000980e7          	jalr	s3
          while (l++ < width) {
80014cf0:	ff8414e3          	bne	s0,s8,80014cd8 <_vsnprintf+0x440>
        break;
80014cf4:	000b0a93          	mv	s5,s6
80014cf8:	c35ff06f          	j	8001492c <_vsnprintf+0x94>
  while (*format)
80014cfc:	000bc503          	lbu	a0,0(s7)
    out = _out_null;
80014d00:	800139b7          	lui	s3,0x80013
80014d04:	7f498993          	addi	s3,s3,2036 # 800137f4 <__clzsi2+0xffffab60>
  while (*format)
80014d08:	00000c13          	li	s8,0
80014d0c:	be0514e3          	bnez	a0,800148f4 <_vsnprintf+0x5c>
80014d10:	00000413          	li	s0,0
  out((char)0, buffer, idx < maxlen ? idx : maxlen - 1U, maxlen);
80014d14:	de9c6ce3          	bltu	s8,s1,80014b0c <_vsnprintf+0x274>
80014d18:	df1ff06f          	j	80014b08 <_vsnprintf+0x270>
        const char* p = va_arg(va, char*);
80014d1c:	000aac83          	lw	s9,0(s5)
80014d20:	004a8793          	addi	a5,s5,4
80014d24:	02f12023          	sw	a5,32(sp)
  for (s = str; *s && maxsize--; ++s);
80014d28:	000cc683          	lbu	a3,0(s9)
80014d2c:	00068513          	mv	a0,a3
        unsigned int l = _strnlen_s(p, precision ? precision : (size_t)-1);
80014d30:	0e0b1063          	bnez	s6,80014e10 <_vsnprintf+0x578>
  for (s = str; *s && maxsize--; ++s);
80014d34:	ffe00793          	li	a5,-2
80014d38:	02068463          	beqz	a3,80014d60 <_vsnprintf+0x4c8>
80014d3c:	00178793          	addi	a5,a5,1
80014d40:	00fc8733          	add	a4,s9,a5
80014d44:	000c8d93          	mv	s11,s9
80014d48:	0080006f          	j	80014d50 <_vsnprintf+0x4b8>
80014d4c:	00ed8863          	beq	s11,a4,80014d5c <_vsnprintf+0x4c4>
80014d50:	001dc783          	lbu	a5,1(s11)
80014d54:	001d8d93          	addi	s11,s11,1
80014d58:	fe079ae3          	bnez	a5,80014d4c <_vsnprintf+0x4b4>
  return (unsigned int)(s - str);
80014d5c:	419d8db3          	sub	s11,s11,s9
        if (flags & FLAGS_PRECISION) {
80014d60:	40047a93          	andi	s5,s0,1024
80014d64:	000a8663          	beqz	s5,80014d70 <_vsnprintf+0x4d8>
          l = (l < precision ? l : precision);
80014d68:	01bb7463          	bgeu	s6,s11,80014d70 <_vsnprintf+0x4d8>
80014d6c:	000b0d93          	mv	s11,s6
        if (!(flags & FLAGS_LEFT)) {
80014d70:	00247413          	andi	s0,s0,2
80014d74:	36040a63          	beqz	s0,800150e8 <_vsnprintf+0x850>
        while ((*p != 0) && (!(flags & FLAGS_PRECISION) || precision--)) {
80014d78:	2e068863          	beqz	a3,80015068 <_vsnprintf+0x7d0>
80014d7c:	2a0a8e63          	beqz	s5,80015038 <_vsnprintf+0x7a0>
80014d80:	018b0b33          	add	s6,s6,s8
  for (s = str; *s && maxsize--; ++s);
80014d84:	000c0613          	mv	a2,s8
        while ((*p != 0) && (!(flags & FLAGS_PRECISION) || precision--)) {
80014d88:	2ccb0c63          	beq	s6,a2,80015060 <_vsnprintf+0x7c8>
          out(*(p++), buffer, idx++, maxlen);
80014d8c:	00048693          	mv	a3,s1
80014d90:	00090593          	mv	a1,s2
80014d94:	00160a93          	addi	s5,a2,1
80014d98:	000980e7          	jalr	s3
        while ((*p != 0) && (!(flags & FLAGS_PRECISION) || precision--)) {
80014d9c:	418a87b3          	sub	a5,s5,s8
80014da0:	00fc87b3          	add	a5,s9,a5
80014da4:	0007c503          	lbu	a0,0(a5)
80014da8:	32050a63          	beqz	a0,800150dc <_vsnprintf+0x844>
          out(*(p++), buffer, idx++, maxlen);
80014dac:	000a8613          	mv	a2,s5
80014db0:	fd9ff06f          	j	80014d88 <_vsnprintf+0x4f0>
  while (_is_digit(**str)) {
80014db4:	00900713          	li	a4,9
    i = i * 10U + (unsigned int)(*((*str)++) - '0');
80014db8:	002b1793          	slli	a5,s6,0x2
80014dbc:	016787b3          	add	a5,a5,s6
80014dc0:	00168693          	addi	a3,a3,1
80014dc4:	00179793          	slli	a5,a5,0x1
80014dc8:	00a787b3          	add	a5,a5,a0
  while (_is_digit(**str)) {
80014dcc:	0006c503          	lbu	a0,0(a3)
    i = i * 10U + (unsigned int)(*((*str)++) - '0');
80014dd0:	fd078b13          	addi	s6,a5,-48
  return (ch >= '0') && (ch <= '9');
80014dd4:	fd050793          	addi	a5,a0,-48
  while (_is_digit(**str)) {
80014dd8:	0ff7f793          	zext.b	a5,a5
80014ddc:	fcf77ee3          	bgeu	a4,a5,80014db8 <_vsnprintf+0x520>
80014de0:	00068b93          	mv	s7,a3
80014de4:	00168713          	addi	a4,a3,1
80014de8:	badff06f          	j	80014994 <_vsnprintf+0xfc>
        precision = prec > 0 ? (unsigned int)prec : 0U;
80014dec:	000aab03          	lw	s6,0(s5)
    switch (*format) {
80014df0:	002bc503          	lbu	a0,2(s7)
        format++;
80014df4:	002b8b93          	addi	s7,s7,2
        precision = prec > 0 ? (unsigned int)prec : 0U;
80014df8:	fffb4793          	not	a5,s6
80014dfc:	41f7d793          	srai	a5,a5,0x1f
80014e00:	00fb7b33          	and	s6,s6,a5
        const int prec = (int)va_arg(va, int);
80014e04:	004a8a93          	addi	s5,s5,4
80014e08:	001b8713          	addi	a4,s7,1
80014e0c:	b89ff06f          	j	80014994 <_vsnprintf+0xfc>
  for (s = str; *s && maxsize--; ++s);
80014e10:	f40688e3          	beqz	a3,80014d60 <_vsnprintf+0x4c8>
80014e14:	fffb0793          	addi	a5,s6,-1
80014e18:	f25ff06f          	j	80014d3c <_vsnprintf+0x4a4>
    flags &= ~FLAGS_HASH;
80014e1c:	fef47e13          	andi	t3,s0,-17
  if (!(flags & FLAGS_PRECISION) || value) {
80014e20:	40047413          	andi	s0,s0,1024
    flags &= ~FLAGS_HASH;
80014e24:	021e6e13          	ori	t3,t3,33
  if (!(flags & FLAGS_PRECISION) || value) {
80014e28:	c8041ce3          	bnez	s0,80014ac0 <_vsnprintf+0x228>
80014e2c:	c55ff06f          	j	80014a80 <_vsnprintf+0x1e8>
        if ((*format == 'g')||(*format == 'G')) flags |= FLAGS_ADAPT_EXP;
80014e30:	0df57793          	andi	a5,a0,223
80014e34:	04700713          	li	a4,71
80014e38:	26e78263          	beq	a5,a4,8001509c <_vsnprintf+0x804>
        if ((*format == 'E')||(*format == 'G')) flags |= FLAGS_UPPERCASE;
80014e3c:	04500793          	li	a5,69
80014e40:	26f50a63          	beq	a0,a5,800150b4 <_vsnprintf+0x81c>
        idx = _etoa(out, buffer, idx, maxlen, va_arg(va, double), precision, width, flags);
80014e44:	007a8a93          	addi	s5,s5,7
80014e48:	ff8afa93          	andi	s5,s5,-8
80014e4c:	000aa703          	lw	a4,0(s5)
80014e50:	004aa783          	lw	a5,4(s5)
80014e54:	000c0613          	mv	a2,s8
80014e58:	00812023          	sw	s0,0(sp)
80014e5c:	000d0893          	mv	a7,s10
80014e60:	000b0813          	mv	a6,s6
80014e64:	00048693          	mv	a3,s1
80014e68:	00090593          	mv	a1,s2
80014e6c:	00098513          	mv	a0,s3
80014e70:	d69fe0ef          	jal	ra,80013bd8 <_etoa>
80014e74:	008a8a93          	addi	s5,s5,8
80014e78:	00050c13          	mv	s8,a0
        break;
80014e7c:	ab1ff06f          	j	8001492c <_vsnprintf+0x94>
80014e80:	06200793          	li	a5,98
80014e84:	2ef50a63          	beq	a0,a5,80015178 <_vsnprintf+0x8e0>
80014e88:	06f00793          	li	a5,111
80014e8c:	30f50263          	beq	a0,a5,80015190 <_vsnprintf+0x8f8>
80014e90:	05800793          	li	a5,88
80014e94:	22f50c63          	beq	a0,a5,800150cc <_vsnprintf+0x834>
          flags &= ~FLAGS_HASH;   // no hash for dec format
80014e98:	fef47413          	andi	s0,s0,-17
          base = 10U;
80014e9c:	00a00d93          	li	s11,10
        if ((*format != 'i') && (*format != 'd')) {
80014ea0:	06900693          	li	a3,105
        if (flags & FLAGS_PRECISION) {
80014ea4:	40047793          	andi	a5,s0,1024
        if ((*format != 'i') && (*format != 'd')) {
80014ea8:	2cd51c63          	bne	a0,a3,80015180 <_vsnprintf+0x8e8>
        if (flags & FLAGS_PRECISION) {
80014eac:	0e079e63          	bnez	a5,80014fa8 <_vsnprintf+0x710>
          if (flags & FLAGS_LONG_LONG) {
80014eb0:	20047e13          	andi	t3,s0,512
80014eb4:	2e0e1263          	bnez	t3,80015198 <_vsnprintf+0x900>
            idx = _ntoa_long(out, buffer, idx, maxlen, va_arg(va, unsigned long), false, base, precision, width, flags);
80014eb8:	004a8793          	addi	a5,s5,4
          else if (flags & FLAGS_LONG) {
80014ebc:	10047c93          	andi	s9,s0,256
            idx = _ntoa_long(out, buffer, idx, maxlen, va_arg(va, unsigned long), false, base, precision, width, flags);
80014ec0:	02f12223          	sw	a5,36(sp)
          else if (flags & FLAGS_LONG) {
80014ec4:	5a0c9e63          	bnez	s9,80015480 <_vsnprintf+0xbe8>
            const int value = (flags & FLAGS_CHAR) ? (char)va_arg(va, int) : (flags & FLAGS_SHORT) ? (short int)va_arg(va, int) : va_arg(va, int);
80014ec8:	04047693          	andi	a3,s0,64
80014ecc:	3c069463          	bnez	a3,80015294 <_vsnprintf+0x9fc>
80014ed0:	08047693          	andi	a3,s0,128
80014ed4:	74068463          	beqz	a3,8001561c <_vsnprintf+0xd84>
80014ed8:	000a9683          	lh	a3,0(s5)
            idx = _ntoa_long(out, buffer, idx, maxlen, (unsigned int)(value > 0 ? value : 0 - value), value < 0, base, precision, width, flags);
80014edc:	40f6d593          	srai	a1,a3,0xf
80014ee0:	00b6c633          	xor	a2,a3,a1
80014ee4:	40b60633          	sub	a2,a2,a1
80014ee8:	01061613          	slli	a2,a2,0x10
80014eec:	01065613          	srli	a2,a2,0x10
80014ef0:	01f6d793          	srli	a5,a3,0x1f
80014ef4:	02f12423          	sw	a5,40(sp)
80014ef8:	00060a93          	mv	s5,a2
  if (!value) {
80014efc:	00069a63          	bnez	a3,80014f10 <_vsnprintf+0x678>
    flags &= ~FLAGS_HASH;
80014f00:	fef47693          	andi	a3,s0,-17
  if (!(flags & FLAGS_PRECISION) || value) {
80014f04:	40047593          	andi	a1,s0,1024
    flags &= ~FLAGS_HASH;
80014f08:	00068413          	mv	s0,a3
  if (!(flags & FLAGS_PRECISION) || value) {
80014f0c:	3c059c63          	bnez	a1,800152e4 <_vsnprintf+0xa4c>
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
80014f10:	02047593          	andi	a1,s0,32
80014f14:	06100693          	li	a3,97
80014f18:	00058463          	beqz	a1,80014f20 <_vsnprintf+0x688>
80014f1c:	04100693          	li	a3,65
80014f20:	ff668793          	addi	a5,a3,-10
80014f24:	02f12023          	sw	a5,32(sp)
80014f28:	0380006f          	j	80014f60 <_vsnprintf+0x6c8>
80014f2c:	03068693          	addi	a3,a3,48
80014f30:	0ff6f693          	zext.b	a3,a3
80014f34:	001c8c93          	addi	s9,s9,1
80014f38:	03010793          	addi	a5,sp,48
80014f3c:	01978e33          	add	t3,a5,s9
      value /= base;
80014f40:	000d8593          	mv	a1,s11
80014f44:	000a8513          	mv	a0,s5
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
80014f48:	fede0fa3          	sb	a3,-1(t3)
      value /= base;
80014f4c:	49d030ef          	jal	ra,80018be8 <__udivsi3>
    } while (value && (len < PRINTF_NTOA_BUFFER_SIZE));
80014f50:	39baea63          	bltu	s5,s11,800152e4 <_vsnprintf+0xa4c>
80014f54:	02000793          	li	a5,32
80014f58:	38fc8663          	beq	s9,a5,800152e4 <_vsnprintf+0xa4c>
      value /= base;
80014f5c:	00050a93          	mv	s5,a0
      const char digit = (char)(value % base);
80014f60:	000d8593          	mv	a1,s11
80014f64:	000a8513          	mv	a0,s5
80014f68:	4c9030ef          	jal	ra,80018c30 <__umodsi3>
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
80014f6c:	00900793          	li	a5,9
      const char digit = (char)(value % base);
80014f70:	0ff57693          	zext.b	a3,a0
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
80014f74:	faa7fce3          	bgeu	a5,a0,80014f2c <_vsnprintf+0x694>
80014f78:	02012783          	lw	a5,32(sp)
80014f7c:	00f686b3          	add	a3,a3,a5
80014f80:	0ff6f693          	zext.b	a3,a3
80014f84:	fb1ff06f          	j	80014f34 <_vsnprintf+0x69c>
        if (*format == 'F') flags |= FLAGS_UPPERCASE;
80014f88:	02046413          	ori	s0,s0,32
80014f8c:	cc5ff06f          	j	80014c50 <_vsnprintf+0x3b8>
        if (*format == 'x' || *format == 'X') {
80014f90:	07800793          	li	a5,120
80014f94:	eef516e3          	bne	a0,a5,80014e80 <_vsnprintf+0x5e8>
          base = 16U;
80014f98:	01000d93          	li	s11,16
80014f9c:	40047793          	andi	a5,s0,1024
          flags &= ~(FLAGS_PLUS | FLAGS_SPACE);
80014fa0:	ff347413          	andi	s0,s0,-13
        if (flags & FLAGS_PRECISION) {
80014fa4:	00078463          	beqz	a5,80014fac <_vsnprintf+0x714>
          flags &= ~FLAGS_ZEROPAD;
80014fa8:	ffe47413          	andi	s0,s0,-2
        if ((*format == 'i') || (*format == 'd')) {
80014fac:	06900793          	li	a5,105
80014fb0:	f0f500e3          	beq	a0,a5,80014eb0 <_vsnprintf+0x618>
80014fb4:	06400793          	li	a5,100
80014fb8:	eef50ce3          	beq	a0,a5,80014eb0 <_vsnprintf+0x618>
          if (flags & FLAGS_LONG_LONG) {
80014fbc:	20047e13          	andi	t3,s0,512
80014fc0:	340e1063          	bnez	t3,80015300 <_vsnprintf+0xa68>
            idx = _ntoa_long(out, buffer, idx, maxlen, va_arg(va, unsigned long), false, base, precision, width, flags);
80014fc4:	004a8793          	addi	a5,s5,4
          else if (flags & FLAGS_LONG) {
80014fc8:	10047c93          	andi	s9,s0,256
            idx = _ntoa_long(out, buffer, idx, maxlen, va_arg(va, unsigned long), false, base, precision, width, flags);
80014fcc:	02f12223          	sw	a5,36(sp)
          else if (flags & FLAGS_LONG) {
80014fd0:	580c9263          	bnez	s9,80015554 <_vsnprintf+0xcbc>
            const unsigned int value = (flags & FLAGS_CHAR) ? (unsigned char)va_arg(va, unsigned int) : (flags & FLAGS_SHORT) ? (unsigned short int)va_arg(va, unsigned int) : va_arg(va, unsigned int);
80014fd4:	04047693          	andi	a3,s0,64
80014fd8:	2e069c63          	bnez	a3,800152d0 <_vsnprintf+0xa38>
80014fdc:	08047693          	andi	a3,s0,128
80014fe0:	64068863          	beqz	a3,80015630 <_vsnprintf+0xd98>
80014fe4:	000ada83          	lhu	s5,0(s5)
  if (!value) {
80014fe8:	420a9063          	bnez	s5,80015408 <_vsnprintf+0xb70>
  if (!(flags & FLAGS_PRECISION) || value) {
80014fec:	40047693          	andi	a3,s0,1024
    flags &= ~FLAGS_HASH;
80014ff0:	fef47413          	andi	s0,s0,-17
  if (!(flags & FLAGS_PRECISION) || value) {
80014ff4:	40068a63          	beqz	a3,80015408 <_vsnprintf+0xb70>
  size_t len = 0U;
80014ff8:	00000c93          	li	s9,0
  return _ntoa_format(out, buffer, idx, maxlen, buf, len, negative, (unsigned int)base, prec, width, flags);
80014ffc:	000c0613          	mv	a2,s8
80015000:	00812423          	sw	s0,8(sp)
80015004:	01a12223          	sw	s10,4(sp)
80015008:	01612023          	sw	s6,0(sp)
8001500c:	000d8893          	mv	a7,s11
80015010:	00000813          	li	a6,0
80015014:	000c8793          	mv	a5,s9
80015018:	03010713          	addi	a4,sp,48
8001501c:	00048693          	mv	a3,s1
80015020:	00090593          	mv	a1,s2
80015024:	00098513          	mv	a0,s3
80015028:	8fdfe0ef          	jal	ra,80013924 <_ntoa_format>
8001502c:	00050c13          	mv	s8,a0
80015030:	02412a83          	lw	s5,36(sp)
        break;
80015034:	8f9ff06f          	j	8001492c <_vsnprintf+0x94>
          out(*(p++), buffer, idx++, maxlen);
80015038:	000c0b13          	mv	s6,s8
8001503c:	000b0613          	mv	a2,s6
80015040:	00048693          	mv	a3,s1
80015044:	00090593          	mv	a1,s2
80015048:	000980e7          	jalr	s3
8001504c:	001b0b13          	addi	s6,s6,1
        while ((*p != 0) && (!(flags & FLAGS_PRECISION) || precision--)) {
80015050:	418b07b3          	sub	a5,s6,s8
80015054:	00fc87b3          	add	a5,s9,a5
80015058:	0007c503          	lbu	a0,0(a5)
8001505c:	fe0510e3          	bnez	a0,8001503c <_vsnprintf+0x7a4>
80015060:	000b0c13          	mv	s8,s6
        if (flags & FLAGS_LEFT) {
80015064:	02040863          	beqz	s0,80015094 <_vsnprintf+0x7fc>
80015068:	000c0413          	mv	s0,s8
          while (l++ < width) {
8001506c:	03adf463          	bgeu	s11,s10,80015094 <_vsnprintf+0x7fc>
80015070:	018d08b3          	add	a7,s10,s8
80015074:	41b88c33          	sub	s8,a7,s11
            out(' ', buffer, idx++, maxlen);
80015078:	00040613          	mv	a2,s0
8001507c:	00048693          	mv	a3,s1
80015080:	00140413          	addi	s0,s0,1
80015084:	00090593          	mv	a1,s2
80015088:	02000513          	li	a0,32
8001508c:	000980e7          	jalr	s3
          while (l++ < width) {
80015090:	ff8414e3          	bne	s0,s8,80015078 <_vsnprintf+0x7e0>
        const char* p = va_arg(va, char*);
80015094:	02012a83          	lw	s5,32(sp)
        break;
80015098:	895ff06f          	j	8001492c <_vsnprintf+0x94>
        if ((*format == 'g')||(*format == 'G')) flags |= FLAGS_ADAPT_EXP;
8001509c:	000017b7          	lui	a5,0x1
800150a0:	80078793          	addi	a5,a5,-2048 # 800 <font6x8-0x7ffff82c>
        if ((*format == 'E')||(*format == 'G')) flags |= FLAGS_UPPERCASE;
800150a4:	0fd57513          	andi	a0,a0,253
800150a8:	04500713          	li	a4,69
        if ((*format == 'g')||(*format == 'G')) flags |= FLAGS_ADAPT_EXP;
800150ac:	00f46433          	or	s0,s0,a5
        if ((*format == 'E')||(*format == 'G')) flags |= FLAGS_UPPERCASE;
800150b0:	d8e51ae3          	bne	a0,a4,80014e44 <_vsnprintf+0x5ac>
800150b4:	02046413          	ori	s0,s0,32
800150b8:	d8dff06f          	j	80014e44 <_vsnprintf+0x5ac>
    switch (*format) {
800150bc:	002bc503          	lbu	a0,2(s7)
          flags |= FLAGS_CHAR;
800150c0:	0c046413          	ori	s0,s0,192
          format++;
800150c4:	003b8713          	addi	a4,s7,3
800150c8:	939ff06f          	j	80014a00 <_vsnprintf+0x168>
          flags |= FLAGS_UPPERCASE;
800150cc:	02046413          	ori	s0,s0,32
800150d0:	01000d93          	li	s11,16
800150d4:	40047793          	andi	a5,s0,1024
800150d8:	ec9ff06f          	j	80014fa0 <_vsnprintf+0x708>
          out(*(p++), buffer, idx++, maxlen);
800150dc:	000a8c13          	mv	s8,s5
        if (flags & FLAGS_LEFT) {
800150e0:	fa040ae3          	beqz	s0,80015094 <_vsnprintf+0x7fc>
800150e4:	f85ff06f          	j	80015068 <_vsnprintf+0x7d0>
          while (l++ < width) {
800150e8:	001d8793          	addi	a5,s11,1
800150ec:	55adfa63          	bgeu	s11,s10,80015640 <_vsnprintf+0xda8>
800150f0:	018d07b3          	add	a5,s10,s8
800150f4:	41b78db3          	sub	s11,a5,s11
            out(' ', buffer, idx++, maxlen);
800150f8:	000c0613          	mv	a2,s8
800150fc:	00048693          	mv	a3,s1
80015100:	001c0c13          	addi	s8,s8,1
80015104:	00090593          	mv	a1,s2
80015108:	02000513          	li	a0,32
8001510c:	000980e7          	jalr	s3
          while (l++ < width) {
80015110:	ffbc14e3          	bne	s8,s11,800150f8 <_vsnprintf+0x860>
80015114:	000cc683          	lbu	a3,0(s9)
80015118:	001d0d93          	addi	s11,s10,1
        while ((*p != 0) && (!(flags & FLAGS_PRECISION) || precision--)) {
8001511c:	00068513          	mv	a0,a3
80015120:	c4069ee3          	bnez	a3,80014d7c <_vsnprintf+0x4e4>
        const char* p = va_arg(va, char*);
80015124:	02012a83          	lw	s5,32(sp)
80015128:	805ff06f          	j	8001492c <_vsnprintf+0x94>
          while (l++ < width) {
8001512c:	00100793          	li	a5,1
80015130:	51a7f463          	bgeu	a5,s10,80015638 <_vsnprintf+0xda0>
80015134:	fffc0413          	addi	s0,s8,-1
80015138:	01a40433          	add	s0,s0,s10
            out(' ', buffer, idx++, maxlen);
8001513c:	000c0613          	mv	a2,s8
80015140:	00048693          	mv	a3,s1
80015144:	001c0c13          	addi	s8,s8,1
80015148:	00090593          	mv	a1,s2
8001514c:	02000513          	li	a0,32
80015150:	000980e7          	jalr	s3
          while (l++ < width) {
80015154:	fe8c14e3          	bne	s8,s0,8001513c <_vsnprintf+0x8a4>
        out((char)va_arg(va, int), buffer, idx++, maxlen);
80015158:	000ac503          	lbu	a0,0(s5)
8001515c:	00048693          	mv	a3,s1
80015160:	00040613          	mv	a2,s0
80015164:	00090593          	mv	a1,s2
80015168:	00140c13          	addi	s8,s0,1
8001516c:	000980e7          	jalr	s3
        break;
80015170:	000b0a93          	mv	s5,s6
80015174:	fb8ff06f          	j	8001492c <_vsnprintf+0x94>
        format++;
80015178:	00200d93          	li	s11,2
8001517c:	40047793          	andi	a5,s0,1024
        if ((*format != 'i') && (*format != 'd')) {
80015180:	06400693          	li	a3,100
80015184:	e0d51ee3          	bne	a0,a3,80014fa0 <_vsnprintf+0x708>
        if (flags & FLAGS_PRECISION) {
80015188:	d20784e3          	beqz	a5,80014eb0 <_vsnprintf+0x618>
8001518c:	e1dff06f          	j	80014fa8 <_vsnprintf+0x710>
          while (l++ < width) {
80015190:	00800d93          	li	s11,8
80015194:	d0dff06f          	j	80014ea0 <_vsnprintf+0x608>
            const long long value = va_arg(va, long long);
80015198:	007a8a93          	addi	s5,s5,7
8001519c:	ff8afa93          	andi	s5,s5,-8
800151a0:	004aa603          	lw	a2,4(s5)
800151a4:	000aa683          	lw	a3,0(s5)
800151a8:	008a8a93          	addi	s5,s5,8
            idx = _ntoa_long_long(out, buffer, idx, maxlen, (unsigned long long)(value > 0 ? value : 0 - value), value < 0, base, precision, width, flags);
800151ac:	41f65793          	srai	a5,a2,0x1f
800151b0:	00d7cf33          	xor	t5,a5,a3
800151b4:	40ff05b3          	sub	a1,t5,a5
800151b8:	00c7ceb3          	xor	t4,a5,a2
800151bc:	00bf3533          	sltu	a0,t5,a1
800151c0:	40fe87b3          	sub	a5,t4,a5
800151c4:	40a78eb3          	sub	t4,a5,a0
800151c8:	01f65793          	srli	a5,a2,0x1f
  if (!value) {
800151cc:	00c6e6b3          	or	a3,a3,a2
            idx = _ntoa_long_long(out, buffer, idx, maxlen, (unsigned long long)(value > 0 ? value : 0 - value), value < 0, base, precision, width, flags);
800151d0:	02f12623          	sw	a5,44(sp)
800151d4:	00058f13          	mv	t5,a1
  if (!value) {
800151d8:	0c068463          	beqz	a3,800152a0 <_vsnprintf+0xa08>
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
800151dc:	02047693          	andi	a3,s0,32
800151e0:	06100793          	li	a5,97
800151e4:	00068463          	beqz	a3,800151ec <_vsnprintf+0x954>
800151e8:	04100793          	li	a5,65
800151ec:	0ff7f693          	zext.b	a3,a5
800151f0:	ff668793          	addi	a5,a3,-10
800151f4:	00000c93          	li	s9,0
800151f8:	02f12423          	sw	a5,40(sp)
800151fc:	0580006f          	j	80015254 <_vsnprintf+0x9bc>
80015200:	03050513          	addi	a0,a0,48
80015204:	0ff57e13          	zext.b	t3,a0
80015208:	03010793          	addi	a5,sp,48
8001520c:	001c8c93          	addi	s9,s9,1
80015210:	01978fb3          	add	t6,a5,s9
      value /= base;
80015214:	000f0513          	mv	a0,t5
80015218:	000e8593          	mv	a1,t4
8001521c:	000d8613          	mv	a2,s11
80015220:	00000693          	li	a3,0
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
80015224:	ffcf8fa3          	sb	t3,-1(t6)
      value /= base;
80015228:	03e12223          	sw	t5,36(sp)
8001522c:	03d12023          	sw	t4,32(sp)
80015230:	3e1000ef          	jal	ra,80015e10 <__udivdi3>
    } while (value && (len < PRINTF_NTOA_BUFFER_SIZE));
80015234:	02012e83          	lw	t4,32(sp)
80015238:	00000793          	li	a5,0
8001523c:	02412f03          	lw	t5,36(sp)
80015240:	3dd78a63          	beq	a5,t4,80015614 <_vsnprintf+0xd7c>
80015244:	02000793          	li	a5,32
80015248:	06fc8463          	beq	s9,a5,800152b0 <_vsnprintf+0xa18>
      value /= base;
8001524c:	00050f13          	mv	t5,a0
80015250:	00058e93          	mv	t4,a1
      const char digit = (char)(value % base);
80015254:	000f0513          	mv	a0,t5
80015258:	000e8593          	mv	a1,t4
8001525c:	000d8613          	mv	a2,s11
80015260:	00000693          	li	a3,0
80015264:	03e12223          	sw	t5,36(sp)
80015268:	03d12023          	sw	t4,32(sp)
8001526c:	17c010ef          	jal	ra,800163e8 <__umoddi3>
80015270:	0ff57513          	zext.b	a0,a0
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
80015274:	00900793          	li	a5,9
80015278:	02012e83          	lw	t4,32(sp)
8001527c:	02412f03          	lw	t5,36(sp)
80015280:	f8a7f0e3          	bgeu	a5,a0,80015200 <_vsnprintf+0x968>
80015284:	02812783          	lw	a5,40(sp)
80015288:	00f50533          	add	a0,a0,a5
8001528c:	0ff57e13          	zext.b	t3,a0
80015290:	f79ff06f          	j	80015208 <_vsnprintf+0x970>
80015294:	000ac683          	lbu	a3,0(s5)
80015298:	00068613          	mv	a2,a3
8001529c:	c55ff06f          	j	80014ef0 <_vsnprintf+0x658>
  if (!(flags & FLAGS_PRECISION) || value) {
800152a0:	40047793          	andi	a5,s0,1024
  size_t len = 0U;
800152a4:	00000c93          	li	s9,0
    flags &= ~FLAGS_HASH;
800152a8:	fef47413          	andi	s0,s0,-17
  if (!(flags & FLAGS_PRECISION) || value) {
800152ac:	f20788e3          	beqz	a5,800151dc <_vsnprintf+0x944>
  return _ntoa_format(out, buffer, idx, maxlen, buf, len, negative, (unsigned int)base, prec, width, flags);
800152b0:	02c12803          	lw	a6,44(sp)
800152b4:	000c0613          	mv	a2,s8
800152b8:	00812423          	sw	s0,8(sp)
800152bc:	01a12223          	sw	s10,4(sp)
800152c0:	01612023          	sw	s6,0(sp)
800152c4:	000d8893          	mv	a7,s11
800152c8:	000c8793          	mv	a5,s9
800152cc:	815ff06f          	j	80014ae0 <_vsnprintf+0x248>
800152d0:	000aca83          	lbu	s5,0(s5)
800152d4:	d15ff06f          	j	80014fe8 <_vsnprintf+0x750>
          while (l++ < width) {
800152d8:	00040c13          	mv	s8,s0
        break;
800152dc:	000b0a93          	mv	s5,s6
800152e0:	e4cff06f          	j	8001492c <_vsnprintf+0x94>
  return _ntoa_format(out, buffer, idx, maxlen, buf, len, negative, (unsigned int)base, prec, width, flags);
800152e4:	02812803          	lw	a6,40(sp)
800152e8:	000c0613          	mv	a2,s8
800152ec:	00812423          	sw	s0,8(sp)
800152f0:	01a12223          	sw	s10,4(sp)
800152f4:	01612023          	sw	s6,0(sp)
800152f8:	000d8893          	mv	a7,s11
800152fc:	d19ff06f          	j	80015014 <_vsnprintf+0x77c>
            idx = _ntoa_long_long(out, buffer, idx, maxlen, va_arg(va, unsigned long long), false, base, precision, width, flags);
80015300:	007a8a93          	addi	s5,s5,7
80015304:	ff8afa93          	andi	s5,s5,-8
80015308:	000aaf03          	lw	t5,0(s5)
8001530c:	004aae83          	lw	t4,4(s5)
80015310:	008a8a93          	addi	s5,s5,8
  if (!value) {
80015314:	01df67b3          	or	a5,t5,t4
80015318:	00079a63          	bnez	a5,8001532c <_vsnprintf+0xa94>
  if (!(flags & FLAGS_PRECISION) || value) {
8001531c:	40047793          	andi	a5,s0,1024
  size_t len = 0U;
80015320:	00000c93          	li	s9,0
    flags &= ~FLAGS_HASH;
80015324:	fef47413          	andi	s0,s0,-17
  if (!(flags & FLAGS_PRECISION) || value) {
80015328:	0c079063          	bnez	a5,800153e8 <_vsnprintf+0xb50>
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
8001532c:	02047693          	andi	a3,s0,32
80015330:	06100793          	li	a5,97
80015334:	00068463          	beqz	a3,8001533c <_vsnprintf+0xaa4>
80015338:	04100793          	li	a5,65
8001533c:	0ff7f693          	zext.b	a3,a5
80015340:	ff668793          	addi	a5,a3,-10
80015344:	00000c93          	li	s9,0
80015348:	02f12423          	sw	a5,40(sp)
8001534c:	0580006f          	j	800153a4 <_vsnprintf+0xb0c>
80015350:	03050513          	addi	a0,a0,48
80015354:	0ff57e13          	zext.b	t3,a0
80015358:	03010793          	addi	a5,sp,48
8001535c:	001c8c93          	addi	s9,s9,1
80015360:	01978fb3          	add	t6,a5,s9
      value /= base;
80015364:	000f0513          	mv	a0,t5
80015368:	000e8593          	mv	a1,t4
8001536c:	000d8613          	mv	a2,s11
80015370:	00000693          	li	a3,0
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
80015374:	ffcf8fa3          	sb	t3,-1(t6)
      value /= base;
80015378:	03e12223          	sw	t5,36(sp)
8001537c:	03d12023          	sw	t4,32(sp)
80015380:	291000ef          	jal	ra,80015e10 <__udivdi3>
    } while (value && (len < PRINTF_NTOA_BUFFER_SIZE));
80015384:	02012e83          	lw	t4,32(sp)
80015388:	00000793          	li	a5,0
8001538c:	02412f03          	lw	t5,36(sp)
80015390:	05d78a63          	beq	a5,t4,800153e4 <_vsnprintf+0xb4c>
80015394:	02000793          	li	a5,32
80015398:	04fc8863          	beq	s9,a5,800153e8 <_vsnprintf+0xb50>
      value /= base;
8001539c:	00050f13          	mv	t5,a0
800153a0:	00058e93          	mv	t4,a1
      const char digit = (char)(value % base);
800153a4:	000f0513          	mv	a0,t5
800153a8:	000e8593          	mv	a1,t4
800153ac:	000d8613          	mv	a2,s11
800153b0:	00000693          	li	a3,0
800153b4:	03e12223          	sw	t5,36(sp)
800153b8:	03d12023          	sw	t4,32(sp)
800153bc:	02c010ef          	jal	ra,800163e8 <__umoddi3>
800153c0:	0ff57513          	zext.b	a0,a0
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
800153c4:	00900793          	li	a5,9
800153c8:	02012e83          	lw	t4,32(sp)
800153cc:	02412f03          	lw	t5,36(sp)
800153d0:	f8a7f0e3          	bgeu	a5,a0,80015350 <_vsnprintf+0xab8>
800153d4:	02812783          	lw	a5,40(sp)
800153d8:	00f50533          	add	a0,a0,a5
800153dc:	0ff57e13          	zext.b	t3,a0
800153e0:	f79ff06f          	j	80015358 <_vsnprintf+0xac0>
    } while (value && (len < PRINTF_NTOA_BUFFER_SIZE));
800153e4:	fbbf78e3          	bgeu	t5,s11,80015394 <_vsnprintf+0xafc>
  return _ntoa_format(out, buffer, idx, maxlen, buf, len, negative, (unsigned int)base, prec, width, flags);
800153e8:	000c0613          	mv	a2,s8
800153ec:	00812423          	sw	s0,8(sp)
800153f0:	01a12223          	sw	s10,4(sp)
800153f4:	01612023          	sw	s6,0(sp)
800153f8:	000d8893          	mv	a7,s11
800153fc:	00000813          	li	a6,0
80015400:	000c8793          	mv	a5,s9
80015404:	edcff06f          	j	80014ae0 <_vsnprintf+0x248>
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
80015408:	02047593          	andi	a1,s0,32
8001540c:	06100693          	li	a3,97
80015410:	00058463          	beqz	a1,80015418 <_vsnprintf+0xb80>
80015414:	04100693          	li	a3,65
80015418:	ff668793          	addi	a5,a3,-10
8001541c:	02f12023          	sw	a5,32(sp)
80015420:	0380006f          	j	80015458 <_vsnprintf+0xbc0>
80015424:	03068693          	addi	a3,a3,48
80015428:	0ff6f693          	zext.b	a3,a3
8001542c:	001c8c93          	addi	s9,s9,1
80015430:	03010793          	addi	a5,sp,48
80015434:	01978e33          	add	t3,a5,s9
      value /= base;
80015438:	000d8593          	mv	a1,s11
8001543c:	000a8513          	mv	a0,s5
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
80015440:	fede0fa3          	sb	a3,-1(t3)
      value /= base;
80015444:	7a4030ef          	jal	ra,80018be8 <__udivsi3>
    } while (value && (len < PRINTF_NTOA_BUFFER_SIZE));
80015448:	bbbaeae3          	bltu	s5,s11,80014ffc <_vsnprintf+0x764>
8001544c:	02000793          	li	a5,32
80015450:	bafc86e3          	beq	s9,a5,80014ffc <_vsnprintf+0x764>
      value /= base;
80015454:	00050a93          	mv	s5,a0
      const char digit = (char)(value % base);
80015458:	000d8593          	mv	a1,s11
8001545c:	000a8513          	mv	a0,s5
80015460:	7d0030ef          	jal	ra,80018c30 <__umodsi3>
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
80015464:	00900793          	li	a5,9
      const char digit = (char)(value % base);
80015468:	0ff57693          	zext.b	a3,a0
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
8001546c:	faa7fce3          	bgeu	a5,a0,80015424 <_vsnprintf+0xb8c>
80015470:	02012783          	lw	a5,32(sp)
80015474:	00f686b3          	add	a3,a3,a5
80015478:	0ff6f693          	zext.b	a3,a3
8001547c:	fb1ff06f          	j	8001542c <_vsnprintf+0xb94>
            const long value = va_arg(va, long);
80015480:	000aa683          	lw	a3,0(s5)
            idx = _ntoa_long(out, buffer, idx, maxlen, (unsigned long)(value > 0 ? value : 0 - value), value < 0, base, precision, width, flags);
80015484:	41f6d613          	srai	a2,a3,0x1f
80015488:	01f6d793          	srli	a5,a3,0x1f
8001548c:	00d64cb3          	xor	s9,a2,a3
80015490:	02f12623          	sw	a5,44(sp)
80015494:	40cc8cb3          	sub	s9,s9,a2
  if (!value) {
80015498:	00069a63          	bnez	a3,800154ac <_vsnprintf+0xc14>
    flags &= ~FLAGS_HASH;
8001549c:	fef47693          	andi	a3,s0,-17
  if (!(flags & FLAGS_PRECISION) || value) {
800154a0:	40047613          	andi	a2,s0,1024
    flags &= ~FLAGS_HASH;
800154a4:	00068413          	mv	s0,a3
  if (!(flags & FLAGS_PRECISION) || value) {
800154a8:	08061663          	bnez	a2,80015534 <_vsnprintf+0xc9c>
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
800154ac:	02047613          	andi	a2,s0,32
800154b0:	06100693          	li	a3,97
800154b4:	00060463          	beqz	a2,800154bc <_vsnprintf+0xc24>
800154b8:	04100693          	li	a3,65
800154bc:	ff668793          	addi	a5,a3,-10
800154c0:	00900a93          	li	s5,9
800154c4:	02f12423          	sw	a5,40(sp)
800154c8:	0400006f          	j	80015508 <_vsnprintf+0xc70>
800154cc:	03068693          	addi	a3,a3,48
800154d0:	0ff6f693          	zext.b	a3,a3
800154d4:	001e0e13          	addi	t3,t3,1
800154d8:	03010793          	addi	a5,sp,48
800154dc:	01c78633          	add	a2,a5,t3
      value /= base;
800154e0:	000d8593          	mv	a1,s11
800154e4:	000c8513          	mv	a0,s9
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
800154e8:	fed60fa3          	sb	a3,-1(a2)
800154ec:	03c12023          	sw	t3,32(sp)
      value /= base;
800154f0:	6f8030ef          	jal	ra,80018be8 <__udivsi3>
    } while (value && (len < PRINTF_NTOA_BUFFER_SIZE));
800154f4:	02012e03          	lw	t3,32(sp)
800154f8:	03bcee63          	bltu	s9,s11,80015534 <_vsnprintf+0xc9c>
800154fc:	02000793          	li	a5,32
80015500:	02fe0a63          	beq	t3,a5,80015534 <_vsnprintf+0xc9c>
      value /= base;
80015504:	00050c93          	mv	s9,a0
      const char digit = (char)(value % base);
80015508:	000d8593          	mv	a1,s11
8001550c:	000c8513          	mv	a0,s9
80015510:	03c12023          	sw	t3,32(sp)
80015514:	71c030ef          	jal	ra,80018c30 <__umodsi3>
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
80015518:	02012e03          	lw	t3,32(sp)
      const char digit = (char)(value % base);
8001551c:	0ff57693          	zext.b	a3,a0
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
80015520:	faaaf6e3          	bgeu	s5,a0,800154cc <_vsnprintf+0xc34>
80015524:	02812783          	lw	a5,40(sp)
80015528:	00f686b3          	add	a3,a3,a5
8001552c:	0ff6f693          	zext.b	a3,a3
80015530:	fa5ff06f          	j	800154d4 <_vsnprintf+0xc3c>
  return _ntoa_format(out, buffer, idx, maxlen, buf, len, negative, (unsigned int)base, prec, width, flags);
80015534:	02c12803          	lw	a6,44(sp)
80015538:	000c0613          	mv	a2,s8
8001553c:	00812423          	sw	s0,8(sp)
80015540:	01a12223          	sw	s10,4(sp)
80015544:	01612023          	sw	s6,0(sp)
80015548:	000d8893          	mv	a7,s11
8001554c:	000e0793          	mv	a5,t3
80015550:	ac9ff06f          	j	80015018 <_vsnprintf+0x780>
            idx = _ntoa_long(out, buffer, idx, maxlen, va_arg(va, unsigned long), false, base, precision, width, flags);
80015554:	000aaa83          	lw	s5,0(s5)
  if (!value) {
80015558:	000a9a63          	bnez	s5,8001556c <_vsnprintf+0xcd4>
    flags &= ~FLAGS_HASH;
8001555c:	fef47693          	andi	a3,s0,-17
  if (!(flags & FLAGS_PRECISION) || value) {
80015560:	40047613          	andi	a2,s0,1024
    flags &= ~FLAGS_HASH;
80015564:	00068413          	mv	s0,a3
  if (!(flags & FLAGS_PRECISION) || value) {
80015568:	08061663          	bnez	a2,800155f4 <_vsnprintf+0xd5c>
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
8001556c:	02047613          	andi	a2,s0,32
80015570:	06100693          	li	a3,97
80015574:	00060463          	beqz	a2,8001557c <_vsnprintf+0xce4>
80015578:	04100693          	li	a3,65
8001557c:	ff668793          	addi	a5,a3,-10
80015580:	00900c93          	li	s9,9
80015584:	02f12423          	sw	a5,40(sp)
80015588:	0400006f          	j	800155c8 <_vsnprintf+0xd30>
8001558c:	03068693          	addi	a3,a3,48
80015590:	0ff6f693          	zext.b	a3,a3
80015594:	001e0e13          	addi	t3,t3,1
80015598:	03010793          	addi	a5,sp,48
8001559c:	01c78633          	add	a2,a5,t3
      value /= base;
800155a0:	000d8593          	mv	a1,s11
800155a4:	000a8513          	mv	a0,s5
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
800155a8:	fed60fa3          	sb	a3,-1(a2)
800155ac:	03c12023          	sw	t3,32(sp)
      value /= base;
800155b0:	638030ef          	jal	ra,80018be8 <__udivsi3>
    } while (value && (len < PRINTF_NTOA_BUFFER_SIZE));
800155b4:	02012e03          	lw	t3,32(sp)
800155b8:	03baee63          	bltu	s5,s11,800155f4 <_vsnprintf+0xd5c>
800155bc:	02000793          	li	a5,32
800155c0:	02fe0a63          	beq	t3,a5,800155f4 <_vsnprintf+0xd5c>
      value /= base;
800155c4:	00050a93          	mv	s5,a0
      const char digit = (char)(value % base);
800155c8:	000d8593          	mv	a1,s11
800155cc:	000a8513          	mv	a0,s5
800155d0:	03c12023          	sw	t3,32(sp)
800155d4:	65c030ef          	jal	ra,80018c30 <__umodsi3>
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
800155d8:	02012e03          	lw	t3,32(sp)
      const char digit = (char)(value % base);
800155dc:	0ff57693          	zext.b	a3,a0
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
800155e0:	faacf6e3          	bgeu	s9,a0,8001558c <_vsnprintf+0xcf4>
800155e4:	02812783          	lw	a5,40(sp)
800155e8:	00f686b3          	add	a3,a3,a5
800155ec:	0ff6f693          	zext.b	a3,a3
800155f0:	fa5ff06f          	j	80015594 <_vsnprintf+0xcfc>
  return _ntoa_format(out, buffer, idx, maxlen, buf, len, negative, (unsigned int)base, prec, width, flags);
800155f4:	000c0613          	mv	a2,s8
800155f8:	00812423          	sw	s0,8(sp)
800155fc:	01a12223          	sw	s10,4(sp)
80015600:	01612023          	sw	s6,0(sp)
80015604:	000d8893          	mv	a7,s11
80015608:	00000813          	li	a6,0
8001560c:	000e0793          	mv	a5,t3
80015610:	a09ff06f          	j	80015018 <_vsnprintf+0x780>
    } while (value && (len < PRINTF_NTOA_BUFFER_SIZE));
80015614:	c3bf78e3          	bgeu	t5,s11,80015244 <_vsnprintf+0x9ac>
80015618:	c99ff06f          	j	800152b0 <_vsnprintf+0xa18>
            const int value = (flags & FLAGS_CHAR) ? (char)va_arg(va, int) : (flags & FLAGS_SHORT) ? (short int)va_arg(va, int) : va_arg(va, int);
8001561c:	000aa683          	lw	a3,0(s5)
            idx = _ntoa_long(out, buffer, idx, maxlen, (unsigned long)(value > 0 ? value : 0 - value), value < 0, base, precision, width, flags);
80015620:	41f6d593          	srai	a1,a3,0x1f
80015624:	00d5c633          	xor	a2,a1,a3
80015628:	40b60633          	sub	a2,a2,a1
8001562c:	8c5ff06f          	j	80014ef0 <_vsnprintf+0x658>
            const unsigned int value = (flags & FLAGS_CHAR) ? (unsigned char)va_arg(va, unsigned int) : (flags & FLAGS_SHORT) ? (unsigned short int)va_arg(va, unsigned int) : va_arg(va, unsigned int);
80015630:	000aaa83          	lw	s5,0(s5)
80015634:	9b5ff06f          	j	80014fe8 <_vsnprintf+0x750>
          while (l++ < width) {
80015638:	000c0413          	mv	s0,s8
8001563c:	b1dff06f          	j	80015158 <_vsnprintf+0x8c0>
          while (l++ < width) {
80015640:	00078d93          	mv	s11,a5
80015644:	ad9ff06f          	j	8001511c <_vsnprintf+0x884>

80015648 <printf_>:


///////////////////////////////////////////////////////////////////////////////

int printf_(const char* format, ...)
{
80015648:	fc010113          	addi	sp,sp,-64
  va_list va;
  va_start(va, format);
8001564c:	02410313          	addi	t1,sp,36
{
80015650:	02d12623          	sw	a3,44(sp)
  char buffer[1];
  const int ret = _vsnprintf(_out_char, buffer, (size_t)-1, format, va);
80015654:	00050693          	mv	a3,a0
80015658:	80014537          	lui	a0,0x80014
{
8001565c:	02b12223          	sw	a1,36(sp)
80015660:	02c12423          	sw	a2,40(sp)
80015664:	02e12823          	sw	a4,48(sp)
  const int ret = _vsnprintf(_out_char, buffer, (size_t)-1, format, va);
80015668:	00810593          	addi	a1,sp,8
8001566c:	00030713          	mv	a4,t1
80015670:	fff00613          	li	a2,-1
80015674:	bb850513          	addi	a0,a0,-1096 # 80013bb8 <__clzsi2+0xffffaf24>
{
80015678:	00112e23          	sw	ra,28(sp)
8001567c:	02f12a23          	sw	a5,52(sp)
80015680:	03012c23          	sw	a6,56(sp)
80015684:	03112e23          	sw	a7,60(sp)
  va_start(va, format);
80015688:	00612623          	sw	t1,12(sp)
  const int ret = _vsnprintf(_out_char, buffer, (size_t)-1, format, va);
8001568c:	a0cff0ef          	jal	ra,80014898 <_vsnprintf>
  va_end(va);
  return ret;
}
80015690:	01c12083          	lw	ra,28(sp)
80015694:	04010113          	addi	sp,sp,64
80015698:	00008067          	ret

8001569c <sprintf_>:


int sprintf_(char* buffer, const char* format, ...)
{
8001569c:	fc010113          	addi	sp,sp,-64
  va_list va;
  va_start(va, format);
800156a0:	02810313          	addi	t1,sp,40
{
800156a4:	02d12623          	sw	a3,44(sp)
  const int ret = _vsnprintf(_out_buffer, buffer, (size_t)-1, format, va);
800156a8:	00058693          	mv	a3,a1
800156ac:	00050593          	mv	a1,a0
800156b0:	80013537          	lui	a0,0x80013
{
800156b4:	02c12423          	sw	a2,40(sp)
800156b8:	02e12823          	sw	a4,48(sp)
  const int ret = _vsnprintf(_out_buffer, buffer, (size_t)-1, format, va);
800156bc:	fff00613          	li	a2,-1
800156c0:	00030713          	mv	a4,t1
800156c4:	7e450513          	addi	a0,a0,2020 # 800137e4 <__clzsi2+0xffffab50>
{
800156c8:	00112e23          	sw	ra,28(sp)
800156cc:	02f12a23          	sw	a5,52(sp)
800156d0:	03012c23          	sw	a6,56(sp)
800156d4:	03112e23          	sw	a7,60(sp)
  va_start(va, format);
800156d8:	00612623          	sw	t1,12(sp)
  const int ret = _vsnprintf(_out_buffer, buffer, (size_t)-1, format, va);
800156dc:	9bcff0ef          	jal	ra,80014898 <_vsnprintf>
  va_end(va);
  return ret;
}
800156e0:	01c12083          	lw	ra,28(sp)
800156e4:	04010113          	addi	sp,sp,64
800156e8:	00008067          	ret

800156ec <snprintf_>:


int snprintf_(char* buffer, size_t count, const char* format, ...)
{
800156ec:	fc010113          	addi	sp,sp,-64
  va_list va;
  va_start(va, format);
800156f0:	02c10313          	addi	t1,sp,44
{
800156f4:	02d12623          	sw	a3,44(sp)
  const int ret = _vsnprintf(_out_buffer, buffer, count, format, va);
800156f8:	00060693          	mv	a3,a2
800156fc:	00058613          	mv	a2,a1
80015700:	00050593          	mv	a1,a0
80015704:	80013537          	lui	a0,0x80013
{
80015708:	02e12823          	sw	a4,48(sp)
  const int ret = _vsnprintf(_out_buffer, buffer, count, format, va);
8001570c:	7e450513          	addi	a0,a0,2020 # 800137e4 <__clzsi2+0xffffab50>
80015710:	00030713          	mv	a4,t1
{
80015714:	00112e23          	sw	ra,28(sp)
80015718:	02f12a23          	sw	a5,52(sp)
8001571c:	03012c23          	sw	a6,56(sp)
80015720:	03112e23          	sw	a7,60(sp)
  va_start(va, format);
80015724:	00612623          	sw	t1,12(sp)
  const int ret = _vsnprintf(_out_buffer, buffer, count, format, va);
80015728:	970ff0ef          	jal	ra,80014898 <_vsnprintf>
  va_end(va);
  return ret;
}
8001572c:	01c12083          	lw	ra,28(sp)
80015730:	04010113          	addi	sp,sp,64
80015734:	00008067          	ret

80015738 <vprintf_>:


int vprintf_(const char* format, va_list va)
{
80015738:	fe010113          	addi	sp,sp,-32
8001573c:	00050693          	mv	a3,a0
  char buffer[1];
  return _vsnprintf(_out_char, buffer, (size_t)-1, format, va);
80015740:	80014537          	lui	a0,0x80014
{
80015744:	00058713          	mv	a4,a1
  return _vsnprintf(_out_char, buffer, (size_t)-1, format, va);
80015748:	fff00613          	li	a2,-1
8001574c:	00c10593          	addi	a1,sp,12
80015750:	bb850513          	addi	a0,a0,-1096 # 80013bb8 <__clzsi2+0xffffaf24>
{
80015754:	00112e23          	sw	ra,28(sp)
  return _vsnprintf(_out_char, buffer, (size_t)-1, format, va);
80015758:	940ff0ef          	jal	ra,80014898 <_vsnprintf>
}
8001575c:	01c12083          	lw	ra,28(sp)
80015760:	02010113          	addi	sp,sp,32
80015764:	00008067          	ret

80015768 <vsnprintf_>:


int vsnprintf_(char* buffer, size_t count, const char* format, va_list va)
{
80015768:	00068713          	mv	a4,a3
  return _vsnprintf(_out_buffer, buffer, count, format, va);
8001576c:	00060693          	mv	a3,a2
80015770:	00058613          	mv	a2,a1
80015774:	00050593          	mv	a1,a0
80015778:	80013537          	lui	a0,0x80013
8001577c:	7e450513          	addi	a0,a0,2020 # 800137e4 <__clzsi2+0xffffab50>
80015780:	918ff06f          	j	80014898 <_vsnprintf>

80015784 <fctprintf>:
}


int fctprintf(void (*out)(char character, void* arg), void* arg, const char* format, ...)
{
80015784:	fc010113          	addi	sp,sp,-64
  va_list va;
  va_start(va, format);
80015788:	02c10313          	addi	t1,sp,44
{
8001578c:	00050e93          	mv	t4,a0
  const out_fct_wrap_type out_fct_wrap = { out, arg };
  const int ret = _vsnprintf(_out_fct, (char*)(uintptr_t)&out_fct_wrap, (size_t)-1, format, va);
80015790:	80014537          	lui	a0,0x80014
{
80015794:	00058e13          	mv	t3,a1
80015798:	02d12623          	sw	a3,44(sp)
8001579c:	02e12823          	sw	a4,48(sp)
  const int ret = _vsnprintf(_out_fct, (char*)(uintptr_t)&out_fct_wrap, (size_t)-1, format, va);
800157a0:	00060693          	mv	a3,a2
800157a4:	00810593          	addi	a1,sp,8
800157a8:	00030713          	mv	a4,t1
800157ac:	fff00613          	li	a2,-1
800157b0:	bc450513          	addi	a0,a0,-1084 # 80013bc4 <__clzsi2+0xffffaf30>
{
800157b4:	00112e23          	sw	ra,28(sp)
800157b8:	02f12a23          	sw	a5,52(sp)
800157bc:	03012c23          	sw	a6,56(sp)
800157c0:	03112e23          	sw	a7,60(sp)
  va_start(va, format);
800157c4:	00612223          	sw	t1,4(sp)
  const out_fct_wrap_type out_fct_wrap = { out, arg };
800157c8:	01d12423          	sw	t4,8(sp)
800157cc:	01c12623          	sw	t3,12(sp)
  const int ret = _vsnprintf(_out_fct, (char*)(uintptr_t)&out_fct_wrap, (size_t)-1, format, va);
800157d0:	8c8ff0ef          	jal	ra,80014898 <_vsnprintf>
  va_end(va);
  return ret;
}
800157d4:	01c12083          	lw	ra,28(sp)
800157d8:	04010113          	addi	sp,sp,64
800157dc:	00008067          	ret

800157e0 <__divdi3>:
#endif

#ifdef L_divdi3
DWtype
__divdi3 (DWtype u, DWtype v)
{
800157e0:	fc010113          	addi	sp,sp,-64
800157e4:	02812c23          	sw	s0,56(sp)
800157e8:	03212823          	sw	s2,48(sp)
800157ec:	01712e23          	sw	s7,28(sp)
800157f0:	02112e23          	sw	ra,60(sp)
800157f4:	02912a23          	sw	s1,52(sp)
800157f8:	03312623          	sw	s3,44(sp)
800157fc:	03412423          	sw	s4,40(sp)
80015800:	03512223          	sw	s5,36(sp)
80015804:	03612023          	sw	s6,32(sp)
80015808:	01812c23          	sw	s8,24(sp)
8001580c:	01912a23          	sw	s9,20(sp)
80015810:	01a12823          	sw	s10,16(sp)
80015814:	01b12623          	sw	s11,12(sp)
80015818:	00050b93          	mv	s7,a0
8001581c:	00058413          	mv	s0,a1
  Wtype c = 0;
  DWunion uu = {.ll = u};
  DWunion vv = {.ll = v};
  DWtype w;

  if (uu.s.high < 0)
80015820:	00000913          	li	s2,0
80015824:	0005dc63          	bgez	a1,8001583c <__divdi3+0x5c>
    c = ~c,
    uu.ll = -uu.ll;
80015828:	00a037b3          	snez	a5,a0
8001582c:	40b00433          	neg	s0,a1
80015830:	40f40433          	sub	s0,s0,a5
80015834:	40a00bb3          	neg	s7,a0
    c = ~c,
80015838:	fff00913          	li	s2,-1
  if (vv.s.high < 0)
8001583c:	0006dc63          	bgez	a3,80015854 <__divdi3+0x74>
    c = ~c,
    vv.ll = -vv.ll;
80015840:	00c037b3          	snez	a5,a2
80015844:	40d00733          	neg	a4,a3
    c = ~c,
80015848:	fff94913          	not	s2,s2
    vv.ll = -vv.ll;
8001584c:	40f706b3          	sub	a3,a4,a5
80015850:	40c00633          	neg	a2,a2
  d0 = dd.s.low;
80015854:	00060993          	mv	s3,a2
  d1 = dd.s.high;
80015858:	00068a93          	mv	s5,a3
  n0 = nn.s.low;
8001585c:	000b8a13          	mv	s4,s7
  n1 = nn.s.high;
80015860:	00040c13          	mv	s8,s0
  if (d1 == 0)
80015864:	3a069263          	bnez	a3,80015c08 <__divdi3+0x428>
      if (d0 > n1)
80015868:	800004b7          	lui	s1,0x80000
8001586c:	6d048493          	addi	s1,s1,1744 # 800006d0 <__clzsi2+0xfffe7a3c>
80015870:	12c47663          	bgeu	s0,a2,8001599c <__divdi3+0x1bc>
	  count_leading_zeros (bm, d0);
80015874:	000107b7          	lui	a5,0x10
80015878:	10f67863          	bgeu	a2,a5,80015988 <__divdi3+0x1a8>
8001587c:	0ff00793          	li	a5,255
80015880:	00c7b7b3          	sltu	a5,a5,a2
80015884:	00379793          	slli	a5,a5,0x3
80015888:	00f65733          	srl	a4,a2,a5
8001588c:	00e484b3          	add	s1,s1,a4
80015890:	0004c703          	lbu	a4,0(s1)
80015894:	02000693          	li	a3,32
80015898:	00f707b3          	add	a5,a4,a5
8001589c:	40f68733          	sub	a4,a3,a5
	  if (bm != 0)
800158a0:	00f68c63          	beq	a3,a5,800158b8 <__divdi3+0xd8>
	      n1 = (n1 << bm) | (n0 >> (W_TYPE_SIZE - bm));
800158a4:	00e41433          	sll	s0,s0,a4
800158a8:	00fbd7b3          	srl	a5,s7,a5
	      d0 = d0 << bm;
800158ac:	00e619b3          	sll	s3,a2,a4
	      n1 = (n1 << bm) | (n0 >> (W_TYPE_SIZE - bm));
800158b0:	0087ec33          	or	s8,a5,s0
	      n0 = n0 << bm;
800158b4:	00eb9a33          	sll	s4,s7,a4
	  udiv_qrnnd (q0, n0, n1, n0, d0);
800158b8:	0109da93          	srli	s5,s3,0x10
800158bc:	000a8593          	mv	a1,s5
800158c0:	000c0513          	mv	a0,s8
800158c4:	36c030ef          	jal	ra,80018c30 <__umodsi3>
800158c8:	00050493          	mv	s1,a0
800158cc:	000a8593          	mv	a1,s5
800158d0:	01099b13          	slli	s6,s3,0x10
800158d4:	000c0513          	mv	a0,s8
800158d8:	310030ef          	jal	ra,80018be8 <__udivsi3>
800158dc:	010b5b13          	srli	s6,s6,0x10
800158e0:	00050413          	mv	s0,a0
800158e4:	00050593          	mv	a1,a0
800158e8:	000b0513          	mv	a0,s6
800158ec:	240030ef          	jal	ra,80018b2c <__mulsi3>
800158f0:	01049493          	slli	s1,s1,0x10
800158f4:	010a5713          	srli	a4,s4,0x10
800158f8:	00e4e733          	or	a4,s1,a4
800158fc:	00040b93          	mv	s7,s0
80015900:	00a77e63          	bgeu	a4,a0,8001591c <__divdi3+0x13c>
80015904:	01370733          	add	a4,a4,s3
80015908:	fff40b93          	addi	s7,s0,-1
8001590c:	01376863          	bltu	a4,s3,8001591c <__divdi3+0x13c>
80015910:	00a77663          	bgeu	a4,a0,8001591c <__divdi3+0x13c>
80015914:	ffe40b93          	addi	s7,s0,-2
80015918:	01370733          	add	a4,a4,s3
8001591c:	40a70433          	sub	s0,a4,a0
80015920:	000a8593          	mv	a1,s5
80015924:	00040513          	mv	a0,s0
80015928:	308030ef          	jal	ra,80018c30 <__umodsi3>
8001592c:	00050493          	mv	s1,a0
80015930:	000a8593          	mv	a1,s5
80015934:	00040513          	mv	a0,s0
80015938:	2b0030ef          	jal	ra,80018be8 <__udivsi3>
8001593c:	010a1a13          	slli	s4,s4,0x10
80015940:	00050413          	mv	s0,a0
80015944:	00050593          	mv	a1,a0
80015948:	01049493          	slli	s1,s1,0x10
8001594c:	000b0513          	mv	a0,s6
80015950:	010a5a13          	srli	s4,s4,0x10
80015954:	1d8030ef          	jal	ra,80018b2c <__mulsi3>
80015958:	0144ea33          	or	s4,s1,s4
8001595c:	00040613          	mv	a2,s0
80015960:	00aa7c63          	bgeu	s4,a0,80015978 <__divdi3+0x198>
80015964:	01498a33          	add	s4,s3,s4
80015968:	fff40613          	addi	a2,s0,-1
8001596c:	013a6663          	bltu	s4,s3,80015978 <__divdi3+0x198>
80015970:	00aa7463          	bgeu	s4,a0,80015978 <__divdi3+0x198>
80015974:	ffe40613          	addi	a2,s0,-2
80015978:	010b9793          	slli	a5,s7,0x10
8001597c:	00c7e7b3          	or	a5,a5,a2
	      q1 = 0;
80015980:	00000a93          	li	s5,0
80015984:	12c0006f          	j	80015ab0 <__divdi3+0x2d0>
	  count_leading_zeros (bm, d0);
80015988:	01000737          	lui	a4,0x1000
8001598c:	01000793          	li	a5,16
80015990:	eee66ce3          	bltu	a2,a4,80015888 <__divdi3+0xa8>
80015994:	01800793          	li	a5,24
80015998:	ef1ff06f          	j	80015888 <__divdi3+0xa8>
	  if (d0 == 0)
8001599c:	00061a63          	bnez	a2,800159b0 <__divdi3+0x1d0>
	    d0 = 1 / d0;	/* Divide intentionally by zero.  */
800159a0:	00000593          	li	a1,0
800159a4:	00100513          	li	a0,1
800159a8:	240030ef          	jal	ra,80018be8 <__udivsi3>
800159ac:	00050993          	mv	s3,a0
	  count_leading_zeros (bm, d0);
800159b0:	000107b7          	lui	a5,0x10
800159b4:	14f9fa63          	bgeu	s3,a5,80015b08 <__divdi3+0x328>
800159b8:	0ff00793          	li	a5,255
800159bc:	0137f463          	bgeu	a5,s3,800159c4 <__divdi3+0x1e4>
800159c0:	00800a93          	li	s5,8
800159c4:	0159d7b3          	srl	a5,s3,s5
800159c8:	00f484b3          	add	s1,s1,a5
800159cc:	0004cb03          	lbu	s6,0(s1)
800159d0:	02000713          	li	a4,32
800159d4:	015b0b33          	add	s6,s6,s5
800159d8:	416707b3          	sub	a5,a4,s6
	  if (bm == 0)
800159dc:	15671063          	bne	a4,s6,80015b1c <__divdi3+0x33c>
	      n1 -= d0;
800159e0:	41340433          	sub	s0,s0,s3
	      q1 = 1;
800159e4:	00100a93          	li	s5,1
	  udiv_qrnnd (q0, n0, n1, n0, d0);
800159e8:	0109db13          	srli	s6,s3,0x10
800159ec:	000b0593          	mv	a1,s6
800159f0:	00040513          	mv	a0,s0
800159f4:	23c030ef          	jal	ra,80018c30 <__umodsi3>
800159f8:	00050493          	mv	s1,a0
800159fc:	000b0593          	mv	a1,s6
80015a00:	00040513          	mv	a0,s0
80015a04:	01099b93          	slli	s7,s3,0x10
80015a08:	1e0030ef          	jal	ra,80018be8 <__udivsi3>
80015a0c:	010bdb93          	srli	s7,s7,0x10
80015a10:	00050413          	mv	s0,a0
80015a14:	00050593          	mv	a1,a0
80015a18:	000b8513          	mv	a0,s7
80015a1c:	110030ef          	jal	ra,80018b2c <__mulsi3>
80015a20:	01049493          	slli	s1,s1,0x10
80015a24:	010a5713          	srli	a4,s4,0x10
80015a28:	00e4e733          	or	a4,s1,a4
80015a2c:	00040c13          	mv	s8,s0
80015a30:	00a77e63          	bgeu	a4,a0,80015a4c <__divdi3+0x26c>
80015a34:	01370733          	add	a4,a4,s3
80015a38:	fff40c13          	addi	s8,s0,-1
80015a3c:	01376863          	bltu	a4,s3,80015a4c <__divdi3+0x26c>
80015a40:	00a77663          	bgeu	a4,a0,80015a4c <__divdi3+0x26c>
80015a44:	ffe40c13          	addi	s8,s0,-2
80015a48:	01370733          	add	a4,a4,s3
80015a4c:	40a70433          	sub	s0,a4,a0
80015a50:	000b0593          	mv	a1,s6
80015a54:	00040513          	mv	a0,s0
80015a58:	1d8030ef          	jal	ra,80018c30 <__umodsi3>
80015a5c:	00050493          	mv	s1,a0
80015a60:	000b0593          	mv	a1,s6
80015a64:	00040513          	mv	a0,s0
80015a68:	180030ef          	jal	ra,80018be8 <__udivsi3>
80015a6c:	010a1a13          	slli	s4,s4,0x10
80015a70:	00050413          	mv	s0,a0
80015a74:	00050593          	mv	a1,a0
80015a78:	01049493          	slli	s1,s1,0x10
80015a7c:	000b8513          	mv	a0,s7
80015a80:	010a5a13          	srli	s4,s4,0x10
80015a84:	0a8030ef          	jal	ra,80018b2c <__mulsi3>
80015a88:	0144ea33          	or	s4,s1,s4
80015a8c:	00040613          	mv	a2,s0
80015a90:	00aa7c63          	bgeu	s4,a0,80015aa8 <__divdi3+0x2c8>
80015a94:	01498a33          	add	s4,s3,s4
80015a98:	fff40613          	addi	a2,s0,-1
80015a9c:	013a6663          	bltu	s4,s3,80015aa8 <__divdi3+0x2c8>
80015aa0:	00aa7463          	bgeu	s4,a0,80015aa8 <__divdi3+0x2c8>
80015aa4:	ffe40613          	addi	a2,s0,-2
80015aa8:	010c1793          	slli	a5,s8,0x10
80015aac:	00c7e7b3          	or	a5,a5,a2
  return ww.ll;
80015ab0:	00078513          	mv	a0,a5
80015ab4:	000a8593          	mv	a1,s5

  w = __udivmoddi4 (uu.ll, vv.ll, (UDWtype *) 0);
  if (c)
80015ab8:	00090a63          	beqz	s2,80015acc <__divdi3+0x2ec>
    w = -w;
80015abc:	00f037b3          	snez	a5,a5
80015ac0:	41500733          	neg	a4,s5
80015ac4:	40f705b3          	sub	a1,a4,a5
80015ac8:	40a00533          	neg	a0,a0

  return w;
}
80015acc:	03c12083          	lw	ra,60(sp)
80015ad0:	03812403          	lw	s0,56(sp)
80015ad4:	03412483          	lw	s1,52(sp)
80015ad8:	03012903          	lw	s2,48(sp)
80015adc:	02c12983          	lw	s3,44(sp)
80015ae0:	02812a03          	lw	s4,40(sp)
80015ae4:	02412a83          	lw	s5,36(sp)
80015ae8:	02012b03          	lw	s6,32(sp)
80015aec:	01c12b83          	lw	s7,28(sp)
80015af0:	01812c03          	lw	s8,24(sp)
80015af4:	01412c83          	lw	s9,20(sp)
80015af8:	01012d03          	lw	s10,16(sp)
80015afc:	00c12d83          	lw	s11,12(sp)
80015b00:	04010113          	addi	sp,sp,64
80015b04:	00008067          	ret
	  count_leading_zeros (bm, d0);
80015b08:	010007b7          	lui	a5,0x1000
80015b0c:	01000a93          	li	s5,16
80015b10:	eaf9eae3          	bltu	s3,a5,800159c4 <__divdi3+0x1e4>
80015b14:	01800a93          	li	s5,24
80015b18:	eadff06f          	j	800159c4 <__divdi3+0x1e4>
	      d0 = d0 << bm;
80015b1c:	00f999b3          	sll	s3,s3,a5
	      n2 = n1 >> b;
80015b20:	01645ab3          	srl	s5,s0,s6
	      n1 = (n1 << bm) | (n0 >> b);
80015b24:	00f41433          	sll	s0,s0,a5
80015b28:	016bdb33          	srl	s6,s7,s6
80015b2c:	008b6b33          	or	s6,s6,s0
	      udiv_qrnnd (q1, n1, n2, n1, d0);
80015b30:	0109d413          	srli	s0,s3,0x10
80015b34:	00040593          	mv	a1,s0
80015b38:	000a8513          	mv	a0,s5
	      n0 = n0 << bm;
80015b3c:	00fb9a33          	sll	s4,s7,a5
	      udiv_qrnnd (q1, n1, n2, n1, d0);
80015b40:	0f0030ef          	jal	ra,80018c30 <__umodsi3>
80015b44:	00050493          	mv	s1,a0
80015b48:	00040593          	mv	a1,s0
80015b4c:	000a8513          	mv	a0,s5
80015b50:	01099b93          	slli	s7,s3,0x10
80015b54:	094030ef          	jal	ra,80018be8 <__udivsi3>
80015b58:	010bdb93          	srli	s7,s7,0x10
80015b5c:	00050a93          	mv	s5,a0
80015b60:	00050593          	mv	a1,a0
80015b64:	000b8513          	mv	a0,s7
80015b68:	7c5020ef          	jal	ra,80018b2c <__mulsi3>
80015b6c:	01049493          	slli	s1,s1,0x10
80015b70:	010b5793          	srli	a5,s6,0x10
80015b74:	00f4e7b3          	or	a5,s1,a5
80015b78:	000a8c13          	mv	s8,s5
80015b7c:	00a7fe63          	bgeu	a5,a0,80015b98 <__divdi3+0x3b8>
80015b80:	013787b3          	add	a5,a5,s3
80015b84:	fffa8c13          	addi	s8,s5,-1
80015b88:	0137e863          	bltu	a5,s3,80015b98 <__divdi3+0x3b8>
80015b8c:	00a7f663          	bgeu	a5,a0,80015b98 <__divdi3+0x3b8>
80015b90:	ffea8c13          	addi	s8,s5,-2
80015b94:	013787b3          	add	a5,a5,s3
80015b98:	40a78ab3          	sub	s5,a5,a0
80015b9c:	00040593          	mv	a1,s0
80015ba0:	000a8513          	mv	a0,s5
80015ba4:	08c030ef          	jal	ra,80018c30 <__umodsi3>
80015ba8:	00040593          	mv	a1,s0
80015bac:	00050493          	mv	s1,a0
80015bb0:	000a8513          	mv	a0,s5
80015bb4:	034030ef          	jal	ra,80018be8 <__udivsi3>
80015bb8:	010b1413          	slli	s0,s6,0x10
80015bbc:	00050c93          	mv	s9,a0
80015bc0:	00050593          	mv	a1,a0
80015bc4:	01049493          	slli	s1,s1,0x10
80015bc8:	000b8513          	mv	a0,s7
80015bcc:	01045413          	srli	s0,s0,0x10
80015bd0:	75d020ef          	jal	ra,80018b2c <__mulsi3>
80015bd4:	0084e433          	or	s0,s1,s0
80015bd8:	000c8a93          	mv	s5,s9
80015bdc:	00a47e63          	bgeu	s0,a0,80015bf8 <__divdi3+0x418>
80015be0:	01340433          	add	s0,s0,s3
80015be4:	fffc8a93          	addi	s5,s9,-1
80015be8:	01346863          	bltu	s0,s3,80015bf8 <__divdi3+0x418>
80015bec:	00a47663          	bgeu	s0,a0,80015bf8 <__divdi3+0x418>
80015bf0:	ffec8a93          	addi	s5,s9,-2
80015bf4:	01340433          	add	s0,s0,s3
80015bf8:	010c1593          	slli	a1,s8,0x10
80015bfc:	40a40433          	sub	s0,s0,a0
80015c00:	0155eab3          	or	s5,a1,s5
80015c04:	de5ff06f          	j	800159e8 <__divdi3+0x208>
      if (d1 > n1)
80015c08:	1ed46e63          	bltu	s0,a3,80015e04 <__divdi3+0x624>
	  count_leading_zeros (bm, d1);
80015c0c:	000107b7          	lui	a5,0x10
80015c10:	04f6f463          	bgeu	a3,a5,80015c58 <__divdi3+0x478>
80015c14:	0ff00b13          	li	s6,255
80015c18:	00db37b3          	sltu	a5,s6,a3
80015c1c:	00379793          	slli	a5,a5,0x3
80015c20:	80000737          	lui	a4,0x80000
80015c24:	00f6d5b3          	srl	a1,a3,a5
80015c28:	6d070713          	addi	a4,a4,1744 # 800006d0 <__clzsi2+0xfffe7a3c>
80015c2c:	00b70733          	add	a4,a4,a1
80015c30:	00074b03          	lbu	s6,0(a4)
80015c34:	00fb0b33          	add	s6,s6,a5
80015c38:	02000793          	li	a5,32
80015c3c:	41678ab3          	sub	s5,a5,s6
	  if (bm == 0)
80015c40:	03679663          	bne	a5,s6,80015c6c <__divdi3+0x48c>
		  q0 = 1;
80015c44:	00100793          	li	a5,1
	      if (n1 > d1 || n0 >= d0)
80015c48:	e686e4e3          	bltu	a3,s0,80015ab0 <__divdi3+0x2d0>
80015c4c:	00cbb7b3          	sltu	a5,s7,a2
80015c50:	0017c793          	xori	a5,a5,1
80015c54:	e5dff06f          	j	80015ab0 <__divdi3+0x2d0>
	  count_leading_zeros (bm, d1);
80015c58:	01000737          	lui	a4,0x1000
80015c5c:	01000793          	li	a5,16
80015c60:	fce6e0e3          	bltu	a3,a4,80015c20 <__divdi3+0x440>
80015c64:	01800793          	li	a5,24
80015c68:	fb9ff06f          	j	80015c20 <__divdi3+0x440>
	      d1 = (d1 << bm) | (d0 >> b);
80015c6c:	01665d33          	srl	s10,a2,s6
80015c70:	015696b3          	sll	a3,a3,s5
80015c74:	00dd6d33          	or	s10,s10,a3
	      n2 = n1 >> b;
80015c78:	016454b3          	srl	s1,s0,s6
	      udiv_qrnnd (q0, n1, n2, n1, d1);
80015c7c:	010d5c13          	srli	s8,s10,0x10
80015c80:	000c0593          	mv	a1,s8
80015c84:	00048513          	mv	a0,s1
	      d0 = d0 << bm;
80015c88:	01561a33          	sll	s4,a2,s5
	      udiv_qrnnd (q0, n1, n2, n1, d1);
80015c8c:	7a5020ef          	jal	ra,80018c30 <__umodsi3>
80015c90:	00050993          	mv	s3,a0
80015c94:	000c0593          	mv	a1,s8
80015c98:	010d1c93          	slli	s9,s10,0x10
80015c9c:	00048513          	mv	a0,s1
80015ca0:	749020ef          	jal	ra,80018be8 <__udivsi3>
	      n1 = (n1 << bm) | (n0 >> b);
80015ca4:	01541433          	sll	s0,s0,s5
80015ca8:	016bdb33          	srl	s6,s7,s6
	      udiv_qrnnd (q0, n1, n2, n1, d1);
80015cac:	010cdc93          	srli	s9,s9,0x10
	      n1 = (n1 << bm) | (n0 >> b);
80015cb0:	008b6b33          	or	s6,s6,s0
	      udiv_qrnnd (q0, n1, n2, n1, d1);
80015cb4:	00050593          	mv	a1,a0
80015cb8:	00050413          	mv	s0,a0
80015cbc:	000c8513          	mv	a0,s9
80015cc0:	66d020ef          	jal	ra,80018b2c <__mulsi3>
80015cc4:	01099993          	slli	s3,s3,0x10
80015cc8:	010b5713          	srli	a4,s6,0x10
80015ccc:	00e9e733          	or	a4,s3,a4
80015cd0:	00040d93          	mv	s11,s0
80015cd4:	00a77e63          	bgeu	a4,a0,80015cf0 <__divdi3+0x510>
80015cd8:	01a70733          	add	a4,a4,s10
80015cdc:	fff40d93          	addi	s11,s0,-1
80015ce0:	01a76863          	bltu	a4,s10,80015cf0 <__divdi3+0x510>
80015ce4:	00a77663          	bgeu	a4,a0,80015cf0 <__divdi3+0x510>
80015ce8:	ffe40d93          	addi	s11,s0,-2
80015cec:	01a70733          	add	a4,a4,s10
80015cf0:	40a704b3          	sub	s1,a4,a0
80015cf4:	000c0593          	mv	a1,s8
80015cf8:	00048513          	mv	a0,s1
80015cfc:	735020ef          	jal	ra,80018c30 <__umodsi3>
80015d00:	00050993          	mv	s3,a0
80015d04:	000c0593          	mv	a1,s8
80015d08:	00048513          	mv	a0,s1
80015d0c:	6dd020ef          	jal	ra,80018be8 <__udivsi3>
80015d10:	010b1413          	slli	s0,s6,0x10
80015d14:	00050493          	mv	s1,a0
80015d18:	00050593          	mv	a1,a0
80015d1c:	01099993          	slli	s3,s3,0x10
80015d20:	000c8513          	mv	a0,s9
80015d24:	01045413          	srli	s0,s0,0x10
80015d28:	605020ef          	jal	ra,80018b2c <__mulsi3>
80015d2c:	0089e433          	or	s0,s3,s0
80015d30:	00048613          	mv	a2,s1
80015d34:	00a47e63          	bgeu	s0,a0,80015d50 <__divdi3+0x570>
80015d38:	01a40433          	add	s0,s0,s10
80015d3c:	fff48613          	addi	a2,s1,-1
80015d40:	01a46863          	bltu	s0,s10,80015d50 <__divdi3+0x570>
80015d44:	00a47663          	bgeu	s0,a0,80015d50 <__divdi3+0x570>
80015d48:	ffe48613          	addi	a2,s1,-2
80015d4c:	01a40433          	add	s0,s0,s10
80015d50:	010d9793          	slli	a5,s11,0x10
	      umul_ppmm (m1, m0, q0, d0);
80015d54:	00010e37          	lui	t3,0x10
	      udiv_qrnnd (q0, n1, n2, n1, d1);
80015d58:	00c7e7b3          	or	a5,a5,a2
	      umul_ppmm (m1, m0, q0, d0);
80015d5c:	fffe0313          	addi	t1,t3,-1 # ffff <font6x8-0x7fff002d>
80015d60:	0067f8b3          	and	a7,a5,t1
80015d64:	006a7333          	and	t1,s4,t1
	      udiv_qrnnd (q0, n1, n2, n1, d1);
80015d68:	40a40433          	sub	s0,s0,a0
	      umul_ppmm (m1, m0, q0, d0);
80015d6c:	0107de93          	srli	t4,a5,0x10
80015d70:	010a5a13          	srli	s4,s4,0x10
80015d74:	00088513          	mv	a0,a7
80015d78:	00030593          	mv	a1,t1
80015d7c:	5b1020ef          	jal	ra,80018b2c <__mulsi3>
80015d80:	00050813          	mv	a6,a0
80015d84:	000a0593          	mv	a1,s4
80015d88:	00088513          	mv	a0,a7
80015d8c:	5a1020ef          	jal	ra,80018b2c <__mulsi3>
80015d90:	00050893          	mv	a7,a0
80015d94:	00030593          	mv	a1,t1
80015d98:	000e8513          	mv	a0,t4
80015d9c:	591020ef          	jal	ra,80018b2c <__mulsi3>
80015da0:	00050313          	mv	t1,a0
80015da4:	000a0593          	mv	a1,s4
80015da8:	000e8513          	mv	a0,t4
80015dac:	581020ef          	jal	ra,80018b2c <__mulsi3>
80015db0:	01085713          	srli	a4,a6,0x10
80015db4:	006888b3          	add	a7,a7,t1
80015db8:	01170733          	add	a4,a4,a7
80015dbc:	00050693          	mv	a3,a0
80015dc0:	00677463          	bgeu	a4,t1,80015dc8 <__divdi3+0x5e8>
80015dc4:	01c506b3          	add	a3,a0,t3
80015dc8:	01075513          	srli	a0,a4,0x10
80015dcc:	00d506b3          	add	a3,a0,a3
	      if (m1 > n1 || (m1 == n1 && m0 > n0))
80015dd0:	02d46663          	bltu	s0,a3,80015dfc <__divdi3+0x61c>
80015dd4:	bad416e3          	bne	s0,a3,80015980 <__divdi3+0x1a0>
	      umul_ppmm (m1, m0, q0, d0);
80015dd8:	00010537          	lui	a0,0x10
80015ddc:	fff50513          	addi	a0,a0,-1 # ffff <font6x8-0x7fff002d>
80015de0:	00a77733          	and	a4,a4,a0
80015de4:	01071713          	slli	a4,a4,0x10
80015de8:	00a87833          	and	a6,a6,a0
	      n0 = n0 << bm;
80015dec:	015b9bb3          	sll	s7,s7,s5
	      umul_ppmm (m1, m0, q0, d0);
80015df0:	01070733          	add	a4,a4,a6
	      q1 = 0;
80015df4:	00000a93          	li	s5,0
	      if (m1 > n1 || (m1 == n1 && m0 > n0))
80015df8:	caebfce3          	bgeu	s7,a4,80015ab0 <__divdi3+0x2d0>
		  q0--;
80015dfc:	fff78793          	addi	a5,a5,-1 # ffff <font6x8-0x7fff002d>
		  sub_ddmmss (m1, m0, m1, m0, d1, d0);
80015e00:	b81ff06f          	j	80015980 <__divdi3+0x1a0>
	  q1 = 0;
80015e04:	00000a93          	li	s5,0
	  q0 = 0;
80015e08:	00000793          	li	a5,0
80015e0c:	ca5ff06f          	j	80015ab0 <__divdi3+0x2d0>

80015e10 <__udivdi3>:
#endif

#ifdef L_udivdi3
UDWtype
__udivdi3 (UDWtype n, UDWtype d)
{
80015e10:	fd010113          	addi	sp,sp,-48
80015e14:	02812423          	sw	s0,40(sp)
80015e18:	01712623          	sw	s7,12(sp)
80015e1c:	02112623          	sw	ra,44(sp)
80015e20:	02912223          	sw	s1,36(sp)
80015e24:	03212023          	sw	s2,32(sp)
80015e28:	01312e23          	sw	s3,28(sp)
80015e2c:	01412c23          	sw	s4,24(sp)
80015e30:	01512a23          	sw	s5,20(sp)
80015e34:	01612823          	sw	s6,16(sp)
80015e38:	01812423          	sw	s8,8(sp)
80015e3c:	01912223          	sw	s9,4(sp)
80015e40:	00050b93          	mv	s7,a0
80015e44:	00058413          	mv	s0,a1
  if (d1 == 0)
80015e48:	38069c63          	bnez	a3,800161e0 <__udivdi3+0x3d0>
80015e4c:	800004b7          	lui	s1,0x80000
80015e50:	00060913          	mv	s2,a2
80015e54:	00050a13          	mv	s4,a0
      if (d0 > n1)
80015e58:	6d048493          	addi	s1,s1,1744 # 800006d0 <__clzsi2+0xfffe7a3c>
80015e5c:	12c5f863          	bgeu	a1,a2,80015f8c <__udivdi3+0x17c>
	  count_leading_zeros (bm, d0);
80015e60:	000107b7          	lui	a5,0x10
80015e64:	00058c13          	mv	s8,a1
80015e68:	10f67863          	bgeu	a2,a5,80015f78 <__udivdi3+0x168>
80015e6c:	0ff00713          	li	a4,255
80015e70:	00c73733          	sltu	a4,a4,a2
80015e74:	00371713          	slli	a4,a4,0x3
80015e78:	00e657b3          	srl	a5,a2,a4
80015e7c:	00f484b3          	add	s1,s1,a5
80015e80:	0004c683          	lbu	a3,0(s1)
80015e84:	00e68733          	add	a4,a3,a4
80015e88:	02000693          	li	a3,32
80015e8c:	40e687b3          	sub	a5,a3,a4
	  if (bm != 0)
80015e90:	00e68c63          	beq	a3,a4,80015ea8 <__udivdi3+0x98>
	      n1 = (n1 << bm) | (n0 >> (W_TYPE_SIZE - bm));
80015e94:	00f41433          	sll	s0,s0,a5
80015e98:	00ebd733          	srl	a4,s7,a4
	      d0 = d0 << bm;
80015e9c:	00f61933          	sll	s2,a2,a5
	      n1 = (n1 << bm) | (n0 >> (W_TYPE_SIZE - bm));
80015ea0:	00876c33          	or	s8,a4,s0
	      n0 = n0 << bm;
80015ea4:	00fb9a33          	sll	s4,s7,a5
	  udiv_qrnnd (q0, n0, n1, n0, d0);
80015ea8:	01095b13          	srli	s6,s2,0x10
80015eac:	000b0593          	mv	a1,s6
80015eb0:	000c0513          	mv	a0,s8
80015eb4:	57d020ef          	jal	ra,80018c30 <__umodsi3>
80015eb8:	00050493          	mv	s1,a0
80015ebc:	000b0593          	mv	a1,s6
80015ec0:	01091a93          	slli	s5,s2,0x10
80015ec4:	000c0513          	mv	a0,s8
80015ec8:	521020ef          	jal	ra,80018be8 <__udivsi3>
80015ecc:	010ada93          	srli	s5,s5,0x10
80015ed0:	00050413          	mv	s0,a0
80015ed4:	00050593          	mv	a1,a0
80015ed8:	000a8513          	mv	a0,s5
80015edc:	451020ef          	jal	ra,80018b2c <__mulsi3>
80015ee0:	01049493          	slli	s1,s1,0x10
80015ee4:	010a5713          	srli	a4,s4,0x10
80015ee8:	00e4e733          	or	a4,s1,a4
80015eec:	00040993          	mv	s3,s0
80015ef0:	00a77e63          	bgeu	a4,a0,80015f0c <__udivdi3+0xfc>
80015ef4:	01270733          	add	a4,a4,s2
80015ef8:	fff40993          	addi	s3,s0,-1
80015efc:	01276863          	bltu	a4,s2,80015f0c <__udivdi3+0xfc>
80015f00:	00a77663          	bgeu	a4,a0,80015f0c <__udivdi3+0xfc>
80015f04:	ffe40993          	addi	s3,s0,-2
80015f08:	01270733          	add	a4,a4,s2
80015f0c:	40a70433          	sub	s0,a4,a0
80015f10:	000b0593          	mv	a1,s6
80015f14:	00040513          	mv	a0,s0
80015f18:	519020ef          	jal	ra,80018c30 <__umodsi3>
80015f1c:	00050493          	mv	s1,a0
80015f20:	000b0593          	mv	a1,s6
80015f24:	00040513          	mv	a0,s0
80015f28:	4c1020ef          	jal	ra,80018be8 <__udivsi3>
80015f2c:	010a1a13          	slli	s4,s4,0x10
80015f30:	00050413          	mv	s0,a0
80015f34:	00050593          	mv	a1,a0
80015f38:	01049493          	slli	s1,s1,0x10
80015f3c:	000a8513          	mv	a0,s5
80015f40:	010a5a13          	srli	s4,s4,0x10
80015f44:	3e9020ef          	jal	ra,80018b2c <__mulsi3>
80015f48:	0144ea33          	or	s4,s1,s4
80015f4c:	00040613          	mv	a2,s0
80015f50:	00aa7c63          	bgeu	s4,a0,80015f68 <__udivdi3+0x158>
80015f54:	01490a33          	add	s4,s2,s4
80015f58:	fff40613          	addi	a2,s0,-1
80015f5c:	012a6663          	bltu	s4,s2,80015f68 <__udivdi3+0x158>
80015f60:	00aa7463          	bgeu	s4,a0,80015f68 <__udivdi3+0x158>
80015f64:	ffe40613          	addi	a2,s0,-2
80015f68:	01099793          	slli	a5,s3,0x10
80015f6c:	00c7e7b3          	or	a5,a5,a2
	      q1 = 0;
80015f70:	00000493          	li	s1,0
80015f74:	1300006f          	j	800160a4 <__udivdi3+0x294>
	  count_leading_zeros (bm, d0);
80015f78:	010007b7          	lui	a5,0x1000
80015f7c:	01000713          	li	a4,16
80015f80:	eef66ce3          	bltu	a2,a5,80015e78 <__udivdi3+0x68>
80015f84:	01800713          	li	a4,24
80015f88:	ef1ff06f          	j	80015e78 <__udivdi3+0x68>
80015f8c:	00068993          	mv	s3,a3
	  if (d0 == 0)
80015f90:	00061a63          	bnez	a2,80015fa4 <__udivdi3+0x194>
	    d0 = 1 / d0;	/* Divide intentionally by zero.  */
80015f94:	00000593          	li	a1,0
80015f98:	00100513          	li	a0,1
80015f9c:	44d020ef          	jal	ra,80018be8 <__udivsi3>
80015fa0:	00050913          	mv	s2,a0
	  count_leading_zeros (bm, d0);
80015fa4:	000107b7          	lui	a5,0x10
80015fa8:	12f97c63          	bgeu	s2,a5,800160e0 <__udivdi3+0x2d0>
80015fac:	0ff00793          	li	a5,255
80015fb0:	0127f463          	bgeu	a5,s2,80015fb8 <__udivdi3+0x1a8>
80015fb4:	00800993          	li	s3,8
80015fb8:	013957b3          	srl	a5,s2,s3
80015fbc:	00f484b3          	add	s1,s1,a5
80015fc0:	0004c783          	lbu	a5,0(s1)
80015fc4:	02000693          	li	a3,32
80015fc8:	013787b3          	add	a5,a5,s3
80015fcc:	40f68733          	sub	a4,a3,a5
	  if (bm == 0)
80015fd0:	12f69263          	bne	a3,a5,800160f4 <__udivdi3+0x2e4>
	      n1 -= d0;
80015fd4:	41240433          	sub	s0,s0,s2
	      q1 = 1;
80015fd8:	00100493          	li	s1,1
	  udiv_qrnnd (q0, n0, n1, n0, d0);
80015fdc:	01095a93          	srli	s5,s2,0x10
80015fe0:	000a8593          	mv	a1,s5
80015fe4:	00040513          	mv	a0,s0
80015fe8:	449020ef          	jal	ra,80018c30 <__umodsi3>
80015fec:	00050993          	mv	s3,a0
80015ff0:	000a8593          	mv	a1,s5
80015ff4:	00040513          	mv	a0,s0
80015ff8:	01091b13          	slli	s6,s2,0x10
80015ffc:	3ed020ef          	jal	ra,80018be8 <__udivsi3>
80016000:	010b5b13          	srli	s6,s6,0x10
80016004:	00050413          	mv	s0,a0
80016008:	00050593          	mv	a1,a0
8001600c:	000b0513          	mv	a0,s6
80016010:	31d020ef          	jal	ra,80018b2c <__mulsi3>
80016014:	01099993          	slli	s3,s3,0x10
80016018:	010a5713          	srli	a4,s4,0x10
8001601c:	00e9e733          	or	a4,s3,a4
80016020:	00040b93          	mv	s7,s0
80016024:	00a77e63          	bgeu	a4,a0,80016040 <__udivdi3+0x230>
80016028:	01270733          	add	a4,a4,s2
8001602c:	fff40b93          	addi	s7,s0,-1
80016030:	01276863          	bltu	a4,s2,80016040 <__udivdi3+0x230>
80016034:	00a77663          	bgeu	a4,a0,80016040 <__udivdi3+0x230>
80016038:	ffe40b93          	addi	s7,s0,-2
8001603c:	01270733          	add	a4,a4,s2
80016040:	40a70433          	sub	s0,a4,a0
80016044:	000a8593          	mv	a1,s5
80016048:	00040513          	mv	a0,s0
8001604c:	3e5020ef          	jal	ra,80018c30 <__umodsi3>
80016050:	00050993          	mv	s3,a0
80016054:	000a8593          	mv	a1,s5
80016058:	00040513          	mv	a0,s0
8001605c:	38d020ef          	jal	ra,80018be8 <__udivsi3>
80016060:	010a1a13          	slli	s4,s4,0x10
80016064:	00050413          	mv	s0,a0
80016068:	00050593          	mv	a1,a0
8001606c:	01099993          	slli	s3,s3,0x10
80016070:	000b0513          	mv	a0,s6
80016074:	010a5a13          	srli	s4,s4,0x10
80016078:	2b5020ef          	jal	ra,80018b2c <__mulsi3>
8001607c:	0149ea33          	or	s4,s3,s4
80016080:	00040613          	mv	a2,s0
80016084:	00aa7c63          	bgeu	s4,a0,8001609c <__udivdi3+0x28c>
80016088:	01490a33          	add	s4,s2,s4
8001608c:	fff40613          	addi	a2,s0,-1
80016090:	012a6663          	bltu	s4,s2,8001609c <__udivdi3+0x28c>
80016094:	00aa7463          	bgeu	s4,a0,8001609c <__udivdi3+0x28c>
80016098:	ffe40613          	addi	a2,s0,-2
8001609c:	010b9793          	slli	a5,s7,0x10
800160a0:	00c7e7b3          	or	a5,a5,a2
  return __udivmoddi4 (n, d, (UDWtype *) 0);
}
800160a4:	00078513          	mv	a0,a5
800160a8:	00048593          	mv	a1,s1
800160ac:	02c12083          	lw	ra,44(sp)
800160b0:	02812403          	lw	s0,40(sp)
800160b4:	02412483          	lw	s1,36(sp)
800160b8:	02012903          	lw	s2,32(sp)
800160bc:	01c12983          	lw	s3,28(sp)
800160c0:	01812a03          	lw	s4,24(sp)
800160c4:	01412a83          	lw	s5,20(sp)
800160c8:	01012b03          	lw	s6,16(sp)
800160cc:	00c12b83          	lw	s7,12(sp)
800160d0:	00812c03          	lw	s8,8(sp)
800160d4:	00412c83          	lw	s9,4(sp)
800160d8:	03010113          	addi	sp,sp,48
800160dc:	00008067          	ret
	  count_leading_zeros (bm, d0);
800160e0:	010007b7          	lui	a5,0x1000
800160e4:	01000993          	li	s3,16
800160e8:	ecf968e3          	bltu	s2,a5,80015fb8 <__udivdi3+0x1a8>
800160ec:	01800993          	li	s3,24
800160f0:	ec9ff06f          	j	80015fb8 <__udivdi3+0x1a8>
	      d0 = d0 << bm;
800160f4:	00e91933          	sll	s2,s2,a4
	      n2 = n1 >> b;
800160f8:	00f459b3          	srl	s3,s0,a5
	      n1 = (n1 << bm) | (n0 >> b);
800160fc:	00fbd7b3          	srl	a5,s7,a5
80016100:	00e41433          	sll	s0,s0,a4
80016104:	0087eab3          	or	s5,a5,s0
	      udiv_qrnnd (q1, n1, n2, n1, d0);
80016108:	01095413          	srli	s0,s2,0x10
8001610c:	00040593          	mv	a1,s0
80016110:	00098513          	mv	a0,s3
	      n0 = n0 << bm;
80016114:	00eb9a33          	sll	s4,s7,a4
	      udiv_qrnnd (q1, n1, n2, n1, d0);
80016118:	319020ef          	jal	ra,80018c30 <__umodsi3>
8001611c:	00050493          	mv	s1,a0
80016120:	00040593          	mv	a1,s0
80016124:	00098513          	mv	a0,s3
80016128:	01091b13          	slli	s6,s2,0x10
8001612c:	2bd020ef          	jal	ra,80018be8 <__udivsi3>
80016130:	010b5b13          	srli	s6,s6,0x10
80016134:	00050993          	mv	s3,a0
80016138:	00050593          	mv	a1,a0
8001613c:	000b0513          	mv	a0,s6
80016140:	1ed020ef          	jal	ra,80018b2c <__mulsi3>
80016144:	01049493          	slli	s1,s1,0x10
80016148:	010ad793          	srli	a5,s5,0x10
8001614c:	00f4e7b3          	or	a5,s1,a5
80016150:	00098b93          	mv	s7,s3
80016154:	00a7fe63          	bgeu	a5,a0,80016170 <__udivdi3+0x360>
80016158:	012787b3          	add	a5,a5,s2
8001615c:	fff98b93          	addi	s7,s3,-1
80016160:	0127e863          	bltu	a5,s2,80016170 <__udivdi3+0x360>
80016164:	00a7f663          	bgeu	a5,a0,80016170 <__udivdi3+0x360>
80016168:	ffe98b93          	addi	s7,s3,-2
8001616c:	012787b3          	add	a5,a5,s2
80016170:	40a789b3          	sub	s3,a5,a0
80016174:	00040593          	mv	a1,s0
80016178:	00098513          	mv	a0,s3
8001617c:	2b5020ef          	jal	ra,80018c30 <__umodsi3>
80016180:	00040593          	mv	a1,s0
80016184:	00050493          	mv	s1,a0
80016188:	00098513          	mv	a0,s3
8001618c:	25d020ef          	jal	ra,80018be8 <__udivsi3>
80016190:	010a9413          	slli	s0,s5,0x10
80016194:	00050993          	mv	s3,a0
80016198:	00050593          	mv	a1,a0
8001619c:	01049493          	slli	s1,s1,0x10
800161a0:	000b0513          	mv	a0,s6
800161a4:	01045413          	srli	s0,s0,0x10
800161a8:	185020ef          	jal	ra,80018b2c <__mulsi3>
800161ac:	0084e433          	or	s0,s1,s0
800161b0:	00098793          	mv	a5,s3
800161b4:	00a47e63          	bgeu	s0,a0,800161d0 <__udivdi3+0x3c0>
800161b8:	01240433          	add	s0,s0,s2
800161bc:	fff98793          	addi	a5,s3,-1
800161c0:	01246863          	bltu	s0,s2,800161d0 <__udivdi3+0x3c0>
800161c4:	00a47663          	bgeu	s0,a0,800161d0 <__udivdi3+0x3c0>
800161c8:	ffe98793          	addi	a5,s3,-2
800161cc:	01240433          	add	s0,s0,s2
800161d0:	010b9493          	slli	s1,s7,0x10
800161d4:	40a40433          	sub	s0,s0,a0
800161d8:	00f4e4b3          	or	s1,s1,a5
800161dc:	e01ff06f          	j	80015fdc <__udivdi3+0x1cc>
      if (d1 > n1)
800161e0:	1ed5ee63          	bltu	a1,a3,800163dc <__udivdi3+0x5cc>
	  count_leading_zeros (bm, d1);
800161e4:	000107b7          	lui	a5,0x10
800161e8:	04f6f463          	bgeu	a3,a5,80016230 <__udivdi3+0x420>
800161ec:	0ff00a93          	li	s5,255
800161f0:	00dab733          	sltu	a4,s5,a3
800161f4:	00371713          	slli	a4,a4,0x3
800161f8:	800007b7          	lui	a5,0x80000
800161fc:	00e6d5b3          	srl	a1,a3,a4
80016200:	6d078793          	addi	a5,a5,1744 # 800006d0 <__clzsi2+0xfffe7a3c>
80016204:	00b787b3          	add	a5,a5,a1
80016208:	0007ca83          	lbu	s5,0(a5)
8001620c:	02000793          	li	a5,32
80016210:	00ea8ab3          	add	s5,s5,a4
80016214:	415784b3          	sub	s1,a5,s5
	  if (bm == 0)
80016218:	03579663          	bne	a5,s5,80016244 <__udivdi3+0x434>
		  q0 = 1;
8001621c:	00100793          	li	a5,1
	      if (n1 > d1 || n0 >= d0)
80016220:	e886e2e3          	bltu	a3,s0,800160a4 <__udivdi3+0x294>
80016224:	00cbb7b3          	sltu	a5,s7,a2
80016228:	0017c793          	xori	a5,a5,1
8001622c:	e79ff06f          	j	800160a4 <__udivdi3+0x294>
	  count_leading_zeros (bm, d1);
80016230:	010007b7          	lui	a5,0x1000
80016234:	01000713          	li	a4,16
80016238:	fcf6e0e3          	bltu	a3,a5,800161f8 <__udivdi3+0x3e8>
8001623c:	01800713          	li	a4,24
80016240:	fb9ff06f          	j	800161f8 <__udivdi3+0x3e8>
	      d1 = (d1 << bm) | (d0 >> b);
80016244:	01565cb3          	srl	s9,a2,s5
80016248:	009696b3          	sll	a3,a3,s1
8001624c:	00dcecb3          	or	s9,s9,a3
	      n2 = n1 >> b;
80016250:	015459b3          	srl	s3,s0,s5
	      n1 = (n1 << bm) | (n0 >> b);
80016254:	00941433          	sll	s0,s0,s1
80016258:	015bdab3          	srl	s5,s7,s5
8001625c:	008aeab3          	or	s5,s5,s0
	      udiv_qrnnd (q0, n1, n2, n1, d1);
80016260:	010cd413          	srli	s0,s9,0x10
80016264:	00040593          	mv	a1,s0
80016268:	00098513          	mv	a0,s3
	      d0 = d0 << bm;
8001626c:	00961933          	sll	s2,a2,s1
	      udiv_qrnnd (q0, n1, n2, n1, d1);
80016270:	1c1020ef          	jal	ra,80018c30 <__umodsi3>
80016274:	00050a13          	mv	s4,a0
80016278:	00040593          	mv	a1,s0
8001627c:	00098513          	mv	a0,s3
80016280:	010c9b13          	slli	s6,s9,0x10
80016284:	165020ef          	jal	ra,80018be8 <__udivsi3>
80016288:	010b5b13          	srli	s6,s6,0x10
8001628c:	00050993          	mv	s3,a0
80016290:	00050593          	mv	a1,a0
80016294:	000b0513          	mv	a0,s6
80016298:	095020ef          	jal	ra,80018b2c <__mulsi3>
8001629c:	010a1a13          	slli	s4,s4,0x10
800162a0:	010ad713          	srli	a4,s5,0x10
800162a4:	00ea6733          	or	a4,s4,a4
800162a8:	00098c13          	mv	s8,s3
800162ac:	00a77e63          	bgeu	a4,a0,800162c8 <__udivdi3+0x4b8>
800162b0:	01970733          	add	a4,a4,s9
800162b4:	fff98c13          	addi	s8,s3,-1
800162b8:	01976863          	bltu	a4,s9,800162c8 <__udivdi3+0x4b8>
800162bc:	00a77663          	bgeu	a4,a0,800162c8 <__udivdi3+0x4b8>
800162c0:	ffe98c13          	addi	s8,s3,-2
800162c4:	01970733          	add	a4,a4,s9
800162c8:	40a709b3          	sub	s3,a4,a0
800162cc:	00040593          	mv	a1,s0
800162d0:	00098513          	mv	a0,s3
800162d4:	15d020ef          	jal	ra,80018c30 <__umodsi3>
800162d8:	00040593          	mv	a1,s0
800162dc:	00050a13          	mv	s4,a0
800162e0:	00098513          	mv	a0,s3
800162e4:	105020ef          	jal	ra,80018be8 <__udivsi3>
800162e8:	010a9413          	slli	s0,s5,0x10
800162ec:	00050993          	mv	s3,a0
800162f0:	00050593          	mv	a1,a0
800162f4:	010a1a13          	slli	s4,s4,0x10
800162f8:	000b0513          	mv	a0,s6
800162fc:	01045413          	srli	s0,s0,0x10
80016300:	02d020ef          	jal	ra,80018b2c <__mulsi3>
80016304:	008a6433          	or	s0,s4,s0
80016308:	00098613          	mv	a2,s3
8001630c:	00a47e63          	bgeu	s0,a0,80016328 <__udivdi3+0x518>
80016310:	01940433          	add	s0,s0,s9
80016314:	fff98613          	addi	a2,s3,-1
80016318:	01946863          	bltu	s0,s9,80016328 <__udivdi3+0x518>
8001631c:	00a47663          	bgeu	s0,a0,80016328 <__udivdi3+0x518>
80016320:	ffe98613          	addi	a2,s3,-2
80016324:	01940433          	add	s0,s0,s9
80016328:	010c1793          	slli	a5,s8,0x10
	      umul_ppmm (m1, m0, q0, d0);
8001632c:	00010e37          	lui	t3,0x10
	      udiv_qrnnd (q0, n1, n2, n1, d1);
80016330:	00c7e7b3          	or	a5,a5,a2
	      umul_ppmm (m1, m0, q0, d0);
80016334:	fffe0313          	addi	t1,t3,-1 # ffff <font6x8-0x7fff002d>
80016338:	0067f8b3          	and	a7,a5,t1
8001633c:	00697333          	and	t1,s2,t1
	      udiv_qrnnd (q0, n1, n2, n1, d1);
80016340:	40a40433          	sub	s0,s0,a0
	      umul_ppmm (m1, m0, q0, d0);
80016344:	0107de93          	srli	t4,a5,0x10
80016348:	01095913          	srli	s2,s2,0x10
8001634c:	00088513          	mv	a0,a7
80016350:	00030593          	mv	a1,t1
80016354:	7d8020ef          	jal	ra,80018b2c <__mulsi3>
80016358:	00050813          	mv	a6,a0
8001635c:	00090593          	mv	a1,s2
80016360:	00088513          	mv	a0,a7
80016364:	7c8020ef          	jal	ra,80018b2c <__mulsi3>
80016368:	00050893          	mv	a7,a0
8001636c:	00030593          	mv	a1,t1
80016370:	000e8513          	mv	a0,t4
80016374:	7b8020ef          	jal	ra,80018b2c <__mulsi3>
80016378:	00050313          	mv	t1,a0
8001637c:	00090593          	mv	a1,s2
80016380:	000e8513          	mv	a0,t4
80016384:	7a8020ef          	jal	ra,80018b2c <__mulsi3>
80016388:	01085713          	srli	a4,a6,0x10
8001638c:	006888b3          	add	a7,a7,t1
80016390:	01170733          	add	a4,a4,a7
80016394:	00050693          	mv	a3,a0
80016398:	00677463          	bgeu	a4,t1,800163a0 <__udivdi3+0x590>
8001639c:	01c506b3          	add	a3,a0,t3
800163a0:	01075513          	srli	a0,a4,0x10
800163a4:	00d506b3          	add	a3,a0,a3
	      if (m1 > n1 || (m1 == n1 && m0 > n0))
800163a8:	02d46663          	bltu	s0,a3,800163d4 <__udivdi3+0x5c4>
800163ac:	bcd412e3          	bne	s0,a3,80015f70 <__udivdi3+0x160>
	      umul_ppmm (m1, m0, q0, d0);
800163b0:	00010537          	lui	a0,0x10
800163b4:	fff50513          	addi	a0,a0,-1 # ffff <font6x8-0x7fff002d>
800163b8:	00a77733          	and	a4,a4,a0
800163bc:	01071713          	slli	a4,a4,0x10
800163c0:	00a87833          	and	a6,a6,a0
	      n0 = n0 << bm;
800163c4:	009b9bb3          	sll	s7,s7,s1
	      umul_ppmm (m1, m0, q0, d0);
800163c8:	01070733          	add	a4,a4,a6
	      q1 = 0;
800163cc:	00000493          	li	s1,0
	      if (m1 > n1 || (m1 == n1 && m0 > n0))
800163d0:	ccebfae3          	bgeu	s7,a4,800160a4 <__udivdi3+0x294>
		  q0--;
800163d4:	fff78793          	addi	a5,a5,-1 # ffffff <font6x8-0x7f00002d>
		  sub_ddmmss (m1, m0, m1, m0, d1, d0);
800163d8:	b99ff06f          	j	80015f70 <__udivdi3+0x160>
	  q1 = 0;
800163dc:	00000493          	li	s1,0
	  q0 = 0;
800163e0:	00000793          	li	a5,0
800163e4:	cc1ff06f          	j	800160a4 <__udivdi3+0x294>

800163e8 <__umoddi3>:
{
800163e8:	fd010113          	addi	sp,sp,-48
800163ec:	02812423          	sw	s0,40(sp)
800163f0:	02912223          	sw	s1,36(sp)
800163f4:	03212023          	sw	s2,32(sp)
800163f8:	01612823          	sw	s6,16(sp)
800163fc:	02112623          	sw	ra,44(sp)
80016400:	01312e23          	sw	s3,28(sp)
80016404:	01412c23          	sw	s4,24(sp)
80016408:	01512a23          	sw	s5,20(sp)
8001640c:	01712623          	sw	s7,12(sp)
80016410:	01812423          	sw	s8,8(sp)
80016414:	01912223          	sw	s9,4(sp)
80016418:	01a12023          	sw	s10,0(sp)
8001641c:	00050913          	mv	s2,a0
80016420:	00058b13          	mv	s6,a1
  n0 = nn.s.low;
80016424:	00050413          	mv	s0,a0
  n1 = nn.s.high;
80016428:	00058493          	mv	s1,a1
  if (d1 == 0)
8001642c:	26069c63          	bnez	a3,800166a4 <__umoddi3+0x2bc>
80016430:	80000ab7          	lui	s5,0x80000
80016434:	00060a13          	mv	s4,a2
80016438:	00068993          	mv	s3,a3
      if (d0 > n1)
8001643c:	6d0a8a93          	addi	s5,s5,1744 # 800006d0 <__clzsi2+0xfffe7a3c>
80016440:	14c5f263          	bgeu	a1,a2,80016584 <__umoddi3+0x19c>
	  count_leading_zeros (bm, d0);
80016444:	000107b7          	lui	a5,0x10
80016448:	12f67463          	bgeu	a2,a5,80016570 <__umoddi3+0x188>
8001644c:	0ff00793          	li	a5,255
80016450:	00c7f463          	bgeu	a5,a2,80016458 <__umoddi3+0x70>
80016454:	00800993          	li	s3,8
80016458:	013657b3          	srl	a5,a2,s3
8001645c:	00fa8ab3          	add	s5,s5,a5
80016460:	000ac783          	lbu	a5,0(s5)
80016464:	02000713          	li	a4,32
80016468:	013787b3          	add	a5,a5,s3
8001646c:	40f709b3          	sub	s3,a4,a5
	  if (bm != 0)
80016470:	00f70c63          	beq	a4,a5,80016488 <__umoddi3+0xa0>
	      n1 = (n1 << bm) | (n0 >> (W_TYPE_SIZE - bm));
80016474:	013b15b3          	sll	a1,s6,s3
80016478:	00f957b3          	srl	a5,s2,a5
	      d0 = d0 << bm;
8001647c:	01361a33          	sll	s4,a2,s3
	      n1 = (n1 << bm) | (n0 >> (W_TYPE_SIZE - bm));
80016480:	00b7e4b3          	or	s1,a5,a1
	      n0 = n0 << bm;
80016484:	01391433          	sll	s0,s2,s3
	  udiv_qrnnd (q0, n0, n1, n0, d0);
80016488:	010a5a93          	srli	s5,s4,0x10
8001648c:	000a8593          	mv	a1,s5
80016490:	00048513          	mv	a0,s1
80016494:	79c020ef          	jal	ra,80018c30 <__umodsi3>
80016498:	00050913          	mv	s2,a0
8001649c:	000a8593          	mv	a1,s5
800164a0:	010a1b13          	slli	s6,s4,0x10
800164a4:	00048513          	mv	a0,s1
800164a8:	740020ef          	jal	ra,80018be8 <__udivsi3>
800164ac:	010b5b13          	srli	s6,s6,0x10
800164b0:	00050593          	mv	a1,a0
800164b4:	000b0513          	mv	a0,s6
800164b8:	674020ef          	jal	ra,80018b2c <__mulsi3>
800164bc:	01091913          	slli	s2,s2,0x10
800164c0:	01045793          	srli	a5,s0,0x10
800164c4:	00f967b3          	or	a5,s2,a5
800164c8:	00a7fa63          	bgeu	a5,a0,800164dc <__umoddi3+0xf4>
800164cc:	014787b3          	add	a5,a5,s4
800164d0:	0147e663          	bltu	a5,s4,800164dc <__umoddi3+0xf4>
800164d4:	00a7f463          	bgeu	a5,a0,800164dc <__umoddi3+0xf4>
800164d8:	014787b3          	add	a5,a5,s4
800164dc:	40a784b3          	sub	s1,a5,a0
800164e0:	000a8593          	mv	a1,s5
800164e4:	00048513          	mv	a0,s1
800164e8:	748020ef          	jal	ra,80018c30 <__umodsi3>
800164ec:	00050913          	mv	s2,a0
800164f0:	000a8593          	mv	a1,s5
800164f4:	00048513          	mv	a0,s1
800164f8:	6f0020ef          	jal	ra,80018be8 <__udivsi3>
800164fc:	01041413          	slli	s0,s0,0x10
80016500:	00050593          	mv	a1,a0
80016504:	01091913          	slli	s2,s2,0x10
80016508:	000b0513          	mv	a0,s6
8001650c:	01045413          	srli	s0,s0,0x10
80016510:	61c020ef          	jal	ra,80018b2c <__mulsi3>
80016514:	00896433          	or	s0,s2,s0
80016518:	00a47a63          	bgeu	s0,a0,8001652c <__umoddi3+0x144>
8001651c:	01440433          	add	s0,s0,s4
80016520:	01446663          	bltu	s0,s4,8001652c <__umoddi3+0x144>
80016524:	00a47463          	bgeu	s0,a0,8001652c <__umoddi3+0x144>
80016528:	01440433          	add	s0,s0,s4
8001652c:	40a40433          	sub	s0,s0,a0
	  rr.s.low = n0 >> bm;
80016530:	01345533          	srl	a0,s0,s3
	  *rp = rr.ll;
80016534:	00000593          	li	a1,0
}
80016538:	02c12083          	lw	ra,44(sp)
8001653c:	02812403          	lw	s0,40(sp)
80016540:	02412483          	lw	s1,36(sp)
80016544:	02012903          	lw	s2,32(sp)
80016548:	01c12983          	lw	s3,28(sp)
8001654c:	01812a03          	lw	s4,24(sp)
80016550:	01412a83          	lw	s5,20(sp)
80016554:	01012b03          	lw	s6,16(sp)
80016558:	00c12b83          	lw	s7,12(sp)
8001655c:	00812c03          	lw	s8,8(sp)
80016560:	00412c83          	lw	s9,4(sp)
80016564:	00012d03          	lw	s10,0(sp)
80016568:	03010113          	addi	sp,sp,48
8001656c:	00008067          	ret
	  count_leading_zeros (bm, d0);
80016570:	010007b7          	lui	a5,0x1000
80016574:	01000993          	li	s3,16
80016578:	eef660e3          	bltu	a2,a5,80016458 <__umoddi3+0x70>
8001657c:	01800993          	li	s3,24
80016580:	ed9ff06f          	j	80016458 <__umoddi3+0x70>
	  if (d0 == 0)
80016584:	00061a63          	bnez	a2,80016598 <__umoddi3+0x1b0>
	    d0 = 1 / d0;	/* Divide intentionally by zero.  */
80016588:	00000593          	li	a1,0
8001658c:	00100513          	li	a0,1
80016590:	658020ef          	jal	ra,80018be8 <__udivsi3>
80016594:	00050a13          	mv	s4,a0
	  count_leading_zeros (bm, d0);
80016598:	000107b7          	lui	a5,0x10
8001659c:	0efa7a63          	bgeu	s4,a5,80016690 <__umoddi3+0x2a8>
800165a0:	0ff00793          	li	a5,255
800165a4:	0147f463          	bgeu	a5,s4,800165ac <__umoddi3+0x1c4>
800165a8:	00800993          	li	s3,8
800165ac:	013a57b3          	srl	a5,s4,s3
800165b0:	00fa8ab3          	add	s5,s5,a5
800165b4:	000ac783          	lbu	a5,0(s5)
800165b8:	02000713          	li	a4,32
	      n1 -= d0;
800165bc:	414b04b3          	sub	s1,s6,s4
	  count_leading_zeros (bm, d0);
800165c0:	013787b3          	add	a5,a5,s3
800165c4:	40f709b3          	sub	s3,a4,a5
	  if (bm == 0)
800165c8:	ecf700e3          	beq	a4,a5,80016488 <__umoddi3+0xa0>
	      d0 = d0 << bm;
800165cc:	013a1a33          	sll	s4,s4,s3
	      n2 = n1 >> b;
800165d0:	00fb5ab3          	srl	s5,s6,a5
	      n1 = (n1 << bm) | (n0 >> b);
800165d4:	013b15b3          	sll	a1,s6,s3
800165d8:	00f957b3          	srl	a5,s2,a5
	      udiv_qrnnd (q1, n1, n2, n1, d0);
800165dc:	010a5493          	srli	s1,s4,0x10
	      n1 = (n1 << bm) | (n0 >> b);
800165e0:	00b7eb33          	or	s6,a5,a1
	      udiv_qrnnd (q1, n1, n2, n1, d0);
800165e4:	000a8513          	mv	a0,s5
800165e8:	00048593          	mv	a1,s1
800165ec:	644020ef          	jal	ra,80018c30 <__umodsi3>
800165f0:	00048593          	mv	a1,s1
	      n0 = n0 << bm;
800165f4:	01391433          	sll	s0,s2,s3
	      udiv_qrnnd (q1, n1, n2, n1, d0);
800165f8:	010a1b93          	slli	s7,s4,0x10
800165fc:	00050913          	mv	s2,a0
80016600:	000a8513          	mv	a0,s5
80016604:	5e4020ef          	jal	ra,80018be8 <__udivsi3>
80016608:	010bdb93          	srli	s7,s7,0x10
8001660c:	00050593          	mv	a1,a0
80016610:	000b8513          	mv	a0,s7
80016614:	518020ef          	jal	ra,80018b2c <__mulsi3>
80016618:	01091913          	slli	s2,s2,0x10
8001661c:	010b5793          	srli	a5,s6,0x10
80016620:	00f967b3          	or	a5,s2,a5
80016624:	00a7fa63          	bgeu	a5,a0,80016638 <__umoddi3+0x250>
80016628:	014787b3          	add	a5,a5,s4
8001662c:	0147e663          	bltu	a5,s4,80016638 <__umoddi3+0x250>
80016630:	00a7f463          	bgeu	a5,a0,80016638 <__umoddi3+0x250>
80016634:	014787b3          	add	a5,a5,s4
80016638:	40a78ab3          	sub	s5,a5,a0
8001663c:	00048593          	mv	a1,s1
80016640:	000a8513          	mv	a0,s5
80016644:	5ec020ef          	jal	ra,80018c30 <__umodsi3>
80016648:	00050913          	mv	s2,a0
8001664c:	00048593          	mv	a1,s1
80016650:	000a8513          	mv	a0,s5
80016654:	594020ef          	jal	ra,80018be8 <__udivsi3>
80016658:	00050593          	mv	a1,a0
8001665c:	000b8513          	mv	a0,s7
80016660:	4cc020ef          	jal	ra,80018b2c <__mulsi3>
80016664:	010b1593          	slli	a1,s6,0x10
80016668:	01091913          	slli	s2,s2,0x10
8001666c:	0105d593          	srli	a1,a1,0x10
80016670:	00b965b3          	or	a1,s2,a1
80016674:	00a5fa63          	bgeu	a1,a0,80016688 <__umoddi3+0x2a0>
80016678:	014585b3          	add	a1,a1,s4
8001667c:	0145e663          	bltu	a1,s4,80016688 <__umoddi3+0x2a0>
80016680:	00a5f463          	bgeu	a1,a0,80016688 <__umoddi3+0x2a0>
80016684:	014585b3          	add	a1,a1,s4
80016688:	40a584b3          	sub	s1,a1,a0
8001668c:	dfdff06f          	j	80016488 <__umoddi3+0xa0>
	  count_leading_zeros (bm, d0);
80016690:	010007b7          	lui	a5,0x1000
80016694:	01000993          	li	s3,16
80016698:	f0fa6ae3          	bltu	s4,a5,800165ac <__umoddi3+0x1c4>
8001669c:	01800993          	li	s3,24
800166a0:	f0dff06f          	j	800165ac <__umoddi3+0x1c4>
      if (d1 > n1)
800166a4:	e8d5eae3          	bltu	a1,a3,80016538 <__umoddi3+0x150>
	  count_leading_zeros (bm, d1);
800166a8:	000107b7          	lui	a5,0x10
800166ac:	04f6fc63          	bgeu	a3,a5,80016704 <__umoddi3+0x31c>
800166b0:	0ff00a93          	li	s5,255
800166b4:	00dab533          	sltu	a0,s5,a3
800166b8:	00351513          	slli	a0,a0,0x3
800166bc:	800007b7          	lui	a5,0x80000
800166c0:	00a6d733          	srl	a4,a3,a0
800166c4:	6d078793          	addi	a5,a5,1744 # 800006d0 <__clzsi2+0xfffe7a3c>
800166c8:	00e787b3          	add	a5,a5,a4
800166cc:	0007ca83          	lbu	s5,0(a5)
800166d0:	02000793          	li	a5,32
800166d4:	00aa8ab3          	add	s5,s5,a0
800166d8:	41578a33          	sub	s4,a5,s5
	  if (bm == 0)
800166dc:	03579e63          	bne	a5,s5,80016718 <__umoddi3+0x330>
	      if (n1 > d1 || n0 >= d0)
800166e0:	0166e463          	bltu	a3,s6,800166e8 <__umoddi3+0x300>
800166e4:	00c96a63          	bltu	s2,a2,800166f8 <__umoddi3+0x310>
		  sub_ddmmss (n1, n0, n1, n0, d1, d0);
800166e8:	40c90433          	sub	s0,s2,a2
800166ec:	40db05b3          	sub	a1,s6,a3
800166f0:	008934b3          	sltu	s1,s2,s0
800166f4:	409584b3          	sub	s1,a1,s1
		  *rp = rr.ll;
800166f8:	00040513          	mv	a0,s0
800166fc:	00048593          	mv	a1,s1
80016700:	e39ff06f          	j	80016538 <__umoddi3+0x150>
	  count_leading_zeros (bm, d1);
80016704:	010007b7          	lui	a5,0x1000
80016708:	01000513          	li	a0,16
8001670c:	faf6e8e3          	bltu	a3,a5,800166bc <__umoddi3+0x2d4>
80016710:	01800513          	li	a0,24
80016714:	fa9ff06f          	j	800166bc <__umoddi3+0x2d4>
	      d1 = (d1 << bm) | (d0 >> b);
80016718:	014696b3          	sll	a3,a3,s4
8001671c:	015657b3          	srl	a5,a2,s5
80016720:	00d7ecb3          	or	s9,a5,a3
	      n2 = n1 >> b;
80016724:	015b5433          	srl	s0,s6,s5
	      n1 = (n1 << bm) | (n0 >> b);
80016728:	014b15b3          	sll	a1,s6,s4
	      udiv_qrnnd (q0, n1, n2, n1, d1);
8001672c:	010cdb93          	srli	s7,s9,0x10
	      n1 = (n1 << bm) | (n0 >> b);
80016730:	01595b33          	srl	s6,s2,s5
80016734:	00bb6b33          	or	s6,s6,a1
	      udiv_qrnnd (q0, n1, n2, n1, d1);
80016738:	00040513          	mv	a0,s0
8001673c:	000b8593          	mv	a1,s7
	      d0 = d0 << bm;
80016740:	01461d33          	sll	s10,a2,s4
	      udiv_qrnnd (q0, n1, n2, n1, d1);
80016744:	4ec020ef          	jal	ra,80018c30 <__umodsi3>
80016748:	00050993          	mv	s3,a0
8001674c:	000b8593          	mv	a1,s7
80016750:	00040513          	mv	a0,s0
80016754:	010c9c13          	slli	s8,s9,0x10
80016758:	490020ef          	jal	ra,80018be8 <__udivsi3>
8001675c:	010c5c13          	srli	s8,s8,0x10
80016760:	00050593          	mv	a1,a0
	      n0 = n0 << bm;
80016764:	014914b3          	sll	s1,s2,s4
	      udiv_qrnnd (q0, n1, n2, n1, d1);
80016768:	00050913          	mv	s2,a0
8001676c:	000c0513          	mv	a0,s8
80016770:	3bc020ef          	jal	ra,80018b2c <__mulsi3>
80016774:	01099993          	slli	s3,s3,0x10
80016778:	010b5713          	srli	a4,s6,0x10
8001677c:	00e9e733          	or	a4,s3,a4
80016780:	00090413          	mv	s0,s2
80016784:	00a77e63          	bgeu	a4,a0,800167a0 <__umoddi3+0x3b8>
80016788:	01970733          	add	a4,a4,s9
8001678c:	fff90413          	addi	s0,s2,-1
80016790:	01976863          	bltu	a4,s9,800167a0 <__umoddi3+0x3b8>
80016794:	00a77663          	bgeu	a4,a0,800167a0 <__umoddi3+0x3b8>
80016798:	ffe90413          	addi	s0,s2,-2
8001679c:	01970733          	add	a4,a4,s9
800167a0:	40a70933          	sub	s2,a4,a0
800167a4:	000b8593          	mv	a1,s7
800167a8:	00090513          	mv	a0,s2
800167ac:	484020ef          	jal	ra,80018c30 <__umodsi3>
800167b0:	00050993          	mv	s3,a0
800167b4:	000b8593          	mv	a1,s7
800167b8:	00090513          	mv	a0,s2
800167bc:	42c020ef          	jal	ra,80018be8 <__udivsi3>
800167c0:	00050913          	mv	s2,a0
800167c4:	00050593          	mv	a1,a0
800167c8:	000c0513          	mv	a0,s8
800167cc:	360020ef          	jal	ra,80018b2c <__mulsi3>
800167d0:	010b1713          	slli	a4,s6,0x10
800167d4:	01099993          	slli	s3,s3,0x10
800167d8:	01075713          	srli	a4,a4,0x10
800167dc:	00e9e733          	or	a4,s3,a4
800167e0:	00090793          	mv	a5,s2
800167e4:	00a77e63          	bgeu	a4,a0,80016800 <__umoddi3+0x418>
800167e8:	01970733          	add	a4,a4,s9
800167ec:	fff90793          	addi	a5,s2,-1
800167f0:	01976863          	bltu	a4,s9,80016800 <__umoddi3+0x418>
800167f4:	00a77663          	bgeu	a4,a0,80016800 <__umoddi3+0x418>
800167f8:	ffe90793          	addi	a5,s2,-2
800167fc:	01970733          	add	a4,a4,s9
	      umul_ppmm (m1, m0, q0, d0);
80016800:	00010eb7          	lui	t4,0x10
	      udiv_qrnnd (q0, n1, n2, n1, d1);
80016804:	01041413          	slli	s0,s0,0x10
80016808:	40a70833          	sub	a6,a4,a0
8001680c:	00f46433          	or	s0,s0,a5
	      umul_ppmm (m1, m0, q0, d0);
80016810:	fffe8713          	addi	a4,t4,-1 # ffff <font6x8-0x7fff002d>
80016814:	00e478b3          	and	a7,s0,a4
80016818:	00ed7733          	and	a4,s10,a4
8001681c:	01045413          	srli	s0,s0,0x10
80016820:	010d5e13          	srli	t3,s10,0x10
80016824:	00088513          	mv	a0,a7
80016828:	00070593          	mv	a1,a4
8001682c:	300020ef          	jal	ra,80018b2c <__mulsi3>
80016830:	00050793          	mv	a5,a0
80016834:	000e0593          	mv	a1,t3
80016838:	00088513          	mv	a0,a7
8001683c:	2f0020ef          	jal	ra,80018b2c <__mulsi3>
80016840:	00050893          	mv	a7,a0
80016844:	00070593          	mv	a1,a4
80016848:	00040513          	mv	a0,s0
8001684c:	2e0020ef          	jal	ra,80018b2c <__mulsi3>
80016850:	00050313          	mv	t1,a0
80016854:	000e0593          	mv	a1,t3
80016858:	00040513          	mv	a0,s0
8001685c:	2d0020ef          	jal	ra,80018b2c <__mulsi3>
80016860:	0107d713          	srli	a4,a5,0x10
80016864:	006888b3          	add	a7,a7,t1
80016868:	01170733          	add	a4,a4,a7
8001686c:	00677463          	bgeu	a4,t1,80016874 <__umoddi3+0x48c>
80016870:	01d50533          	add	a0,a0,t4
80016874:	01075593          	srli	a1,a4,0x10
80016878:	00a585b3          	add	a1,a1,a0
8001687c:	00010537          	lui	a0,0x10
80016880:	fff50513          	addi	a0,a0,-1 # ffff <font6x8-0x7fff002d>
80016884:	00a77733          	and	a4,a4,a0
80016888:	01071713          	slli	a4,a4,0x10
8001688c:	00a7f7b3          	and	a5,a5,a0
80016890:	00f707b3          	add	a5,a4,a5
	      if (m1 > n1 || (m1 == n1 && m0 > n0))
80016894:	00b86663          	bltu	a6,a1,800168a0 <__umoddi3+0x4b8>
80016898:	00b81e63          	bne	a6,a1,800168b4 <__umoddi3+0x4cc>
8001689c:	00f4fc63          	bgeu	s1,a5,800168b4 <__umoddi3+0x4cc>
		  sub_ddmmss (m1, m0, m1, m0, d1, d0);
800168a0:	41a78633          	sub	a2,a5,s10
800168a4:	00c7b7b3          	sltu	a5,a5,a2
800168a8:	019787b3          	add	a5,a5,s9
800168ac:	40f585b3          	sub	a1,a1,a5
800168b0:	00060793          	mv	a5,a2
		  sub_ddmmss (n1, n0, n1, n0, m1, m0);
800168b4:	40f487b3          	sub	a5,s1,a5
800168b8:	00f4b4b3          	sltu	s1,s1,a5
800168bc:	40b805b3          	sub	a1,a6,a1
800168c0:	409585b3          	sub	a1,a1,s1
		  rr.s.low = (n1 << b) | (n0 >> bm);
800168c4:	01559433          	sll	s0,a1,s5
800168c8:	0147d7b3          	srl	a5,a5,s4
		  *rp = rr.ll;
800168cc:	00f46533          	or	a0,s0,a5
800168d0:	0145d5b3          	srl	a1,a1,s4
800168d4:	c65ff06f          	j	80016538 <__umoddi3+0x150>

800168d8 <__adddf3>:
  FP_DECL_D (B);
  FP_DECL_D (R);
  DFtype r;

  FP_INIT_ROUNDMODE;
  FP_UNPACK_SEMIRAW_D (A, a);
800168d8:	00100837          	lui	a6,0x100
800168dc:	fff80813          	addi	a6,a6,-1 # fffff <font6x8-0x7ff0002d>
{
800168e0:	fe010113          	addi	sp,sp,-32
  FP_UNPACK_SEMIRAW_D (A, a);
800168e4:	00b877b3          	and	a5,a6,a1
800168e8:	0145d713          	srli	a4,a1,0x14
  FP_UNPACK_SEMIRAW_D (B, b);
800168ec:	00d87833          	and	a6,a6,a3
800168f0:	0146d893          	srli	a7,a3,0x14
{
800168f4:	00812c23          	sw	s0,24(sp)
  FP_UNPACK_SEMIRAW_D (A, a);
800168f8:	00379793          	slli	a5,a5,0x3
800168fc:	7ff77413          	andi	s0,a4,2047
80016900:	01d55713          	srli	a4,a0,0x1d
{
80016904:	00912a23          	sw	s1,20(sp)
  FP_UNPACK_SEMIRAW_D (A, a);
80016908:	00f767b3          	or	a5,a4,a5
8001690c:	01f5d493          	srli	s1,a1,0x1f
  FP_UNPACK_SEMIRAW_D (B, b);
80016910:	01d65713          	srli	a4,a2,0x1d
80016914:	7ff8f893          	andi	a7,a7,2047
80016918:	01f6d593          	srli	a1,a3,0x1f
8001691c:	00381813          	slli	a6,a6,0x3
{
80016920:	00112e23          	sw	ra,28(sp)
80016924:	01212823          	sw	s2,16(sp)
80016928:	01312623          	sw	s3,12(sp)
  FP_UNPACK_SEMIRAW_D (A, a);
8001692c:	00351513          	slli	a0,a0,0x3
  FP_UNPACK_SEMIRAW_D (B, b);
80016930:	01076733          	or	a4,a4,a6
80016934:	00361613          	slli	a2,a2,0x3
  FP_ADD_D (R, A, B);
80016938:	411406b3          	sub	a3,s0,a7
8001693c:	2cb49463          	bne	s1,a1,80016c04 <__adddf3+0x32c>
80016940:	10d05e63          	blez	a3,80016a5c <__adddf3+0x184>
80016944:	04089063          	bnez	a7,80016984 <__adddf3+0xac>
80016948:	00c765b3          	or	a1,a4,a2
8001694c:	2a058863          	beqz	a1,80016bfc <__adddf3+0x324>
80016950:	fff68593          	addi	a1,a3,-1
80016954:	02059063          	bnez	a1,80016974 <__adddf3+0x9c>
80016958:	00c50633          	add	a2,a0,a2
8001695c:	00a63533          	sltu	a0,a2,a0
80016960:	00e787b3          	add	a5,a5,a4
80016964:	00a787b3          	add	a5,a5,a0
80016968:	00060513          	mv	a0,a2
  FP_UNPACK_SEMIRAW_D (B, b);
8001696c:	00100413          	li	s0,1
  FP_ADD_D (R, A, B);
80016970:	0700006f          	j	800169e0 <__adddf3+0x108>
80016974:	7ff00813          	li	a6,2047
80016978:	03069063          	bne	a3,a6,80016998 <__adddf3+0xc0>
8001697c:	7ff00413          	li	s0,2047
80016980:	20c0006f          	j	80016b8c <__adddf3+0x2b4>
80016984:	7ff00593          	li	a1,2047
80016988:	20b40263          	beq	s0,a1,80016b8c <__adddf3+0x2b4>
8001698c:	008005b7          	lui	a1,0x800
80016990:	00b76733          	or	a4,a4,a1
80016994:	00068593          	mv	a1,a3
80016998:	03800693          	li	a3,56
8001699c:	0ab6ca63          	blt	a3,a1,80016a50 <__adddf3+0x178>
800169a0:	01f00693          	li	a3,31
800169a4:	06b6cc63          	blt	a3,a1,80016a1c <__adddf3+0x144>
800169a8:	02000813          	li	a6,32
800169ac:	40b80833          	sub	a6,a6,a1
800169b0:	010716b3          	sll	a3,a4,a6
800169b4:	00b658b3          	srl	a7,a2,a1
800169b8:	01061633          	sll	a2,a2,a6
800169bc:	0116e6b3          	or	a3,a3,a7
800169c0:	00c03633          	snez	a2,a2
800169c4:	00c6e633          	or	a2,a3,a2
800169c8:	00b75733          	srl	a4,a4,a1
800169cc:	00a60633          	add	a2,a2,a0
800169d0:	00f70733          	add	a4,a4,a5
800169d4:	00a637b3          	sltu	a5,a2,a0
800169d8:	00f707b3          	add	a5,a4,a5
800169dc:	00060513          	mv	a0,a2
800169e0:	00879713          	slli	a4,a5,0x8
800169e4:	1a075463          	bgez	a4,80016b8c <__adddf3+0x2b4>
800169e8:	00140413          	addi	s0,s0,1
800169ec:	7ff00713          	li	a4,2047
800169f0:	5ce40463          	beq	s0,a4,80016fb8 <__adddf3+0x6e0>
800169f4:	ff8006b7          	lui	a3,0xff800
800169f8:	fff68693          	addi	a3,a3,-1 # ff7fffff <__clzsi2+0x7f7e736b>
800169fc:	00d7f6b3          	and	a3,a5,a3
80016a00:	00155593          	srli	a1,a0,0x1
80016a04:	00157513          	andi	a0,a0,1
80016a08:	01f69793          	slli	a5,a3,0x1f
80016a0c:	00a5e533          	or	a0,a1,a0
80016a10:	00a7e533          	or	a0,a5,a0
80016a14:	0016d793          	srli	a5,a3,0x1
80016a18:	1740006f          	j	80016b8c <__adddf3+0x2b4>
80016a1c:	fe058693          	addi	a3,a1,-32 # 7fffe0 <font6x8-0x7f80004c>
80016a20:	02000893          	li	a7,32
80016a24:	00d756b3          	srl	a3,a4,a3
80016a28:	00000813          	li	a6,0
80016a2c:	01158863          	beq	a1,a7,80016a3c <__adddf3+0x164>
80016a30:	04000813          	li	a6,64
80016a34:	40b805b3          	sub	a1,a6,a1
80016a38:	00b71833          	sll	a6,a4,a1
80016a3c:	00c86633          	or	a2,a6,a2
80016a40:	00c03633          	snez	a2,a2
80016a44:	00c6e633          	or	a2,a3,a2
80016a48:	00000713          	li	a4,0
80016a4c:	f81ff06f          	j	800169cc <__adddf3+0xf4>
80016a50:	00c76633          	or	a2,a4,a2
80016a54:	00c03633          	snez	a2,a2
80016a58:	ff1ff06f          	j	80016a48 <__adddf3+0x170>
80016a5c:	0e068263          	beqz	a3,80016b40 <__adddf3+0x268>
80016a60:	408885b3          	sub	a1,a7,s0
80016a64:	02041e63          	bnez	s0,80016aa0 <__adddf3+0x1c8>
80016a68:	00a7e6b3          	or	a3,a5,a0
80016a6c:	52068663          	beqz	a3,80016f98 <__adddf3+0x6c0>
80016a70:	fff58693          	addi	a3,a1,-1
80016a74:	00069c63          	bnez	a3,80016a8c <__adddf3+0x1b4>
80016a78:	00c50533          	add	a0,a0,a2
80016a7c:	00e787b3          	add	a5,a5,a4
80016a80:	00c53633          	sltu	a2,a0,a2
80016a84:	00c787b3          	add	a5,a5,a2
80016a88:	ee5ff06f          	j	8001696c <__adddf3+0x94>
80016a8c:	7ff00813          	li	a6,2047
80016a90:	03059263          	bne	a1,a6,80016ab4 <__adddf3+0x1dc>
  FP_UNPACK_SEMIRAW_D (B, b);
80016a94:	00070793          	mv	a5,a4
80016a98:	00060513          	mv	a0,a2
80016a9c:	ee1ff06f          	j	8001697c <__adddf3+0xa4>
  FP_ADD_D (R, A, B);
80016aa0:	7ff00693          	li	a3,2047
80016aa4:	fed888e3          	beq	a7,a3,80016a94 <__adddf3+0x1bc>
80016aa8:	008006b7          	lui	a3,0x800
80016aac:	00d7e7b3          	or	a5,a5,a3
80016ab0:	00058693          	mv	a3,a1
80016ab4:	03800593          	li	a1,56
80016ab8:	06d5ce63          	blt	a1,a3,80016b34 <__adddf3+0x25c>
80016abc:	01f00593          	li	a1,31
80016ac0:	04d5c063          	blt	a1,a3,80016b00 <__adddf3+0x228>
80016ac4:	02000813          	li	a6,32
80016ac8:	40d80833          	sub	a6,a6,a3
80016acc:	00d55333          	srl	t1,a0,a3
80016ad0:	010795b3          	sll	a1,a5,a6
80016ad4:	01051533          	sll	a0,a0,a6
80016ad8:	0065e5b3          	or	a1,a1,t1
80016adc:	00a03533          	snez	a0,a0
80016ae0:	00a5e533          	or	a0,a1,a0
80016ae4:	00d7d6b3          	srl	a3,a5,a3
80016ae8:	00c50533          	add	a0,a0,a2
80016aec:	00e687b3          	add	a5,a3,a4
80016af0:	00c53633          	sltu	a2,a0,a2
80016af4:	00c787b3          	add	a5,a5,a2
  FP_UNPACK_SEMIRAW_D (B, b);
80016af8:	00088413          	mv	s0,a7
80016afc:	ee5ff06f          	j	800169e0 <__adddf3+0x108>
  FP_ADD_D (R, A, B);
80016b00:	fe068593          	addi	a1,a3,-32 # 7fffe0 <font6x8-0x7f80004c>
80016b04:	02000313          	li	t1,32
80016b08:	00b7d5b3          	srl	a1,a5,a1
80016b0c:	00000813          	li	a6,0
80016b10:	00668863          	beq	a3,t1,80016b20 <__adddf3+0x248>
80016b14:	04000813          	li	a6,64
80016b18:	40d806b3          	sub	a3,a6,a3
80016b1c:	00d79833          	sll	a6,a5,a3
80016b20:	00a86533          	or	a0,a6,a0
80016b24:	00a03533          	snez	a0,a0
80016b28:	00a5e533          	or	a0,a1,a0
80016b2c:	00000693          	li	a3,0
80016b30:	fb9ff06f          	j	80016ae8 <__adddf3+0x210>
80016b34:	00a7e533          	or	a0,a5,a0
80016b38:	00a03533          	snez	a0,a0
80016b3c:	ff1ff06f          	j	80016b2c <__adddf3+0x254>
80016b40:	00140693          	addi	a3,s0,1
80016b44:	7fe6f593          	andi	a1,a3,2046
80016b48:	08059663          	bnez	a1,80016bd4 <__adddf3+0x2fc>
80016b4c:	00a7e6b3          	or	a3,a5,a0
80016b50:	06041263          	bnez	s0,80016bb4 <__adddf3+0x2dc>
80016b54:	44068a63          	beqz	a3,80016fa8 <__adddf3+0x6d0>
80016b58:	00c766b3          	or	a3,a4,a2
80016b5c:	02068863          	beqz	a3,80016b8c <__adddf3+0x2b4>
80016b60:	00c50633          	add	a2,a0,a2
80016b64:	00e787b3          	add	a5,a5,a4
80016b68:	00a63533          	sltu	a0,a2,a0
80016b6c:	00a787b3          	add	a5,a5,a0
80016b70:	00879713          	slli	a4,a5,0x8
80016b74:	00060513          	mv	a0,a2
80016b78:	00075a63          	bgez	a4,80016b8c <__adddf3+0x2b4>
80016b7c:	ff800737          	lui	a4,0xff800
80016b80:	fff70713          	addi	a4,a4,-1 # ff7fffff <__clzsi2+0x7f7e736b>
80016b84:	00e7f7b3          	and	a5,a5,a4
80016b88:	00100413          	li	s0,1
  FP_PACK_SEMIRAW_D (r, R);
80016b8c:	00757713          	andi	a4,a0,7
80016b90:	42070863          	beqz	a4,80016fc0 <__adddf3+0x6e8>
80016b94:	00f57713          	andi	a4,a0,15
80016b98:	00400693          	li	a3,4
80016b9c:	42d70263          	beq	a4,a3,80016fc0 <__adddf3+0x6e8>
80016ba0:	00450693          	addi	a3,a0,4
80016ba4:	00a6b533          	sltu	a0,a3,a0
80016ba8:	00a787b3          	add	a5,a5,a0
80016bac:	00068513          	mv	a0,a3
80016bb0:	4100006f          	j	80016fc0 <__adddf3+0x6e8>
  FP_ADD_D (R, A, B);
80016bb4:	ee0680e3          	beqz	a3,80016a94 <__adddf3+0x1bc>
80016bb8:	00c76633          	or	a2,a4,a2
80016bbc:	dc0600e3          	beqz	a2,8001697c <__adddf3+0xa4>
80016bc0:	00000493          	li	s1,0
80016bc4:	004007b7          	lui	a5,0x400
80016bc8:	00000513          	li	a0,0
80016bcc:	7ff00413          	li	s0,2047
80016bd0:	3f00006f          	j	80016fc0 <__adddf3+0x6e8>
80016bd4:	7ff00593          	li	a1,2047
80016bd8:	3cb68e63          	beq	a3,a1,80016fb4 <__adddf3+0x6dc>
80016bdc:	00c50633          	add	a2,a0,a2
80016be0:	00e78733          	add	a4,a5,a4
80016be4:	00a637b3          	sltu	a5,a2,a0
80016be8:	00f70733          	add	a4,a4,a5
80016bec:	01f71513          	slli	a0,a4,0x1f
80016bf0:	00165613          	srli	a2,a2,0x1
80016bf4:	00c56533          	or	a0,a0,a2
80016bf8:	00175793          	srli	a5,a4,0x1
80016bfc:	00068413          	mv	s0,a3
80016c00:	f8dff06f          	j	80016b8c <__adddf3+0x2b4>
80016c04:	0ed05c63          	blez	a3,80016cfc <__adddf3+0x424>
80016c08:	08089e63          	bnez	a7,80016ca4 <__adddf3+0x3cc>
80016c0c:	00c765b3          	or	a1,a4,a2
80016c10:	fe0586e3          	beqz	a1,80016bfc <__adddf3+0x324>
80016c14:	fff68593          	addi	a1,a3,-1
80016c18:	02059063          	bnez	a1,80016c38 <__adddf3+0x360>
80016c1c:	40c50633          	sub	a2,a0,a2
80016c20:	00c53533          	sltu	a0,a0,a2
80016c24:	40e787b3          	sub	a5,a5,a4
80016c28:	40a787b3          	sub	a5,a5,a0
80016c2c:	00060513          	mv	a0,a2
  FP_UNPACK_SEMIRAW_D (B, b);
80016c30:	00100413          	li	s0,1
  FP_ADD_D (R, A, B);
80016c34:	0540006f          	j	80016c88 <__adddf3+0x3b0>
80016c38:	7ff00813          	li	a6,2047
80016c3c:	d50680e3          	beq	a3,a6,8001697c <__adddf3+0xa4>
80016c40:	03800693          	li	a3,56
80016c44:	0ab6c663          	blt	a3,a1,80016cf0 <__adddf3+0x418>
80016c48:	01f00693          	li	a3,31
80016c4c:	06b6c863          	blt	a3,a1,80016cbc <__adddf3+0x3e4>
80016c50:	02000813          	li	a6,32
80016c54:	40b80833          	sub	a6,a6,a1
80016c58:	010716b3          	sll	a3,a4,a6
80016c5c:	00b658b3          	srl	a7,a2,a1
80016c60:	01061633          	sll	a2,a2,a6
80016c64:	0116e6b3          	or	a3,a3,a7
80016c68:	00c03633          	snez	a2,a2
80016c6c:	00c6e633          	or	a2,a3,a2
80016c70:	00b75733          	srl	a4,a4,a1
80016c74:	40c50633          	sub	a2,a0,a2
80016c78:	40e78733          	sub	a4,a5,a4
80016c7c:	00c537b3          	sltu	a5,a0,a2
80016c80:	40f707b3          	sub	a5,a4,a5
80016c84:	00060513          	mv	a0,a2
80016c88:	00879713          	slli	a4,a5,0x8
80016c8c:	f00750e3          	bgez	a4,80016b8c <__adddf3+0x2b4>
80016c90:	00800937          	lui	s2,0x800
80016c94:	fff90913          	addi	s2,s2,-1 # 7fffff <font6x8-0x7f80002d>
80016c98:	0127f933          	and	s2,a5,s2
80016c9c:	00050993          	mv	s3,a0
80016ca0:	20c0006f          	j	80016eac <__adddf3+0x5d4>
80016ca4:	7ff00593          	li	a1,2047
80016ca8:	eeb402e3          	beq	s0,a1,80016b8c <__adddf3+0x2b4>
80016cac:	008005b7          	lui	a1,0x800
80016cb0:	00b76733          	or	a4,a4,a1
80016cb4:	00068593          	mv	a1,a3
80016cb8:	f89ff06f          	j	80016c40 <__adddf3+0x368>
80016cbc:	fe058693          	addi	a3,a1,-32 # 7fffe0 <font6x8-0x7f80004c>
80016cc0:	02000893          	li	a7,32
80016cc4:	00d756b3          	srl	a3,a4,a3
80016cc8:	00000813          	li	a6,0
80016ccc:	01158863          	beq	a1,a7,80016cdc <__adddf3+0x404>
80016cd0:	04000813          	li	a6,64
80016cd4:	40b805b3          	sub	a1,a6,a1
80016cd8:	00b71833          	sll	a6,a4,a1
80016cdc:	00c86633          	or	a2,a6,a2
80016ce0:	00c03633          	snez	a2,a2
80016ce4:	00c6e633          	or	a2,a3,a2
80016ce8:	00000713          	li	a4,0
80016cec:	f89ff06f          	j	80016c74 <__adddf3+0x39c>
80016cf0:	00c76633          	or	a2,a4,a2
80016cf4:	00c03633          	snez	a2,a2
80016cf8:	ff1ff06f          	j	80016ce8 <__adddf3+0x410>
80016cfc:	0e068863          	beqz	a3,80016dec <__adddf3+0x514>
80016d00:	40888833          	sub	a6,a7,s0
80016d04:	04041263          	bnez	s0,80016d48 <__adddf3+0x470>
80016d08:	00a7e6b3          	or	a3,a5,a0
80016d0c:	34068263          	beqz	a3,80017050 <__adddf3+0x778>
80016d10:	fff80693          	addi	a3,a6,-1
80016d14:	00069e63          	bnez	a3,80016d30 <__adddf3+0x458>
80016d18:	40a60533          	sub	a0,a2,a0
80016d1c:	40f707b3          	sub	a5,a4,a5
80016d20:	00a63633          	sltu	a2,a2,a0
80016d24:	40c787b3          	sub	a5,a5,a2
80016d28:	00058493          	mv	s1,a1
80016d2c:	f05ff06f          	j	80016c30 <__adddf3+0x358>
80016d30:	7ff00313          	li	t1,2047
80016d34:	02681463          	bne	a6,t1,80016d5c <__adddf3+0x484>
  FP_UNPACK_SEMIRAW_D (B, b);
80016d38:	00070793          	mv	a5,a4
80016d3c:	00060513          	mv	a0,a2
  FP_ADD_D (R, A, B);
80016d40:	7ff00413          	li	s0,2047
80016d44:	0d00006f          	j	80016e14 <__adddf3+0x53c>
80016d48:	7ff00693          	li	a3,2047
80016d4c:	fed886e3          	beq	a7,a3,80016d38 <__adddf3+0x460>
80016d50:	008006b7          	lui	a3,0x800
80016d54:	00d7e7b3          	or	a5,a5,a3
80016d58:	00080693          	mv	a3,a6
80016d5c:	03800813          	li	a6,56
80016d60:	08d84063          	blt	a6,a3,80016de0 <__adddf3+0x508>
80016d64:	01f00813          	li	a6,31
80016d68:	04d84263          	blt	a6,a3,80016dac <__adddf3+0x4d4>
80016d6c:	02000313          	li	t1,32
80016d70:	40d30333          	sub	t1,t1,a3
80016d74:	00d55e33          	srl	t3,a0,a3
80016d78:	00679833          	sll	a6,a5,t1
80016d7c:	00651533          	sll	a0,a0,t1
80016d80:	01c86833          	or	a6,a6,t3
80016d84:	00a03533          	snez	a0,a0
80016d88:	00a86533          	or	a0,a6,a0
80016d8c:	00d7d6b3          	srl	a3,a5,a3
80016d90:	40a60533          	sub	a0,a2,a0
80016d94:	40d707b3          	sub	a5,a4,a3
80016d98:	00a63633          	sltu	a2,a2,a0
80016d9c:	40c787b3          	sub	a5,a5,a2
  FP_UNPACK_SEMIRAW_D (B, b);
80016da0:	00088413          	mv	s0,a7
80016da4:	00058493          	mv	s1,a1
80016da8:	ee1ff06f          	j	80016c88 <__adddf3+0x3b0>
  FP_ADD_D (R, A, B);
80016dac:	fe068813          	addi	a6,a3,-32 # 7fffe0 <font6x8-0x7f80004c>
80016db0:	02000e13          	li	t3,32
80016db4:	0107d833          	srl	a6,a5,a6
80016db8:	00000313          	li	t1,0
80016dbc:	01c68863          	beq	a3,t3,80016dcc <__adddf3+0x4f4>
80016dc0:	04000313          	li	t1,64
80016dc4:	40d306b3          	sub	a3,t1,a3
80016dc8:	00d79333          	sll	t1,a5,a3
80016dcc:	00a36533          	or	a0,t1,a0
80016dd0:	00a03533          	snez	a0,a0
80016dd4:	00a86533          	or	a0,a6,a0
80016dd8:	00000693          	li	a3,0
80016ddc:	fb5ff06f          	j	80016d90 <__adddf3+0x4b8>
80016de0:	00a7e533          	or	a0,a5,a0
80016de4:	00a03533          	snez	a0,a0
80016de8:	ff1ff06f          	j	80016dd8 <__adddf3+0x500>
80016dec:	00140693          	addi	a3,s0,1
80016df0:	7fe6f693          	andi	a3,a3,2046
80016df4:	08069663          	bnez	a3,80016e80 <__adddf3+0x5a8>
80016df8:	00a7e833          	or	a6,a5,a0
80016dfc:	00c766b3          	or	a3,a4,a2
80016e00:	06041063          	bnez	s0,80016e60 <__adddf3+0x588>
80016e04:	00081c63          	bnez	a6,80016e1c <__adddf3+0x544>
80016e08:	24068c63          	beqz	a3,80017060 <__adddf3+0x788>
  FP_UNPACK_SEMIRAW_D (B, b);
80016e0c:	00070793          	mv	a5,a4
80016e10:	00060513          	mv	a0,a2
80016e14:	00058493          	mv	s1,a1
80016e18:	d75ff06f          	j	80016b8c <__adddf3+0x2b4>
  FP_ADD_D (R, A, B);
80016e1c:	d60688e3          	beqz	a3,80016b8c <__adddf3+0x2b4>
80016e20:	40c50833          	sub	a6,a0,a2
80016e24:	010538b3          	sltu	a7,a0,a6
80016e28:	40e786b3          	sub	a3,a5,a4
80016e2c:	411686b3          	sub	a3,a3,a7
80016e30:	00869893          	slli	a7,a3,0x8
80016e34:	0008dc63          	bgez	a7,80016e4c <__adddf3+0x574>
80016e38:	40a60533          	sub	a0,a2,a0
80016e3c:	40f707b3          	sub	a5,a4,a5
80016e40:	00a63633          	sltu	a2,a2,a0
80016e44:	40c787b3          	sub	a5,a5,a2
80016e48:	fcdff06f          	j	80016e14 <__adddf3+0x53c>
80016e4c:	00d86533          	or	a0,a6,a3
80016e50:	22050063          	beqz	a0,80017070 <__adddf3+0x798>
80016e54:	00068793          	mv	a5,a3
80016e58:	00080513          	mv	a0,a6
80016e5c:	d31ff06f          	j	80016b8c <__adddf3+0x2b4>
80016e60:	00081c63          	bnez	a6,80016e78 <__adddf3+0x5a0>
80016e64:	20068a63          	beqz	a3,80017078 <__adddf3+0x7a0>
  FP_UNPACK_SEMIRAW_D (B, b);
80016e68:	00070793          	mv	a5,a4
80016e6c:	00060513          	mv	a0,a2
  FP_ADD_D (R, A, B);
80016e70:	00058493          	mv	s1,a1
80016e74:	b09ff06f          	j	8001697c <__adddf3+0xa4>
80016e78:	b00682e3          	beqz	a3,8001697c <__adddf3+0xa4>
80016e7c:	d45ff06f          	j	80016bc0 <__adddf3+0x2e8>
80016e80:	40c509b3          	sub	s3,a0,a2
80016e84:	013536b3          	sltu	a3,a0,s3
80016e88:	40e78933          	sub	s2,a5,a4
80016e8c:	40d90933          	sub	s2,s2,a3
80016e90:	00891693          	slli	a3,s2,0x8
80016e94:	0806d463          	bgez	a3,80016f1c <__adddf3+0x644>
80016e98:	40a609b3          	sub	s3,a2,a0
80016e9c:	40f707b3          	sub	a5,a4,a5
80016ea0:	01363633          	sltu	a2,a2,s3
80016ea4:	40c78933          	sub	s2,a5,a2
80016ea8:	00058493          	mv	s1,a1
80016eac:	08090263          	beqz	s2,80016f30 <__adddf3+0x658>
80016eb0:	00090513          	mv	a0,s2
80016eb4:	5e1010ef          	jal	ra,80018c94 <__clzsi2>
80016eb8:	ff850713          	addi	a4,a0,-8
80016ebc:	01f00793          	li	a5,31
80016ec0:	08e7c063          	blt	a5,a4,80016f40 <__adddf3+0x668>
80016ec4:	02000693          	li	a3,32
80016ec8:	40e686b3          	sub	a3,a3,a4
80016ecc:	00e917b3          	sll	a5,s2,a4
80016ed0:	00d9d6b3          	srl	a3,s3,a3
80016ed4:	00f6e6b3          	or	a3,a3,a5
80016ed8:	00e99533          	sll	a0,s3,a4
80016edc:	0a874463          	blt	a4,s0,80016f84 <__adddf3+0x6ac>
80016ee0:	40870733          	sub	a4,a4,s0
80016ee4:	00170793          	addi	a5,a4,1
80016ee8:	01f00613          	li	a2,31
80016eec:	06f64263          	blt	a2,a5,80016f50 <__adddf3+0x678>
80016ef0:	02000713          	li	a4,32
80016ef4:	40f70733          	sub	a4,a4,a5
80016ef8:	00f555b3          	srl	a1,a0,a5
80016efc:	00e69633          	sll	a2,a3,a4
80016f00:	00e51533          	sll	a0,a0,a4
80016f04:	00b66633          	or	a2,a2,a1
80016f08:	00a03533          	snez	a0,a0
80016f0c:	00a66533          	or	a0,a2,a0
80016f10:	00f6d7b3          	srl	a5,a3,a5
80016f14:	00000413          	li	s0,0
80016f18:	c75ff06f          	j	80016b8c <__adddf3+0x2b4>
80016f1c:	0129e533          	or	a0,s3,s2
80016f20:	f80516e3          	bnez	a0,80016eac <__adddf3+0x5d4>
80016f24:	00000793          	li	a5,0
80016f28:	00000413          	li	s0,0
80016f2c:	13c0006f          	j	80017068 <__adddf3+0x790>
80016f30:	00098513          	mv	a0,s3
80016f34:	561010ef          	jal	ra,80018c94 <__clzsi2>
80016f38:	02050513          	addi	a0,a0,32
80016f3c:	f7dff06f          	j	80016eb8 <__adddf3+0x5e0>
80016f40:	fd850513          	addi	a0,a0,-40
80016f44:	00a996b3          	sll	a3,s3,a0
80016f48:	00000513          	li	a0,0
80016f4c:	f91ff06f          	j	80016edc <__adddf3+0x604>
80016f50:	fe170713          	addi	a4,a4,-31
80016f54:	02000593          	li	a1,32
80016f58:	00e6d733          	srl	a4,a3,a4
80016f5c:	00000613          	li	a2,0
80016f60:	00b78863          	beq	a5,a1,80016f70 <__adddf3+0x698>
80016f64:	04000613          	li	a2,64
80016f68:	40f60633          	sub	a2,a2,a5
80016f6c:	00c69633          	sll	a2,a3,a2
80016f70:	00c56533          	or	a0,a0,a2
80016f74:	00a03533          	snez	a0,a0
80016f78:	00a76533          	or	a0,a4,a0
80016f7c:	00000793          	li	a5,0
80016f80:	f95ff06f          	j	80016f14 <__adddf3+0x63c>
80016f84:	ff8007b7          	lui	a5,0xff800
80016f88:	fff78793          	addi	a5,a5,-1 # ff7fffff <__clzsi2+0x7f7e736b>
80016f8c:	40e40433          	sub	s0,s0,a4
80016f90:	00f6f7b3          	and	a5,a3,a5
80016f94:	bf9ff06f          	j	80016b8c <__adddf3+0x2b4>
  FP_UNPACK_SEMIRAW_D (B, b);
80016f98:	00070793          	mv	a5,a4
80016f9c:	00060513          	mv	a0,a2
  FP_ADD_D (R, A, B);
80016fa0:	00058413          	mv	s0,a1
80016fa4:	be9ff06f          	j	80016b8c <__adddf3+0x2b4>
  FP_UNPACK_SEMIRAW_D (B, b);
80016fa8:	00070793          	mv	a5,a4
80016fac:	00060513          	mv	a0,a2
80016fb0:	bddff06f          	j	80016b8c <__adddf3+0x2b4>
80016fb4:	7ff00413          	li	s0,2047
80016fb8:	00000793          	li	a5,0
80016fbc:	00000513          	li	a0,0
  FP_PACK_SEMIRAW_D (r, R);
80016fc0:	00879713          	slli	a4,a5,0x8
80016fc4:	00075e63          	bgez	a4,80016fe0 <__adddf3+0x708>
80016fc8:	00140413          	addi	s0,s0,1
80016fcc:	7ff00713          	li	a4,2047
80016fd0:	0ae40c63          	beq	s0,a4,80017088 <__adddf3+0x7b0>
80016fd4:	ff800737          	lui	a4,0xff800
80016fd8:	fff70713          	addi	a4,a4,-1 # ff7fffff <__clzsi2+0x7f7e736b>
80016fdc:	00e7f7b3          	and	a5,a5,a4
80016fe0:	01d79713          	slli	a4,a5,0x1d
80016fe4:	00355513          	srli	a0,a0,0x3
80016fe8:	00a76533          	or	a0,a4,a0
80016fec:	7ff00713          	li	a4,2047
80016ff0:	0037d793          	srli	a5,a5,0x3
80016ff4:	00e41e63          	bne	s0,a4,80017010 <__adddf3+0x738>
80016ff8:	00f56533          	or	a0,a0,a5
80016ffc:	00000793          	li	a5,0
80017000:	00050863          	beqz	a0,80017010 <__adddf3+0x738>
80017004:	000807b7          	lui	a5,0x80
80017008:	00000513          	li	a0,0
8001700c:	00000493          	li	s1,0
80017010:	01441713          	slli	a4,s0,0x14
80017014:	7ff006b7          	lui	a3,0x7ff00
80017018:	00c79793          	slli	a5,a5,0xc
  FP_HANDLE_EXCEPTIONS;

  return r;
}
8001701c:	01c12083          	lw	ra,28(sp)
80017020:	01812403          	lw	s0,24(sp)
  FP_PACK_SEMIRAW_D (r, R);
80017024:	00d77733          	and	a4,a4,a3
80017028:	00c7d793          	srli	a5,a5,0xc
8001702c:	01f49493          	slli	s1,s1,0x1f
80017030:	00f767b3          	or	a5,a4,a5
80017034:	0097e733          	or	a4,a5,s1
}
80017038:	01012903          	lw	s2,16(sp)
8001703c:	01412483          	lw	s1,20(sp)
80017040:	00c12983          	lw	s3,12(sp)
80017044:	00070593          	mv	a1,a4
80017048:	02010113          	addi	sp,sp,32
8001704c:	00008067          	ret
  FP_UNPACK_SEMIRAW_D (B, b);
80017050:	00070793          	mv	a5,a4
80017054:	00060513          	mv	a0,a2
  FP_ADD_D (R, A, B);
80017058:	00080413          	mv	s0,a6
8001705c:	db9ff06f          	j	80016e14 <__adddf3+0x53c>
80017060:	00000793          	li	a5,0
80017064:	00000513          	li	a0,0
80017068:	00000493          	li	s1,0
8001706c:	f55ff06f          	j	80016fc0 <__adddf3+0x6e8>
80017070:	00000793          	li	a5,0
80017074:	ff5ff06f          	j	80017068 <__adddf3+0x790>
80017078:	00000513          	li	a0,0
8001707c:	00000493          	li	s1,0
80017080:	004007b7          	lui	a5,0x400
80017084:	b49ff06f          	j	80016bcc <__adddf3+0x2f4>
80017088:	00000793          	li	a5,0
8001708c:	00000513          	li	a0,0
80017090:	f51ff06f          	j	80016fe0 <__adddf3+0x708>

80017094 <__divdf3>:
#include "soft-fp.h"
#include "double.h"

DFtype
__divdf3 (DFtype a, DFtype b)
{
80017094:	fb010113          	addi	sp,sp,-80
  FP_DECL_D (B);
  FP_DECL_D (R);
  DFtype r;

  FP_INIT_ROUNDMODE;
  FP_UNPACK_D (A, a);
80017098:	0145d793          	srli	a5,a1,0x14
{
8001709c:	04912223          	sw	s1,68(sp)
800170a0:	03312e23          	sw	s3,60(sp)
800170a4:	03412c23          	sw	s4,56(sp)
800170a8:	03512a23          	sw	s5,52(sp)
800170ac:	03612823          	sw	s6,48(sp)
  FP_UNPACK_D (A, a);
800170b0:	00c59493          	slli	s1,a1,0xc
{
800170b4:	04112623          	sw	ra,76(sp)
800170b8:	04812423          	sw	s0,72(sp)
800170bc:	05212023          	sw	s2,64(sp)
800170c0:	03712623          	sw	s7,44(sp)
800170c4:	03812423          	sw	s8,40(sp)
800170c8:	03912223          	sw	s9,36(sp)
800170cc:	03a12023          	sw	s10,32(sp)
800170d0:	01b12e23          	sw	s11,28(sp)
  FP_UNPACK_D (A, a);
800170d4:	01579713          	slli	a4,a5,0x15
{
800170d8:	00050993          	mv	s3,a0
800170dc:	00060a13          	mv	s4,a2
800170e0:	00068b13          	mv	s6,a3
  FP_UNPACK_D (A, a);
800170e4:	00c4d493          	srli	s1,s1,0xc
800170e8:	01f5da93          	srli	s5,a1,0x1f
800170ec:	0a070463          	beqz	a4,80017194 <__divdf3+0x100>
800170f0:	7ff7f913          	andi	s2,a5,2047
800170f4:	7ff00793          	li	a5,2047
800170f8:	10f90063          	beq	s2,a5,800171f8 <__divdf3+0x164>
800170fc:	01d55c93          	srli	s9,a0,0x1d
80017100:	00349493          	slli	s1,s1,0x3
80017104:	009ce4b3          	or	s1,s9,s1
80017108:	00800cb7          	lui	s9,0x800
8001710c:	0194ecb3          	or	s9,s1,s9
80017110:	00351413          	slli	s0,a0,0x3
80017114:	c0190913          	addi	s2,s2,-1023
80017118:	00000c13          	li	s8,0
  FP_UNPACK_D (B, b);
8001711c:	014b5793          	srli	a5,s6,0x14
80017120:	00cb1593          	slli	a1,s6,0xc
80017124:	01579713          	slli	a4,a5,0x15
80017128:	00c5d993          	srli	s3,a1,0xc
8001712c:	7ff7f513          	andi	a0,a5,2047
80017130:	01fb5b13          	srli	s6,s6,0x1f
80017134:	10070063          	beqz	a4,80017234 <__divdf3+0x1a0>
80017138:	7ff00793          	li	a5,2047
8001713c:	16f50063          	beq	a0,a5,8001729c <__divdf3+0x208>
80017140:	00399593          	slli	a1,s3,0x3
80017144:	01da5793          	srli	a5,s4,0x1d
80017148:	00b7e5b3          	or	a1,a5,a1
8001714c:	008009b7          	lui	s3,0x800
80017150:	0135e9b3          	or	s3,a1,s3
80017154:	003a1813          	slli	a6,s4,0x3
80017158:	c0150513          	addi	a0,a0,-1023
8001715c:	00000693          	li	a3,0
  FP_DIV_D (R, A, B);
80017160:	002c1793          	slli	a5,s8,0x2
80017164:	00d7e7b3          	or	a5,a5,a3
80017168:	fff78793          	addi	a5,a5,-1 # 3fffff <font6x8-0x7fc0002d>
8001716c:	00e00713          	li	a4,14
80017170:	016ac4b3          	xor	s1,s5,s6
80017174:	40a90933          	sub	s2,s2,a0
80017178:	14f76e63          	bltu	a4,a5,800172d4 <__divdf3+0x240>
8001717c:	80000737          	lui	a4,0x80000
80017180:	00279793          	slli	a5,a5,0x2
80017184:	65870713          	addi	a4,a4,1624 # 80000658 <__clzsi2+0xfffe79c4>
80017188:	00e787b3          	add	a5,a5,a4
8001718c:	0007a783          	lw	a5,0(a5)
80017190:	00078067          	jr	a5
  FP_UNPACK_D (A, a);
80017194:	00a4ecb3          	or	s9,s1,a0
80017198:	060c8e63          	beqz	s9,80017214 <__divdf3+0x180>
8001719c:	04048063          	beqz	s1,800171dc <__divdf3+0x148>
800171a0:	00048513          	mv	a0,s1
800171a4:	2f1010ef          	jal	ra,80018c94 <__clzsi2>
800171a8:	ff550793          	addi	a5,a0,-11
800171ac:	01c00713          	li	a4,28
800171b0:	02f74c63          	blt	a4,a5,800171e8 <__divdf3+0x154>
800171b4:	01d00c93          	li	s9,29
800171b8:	ff850413          	addi	s0,a0,-8
800171bc:	40fc8cb3          	sub	s9,s9,a5
800171c0:	008494b3          	sll	s1,s1,s0
800171c4:	0199dcb3          	srl	s9,s3,s9
800171c8:	009cecb3          	or	s9,s9,s1
800171cc:	00899433          	sll	s0,s3,s0
800171d0:	c0d00913          	li	s2,-1011
800171d4:	40a90933          	sub	s2,s2,a0
800171d8:	f41ff06f          	j	80017118 <__divdf3+0x84>
800171dc:	2b9010ef          	jal	ra,80018c94 <__clzsi2>
800171e0:	02050513          	addi	a0,a0,32
800171e4:	fc5ff06f          	j	800171a8 <__divdf3+0x114>
800171e8:	fd850493          	addi	s1,a0,-40
800171ec:	00999cb3          	sll	s9,s3,s1
800171f0:	00000413          	li	s0,0
800171f4:	fddff06f          	j	800171d0 <__divdf3+0x13c>
800171f8:	00a4ecb3          	or	s9,s1,a0
800171fc:	020c8463          	beqz	s9,80017224 <__divdf3+0x190>
80017200:	00050413          	mv	s0,a0
80017204:	00048c93          	mv	s9,s1
80017208:	7ff00913          	li	s2,2047
8001720c:	00300c13          	li	s8,3
80017210:	f0dff06f          	j	8001711c <__divdf3+0x88>
80017214:	00000413          	li	s0,0
80017218:	00000913          	li	s2,0
8001721c:	00100c13          	li	s8,1
80017220:	efdff06f          	j	8001711c <__divdf3+0x88>
80017224:	00000413          	li	s0,0
80017228:	7ff00913          	li	s2,2047
8001722c:	00200c13          	li	s8,2
80017230:	eedff06f          	j	8001711c <__divdf3+0x88>
  FP_UNPACK_D (B, b);
80017234:	0149e833          	or	a6,s3,s4
80017238:	06080e63          	beqz	a6,800172b4 <__divdf3+0x220>
8001723c:	04098063          	beqz	s3,8001727c <__divdf3+0x1e8>
80017240:	00098513          	mv	a0,s3
80017244:	251010ef          	jal	ra,80018c94 <__clzsi2>
80017248:	ff550793          	addi	a5,a0,-11
8001724c:	01c00713          	li	a4,28
80017250:	02f74e63          	blt	a4,a5,8001728c <__divdf3+0x1f8>
80017254:	01d00693          	li	a3,29
80017258:	ff850813          	addi	a6,a0,-8
8001725c:	40f686b3          	sub	a3,a3,a5
80017260:	010995b3          	sll	a1,s3,a6
80017264:	00da56b3          	srl	a3,s4,a3
80017268:	00b6e9b3          	or	s3,a3,a1
8001726c:	010a1833          	sll	a6,s4,a6
80017270:	c0d00713          	li	a4,-1011
80017274:	40a70533          	sub	a0,a4,a0
80017278:	ee5ff06f          	j	8001715c <__divdf3+0xc8>
8001727c:	000a0513          	mv	a0,s4
80017280:	215010ef          	jal	ra,80018c94 <__clzsi2>
80017284:	02050513          	addi	a0,a0,32
80017288:	fc1ff06f          	j	80017248 <__divdf3+0x1b4>
8001728c:	fd850593          	addi	a1,a0,-40
80017290:	00ba19b3          	sll	s3,s4,a1
80017294:	00000813          	li	a6,0
80017298:	fd9ff06f          	j	80017270 <__divdf3+0x1dc>
8001729c:	0149e833          	or	a6,s3,s4
800172a0:	02080263          	beqz	a6,800172c4 <__divdf3+0x230>
800172a4:	000a0813          	mv	a6,s4
800172a8:	7ff00513          	li	a0,2047
800172ac:	00300693          	li	a3,3
800172b0:	eb1ff06f          	j	80017160 <__divdf3+0xcc>
800172b4:	00000993          	li	s3,0
800172b8:	00000513          	li	a0,0
800172bc:	00100693          	li	a3,1
800172c0:	ea1ff06f          	j	80017160 <__divdf3+0xcc>
800172c4:	00000993          	li	s3,0
800172c8:	7ff00513          	li	a0,2047
800172cc:	00200693          	li	a3,2
800172d0:	e91ff06f          	j	80017160 <__divdf3+0xcc>
  FP_DIV_D (R, A, B);
800172d4:	0199e663          	bltu	s3,s9,800172e0 <__divdf3+0x24c>
800172d8:	453c9463          	bne	s9,s3,80017720 <__divdf3+0x68c>
800172dc:	45046263          	bltu	s0,a6,80017720 <__divdf3+0x68c>
800172e0:	01fc9713          	slli	a4,s9,0x1f
800172e4:	00145793          	srli	a5,s0,0x1
800172e8:	01f41d93          	slli	s11,s0,0x1f
800172ec:	001cdc93          	srli	s9,s9,0x1
800172f0:	00f76433          	or	s0,a4,a5
800172f4:	00899593          	slli	a1,s3,0x8
800172f8:	01885a93          	srli	s5,a6,0x18
800172fc:	00baeab3          	or	s5,s5,a1
80017300:	0105db93          	srli	s7,a1,0x10
80017304:	000b8593          	mv	a1,s7
80017308:	010a9c13          	slli	s8,s5,0x10
8001730c:	000c8513          	mv	a0,s9
80017310:	00881b13          	slli	s6,a6,0x8
80017314:	010c5c13          	srli	s8,s8,0x10
80017318:	0d1010ef          	jal	ra,80018be8 <__udivsi3>
8001731c:	00050593          	mv	a1,a0
80017320:	00050d13          	mv	s10,a0
80017324:	000c0513          	mv	a0,s8
80017328:	005010ef          	jal	ra,80018b2c <__mulsi3>
8001732c:	00050993          	mv	s3,a0
80017330:	000b8593          	mv	a1,s7
80017334:	000c8513          	mv	a0,s9
80017338:	0f9010ef          	jal	ra,80018c30 <__umodsi3>
8001733c:	01051513          	slli	a0,a0,0x10
80017340:	01045713          	srli	a4,s0,0x10
80017344:	00a76733          	or	a4,a4,a0
80017348:	000d0a13          	mv	s4,s10
8001734c:	01377e63          	bgeu	a4,s3,80017368 <__divdf3+0x2d4>
80017350:	01570733          	add	a4,a4,s5
80017354:	fffd0a13          	addi	s4,s10,-1
80017358:	01576863          	bltu	a4,s5,80017368 <__divdf3+0x2d4>
8001735c:	01377663          	bgeu	a4,s3,80017368 <__divdf3+0x2d4>
80017360:	ffed0a13          	addi	s4,s10,-2
80017364:	01570733          	add	a4,a4,s5
80017368:	413709b3          	sub	s3,a4,s3
8001736c:	000b8593          	mv	a1,s7
80017370:	00098513          	mv	a0,s3
80017374:	075010ef          	jal	ra,80018be8 <__udivsi3>
80017378:	00050593          	mv	a1,a0
8001737c:	00050d13          	mv	s10,a0
80017380:	000c0513          	mv	a0,s8
80017384:	7a8010ef          	jal	ra,80018b2c <__mulsi3>
80017388:	00050c93          	mv	s9,a0
8001738c:	000b8593          	mv	a1,s7
80017390:	00098513          	mv	a0,s3
80017394:	09d010ef          	jal	ra,80018c30 <__umodsi3>
80017398:	01041413          	slli	s0,s0,0x10
8001739c:	01051513          	slli	a0,a0,0x10
800173a0:	01045413          	srli	s0,s0,0x10
800173a4:	00a46433          	or	s0,s0,a0
800173a8:	000d0793          	mv	a5,s10
800173ac:	01947e63          	bgeu	s0,s9,800173c8 <__divdf3+0x334>
800173b0:	01540433          	add	s0,s0,s5
800173b4:	fffd0793          	addi	a5,s10,-1
800173b8:	01546863          	bltu	s0,s5,800173c8 <__divdf3+0x334>
800173bc:	01947663          	bgeu	s0,s9,800173c8 <__divdf3+0x334>
800173c0:	ffed0793          	addi	a5,s10,-2
800173c4:	01540433          	add	s0,s0,s5
800173c8:	010a1813          	slli	a6,s4,0x10
800173cc:	00010337          	lui	t1,0x10
800173d0:	00f86833          	or	a6,a6,a5
800173d4:	41940433          	sub	s0,s0,s9
800173d8:	fff30c93          	addi	s9,t1,-1 # ffff <font6x8-0x7fff002d>
800173dc:	019878b3          	and	a7,a6,s9
800173e0:	019b7cb3          	and	s9,s6,s9
800173e4:	01085713          	srli	a4,a6,0x10
800173e8:	010b5d13          	srli	s10,s6,0x10
800173ec:	00088513          	mv	a0,a7
800173f0:	000c8593          	mv	a1,s9
800173f4:	738010ef          	jal	ra,80018b2c <__mulsi3>
800173f8:	00050793          	mv	a5,a0
800173fc:	000d0593          	mv	a1,s10
80017400:	00088513          	mv	a0,a7
80017404:	728010ef          	jal	ra,80018b2c <__mulsi3>
80017408:	00050a13          	mv	s4,a0
8001740c:	000c8593          	mv	a1,s9
80017410:	00070513          	mv	a0,a4
80017414:	718010ef          	jal	ra,80018b2c <__mulsi3>
80017418:	00050893          	mv	a7,a0
8001741c:	000d0593          	mv	a1,s10
80017420:	00070513          	mv	a0,a4
80017424:	708010ef          	jal	ra,80018b2c <__mulsi3>
80017428:	0107d713          	srli	a4,a5,0x10
8001742c:	011a0a33          	add	s4,s4,a7
80017430:	01470733          	add	a4,a4,s4
80017434:	01177463          	bgeu	a4,a7,8001743c <__divdf3+0x3a8>
80017438:	00650533          	add	a0,a0,t1
8001743c:	01075693          	srli	a3,a4,0x10
80017440:	00a686b3          	add	a3,a3,a0
80017444:	00010537          	lui	a0,0x10
80017448:	fff50513          	addi	a0,a0,-1 # ffff <font6x8-0x7fff002d>
8001744c:	00a77a33          	and	s4,a4,a0
80017450:	010a1a13          	slli	s4,s4,0x10
80017454:	00a7f7b3          	and	a5,a5,a0
80017458:	00fa0a33          	add	s4,s4,a5
8001745c:	00d46863          	bltu	s0,a3,8001746c <__divdf3+0x3d8>
80017460:	00080993          	mv	s3,a6
80017464:	04d41463          	bne	s0,a3,800174ac <__divdf3+0x418>
80017468:	054df263          	bgeu	s11,s4,800174ac <__divdf3+0x418>
8001746c:	016d8db3          	add	s11,s11,s6
80017470:	016db7b3          	sltu	a5,s11,s6
80017474:	015787b3          	add	a5,a5,s5
80017478:	00f40433          	add	s0,s0,a5
8001747c:	fff80993          	addi	s3,a6,-1
80017480:	008ae663          	bltu	s5,s0,8001748c <__divdf3+0x3f8>
80017484:	028a9463          	bne	s5,s0,800174ac <__divdf3+0x418>
80017488:	036de263          	bltu	s11,s6,800174ac <__divdf3+0x418>
8001748c:	00d46663          	bltu	s0,a3,80017498 <__divdf3+0x404>
80017490:	00869e63          	bne	a3,s0,800174ac <__divdf3+0x418>
80017494:	014dfc63          	bgeu	s11,s4,800174ac <__divdf3+0x418>
80017498:	016d8db3          	add	s11,s11,s6
8001749c:	016db7b3          	sltu	a5,s11,s6
800174a0:	015787b3          	add	a5,a5,s5
800174a4:	ffe80993          	addi	s3,a6,-2
800174a8:	00f40433          	add	s0,s0,a5
800174ac:	414d8a33          	sub	s4,s11,s4
800174b0:	40d40433          	sub	s0,s0,a3
800174b4:	014db7b3          	sltu	a5,s11,s4
800174b8:	40f40433          	sub	s0,s0,a5
800174bc:	fff00813          	li	a6,-1
800174c0:	1a8a8063          	beq	s5,s0,80017660 <__divdf3+0x5cc>
800174c4:	000b8593          	mv	a1,s7
800174c8:	00040513          	mv	a0,s0
800174cc:	71c010ef          	jal	ra,80018be8 <__udivsi3>
800174d0:	00050593          	mv	a1,a0
800174d4:	00a12623          	sw	a0,12(sp)
800174d8:	000c0513          	mv	a0,s8
800174dc:	650010ef          	jal	ra,80018b2c <__mulsi3>
800174e0:	00a12423          	sw	a0,8(sp)
800174e4:	000b8593          	mv	a1,s7
800174e8:	00040513          	mv	a0,s0
800174ec:	744010ef          	jal	ra,80018c30 <__umodsi3>
800174f0:	00c12683          	lw	a3,12(sp)
800174f4:	00812703          	lw	a4,8(sp)
800174f8:	01051513          	slli	a0,a0,0x10
800174fc:	010a5793          	srli	a5,s4,0x10
80017500:	00a7e7b3          	or	a5,a5,a0
80017504:	00068d93          	mv	s11,a3
80017508:	00e7fe63          	bgeu	a5,a4,80017524 <__divdf3+0x490>
8001750c:	015787b3          	add	a5,a5,s5
80017510:	fff68d93          	addi	s11,a3,-1 # 7fefffff <font6x8-0x10002d>
80017514:	0157e863          	bltu	a5,s5,80017524 <__divdf3+0x490>
80017518:	00e7f663          	bgeu	a5,a4,80017524 <__divdf3+0x490>
8001751c:	ffe68d93          	addi	s11,a3,-2
80017520:	015787b3          	add	a5,a5,s5
80017524:	40e78433          	sub	s0,a5,a4
80017528:	000b8593          	mv	a1,s7
8001752c:	00040513          	mv	a0,s0
80017530:	6b8010ef          	jal	ra,80018be8 <__udivsi3>
80017534:	00050593          	mv	a1,a0
80017538:	00a12423          	sw	a0,8(sp)
8001753c:	000c0513          	mv	a0,s8
80017540:	5ec010ef          	jal	ra,80018b2c <__mulsi3>
80017544:	00050c13          	mv	s8,a0
80017548:	000b8593          	mv	a1,s7
8001754c:	00040513          	mv	a0,s0
80017550:	6e0010ef          	jal	ra,80018c30 <__umodsi3>
80017554:	00812703          	lw	a4,8(sp)
80017558:	010a1a13          	slli	s4,s4,0x10
8001755c:	01051793          	slli	a5,a0,0x10
80017560:	010a5a13          	srli	s4,s4,0x10
80017564:	00fa67b3          	or	a5,s4,a5
80017568:	00070693          	mv	a3,a4
8001756c:	0187fe63          	bgeu	a5,s8,80017588 <__divdf3+0x4f4>
80017570:	015787b3          	add	a5,a5,s5
80017574:	fff70693          	addi	a3,a4,-1
80017578:	0157e863          	bltu	a5,s5,80017588 <__divdf3+0x4f4>
8001757c:	0187f663          	bgeu	a5,s8,80017588 <__divdf3+0x4f4>
80017580:	ffe70693          	addi	a3,a4,-2
80017584:	015787b3          	add	a5,a5,s5
80017588:	010d9893          	slli	a7,s11,0x10
8001758c:	00d8e8b3          	or	a7,a7,a3
80017590:	01089313          	slli	t1,a7,0x10
80017594:	01035313          	srli	t1,t1,0x10
80017598:	418787b3          	sub	a5,a5,s8
8001759c:	0108d713          	srli	a4,a7,0x10
800175a0:	00030513          	mv	a0,t1
800175a4:	000c8593          	mv	a1,s9
800175a8:	584010ef          	jal	ra,80018b2c <__mulsi3>
800175ac:	00050813          	mv	a6,a0
800175b0:	000d0593          	mv	a1,s10
800175b4:	00030513          	mv	a0,t1
800175b8:	574010ef          	jal	ra,80018b2c <__mulsi3>
800175bc:	00050313          	mv	t1,a0
800175c0:	000c8593          	mv	a1,s9
800175c4:	00070513          	mv	a0,a4
800175c8:	564010ef          	jal	ra,80018b2c <__mulsi3>
800175cc:	00050e13          	mv	t3,a0
800175d0:	000d0593          	mv	a1,s10
800175d4:	00070513          	mv	a0,a4
800175d8:	554010ef          	jal	ra,80018b2c <__mulsi3>
800175dc:	01085713          	srli	a4,a6,0x10
800175e0:	01c30333          	add	t1,t1,t3
800175e4:	00670733          	add	a4,a4,t1
800175e8:	01c77663          	bgeu	a4,t3,800175f4 <__divdf3+0x560>
800175ec:	000106b7          	lui	a3,0x10
800175f0:	00d50533          	add	a0,a0,a3
800175f4:	01075593          	srli	a1,a4,0x10
800175f8:	00a585b3          	add	a1,a1,a0
800175fc:	00010537          	lui	a0,0x10
80017600:	fff50513          	addi	a0,a0,-1 # ffff <font6x8-0x7fff002d>
80017604:	00a776b3          	and	a3,a4,a0
80017608:	01069693          	slli	a3,a3,0x10
8001760c:	00a87833          	and	a6,a6,a0
80017610:	010686b3          	add	a3,a3,a6
80017614:	00b7e863          	bltu	a5,a1,80017624 <__divdf3+0x590>
80017618:	24b79663          	bne	a5,a1,80017864 <__divdf3+0x7d0>
8001761c:	00088813          	mv	a6,a7
80017620:	04068063          	beqz	a3,80017660 <__divdf3+0x5cc>
80017624:	00fa8633          	add	a2,s5,a5
80017628:	fff88813          	addi	a6,a7,-1
8001762c:	03566463          	bltu	a2,s5,80017654 <__divdf3+0x5c0>
80017630:	00b66663          	bltu	a2,a1,8001763c <__divdf3+0x5a8>
80017634:	22b61663          	bne	a2,a1,80017860 <__divdf3+0x7cc>
80017638:	02db7063          	bgeu	s6,a3,80017658 <__divdf3+0x5c4>
8001763c:	001b1793          	slli	a5,s6,0x1
80017640:	0167bb33          	sltu	s6,a5,s6
80017644:	015b0ab3          	add	s5,s6,s5
80017648:	ffe88813          	addi	a6,a7,-2
8001764c:	01560633          	add	a2,a2,s5
80017650:	00078b13          	mv	s6,a5
80017654:	00b61463          	bne	a2,a1,8001765c <__divdf3+0x5c8>
80017658:	00db0463          	beq	s6,a3,80017660 <__divdf3+0x5cc>
8001765c:	00186813          	ori	a6,a6,1
  FP_PACK_D (r, R);
80017660:	3ff90793          	addi	a5,s2,1023
80017664:	10f05e63          	blez	a5,80017780 <__divdf3+0x6ec>
80017668:	00787713          	andi	a4,a6,7
8001766c:	02070063          	beqz	a4,8001768c <__divdf3+0x5f8>
80017670:	00f87713          	andi	a4,a6,15
80017674:	00400693          	li	a3,4
80017678:	00d70a63          	beq	a4,a3,8001768c <__divdf3+0x5f8>
8001767c:	00480693          	addi	a3,a6,4
80017680:	0106b833          	sltu	a6,a3,a6
80017684:	010989b3          	add	s3,s3,a6
80017688:	00068813          	mv	a6,a3
8001768c:	00799713          	slli	a4,s3,0x7
80017690:	00075a63          	bgez	a4,800176a4 <__divdf3+0x610>
80017694:	ff0007b7          	lui	a5,0xff000
80017698:	fff78793          	addi	a5,a5,-1 # feffffff <__clzsi2+0x7efe736b>
8001769c:	00f9f9b3          	and	s3,s3,a5
800176a0:	40090793          	addi	a5,s2,1024
800176a4:	7fe00713          	li	a4,2046
800176a8:	0af74663          	blt	a4,a5,80017754 <__divdf3+0x6c0>
800176ac:	00385813          	srli	a6,a6,0x3
800176b0:	01d99713          	slli	a4,s3,0x1d
800176b4:	01076733          	or	a4,a4,a6
800176b8:	0039d593          	srli	a1,s3,0x3
800176bc:	01479793          	slli	a5,a5,0x14
800176c0:	7ff006b7          	lui	a3,0x7ff00
800176c4:	00c59593          	slli	a1,a1,0xc
  FP_HANDLE_EXCEPTIONS;

  return r;
}
800176c8:	04c12083          	lw	ra,76(sp)
800176cc:	04812403          	lw	s0,72(sp)
  FP_PACK_D (r, R);
800176d0:	00d7f7b3          	and	a5,a5,a3
800176d4:	00c5d593          	srli	a1,a1,0xc
800176d8:	00b7e5b3          	or	a1,a5,a1
800176dc:	01f49493          	slli	s1,s1,0x1f
800176e0:	0095e7b3          	or	a5,a1,s1
}
800176e4:	04012903          	lw	s2,64(sp)
800176e8:	04412483          	lw	s1,68(sp)
800176ec:	03c12983          	lw	s3,60(sp)
800176f0:	03812a03          	lw	s4,56(sp)
800176f4:	03412a83          	lw	s5,52(sp)
800176f8:	03012b03          	lw	s6,48(sp)
800176fc:	02c12b83          	lw	s7,44(sp)
80017700:	02812c03          	lw	s8,40(sp)
80017704:	02412c83          	lw	s9,36(sp)
80017708:	02012d03          	lw	s10,32(sp)
8001770c:	01c12d83          	lw	s11,28(sp)
80017710:	00070513          	mv	a0,a4
80017714:	00078593          	mv	a1,a5
80017718:	05010113          	addi	sp,sp,80
8001771c:	00008067          	ret
  FP_DIV_D (R, A, B);
80017720:	fff90913          	addi	s2,s2,-1
80017724:	00000d93          	li	s11,0
80017728:	bcdff06f          	j	800172f4 <__divdf3+0x260>
  FP_UNPACK_D (A, a);
8001772c:	000a8493          	mv	s1,s5
  FP_DIV_D (R, A, B);
80017730:	000c8993          	mv	s3,s9
80017734:	00040813          	mv	a6,s0
80017738:	000c0693          	mv	a3,s8
  FP_PACK_D (r, R);
8001773c:	00300793          	li	a5,3
80017740:	0ef68863          	beq	a3,a5,80017830 <__divdf3+0x79c>
80017744:	00100793          	li	a5,1
80017748:	0ef68e63          	beq	a3,a5,80017844 <__divdf3+0x7b0>
8001774c:	00200793          	li	a5,2
80017750:	f0f698e3          	bne	a3,a5,80017660 <__divdf3+0x5cc>
80017754:	00000593          	li	a1,0
80017758:	00000713          	li	a4,0
8001775c:	7ff00793          	li	a5,2047
80017760:	f5dff06f          	j	800176bc <__divdf3+0x628>
  FP_UNPACK_D (B, b);
80017764:	000b0493          	mv	s1,s6
  FP_DIV_D (R, A, B);
80017768:	fd5ff06f          	j	8001773c <__divdf3+0x6a8>
8001776c:	000809b7          	lui	s3,0x80
80017770:	00000813          	li	a6,0
80017774:	00000493          	li	s1,0
80017778:	00300693          	li	a3,3
8001777c:	fc1ff06f          	j	8001773c <__divdf3+0x6a8>
  FP_PACK_D (r, R);
80017780:	00100593          	li	a1,1
80017784:	40f585b3          	sub	a1,a1,a5
80017788:	03800713          	li	a4,56
8001778c:	0ab74c63          	blt	a4,a1,80017844 <__divdf3+0x7b0>
80017790:	01f00713          	li	a4,31
80017794:	06b74463          	blt	a4,a1,800177fc <__divdf3+0x768>
80017798:	41e90913          	addi	s2,s2,1054
8001779c:	012997b3          	sll	a5,s3,s2
800177a0:	00b85733          	srl	a4,a6,a1
800177a4:	01281933          	sll	s2,a6,s2
800177a8:	00e7e7b3          	or	a5,a5,a4
800177ac:	01203933          	snez	s2,s2
800177b0:	0127e7b3          	or	a5,a5,s2
800177b4:	00b9d5b3          	srl	a1,s3,a1
800177b8:	0077f713          	andi	a4,a5,7
800177bc:	02070063          	beqz	a4,800177dc <__divdf3+0x748>
800177c0:	00f7f713          	andi	a4,a5,15
800177c4:	00400693          	li	a3,4
800177c8:	00d70a63          	beq	a4,a3,800177dc <__divdf3+0x748>
800177cc:	00478713          	addi	a4,a5,4
800177d0:	00f737b3          	sltu	a5,a4,a5
800177d4:	00f585b3          	add	a1,a1,a5
800177d8:	00070793          	mv	a5,a4
800177dc:	00859713          	slli	a4,a1,0x8
800177e0:	06074863          	bltz	a4,80017850 <__divdf3+0x7bc>
800177e4:	01d59713          	slli	a4,a1,0x1d
800177e8:	0037d793          	srli	a5,a5,0x3
800177ec:	00f76733          	or	a4,a4,a5
800177f0:	0035d593          	srli	a1,a1,0x3
800177f4:	00000793          	li	a5,0
800177f8:	ec5ff06f          	j	800176bc <__divdf3+0x628>
800177fc:	fe100713          	li	a4,-31
80017800:	40f707b3          	sub	a5,a4,a5
80017804:	02000693          	li	a3,32
80017808:	00f9d7b3          	srl	a5,s3,a5
8001780c:	00000713          	li	a4,0
80017810:	00d58663          	beq	a1,a3,8001781c <__divdf3+0x788>
80017814:	43e90913          	addi	s2,s2,1086
80017818:	01299733          	sll	a4,s3,s2
8001781c:	01076733          	or	a4,a4,a6
80017820:	00e03733          	snez	a4,a4
80017824:	00e7e7b3          	or	a5,a5,a4
80017828:	00000593          	li	a1,0
8001782c:	f8dff06f          	j	800177b8 <__divdf3+0x724>
80017830:	000805b7          	lui	a1,0x80
80017834:	00000713          	li	a4,0
80017838:	7ff00793          	li	a5,2047
8001783c:	00000493          	li	s1,0
80017840:	e7dff06f          	j	800176bc <__divdf3+0x628>
80017844:	00000593          	li	a1,0
80017848:	00000713          	li	a4,0
8001784c:	fa9ff06f          	j	800177f4 <__divdf3+0x760>
80017850:	00000593          	li	a1,0
80017854:	00000713          	li	a4,0
80017858:	00100793          	li	a5,1
8001785c:	e61ff06f          	j	800176bc <__divdf3+0x628>
  FP_DIV_D (R, A, B);
80017860:	00080893          	mv	a7,a6
80017864:	00088813          	mv	a6,a7
80017868:	df5ff06f          	j	8001765c <__divdf3+0x5c8>

8001786c <__eqdf2>:
  FP_DECL_D (A);
  FP_DECL_D (B);
  CMPtype r;

  FP_INIT_EXCEPTIONS;
  FP_UNPACK_RAW_D (A, a);
8001786c:	0145d713          	srli	a4,a1,0x14
80017870:	001007b7          	lui	a5,0x100
80017874:	fff78793          	addi	a5,a5,-1 # fffff <font6x8-0x7ff0002d>
  FP_UNPACK_RAW_D (B, b);
80017878:	0146d813          	srli	a6,a3,0x14
{
8001787c:	00050313          	mv	t1,a0
80017880:	00050e93          	mv	t4,a0
  FP_UNPACK_RAW_D (A, a);
80017884:	7ff77713          	andi	a4,a4,2047
  FP_CMP_EQ_D (r, A, B, 1);
80017888:	7ff00513          	li	a0,2047
  FP_UNPACK_RAW_D (A, a);
8001788c:	00b7f8b3          	and	a7,a5,a1
80017890:	00060f13          	mv	t5,a2
  FP_UNPACK_RAW_D (B, b);
80017894:	00d7f7b3          	and	a5,a5,a3
  FP_UNPACK_RAW_D (A, a);
80017898:	01f5d593          	srli	a1,a1,0x1f
  FP_UNPACK_RAW_D (B, b);
8001789c:	7ff87813          	andi	a6,a6,2047
800178a0:	01f6d693          	srli	a3,a3,0x1f
  FP_CMP_EQ_D (r, A, B, 1);
800178a4:	00a71c63          	bne	a4,a0,800178bc <__eqdf2+0x50>
800178a8:	0068ee33          	or	t3,a7,t1
800178ac:	00100513          	li	a0,1
800178b0:	000e1463          	bnez	t3,800178b8 <__eqdf2+0x4c>
800178b4:	00e80663          	beq	a6,a4,800178c0 <__eqdf2+0x54>
  FP_HANDLE_EXCEPTIONS;

  return r;
}
800178b8:	00008067          	ret
  FP_CMP_EQ_D (r, A, B, 1);
800178bc:	00a81863          	bne	a6,a0,800178cc <__eqdf2+0x60>
800178c0:	00c7e633          	or	a2,a5,a2
800178c4:	00100513          	li	a0,1
800178c8:	fe0618e3          	bnez	a2,800178b8 <__eqdf2+0x4c>
800178cc:	00100513          	li	a0,1
800178d0:	ff0714e3          	bne	a4,a6,800178b8 <__eqdf2+0x4c>
800178d4:	fef892e3          	bne	a7,a5,800178b8 <__eqdf2+0x4c>
800178d8:	ffee90e3          	bne	t4,t5,800178b8 <__eqdf2+0x4c>
800178dc:	00d58a63          	beq	a1,a3,800178f0 <__eqdf2+0x84>
800178e0:	fc071ce3          	bnez	a4,800178b8 <__eqdf2+0x4c>
800178e4:	0068e533          	or	a0,a7,t1
800178e8:	00a03533          	snez	a0,a0
800178ec:	00008067          	ret
800178f0:	00000513          	li	a0,0
800178f4:	00008067          	ret

800178f8 <__gedf2>:
  FP_DECL_D (A);
  FP_DECL_D (B);
  CMPtype r;

  FP_INIT_EXCEPTIONS;
  FP_UNPACK_RAW_D (A, a);
800178f8:	00100737          	lui	a4,0x100
800178fc:	fff70713          	addi	a4,a4,-1 # fffff <font6x8-0x7ff0002d>
80017900:	0145d813          	srli	a6,a1,0x14
80017904:	00b778b3          	and	a7,a4,a1
{
80017908:	00050793          	mv	a5,a0
8001790c:	00050313          	mv	t1,a0
  FP_UNPACK_RAW_D (A, a);
80017910:	7ff87813          	andi	a6,a6,2047
80017914:	01f5d513          	srli	a0,a1,0x1f
  FP_UNPACK_RAW_D (B, b);
  FP_CMP_D (r, A, B, -2, 2);
80017918:	7ff00e93          	li	t4,2047
  FP_UNPACK_RAW_D (B, b);
8001791c:	0146d593          	srli	a1,a3,0x14
80017920:	00d77733          	and	a4,a4,a3
80017924:	00060e13          	mv	t3,a2
80017928:	7ff5f593          	andi	a1,a1,2047
8001792c:	01f6d693          	srli	a3,a3,0x1f
  FP_CMP_D (r, A, B, -2, 2);
80017930:	01d81a63          	bne	a6,t4,80017944 <__gedf2+0x4c>
80017934:	00f8eeb3          	or	t4,a7,a5
80017938:	080e8a63          	beqz	t4,800179cc <__gedf2+0xd4>
8001793c:	ffe00513          	li	a0,-2
80017940:	00008067          	ret
80017944:	01d59663          	bne	a1,t4,80017950 <__gedf2+0x58>
80017948:	00c76eb3          	or	t4,a4,a2
8001794c:	fe0e98e3          	bnez	t4,8001793c <__gedf2+0x44>
80017950:	08081063          	bnez	a6,800179d0 <__gedf2+0xd8>
80017954:	00f8e7b3          	or	a5,a7,a5
80017958:	0017b793          	seqz	a5,a5
8001795c:	00059663          	bnez	a1,80017968 <__gedf2+0x70>
80017960:	00c76633          	or	a2,a4,a2
80017964:	04060c63          	beqz	a2,800179bc <__gedf2+0xc4>
80017968:	00079c63          	bnez	a5,80017980 <__gedf2+0x88>
8001796c:	00d51463          	bne	a0,a3,80017974 <__gedf2+0x7c>
80017970:	0305d063          	bge	a1,a6,80017990 <__gedf2+0x98>
80017974:	04050063          	beqz	a0,800179b4 <__gedf2+0xbc>
80017978:	fff00513          	li	a0,-1
8001797c:	00008067          	ret
80017980:	fff00513          	li	a0,-1
80017984:	00068a63          	beqz	a3,80017998 <__gedf2+0xa0>
80017988:	00068513          	mv	a0,a3
8001798c:	00008067          	ret
80017990:	00b85663          	bge	a6,a1,8001799c <__gedf2+0xa4>
80017994:	fe0502e3          	beqz	a0,80017978 <__gedf2+0x80>
  FP_HANDLE_EXCEPTIONS;

  return r;
}
80017998:	00008067          	ret
  FP_CMP_D (r, A, B, -2, 2);
8001799c:	fd176ce3          	bltu	a4,a7,80017974 <__gedf2+0x7c>
800179a0:	02e89263          	bne	a7,a4,800179c4 <__gedf2+0xcc>
800179a4:	fc6e68e3          	bltu	t3,t1,80017974 <__gedf2+0x7c>
800179a8:	ffc366e3          	bltu	t1,t3,80017994 <__gedf2+0x9c>
800179ac:	00000513          	li	a0,0
800179b0:	00008067          	ret
800179b4:	00100513          	li	a0,1
800179b8:	00008067          	ret
800179bc:	fe0798e3          	bnez	a5,800179ac <__gedf2+0xb4>
800179c0:	fb5ff06f          	j	80017974 <__gedf2+0x7c>
800179c4:	fce8e8e3          	bltu	a7,a4,80017994 <__gedf2+0x9c>
800179c8:	fe5ff06f          	j	800179ac <__gedf2+0xb4>
800179cc:	f7058ee3          	beq	a1,a6,80017948 <__gedf2+0x50>
800179d0:	f8059ee3          	bnez	a1,8001796c <__gedf2+0x74>
800179d4:	00000793          	li	a5,0
800179d8:	f89ff06f          	j	80017960 <__gedf2+0x68>

800179dc <__ledf2>:
  FP_DECL_D (A);
  FP_DECL_D (B);
  CMPtype r;

  FP_INIT_EXCEPTIONS;
  FP_UNPACK_RAW_D (A, a);
800179dc:	00100737          	lui	a4,0x100
800179e0:	fff70713          	addi	a4,a4,-1 # fffff <font6x8-0x7ff0002d>
800179e4:	0145d813          	srli	a6,a1,0x14
800179e8:	00b778b3          	and	a7,a4,a1
{
800179ec:	00050793          	mv	a5,a0
800179f0:	00050313          	mv	t1,a0
  FP_UNPACK_RAW_D (A, a);
800179f4:	7ff87813          	andi	a6,a6,2047
800179f8:	01f5d513          	srli	a0,a1,0x1f
  FP_UNPACK_RAW_D (B, b);
  FP_CMP_D (r, A, B, 2, 2);
800179fc:	7ff00e93          	li	t4,2047
  FP_UNPACK_RAW_D (B, b);
80017a00:	0146d593          	srli	a1,a3,0x14
80017a04:	00d77733          	and	a4,a4,a3
80017a08:	00060e13          	mv	t3,a2
80017a0c:	7ff5f593          	andi	a1,a1,2047
80017a10:	01f6d693          	srli	a3,a3,0x1f
  FP_CMP_D (r, A, B, 2, 2);
80017a14:	01d81a63          	bne	a6,t4,80017a28 <__ledf2+0x4c>
80017a18:	00f8eeb3          	or	t4,a7,a5
80017a1c:	080e8a63          	beqz	t4,80017ab0 <__ledf2+0xd4>
80017a20:	00200513          	li	a0,2
80017a24:	00008067          	ret
80017a28:	01d59663          	bne	a1,t4,80017a34 <__ledf2+0x58>
80017a2c:	00c76eb3          	or	t4,a4,a2
80017a30:	fe0e98e3          	bnez	t4,80017a20 <__ledf2+0x44>
80017a34:	08081063          	bnez	a6,80017ab4 <__ledf2+0xd8>
80017a38:	00f8e7b3          	or	a5,a7,a5
80017a3c:	0017b793          	seqz	a5,a5
80017a40:	00059663          	bnez	a1,80017a4c <__ledf2+0x70>
80017a44:	00c76633          	or	a2,a4,a2
80017a48:	04060c63          	beqz	a2,80017aa0 <__ledf2+0xc4>
80017a4c:	00079c63          	bnez	a5,80017a64 <__ledf2+0x88>
80017a50:	00d51463          	bne	a0,a3,80017a58 <__ledf2+0x7c>
80017a54:	0305d063          	bge	a1,a6,80017a74 <__ledf2+0x98>
80017a58:	04050063          	beqz	a0,80017a98 <__ledf2+0xbc>
80017a5c:	fff00513          	li	a0,-1
80017a60:	00008067          	ret
80017a64:	fff00513          	li	a0,-1
80017a68:	00068a63          	beqz	a3,80017a7c <__ledf2+0xa0>
80017a6c:	00068513          	mv	a0,a3
80017a70:	00008067          	ret
80017a74:	00b85663          	bge	a6,a1,80017a80 <__ledf2+0xa4>
80017a78:	fe0502e3          	beqz	a0,80017a5c <__ledf2+0x80>
  FP_HANDLE_EXCEPTIONS;

  return r;
}
80017a7c:	00008067          	ret
  FP_CMP_D (r, A, B, 2, 2);
80017a80:	fd176ce3          	bltu	a4,a7,80017a58 <__ledf2+0x7c>
80017a84:	02e89263          	bne	a7,a4,80017aa8 <__ledf2+0xcc>
80017a88:	fc6e68e3          	bltu	t3,t1,80017a58 <__ledf2+0x7c>
80017a8c:	ffc366e3          	bltu	t1,t3,80017a78 <__ledf2+0x9c>
80017a90:	00000513          	li	a0,0
80017a94:	00008067          	ret
80017a98:	00100513          	li	a0,1
80017a9c:	00008067          	ret
80017aa0:	fe0798e3          	bnez	a5,80017a90 <__ledf2+0xb4>
80017aa4:	fb5ff06f          	j	80017a58 <__ledf2+0x7c>
80017aa8:	fce8e8e3          	bltu	a7,a4,80017a78 <__ledf2+0x9c>
80017aac:	fe5ff06f          	j	80017a90 <__ledf2+0xb4>
80017ab0:	f7058ee3          	beq	a1,a6,80017a2c <__ledf2+0x50>
80017ab4:	f8059ee3          	bnez	a1,80017a50 <__ledf2+0x74>
80017ab8:	00000793          	li	a5,0
80017abc:	f89ff06f          	j	80017a44 <__ledf2+0x68>

80017ac0 <__muldf3>:
#include "soft-fp.h"
#include "double.h"

DFtype
__muldf3 (DFtype a, DFtype b)
{
80017ac0:	fd010113          	addi	sp,sp,-48
  FP_DECL_D (B);
  FP_DECL_D (R);
  DFtype r;

  FP_INIT_ROUNDMODE;
  FP_UNPACK_D (A, a);
80017ac4:	0145d793          	srli	a5,a1,0x14
{
80017ac8:	02812423          	sw	s0,40(sp)
80017acc:	02912223          	sw	s1,36(sp)
80017ad0:	01312e23          	sw	s3,28(sp)
80017ad4:	01412c23          	sw	s4,24(sp)
80017ad8:	01512a23          	sw	s5,20(sp)
  FP_UNPACK_D (A, a);
80017adc:	00c59493          	slli	s1,a1,0xc
{
80017ae0:	02112623          	sw	ra,44(sp)
80017ae4:	03212023          	sw	s2,32(sp)
80017ae8:	01612823          	sw	s6,16(sp)
80017aec:	01712623          	sw	s7,12(sp)
  FP_UNPACK_D (A, a);
80017af0:	01579713          	slli	a4,a5,0x15
{
80017af4:	00050413          	mv	s0,a0
80017af8:	00060993          	mv	s3,a2
80017afc:	00068a93          	mv	s5,a3
  FP_UNPACK_D (A, a);
80017b00:	00c4d493          	srli	s1,s1,0xc
80017b04:	01f5da13          	srli	s4,a1,0x1f
80017b08:	0a070663          	beqz	a4,80017bb4 <__muldf3+0xf4>
80017b0c:	7ff7fb13          	andi	s6,a5,2047
80017b10:	7ff00793          	li	a5,2047
80017b14:	10fb0263          	beq	s6,a5,80017c18 <__muldf3+0x158>
80017b18:	01d55793          	srli	a5,a0,0x1d
80017b1c:	00349493          	slli	s1,s1,0x3
80017b20:	0097e4b3          	or	s1,a5,s1
80017b24:	008007b7          	lui	a5,0x800
80017b28:	00f4e4b3          	or	s1,s1,a5
80017b2c:	00351913          	slli	s2,a0,0x3
80017b30:	c01b0b13          	addi	s6,s6,-1023
80017b34:	00000b93          	li	s7,0
  FP_UNPACK_D (B, b);
80017b38:	014ad793          	srli	a5,s5,0x14
80017b3c:	00ca9413          	slli	s0,s5,0xc
80017b40:	01579713          	slli	a4,a5,0x15
80017b44:	00c45413          	srli	s0,s0,0xc
80017b48:	7ff7f513          	andi	a0,a5,2047
80017b4c:	01fada93          	srli	s5,s5,0x1f
80017b50:	10070063          	beqz	a4,80017c50 <__muldf3+0x190>
80017b54:	7ff00793          	li	a5,2047
80017b58:	16f50063          	beq	a0,a5,80017cb8 <__muldf3+0x1f8>
80017b5c:	01d9d793          	srli	a5,s3,0x1d
80017b60:	00341413          	slli	s0,s0,0x3
80017b64:	0087e433          	or	s0,a5,s0
80017b68:	008007b7          	lui	a5,0x800
80017b6c:	00f46433          	or	s0,s0,a5
80017b70:	c0150513          	addi	a0,a0,-1023
80017b74:	00399793          	slli	a5,s3,0x3
80017b78:	00000713          	li	a4,0
  FP_MUL_D (R, A, B);
80017b7c:	002b9693          	slli	a3,s7,0x2
80017b80:	00e6e6b3          	or	a3,a3,a4
80017b84:	00ab0b33          	add	s6,s6,a0
80017b88:	fff68693          	addi	a3,a3,-1 # 7fefffff <font6x8-0x10002d>
80017b8c:	00e00613          	li	a2,14
80017b90:	015a4833          	xor	a6,s4,s5
80017b94:	001b0893          	addi	a7,s6,1
80017b98:	14d66c63          	bltu	a2,a3,80017cf0 <__muldf3+0x230>
80017b9c:	80000637          	lui	a2,0x80000
80017ba0:	00269693          	slli	a3,a3,0x2
80017ba4:	69460613          	addi	a2,a2,1684 # 80000694 <__clzsi2+0xfffe7a00>
80017ba8:	00c686b3          	add	a3,a3,a2
80017bac:	0006a683          	lw	a3,0(a3)
80017bb0:	00068067          	jr	a3
  FP_UNPACK_D (A, a);
80017bb4:	00a4e933          	or	s2,s1,a0
80017bb8:	06090c63          	beqz	s2,80017c30 <__muldf3+0x170>
80017bbc:	04048063          	beqz	s1,80017bfc <__muldf3+0x13c>
80017bc0:	00048513          	mv	a0,s1
80017bc4:	0d0010ef          	jal	ra,80018c94 <__clzsi2>
80017bc8:	ff550713          	addi	a4,a0,-11
80017bcc:	01c00793          	li	a5,28
80017bd0:	02e7cc63          	blt	a5,a4,80017c08 <__muldf3+0x148>
80017bd4:	01d00793          	li	a5,29
80017bd8:	ff850913          	addi	s2,a0,-8
80017bdc:	40e787b3          	sub	a5,a5,a4
80017be0:	012494b3          	sll	s1,s1,s2
80017be4:	00f457b3          	srl	a5,s0,a5
80017be8:	0097e4b3          	or	s1,a5,s1
80017bec:	01241933          	sll	s2,s0,s2
80017bf0:	c0d00b13          	li	s6,-1011
80017bf4:	40ab0b33          	sub	s6,s6,a0
80017bf8:	f3dff06f          	j	80017b34 <__muldf3+0x74>
80017bfc:	098010ef          	jal	ra,80018c94 <__clzsi2>
80017c00:	02050513          	addi	a0,a0,32
80017c04:	fc5ff06f          	j	80017bc8 <__muldf3+0x108>
80017c08:	fd850493          	addi	s1,a0,-40
80017c0c:	009414b3          	sll	s1,s0,s1
80017c10:	00000913          	li	s2,0
80017c14:	fddff06f          	j	80017bf0 <__muldf3+0x130>
80017c18:	00a4e933          	or	s2,s1,a0
80017c1c:	02090263          	beqz	s2,80017c40 <__muldf3+0x180>
80017c20:	00050913          	mv	s2,a0
80017c24:	7ff00b13          	li	s6,2047
80017c28:	00300b93          	li	s7,3
80017c2c:	f0dff06f          	j	80017b38 <__muldf3+0x78>
80017c30:	00000493          	li	s1,0
80017c34:	00000b13          	li	s6,0
80017c38:	00100b93          	li	s7,1
80017c3c:	efdff06f          	j	80017b38 <__muldf3+0x78>
80017c40:	00000493          	li	s1,0
80017c44:	7ff00b13          	li	s6,2047
80017c48:	00200b93          	li	s7,2
80017c4c:	eedff06f          	j	80017b38 <__muldf3+0x78>
  FP_UNPACK_D (B, b);
80017c50:	013467b3          	or	a5,s0,s3
80017c54:	06078e63          	beqz	a5,80017cd0 <__muldf3+0x210>
80017c58:	04040063          	beqz	s0,80017c98 <__muldf3+0x1d8>
80017c5c:	00040513          	mv	a0,s0
80017c60:	034010ef          	jal	ra,80018c94 <__clzsi2>
80017c64:	ff550693          	addi	a3,a0,-11
80017c68:	01c00793          	li	a5,28
80017c6c:	02d7ce63          	blt	a5,a3,80017ca8 <__muldf3+0x1e8>
80017c70:	01d00713          	li	a4,29
80017c74:	ff850793          	addi	a5,a0,-8
80017c78:	40d70733          	sub	a4,a4,a3
80017c7c:	00f41433          	sll	s0,s0,a5
80017c80:	00e9d733          	srl	a4,s3,a4
80017c84:	00876433          	or	s0,a4,s0
80017c88:	00f997b3          	sll	a5,s3,a5
80017c8c:	c0d00713          	li	a4,-1011
80017c90:	40a70533          	sub	a0,a4,a0
80017c94:	ee5ff06f          	j	80017b78 <__muldf3+0xb8>
80017c98:	00098513          	mv	a0,s3
80017c9c:	7f9000ef          	jal	ra,80018c94 <__clzsi2>
80017ca0:	02050513          	addi	a0,a0,32
80017ca4:	fc1ff06f          	j	80017c64 <__muldf3+0x1a4>
80017ca8:	fd850413          	addi	s0,a0,-40
80017cac:	00899433          	sll	s0,s3,s0
80017cb0:	00000793          	li	a5,0
80017cb4:	fd9ff06f          	j	80017c8c <__muldf3+0x1cc>
80017cb8:	013467b3          	or	a5,s0,s3
80017cbc:	02078263          	beqz	a5,80017ce0 <__muldf3+0x220>
80017cc0:	00098793          	mv	a5,s3
80017cc4:	7ff00513          	li	a0,2047
80017cc8:	00300713          	li	a4,3
80017ccc:	eb1ff06f          	j	80017b7c <__muldf3+0xbc>
80017cd0:	00000413          	li	s0,0
80017cd4:	00000513          	li	a0,0
80017cd8:	00100713          	li	a4,1
80017cdc:	ea1ff06f          	j	80017b7c <__muldf3+0xbc>
80017ce0:	00000413          	li	s0,0
80017ce4:	7ff00513          	li	a0,2047
80017ce8:	00200713          	li	a4,2
80017cec:	e91ff06f          	j	80017b7c <__muldf3+0xbc>
  FP_MUL_D (R, A, B);
80017cf0:	00010ab7          	lui	s5,0x10
80017cf4:	fffa8313          	addi	t1,s5,-1 # ffff <font6x8-0x7fff002d>
80017cf8:	01095f93          	srli	t6,s2,0x10
80017cfc:	0067f9b3          	and	s3,a5,t1
80017d00:	00697933          	and	s2,s2,t1
80017d04:	0107d293          	srli	t0,a5,0x10
80017d08:	00090513          	mv	a0,s2
80017d0c:	00098593          	mv	a1,s3
80017d10:	61d000ef          	jal	ra,80018b2c <__mulsi3>
80017d14:	00050a13          	mv	s4,a0
80017d18:	00028593          	mv	a1,t0
80017d1c:	00090513          	mv	a0,s2
80017d20:	60d000ef          	jal	ra,80018b2c <__mulsi3>
80017d24:	00050e93          	mv	t4,a0
80017d28:	00098593          	mv	a1,s3
80017d2c:	000f8513          	mv	a0,t6
80017d30:	5fd000ef          	jal	ra,80018b2c <__mulsi3>
80017d34:	00050393          	mv	t2,a0
80017d38:	00028593          	mv	a1,t0
80017d3c:	000f8513          	mv	a0,t6
80017d40:	5ed000ef          	jal	ra,80018b2c <__mulsi3>
80017d44:	010a5713          	srli	a4,s4,0x10
80017d48:	007e8eb3          	add	t4,t4,t2
80017d4c:	01d70733          	add	a4,a4,t4
80017d50:	00050f13          	mv	t5,a0
80017d54:	00777463          	bgeu	a4,t2,80017d5c <__muldf3+0x29c>
80017d58:	01550f33          	add	t5,a0,s5
80017d5c:	00677e33          	and	t3,a4,t1
80017d60:	006a7533          	and	a0,s4,t1
80017d64:	01045393          	srli	t2,s0,0x10
80017d68:	010e1e13          	slli	t3,t3,0x10
80017d6c:	00647433          	and	s0,s0,t1
80017d70:	00ae0e33          	add	t3,t3,a0
80017d74:	01075793          	srli	a5,a4,0x10
80017d78:	00090513          	mv	a0,s2
80017d7c:	00040593          	mv	a1,s0
80017d80:	5ad000ef          	jal	ra,80018b2c <__mulsi3>
80017d84:	00050e93          	mv	t4,a0
80017d88:	00038593          	mv	a1,t2
80017d8c:	00090513          	mv	a0,s2
80017d90:	59d000ef          	jal	ra,80018b2c <__mulsi3>
80017d94:	00050313          	mv	t1,a0
80017d98:	00040593          	mv	a1,s0
80017d9c:	000f8513          	mv	a0,t6
80017da0:	58d000ef          	jal	ra,80018b2c <__mulsi3>
80017da4:	00050913          	mv	s2,a0
80017da8:	00038593          	mv	a1,t2
80017dac:	000f8513          	mv	a0,t6
80017db0:	57d000ef          	jal	ra,80018b2c <__mulsi3>
80017db4:	010ed693          	srli	a3,t4,0x10
80017db8:	01230333          	add	t1,t1,s2
80017dbc:	006686b3          	add	a3,a3,t1
80017dc0:	00050713          	mv	a4,a0
80017dc4:	0126f663          	bgeu	a3,s2,80017dd0 <__muldf3+0x310>
80017dc8:	00010637          	lui	a2,0x10
80017dcc:	00c50733          	add	a4,a0,a2
80017dd0:	0106df93          	srli	t6,a3,0x10
80017dd4:	00010bb7          	lui	s7,0x10
80017dd8:	00ef8fb3          	add	t6,t6,a4
80017ddc:	fffb8713          	addi	a4,s7,-1 # ffff <font6x8-0x7fff002d>
80017de0:	00e6f6b3          	and	a3,a3,a4
80017de4:	01069693          	slli	a3,a3,0x10
80017de8:	00eefeb3          	and	t4,t4,a4
80017dec:	01d68eb3          	add	t4,a3,t4
80017df0:	0104da93          	srli	s5,s1,0x10
80017df4:	00e4f4b3          	and	s1,s1,a4
80017df8:	01d78933          	add	s2,a5,t4
80017dfc:	00048513          	mv	a0,s1
80017e00:	00098593          	mv	a1,s3
80017e04:	529000ef          	jal	ra,80018b2c <__mulsi3>
80017e08:	00050a13          	mv	s4,a0
80017e0c:	00028593          	mv	a1,t0
80017e10:	00048513          	mv	a0,s1
80017e14:	519000ef          	jal	ra,80018b2c <__mulsi3>
80017e18:	00050713          	mv	a4,a0
80017e1c:	00098593          	mv	a1,s3
80017e20:	000a8513          	mv	a0,s5
80017e24:	509000ef          	jal	ra,80018b2c <__mulsi3>
80017e28:	00050313          	mv	t1,a0
80017e2c:	00028593          	mv	a1,t0
80017e30:	000a8513          	mv	a0,s5
80017e34:	4f9000ef          	jal	ra,80018b2c <__mulsi3>
80017e38:	010a5793          	srli	a5,s4,0x10
80017e3c:	00670733          	add	a4,a4,t1
80017e40:	00e787b3          	add	a5,a5,a4
80017e44:	00050293          	mv	t0,a0
80017e48:	0067f463          	bgeu	a5,t1,80017e50 <__muldf3+0x390>
80017e4c:	017502b3          	add	t0,a0,s7
80017e50:	0107d513          	srli	a0,a5,0x10
80017e54:	00010bb7          	lui	s7,0x10
80017e58:	005502b3          	add	t0,a0,t0
80017e5c:	fffb8513          	addi	a0,s7,-1 # ffff <font6x8-0x7fff002d>
80017e60:	00a7f333          	and	t1,a5,a0
80017e64:	00aa7a33          	and	s4,s4,a0
80017e68:	01031313          	slli	t1,t1,0x10
80017e6c:	01430333          	add	t1,t1,s4
80017e70:	00048513          	mv	a0,s1
80017e74:	00040593          	mv	a1,s0
80017e78:	4b5000ef          	jal	ra,80018b2c <__mulsi3>
80017e7c:	00050993          	mv	s3,a0
80017e80:	00038593          	mv	a1,t2
80017e84:	00048513          	mv	a0,s1
80017e88:	4a5000ef          	jal	ra,80018b2c <__mulsi3>
80017e8c:	00050793          	mv	a5,a0
80017e90:	00040593          	mv	a1,s0
80017e94:	000a8513          	mv	a0,s5
80017e98:	495000ef          	jal	ra,80018b2c <__mulsi3>
80017e9c:	00050413          	mv	s0,a0
80017ea0:	00038593          	mv	a1,t2
80017ea4:	000a8513          	mv	a0,s5
80017ea8:	485000ef          	jal	ra,80018b2c <__mulsi3>
80017eac:	0109d593          	srli	a1,s3,0x10
80017eb0:	008787b3          	add	a5,a5,s0
80017eb4:	00f585b3          	add	a1,a1,a5
80017eb8:	0085f463          	bgeu	a1,s0,80017ec0 <__muldf3+0x400>
80017ebc:	01750533          	add	a0,a0,s7
80017ec0:	00010737          	lui	a4,0x10
80017ec4:	fff70713          	addi	a4,a4,-1 # ffff <font6x8-0x7fff002d>
80017ec8:	00e5f6b3          	and	a3,a1,a4
80017ecc:	01069693          	slli	a3,a3,0x10
80017ed0:	00e9f733          	and	a4,s3,a4
80017ed4:	01e90633          	add	a2,s2,t5
80017ed8:	00e686b3          	add	a3,a3,a4
80017edc:	01d63eb3          	sltu	t4,a2,t4
80017ee0:	01f68733          	add	a4,a3,t6
80017ee4:	01d706b3          	add	a3,a4,t4
80017ee8:	00660633          	add	a2,a2,t1
80017eec:	00663333          	sltu	t1,a2,t1
80017ef0:	00568f33          	add	t5,a3,t0
80017ef4:	006f03b3          	add	t2,t5,t1
80017ef8:	01f73733          	sltu	a4,a4,t6
80017efc:	01d6beb3          	sltu	t4,a3,t4
80017f00:	01d76733          	or	a4,a4,t4
80017f04:	0105d593          	srli	a1,a1,0x10
80017f08:	005f32b3          	sltu	t0,t5,t0
80017f0c:	0063b333          	sltu	t1,t2,t1
80017f10:	00961793          	slli	a5,a2,0x9
80017f14:	00b70733          	add	a4,a4,a1
80017f18:	0062e333          	or	t1,t0,t1
80017f1c:	00670733          	add	a4,a4,t1
80017f20:	01c7e7b3          	or	a5,a5,t3
80017f24:	00a70533          	add	a0,a4,a0
80017f28:	00f037b3          	snez	a5,a5
80017f2c:	01765613          	srli	a2,a2,0x17
80017f30:	00951513          	slli	a0,a0,0x9
80017f34:	00939713          	slli	a4,t2,0x9
80017f38:	00c7e7b3          	or	a5,a5,a2
80017f3c:	0173d413          	srli	s0,t2,0x17
80017f40:	00e7e7b3          	or	a5,a5,a4
80017f44:	00751713          	slli	a4,a0,0x7
80017f48:	00856433          	or	s0,a0,s0
80017f4c:	10075463          	bgez	a4,80018054 <__muldf3+0x594>
80017f50:	0017d713          	srli	a4,a5,0x1
80017f54:	0017f793          	andi	a5,a5,1
80017f58:	00f767b3          	or	a5,a4,a5
80017f5c:	01f41713          	slli	a4,s0,0x1f
80017f60:	00e7e7b3          	or	a5,a5,a4
80017f64:	00145413          	srli	s0,s0,0x1
  FP_PACK_D (r, R);
80017f68:	3ff88693          	addi	a3,a7,1023
80017f6c:	0ed05863          	blez	a3,8001805c <__muldf3+0x59c>
80017f70:	0077f713          	andi	a4,a5,7
80017f74:	02070063          	beqz	a4,80017f94 <__muldf3+0x4d4>
80017f78:	00f7f713          	andi	a4,a5,15
80017f7c:	00400613          	li	a2,4
80017f80:	00c70a63          	beq	a4,a2,80017f94 <__muldf3+0x4d4>
80017f84:	00478713          	addi	a4,a5,4 # 800004 <font6x8-0x7f800028>
80017f88:	00f737b3          	sltu	a5,a4,a5
80017f8c:	00f40433          	add	s0,s0,a5
80017f90:	00070793          	mv	a5,a4
80017f94:	00741713          	slli	a4,s0,0x7
80017f98:	00075a63          	bgez	a4,80017fac <__muldf3+0x4ec>
80017f9c:	ff000737          	lui	a4,0xff000
80017fa0:	fff70713          	addi	a4,a4,-1 # feffffff <__clzsi2+0x7efe736b>
80017fa4:	00e47433          	and	s0,s0,a4
80017fa8:	40088693          	addi	a3,a7,1024
80017fac:	7fe00713          	li	a4,2046
80017fb0:	16d74863          	blt	a4,a3,80018120 <__muldf3+0x660>
80017fb4:	0037d713          	srli	a4,a5,0x3
80017fb8:	01d41793          	slli	a5,s0,0x1d
80017fbc:	00e7e7b3          	or	a5,a5,a4
80017fc0:	00345413          	srli	s0,s0,0x3
80017fc4:	01469713          	slli	a4,a3,0x14
80017fc8:	00c41413          	slli	s0,s0,0xc
80017fcc:	7ff006b7          	lui	a3,0x7ff00
80017fd0:	00d77733          	and	a4,a4,a3
80017fd4:	00c45413          	srli	s0,s0,0xc
80017fd8:	00876433          	or	s0,a4,s0
80017fdc:	01f81813          	slli	a6,a6,0x1f
  FP_HANDLE_EXCEPTIONS;

  return r;
}
80017fe0:	02c12083          	lw	ra,44(sp)
  FP_PACK_D (r, R);
80017fe4:	01046733          	or	a4,s0,a6
}
80017fe8:	02812403          	lw	s0,40(sp)
80017fec:	02412483          	lw	s1,36(sp)
80017ff0:	02012903          	lw	s2,32(sp)
80017ff4:	01c12983          	lw	s3,28(sp)
80017ff8:	01812a03          	lw	s4,24(sp)
80017ffc:	01412a83          	lw	s5,20(sp)
80018000:	01012b03          	lw	s6,16(sp)
80018004:	00c12b83          	lw	s7,12(sp)
80018008:	00078513          	mv	a0,a5
8001800c:	00070593          	mv	a1,a4
80018010:	03010113          	addi	sp,sp,48
80018014:	00008067          	ret
  FP_UNPACK_D (A, a);
80018018:	000a0813          	mv	a6,s4
  FP_MUL_D (R, A, B);
8001801c:	00048413          	mv	s0,s1
80018020:	00090793          	mv	a5,s2
80018024:	000b8713          	mv	a4,s7
  FP_PACK_D (r, R);
80018028:	00200693          	li	a3,2
8001802c:	0ed70a63          	beq	a4,a3,80018120 <__muldf3+0x660>
80018030:	00300693          	li	a3,3
80018034:	0cd70c63          	beq	a4,a3,8001810c <__muldf3+0x64c>
80018038:	00100693          	li	a3,1
8001803c:	f2d716e3          	bne	a4,a3,80017f68 <__muldf3+0x4a8>
80018040:	00000413          	li	s0,0
80018044:	00000793          	li	a5,0
80018048:	0880006f          	j	800180d0 <__muldf3+0x610>
  FP_UNPACK_D (B, b);
8001804c:	000a8813          	mv	a6,s5
  FP_MUL_D (R, A, B);
80018050:	fd9ff06f          	j	80018028 <__muldf3+0x568>
80018054:	000b0893          	mv	a7,s6
80018058:	f11ff06f          	j	80017f68 <__muldf3+0x4a8>
  FP_PACK_D (r, R);
8001805c:	00100613          	li	a2,1
80018060:	40d60633          	sub	a2,a2,a3
80018064:	03800713          	li	a4,56
80018068:	fcc74ce3          	blt	a4,a2,80018040 <__muldf3+0x580>
8001806c:	01f00713          	li	a4,31
80018070:	06c74463          	blt	a4,a2,800180d8 <__muldf3+0x618>
80018074:	41e88893          	addi	a7,a7,1054
80018078:	01141733          	sll	a4,s0,a7
8001807c:	00c7d6b3          	srl	a3,a5,a2
80018080:	011797b3          	sll	a5,a5,a7
80018084:	00d76733          	or	a4,a4,a3
80018088:	00f037b3          	snez	a5,a5
8001808c:	00f767b3          	or	a5,a4,a5
80018090:	00c45433          	srl	s0,s0,a2
80018094:	0077f713          	andi	a4,a5,7
80018098:	02070063          	beqz	a4,800180b8 <__muldf3+0x5f8>
8001809c:	00f7f713          	andi	a4,a5,15
800180a0:	00400693          	li	a3,4
800180a4:	00d70a63          	beq	a4,a3,800180b8 <__muldf3+0x5f8>
800180a8:	00478713          	addi	a4,a5,4
800180ac:	00f737b3          	sltu	a5,a4,a5
800180b0:	00f40433          	add	s0,s0,a5
800180b4:	00070793          	mv	a5,a4
800180b8:	00841713          	slli	a4,s0,0x8
800180bc:	06074a63          	bltz	a4,80018130 <__muldf3+0x670>
800180c0:	01d41713          	slli	a4,s0,0x1d
800180c4:	0037d793          	srli	a5,a5,0x3
800180c8:	00f767b3          	or	a5,a4,a5
800180cc:	00345413          	srli	s0,s0,0x3
800180d0:	00000693          	li	a3,0
800180d4:	ef1ff06f          	j	80017fc4 <__muldf3+0x504>
800180d8:	fe100713          	li	a4,-31
800180dc:	40d70733          	sub	a4,a4,a3
800180e0:	02000593          	li	a1,32
800180e4:	00e45733          	srl	a4,s0,a4
800180e8:	00000693          	li	a3,0
800180ec:	00b60663          	beq	a2,a1,800180f8 <__muldf3+0x638>
800180f0:	43e88893          	addi	a7,a7,1086
800180f4:	011416b3          	sll	a3,s0,a7
800180f8:	00f6e7b3          	or	a5,a3,a5
800180fc:	00f037b3          	snez	a5,a5
80018100:	00f767b3          	or	a5,a4,a5
80018104:	00000413          	li	s0,0
80018108:	f8dff06f          	j	80018094 <__muldf3+0x5d4>
8001810c:	00080437          	lui	s0,0x80
80018110:	00000793          	li	a5,0
80018114:	7ff00693          	li	a3,2047
80018118:	00000813          	li	a6,0
8001811c:	ea9ff06f          	j	80017fc4 <__muldf3+0x504>
80018120:	00000413          	li	s0,0
80018124:	00000793          	li	a5,0
80018128:	7ff00693          	li	a3,2047
8001812c:	e99ff06f          	j	80017fc4 <__muldf3+0x504>
80018130:	00000413          	li	s0,0
80018134:	00000793          	li	a5,0
80018138:	00100693          	li	a3,1
8001813c:	e89ff06f          	j	80017fc4 <__muldf3+0x504>

80018140 <__subdf3>:
  FP_DECL_D (B);
  FP_DECL_D (R);
  DFtype r;

  FP_INIT_ROUNDMODE;
  FP_UNPACK_SEMIRAW_D (A, a);
80018140:	00100837          	lui	a6,0x100
80018144:	fff80813          	addi	a6,a6,-1 # fffff <font6x8-0x7ff0002d>
{
80018148:	fe010113          	addi	sp,sp,-32
  FP_UNPACK_SEMIRAW_D (A, a);
8001814c:	00b877b3          	and	a5,a6,a1
80018150:	0145d713          	srli	a4,a1,0x14
  FP_UNPACK_SEMIRAW_D (B, b);
80018154:	00d87833          	and	a6,a6,a3
80018158:	0146d893          	srli	a7,a3,0x14
{
8001815c:	00812c23          	sw	s0,24(sp)
  FP_UNPACK_SEMIRAW_D (A, a);
80018160:	00379793          	slli	a5,a5,0x3
80018164:	7ff77413          	andi	s0,a4,2047
80018168:	01d55713          	srli	a4,a0,0x1d
{
8001816c:	00912a23          	sw	s1,20(sp)
  FP_UNPACK_SEMIRAW_D (A, a);
80018170:	00f767b3          	or	a5,a4,a5
80018174:	01f5d493          	srli	s1,a1,0x1f
  FP_UNPACK_SEMIRAW_D (B, b);
80018178:	01d65713          	srli	a4,a2,0x1d
8001817c:	01f6d593          	srli	a1,a3,0x1f
80018180:	00381813          	slli	a6,a6,0x3
{
80018184:	00112e23          	sw	ra,28(sp)
80018188:	01212823          	sw	s2,16(sp)
8001818c:	01312623          	sw	s3,12(sp)
  FP_UNPACK_SEMIRAW_D (B, b);
80018190:	7ff8f893          	andi	a7,a7,2047
  FP_SUB_D (R, A, B);
80018194:	7ff00693          	li	a3,2047
  FP_UNPACK_SEMIRAW_D (A, a);
80018198:	00351513          	slli	a0,a0,0x3
  FP_UNPACK_SEMIRAW_D (B, b);
8001819c:	01076733          	or	a4,a4,a6
800181a0:	00361613          	slli	a2,a2,0x3
  FP_SUB_D (R, A, B);
800181a4:	00d89663          	bne	a7,a3,800181b0 <__subdf3+0x70>
800181a8:	00c766b3          	or	a3,a4,a2
800181ac:	00069463          	bnez	a3,800181b4 <__subdf3+0x74>
800181b0:	0015c593          	xori	a1,a1,1
800181b4:	41140833          	sub	a6,s0,a7
800181b8:	2c959063          	bne	a1,s1,80018478 <__subdf3+0x338>
800181bc:	13005c63          	blez	a6,800182f4 <__subdf3+0x1b4>
800181c0:	06089063          	bnez	a7,80018220 <__subdf3+0xe0>
800181c4:	00c766b3          	or	a3,a4,a2
800181c8:	02068863          	beqz	a3,800181f8 <__subdf3+0xb8>
800181cc:	fff40813          	addi	a6,s0,-1 # 7ffff <font6x8-0x7ff8002d>
800181d0:	02081063          	bnez	a6,800181f0 <__subdf3+0xb0>
800181d4:	00c50633          	add	a2,a0,a2
800181d8:	00a63533          	sltu	a0,a2,a0
800181dc:	00e787b3          	add	a5,a5,a4
800181e0:	00a787b3          	add	a5,a5,a0
800181e4:	00060513          	mv	a0,a2
  FP_UNPACK_SEMIRAW_D (B, b);
800181e8:	00100413          	li	s0,1
  FP_SUB_D (R, A, B);
800181ec:	08c0006f          	j	80018278 <__subdf3+0x138>
800181f0:	7ff00693          	li	a3,2047
800181f4:	02d41e63          	bne	s0,a3,80018230 <__subdf3+0xf0>
  FP_PACK_SEMIRAW_D (r, R);
800181f8:	00757713          	andi	a4,a0,7
800181fc:	62070c63          	beqz	a4,80018834 <__subdf3+0x6f4>
80018200:	00f57713          	andi	a4,a0,15
80018204:	00400693          	li	a3,4
80018208:	62d70663          	beq	a4,a3,80018834 <__subdf3+0x6f4>
8001820c:	00450693          	addi	a3,a0,4
80018210:	00a6b533          	sltu	a0,a3,a0
80018214:	00a787b3          	add	a5,a5,a0
80018218:	00068513          	mv	a0,a3
8001821c:	6180006f          	j	80018834 <__subdf3+0x6f4>
  FP_SUB_D (R, A, B);
80018220:	7ff00693          	li	a3,2047
80018224:	fcd40ae3          	beq	s0,a3,800181f8 <__subdf3+0xb8>
80018228:	008006b7          	lui	a3,0x800
8001822c:	00d76733          	or	a4,a4,a3
80018230:	03800693          	li	a3,56
80018234:	0b06ca63          	blt	a3,a6,800182e8 <__subdf3+0x1a8>
80018238:	01f00693          	li	a3,31
8001823c:	0706cc63          	blt	a3,a6,800182b4 <__subdf3+0x174>
80018240:	02000593          	li	a1,32
80018244:	410585b3          	sub	a1,a1,a6
80018248:	00b716b3          	sll	a3,a4,a1
8001824c:	010658b3          	srl	a7,a2,a6
80018250:	00b61633          	sll	a2,a2,a1
80018254:	0116e6b3          	or	a3,a3,a7
80018258:	00c03633          	snez	a2,a2
8001825c:	00c6e633          	or	a2,a3,a2
80018260:	01075733          	srl	a4,a4,a6
80018264:	00a60633          	add	a2,a2,a0
80018268:	00f70733          	add	a4,a4,a5
8001826c:	00a637b3          	sltu	a5,a2,a0
80018270:	00f707b3          	add	a5,a4,a5
80018274:	00060513          	mv	a0,a2
80018278:	00879713          	slli	a4,a5,0x8
8001827c:	f6075ee3          	bgez	a4,800181f8 <__subdf3+0xb8>
80018280:	00140413          	addi	s0,s0,1
80018284:	7ff00713          	li	a4,2047
80018288:	5ae40263          	beq	s0,a4,8001882c <__subdf3+0x6ec>
8001828c:	ff8006b7          	lui	a3,0xff800
80018290:	fff68693          	addi	a3,a3,-1 # ff7fffff <__clzsi2+0x7f7e736b>
80018294:	00d7f6b3          	and	a3,a5,a3
80018298:	00155593          	srli	a1,a0,0x1
8001829c:	00157513          	andi	a0,a0,1
800182a0:	01f69793          	slli	a5,a3,0x1f
800182a4:	00a5e533          	or	a0,a1,a0
800182a8:	00a7e533          	or	a0,a5,a0
800182ac:	0016d793          	srli	a5,a3,0x1
800182b0:	f49ff06f          	j	800181f8 <__subdf3+0xb8>
800182b4:	fe080693          	addi	a3,a6,-32
800182b8:	02000893          	li	a7,32
800182bc:	00d756b3          	srl	a3,a4,a3
800182c0:	00000593          	li	a1,0
800182c4:	01180863          	beq	a6,a7,800182d4 <__subdf3+0x194>
800182c8:	04000593          	li	a1,64
800182cc:	41058833          	sub	a6,a1,a6
800182d0:	010715b3          	sll	a1,a4,a6
800182d4:	00c5e633          	or	a2,a1,a2
800182d8:	00c03633          	snez	a2,a2
800182dc:	00c6e633          	or	a2,a3,a2
800182e0:	00000713          	li	a4,0
800182e4:	f81ff06f          	j	80018264 <__subdf3+0x124>
800182e8:	00c76633          	or	a2,a4,a2
800182ec:	00c03633          	snez	a2,a2
800182f0:	ff1ff06f          	j	800182e0 <__subdf3+0x1a0>
800182f4:	0e080263          	beqz	a6,800183d8 <__subdf3+0x298>
800182f8:	408885b3          	sub	a1,a7,s0
800182fc:	02041e63          	bnez	s0,80018338 <__subdf3+0x1f8>
80018300:	00a7e6b3          	or	a3,a5,a0
80018304:	50068463          	beqz	a3,8001880c <__subdf3+0x6cc>
80018308:	fff58693          	addi	a3,a1,-1 # 7ffff <font6x8-0x7ff8002d>
8001830c:	00069c63          	bnez	a3,80018324 <__subdf3+0x1e4>
80018310:	00c50533          	add	a0,a0,a2
80018314:	00e787b3          	add	a5,a5,a4
80018318:	00c53633          	sltu	a2,a0,a2
8001831c:	00c787b3          	add	a5,a5,a2
80018320:	ec9ff06f          	j	800181e8 <__subdf3+0xa8>
80018324:	7ff00813          	li	a6,2047
80018328:	03059263          	bne	a1,a6,8001834c <__subdf3+0x20c>
  FP_UNPACK_SEMIRAW_D (B, b);
8001832c:	00070793          	mv	a5,a4
80018330:	00060513          	mv	a0,a2
80018334:	3b00006f          	j	800186e4 <__subdf3+0x5a4>
  FP_SUB_D (R, A, B);
80018338:	7ff00693          	li	a3,2047
8001833c:	fed888e3          	beq	a7,a3,8001832c <__subdf3+0x1ec>
80018340:	008006b7          	lui	a3,0x800
80018344:	00d7e7b3          	or	a5,a5,a3
80018348:	00058693          	mv	a3,a1
8001834c:	03800593          	li	a1,56
80018350:	06d5ce63          	blt	a1,a3,800183cc <__subdf3+0x28c>
80018354:	01f00593          	li	a1,31
80018358:	04d5c063          	blt	a1,a3,80018398 <__subdf3+0x258>
8001835c:	02000813          	li	a6,32
80018360:	40d80833          	sub	a6,a6,a3
80018364:	00d55333          	srl	t1,a0,a3
80018368:	010795b3          	sll	a1,a5,a6
8001836c:	01051533          	sll	a0,a0,a6
80018370:	0065e5b3          	or	a1,a1,t1
80018374:	00a03533          	snez	a0,a0
80018378:	00a5e533          	or	a0,a1,a0
8001837c:	00d7d6b3          	srl	a3,a5,a3
80018380:	00c50533          	add	a0,a0,a2
80018384:	00e687b3          	add	a5,a3,a4
80018388:	00c53633          	sltu	a2,a0,a2
8001838c:	00c787b3          	add	a5,a5,a2
  FP_UNPACK_SEMIRAW_D (B, b);
80018390:	00088413          	mv	s0,a7
80018394:	ee5ff06f          	j	80018278 <__subdf3+0x138>
  FP_SUB_D (R, A, B);
80018398:	fe068593          	addi	a1,a3,-32 # 7fffe0 <font6x8-0x7f80004c>
8001839c:	02000313          	li	t1,32
800183a0:	00b7d5b3          	srl	a1,a5,a1
800183a4:	00000813          	li	a6,0
800183a8:	00668863          	beq	a3,t1,800183b8 <__subdf3+0x278>
800183ac:	04000813          	li	a6,64
800183b0:	40d806b3          	sub	a3,a6,a3
800183b4:	00d79833          	sll	a6,a5,a3
800183b8:	00a86533          	or	a0,a6,a0
800183bc:	00a03533          	snez	a0,a0
800183c0:	00a5e533          	or	a0,a1,a0
800183c4:	00000693          	li	a3,0
800183c8:	fb9ff06f          	j	80018380 <__subdf3+0x240>
800183cc:	00a7e533          	or	a0,a5,a0
800183d0:	00a03533          	snez	a0,a0
800183d4:	ff1ff06f          	j	800183c4 <__subdf3+0x284>
800183d8:	00140693          	addi	a3,s0,1
800183dc:	7fe6f593          	andi	a1,a3,2046
800183e0:	06059463          	bnez	a1,80018448 <__subdf3+0x308>
800183e4:	00a7e6b3          	or	a3,a5,a0
800183e8:	04041063          	bnez	s0,80018428 <__subdf3+0x2e8>
800183ec:	42068863          	beqz	a3,8001881c <__subdf3+0x6dc>
800183f0:	00c766b3          	or	a3,a4,a2
800183f4:	e00682e3          	beqz	a3,800181f8 <__subdf3+0xb8>
800183f8:	00c50633          	add	a2,a0,a2
800183fc:	00e787b3          	add	a5,a5,a4
80018400:	00a63533          	sltu	a0,a2,a0
80018404:	00a787b3          	add	a5,a5,a0
80018408:	00879713          	slli	a4,a5,0x8
8001840c:	00060513          	mv	a0,a2
80018410:	de0754e3          	bgez	a4,800181f8 <__subdf3+0xb8>
80018414:	ff800737          	lui	a4,0xff800
80018418:	fff70713          	addi	a4,a4,-1 # ff7fffff <__clzsi2+0x7f7e736b>
8001841c:	00e7f7b3          	and	a5,a5,a4
80018420:	00100413          	li	s0,1
80018424:	dd5ff06f          	j	800181f8 <__subdf3+0xb8>
80018428:	f00682e3          	beqz	a3,8001832c <__subdf3+0x1ec>
8001842c:	00c76633          	or	a2,a4,a2
80018430:	2a060a63          	beqz	a2,800186e4 <__subdf3+0x5a4>
80018434:	00000493          	li	s1,0
80018438:	004007b7          	lui	a5,0x400
8001843c:	00000513          	li	a0,0
80018440:	7ff00413          	li	s0,2047
80018444:	3f00006f          	j	80018834 <__subdf3+0x6f4>
80018448:	7ff00593          	li	a1,2047
8001844c:	3cb68e63          	beq	a3,a1,80018828 <__subdf3+0x6e8>
80018450:	00c50633          	add	a2,a0,a2
80018454:	00e78733          	add	a4,a5,a4
80018458:	00a637b3          	sltu	a5,a2,a0
8001845c:	00f70733          	add	a4,a4,a5
80018460:	01f71513          	slli	a0,a4,0x1f
80018464:	00165613          	srli	a2,a2,0x1
80018468:	00c56533          	or	a0,a0,a2
8001846c:	00175793          	srli	a5,a4,0x1
80018470:	00068413          	mv	s0,a3
80018474:	d85ff06f          	j	800181f8 <__subdf3+0xb8>
80018478:	0f005a63          	blez	a6,8001856c <__subdf3+0x42c>
8001847c:	08089e63          	bnez	a7,80018518 <__subdf3+0x3d8>
80018480:	00c766b3          	or	a3,a4,a2
80018484:	d6068ae3          	beqz	a3,800181f8 <__subdf3+0xb8>
80018488:	fff40813          	addi	a6,s0,-1
8001848c:	02081063          	bnez	a6,800184ac <__subdf3+0x36c>
80018490:	40c50633          	sub	a2,a0,a2
80018494:	00c53533          	sltu	a0,a0,a2
80018498:	40e787b3          	sub	a5,a5,a4
8001849c:	40a787b3          	sub	a5,a5,a0
800184a0:	00060513          	mv	a0,a2
  FP_UNPACK_SEMIRAW_D (B, b);
800184a4:	00100413          	li	s0,1
  FP_SUB_D (R, A, B);
800184a8:	0540006f          	j	800184fc <__subdf3+0x3bc>
800184ac:	7ff00693          	li	a3,2047
800184b0:	d4d404e3          	beq	s0,a3,800181f8 <__subdf3+0xb8>
800184b4:	03800693          	li	a3,56
800184b8:	0b06c463          	blt	a3,a6,80018560 <__subdf3+0x420>
800184bc:	01f00693          	li	a3,31
800184c0:	0706c663          	blt	a3,a6,8001852c <__subdf3+0x3ec>
800184c4:	02000593          	li	a1,32
800184c8:	410585b3          	sub	a1,a1,a6
800184cc:	00b716b3          	sll	a3,a4,a1
800184d0:	010658b3          	srl	a7,a2,a6
800184d4:	00b61633          	sll	a2,a2,a1
800184d8:	0116e6b3          	or	a3,a3,a7
800184dc:	00c03633          	snez	a2,a2
800184e0:	00c6e633          	or	a2,a3,a2
800184e4:	01075733          	srl	a4,a4,a6
800184e8:	40c50633          	sub	a2,a0,a2
800184ec:	40e78733          	sub	a4,a5,a4
800184f0:	00c537b3          	sltu	a5,a0,a2
800184f4:	40f707b3          	sub	a5,a4,a5
800184f8:	00060513          	mv	a0,a2
800184fc:	00879713          	slli	a4,a5,0x8
80018500:	ce075ce3          	bgez	a4,800181f8 <__subdf3+0xb8>
80018504:	00800937          	lui	s2,0x800
80018508:	fff90913          	addi	s2,s2,-1 # 7fffff <font6x8-0x7f80002d>
8001850c:	0127f933          	and	s2,a5,s2
80018510:	00050993          	mv	s3,a0
80018514:	20c0006f          	j	80018720 <__subdf3+0x5e0>
80018518:	7ff00693          	li	a3,2047
8001851c:	ccd40ee3          	beq	s0,a3,800181f8 <__subdf3+0xb8>
80018520:	008006b7          	lui	a3,0x800
80018524:	00d76733          	or	a4,a4,a3
80018528:	f8dff06f          	j	800184b4 <__subdf3+0x374>
8001852c:	fe080693          	addi	a3,a6,-32
80018530:	02000893          	li	a7,32
80018534:	00d756b3          	srl	a3,a4,a3
80018538:	00000593          	li	a1,0
8001853c:	01180863          	beq	a6,a7,8001854c <__subdf3+0x40c>
80018540:	04000593          	li	a1,64
80018544:	41058833          	sub	a6,a1,a6
80018548:	010715b3          	sll	a1,a4,a6
8001854c:	00c5e633          	or	a2,a1,a2
80018550:	00c03633          	snez	a2,a2
80018554:	00c6e633          	or	a2,a3,a2
80018558:	00000713          	li	a4,0
8001855c:	f8dff06f          	j	800184e8 <__subdf3+0x3a8>
80018560:	00c76633          	or	a2,a4,a2
80018564:	00c03633          	snez	a2,a2
80018568:	ff1ff06f          	j	80018558 <__subdf3+0x418>
8001856c:	0e080863          	beqz	a6,8001865c <__subdf3+0x51c>
80018570:	40888833          	sub	a6,a7,s0
80018574:	04041263          	bnez	s0,800185b8 <__subdf3+0x478>
80018578:	00a7e6b3          	or	a3,a5,a0
8001857c:	34068463          	beqz	a3,800188c4 <__subdf3+0x784>
80018580:	fff80693          	addi	a3,a6,-1
80018584:	00069e63          	bnez	a3,800185a0 <__subdf3+0x460>
80018588:	40a60533          	sub	a0,a2,a0
8001858c:	40f707b3          	sub	a5,a4,a5
80018590:	00a63633          	sltu	a2,a2,a0
80018594:	40c787b3          	sub	a5,a5,a2
80018598:	00058493          	mv	s1,a1
8001859c:	f09ff06f          	j	800184a4 <__subdf3+0x364>
800185a0:	7ff00313          	li	t1,2047
800185a4:	02681463          	bne	a6,t1,800185cc <__subdf3+0x48c>
  FP_UNPACK_SEMIRAW_D (B, b);
800185a8:	00070793          	mv	a5,a4
800185ac:	00060513          	mv	a0,a2
  FP_SUB_D (R, A, B);
800185b0:	7ff00413          	li	s0,2047
800185b4:	0d00006f          	j	80018684 <__subdf3+0x544>
800185b8:	7ff00693          	li	a3,2047
800185bc:	fed886e3          	beq	a7,a3,800185a8 <__subdf3+0x468>
800185c0:	008006b7          	lui	a3,0x800
800185c4:	00d7e7b3          	or	a5,a5,a3
800185c8:	00080693          	mv	a3,a6
800185cc:	03800813          	li	a6,56
800185d0:	08d84063          	blt	a6,a3,80018650 <__subdf3+0x510>
800185d4:	01f00813          	li	a6,31
800185d8:	04d84263          	blt	a6,a3,8001861c <__subdf3+0x4dc>
800185dc:	02000313          	li	t1,32
800185e0:	40d30333          	sub	t1,t1,a3
800185e4:	00d55e33          	srl	t3,a0,a3
800185e8:	00679833          	sll	a6,a5,t1
800185ec:	00651533          	sll	a0,a0,t1
800185f0:	01c86833          	or	a6,a6,t3
800185f4:	00a03533          	snez	a0,a0
800185f8:	00a86533          	or	a0,a6,a0
800185fc:	00d7d6b3          	srl	a3,a5,a3
80018600:	40a60533          	sub	a0,a2,a0
80018604:	40d707b3          	sub	a5,a4,a3
80018608:	00a63633          	sltu	a2,a2,a0
8001860c:	40c787b3          	sub	a5,a5,a2
  FP_UNPACK_SEMIRAW_D (B, b);
80018610:	00088413          	mv	s0,a7
80018614:	00058493          	mv	s1,a1
80018618:	ee5ff06f          	j	800184fc <__subdf3+0x3bc>
  FP_SUB_D (R, A, B);
8001861c:	fe068813          	addi	a6,a3,-32 # 7fffe0 <font6x8-0x7f80004c>
80018620:	02000e13          	li	t3,32
80018624:	0107d833          	srl	a6,a5,a6
80018628:	00000313          	li	t1,0
8001862c:	01c68863          	beq	a3,t3,8001863c <__subdf3+0x4fc>
80018630:	04000313          	li	t1,64
80018634:	40d306b3          	sub	a3,t1,a3
80018638:	00d79333          	sll	t1,a5,a3
8001863c:	00a36533          	or	a0,t1,a0
80018640:	00a03533          	snez	a0,a0
80018644:	00a86533          	or	a0,a6,a0
80018648:	00000693          	li	a3,0
8001864c:	fb5ff06f          	j	80018600 <__subdf3+0x4c0>
80018650:	00a7e533          	or	a0,a5,a0
80018654:	00a03533          	snez	a0,a0
80018658:	ff1ff06f          	j	80018648 <__subdf3+0x508>
8001865c:	00140693          	addi	a3,s0,1
80018660:	7fe6f693          	andi	a3,a3,2046
80018664:	08069863          	bnez	a3,800186f4 <__subdf3+0x5b4>
80018668:	00a7e833          	or	a6,a5,a0
8001866c:	00c766b3          	or	a3,a4,a2
80018670:	06041063          	bnez	s0,800186d0 <__subdf3+0x590>
80018674:	00081c63          	bnez	a6,8001868c <__subdf3+0x54c>
80018678:	24068e63          	beqz	a3,800188d4 <__subdf3+0x794>
  FP_UNPACK_SEMIRAW_D (B, b);
8001867c:	00070793          	mv	a5,a4
80018680:	00060513          	mv	a0,a2
80018684:	00058493          	mv	s1,a1
80018688:	b71ff06f          	j	800181f8 <__subdf3+0xb8>
  FP_SUB_D (R, A, B);
8001868c:	b60686e3          	beqz	a3,800181f8 <__subdf3+0xb8>
80018690:	40c50833          	sub	a6,a0,a2
80018694:	010538b3          	sltu	a7,a0,a6
80018698:	40e786b3          	sub	a3,a5,a4
8001869c:	411686b3          	sub	a3,a3,a7
800186a0:	00869893          	slli	a7,a3,0x8
800186a4:	0008dc63          	bgez	a7,800186bc <__subdf3+0x57c>
800186a8:	40a60533          	sub	a0,a2,a0
800186ac:	40f707b3          	sub	a5,a4,a5
800186b0:	00a63633          	sltu	a2,a2,a0
800186b4:	40c787b3          	sub	a5,a5,a2
800186b8:	fcdff06f          	j	80018684 <__subdf3+0x544>
800186bc:	00d86533          	or	a0,a6,a3
800186c0:	22050263          	beqz	a0,800188e4 <__subdf3+0x7a4>
800186c4:	00068793          	mv	a5,a3
800186c8:	00080513          	mv	a0,a6
800186cc:	b2dff06f          	j	800181f8 <__subdf3+0xb8>
800186d0:	00081e63          	bnez	a6,800186ec <__subdf3+0x5ac>
800186d4:	20068c63          	beqz	a3,800188ec <__subdf3+0x7ac>
  FP_UNPACK_SEMIRAW_D (B, b);
800186d8:	00070793          	mv	a5,a4
800186dc:	00060513          	mv	a0,a2
  FP_SUB_D (R, A, B);
800186e0:	00058493          	mv	s1,a1
800186e4:	7ff00413          	li	s0,2047
800186e8:	b11ff06f          	j	800181f8 <__subdf3+0xb8>
800186ec:	fe068ce3          	beqz	a3,800186e4 <__subdf3+0x5a4>
800186f0:	d45ff06f          	j	80018434 <__subdf3+0x2f4>
800186f4:	40c509b3          	sub	s3,a0,a2
800186f8:	013536b3          	sltu	a3,a0,s3
800186fc:	40e78933          	sub	s2,a5,a4
80018700:	40d90933          	sub	s2,s2,a3
80018704:	00891693          	slli	a3,s2,0x8
80018708:	0806d463          	bgez	a3,80018790 <__subdf3+0x650>
8001870c:	40a609b3          	sub	s3,a2,a0
80018710:	40f707b3          	sub	a5,a4,a5
80018714:	01363633          	sltu	a2,a2,s3
80018718:	40c78933          	sub	s2,a5,a2
8001871c:	00058493          	mv	s1,a1
80018720:	08090263          	beqz	s2,800187a4 <__subdf3+0x664>
80018724:	00090513          	mv	a0,s2
80018728:	56c000ef          	jal	ra,80018c94 <__clzsi2>
8001872c:	ff850713          	addi	a4,a0,-8
80018730:	01f00793          	li	a5,31
80018734:	08e7c063          	blt	a5,a4,800187b4 <__subdf3+0x674>
80018738:	02000693          	li	a3,32
8001873c:	40e686b3          	sub	a3,a3,a4
80018740:	00e917b3          	sll	a5,s2,a4
80018744:	00d9d6b3          	srl	a3,s3,a3
80018748:	00f6e6b3          	or	a3,a3,a5
8001874c:	00e99533          	sll	a0,s3,a4
80018750:	0a874463          	blt	a4,s0,800187f8 <__subdf3+0x6b8>
80018754:	40870733          	sub	a4,a4,s0
80018758:	00170793          	addi	a5,a4,1
8001875c:	01f00613          	li	a2,31
80018760:	06f64263          	blt	a2,a5,800187c4 <__subdf3+0x684>
80018764:	02000713          	li	a4,32
80018768:	40f70733          	sub	a4,a4,a5
8001876c:	00f555b3          	srl	a1,a0,a5
80018770:	00e69633          	sll	a2,a3,a4
80018774:	00e51533          	sll	a0,a0,a4
80018778:	00b66633          	or	a2,a2,a1
8001877c:	00a03533          	snez	a0,a0
80018780:	00a66533          	or	a0,a2,a0
80018784:	00f6d7b3          	srl	a5,a3,a5
80018788:	00000413          	li	s0,0
8001878c:	a6dff06f          	j	800181f8 <__subdf3+0xb8>
80018790:	0129e533          	or	a0,s3,s2
80018794:	f80516e3          	bnez	a0,80018720 <__subdf3+0x5e0>
80018798:	00000793          	li	a5,0
8001879c:	00000413          	li	s0,0
800187a0:	13c0006f          	j	800188dc <__subdf3+0x79c>
800187a4:	00098513          	mv	a0,s3
800187a8:	4ec000ef          	jal	ra,80018c94 <__clzsi2>
800187ac:	02050513          	addi	a0,a0,32
800187b0:	f7dff06f          	j	8001872c <__subdf3+0x5ec>
800187b4:	fd850513          	addi	a0,a0,-40
800187b8:	00a996b3          	sll	a3,s3,a0
800187bc:	00000513          	li	a0,0
800187c0:	f91ff06f          	j	80018750 <__subdf3+0x610>
800187c4:	fe170713          	addi	a4,a4,-31
800187c8:	02000593          	li	a1,32
800187cc:	00e6d733          	srl	a4,a3,a4
800187d0:	00000613          	li	a2,0
800187d4:	00b78863          	beq	a5,a1,800187e4 <__subdf3+0x6a4>
800187d8:	04000613          	li	a2,64
800187dc:	40f60633          	sub	a2,a2,a5
800187e0:	00c69633          	sll	a2,a3,a2
800187e4:	00c56533          	or	a0,a0,a2
800187e8:	00a03533          	snez	a0,a0
800187ec:	00a76533          	or	a0,a4,a0
800187f0:	00000793          	li	a5,0
800187f4:	f95ff06f          	j	80018788 <__subdf3+0x648>
800187f8:	ff8007b7          	lui	a5,0xff800
800187fc:	fff78793          	addi	a5,a5,-1 # ff7fffff <__clzsi2+0x7f7e736b>
80018800:	40e40433          	sub	s0,s0,a4
80018804:	00f6f7b3          	and	a5,a3,a5
80018808:	9f1ff06f          	j	800181f8 <__subdf3+0xb8>
  FP_UNPACK_SEMIRAW_D (B, b);
8001880c:	00070793          	mv	a5,a4
80018810:	00060513          	mv	a0,a2
  FP_SUB_D (R, A, B);
80018814:	00058413          	mv	s0,a1
80018818:	9e1ff06f          	j	800181f8 <__subdf3+0xb8>
  FP_UNPACK_SEMIRAW_D (B, b);
8001881c:	00070793          	mv	a5,a4
80018820:	00060513          	mv	a0,a2
80018824:	9d5ff06f          	j	800181f8 <__subdf3+0xb8>
80018828:	7ff00413          	li	s0,2047
8001882c:	00000793          	li	a5,0
80018830:	00000513          	li	a0,0
  FP_PACK_SEMIRAW_D (r, R);
80018834:	00879713          	slli	a4,a5,0x8
80018838:	00075e63          	bgez	a4,80018854 <__subdf3+0x714>
8001883c:	00140413          	addi	s0,s0,1
80018840:	7ff00713          	li	a4,2047
80018844:	0ae40c63          	beq	s0,a4,800188fc <__subdf3+0x7bc>
80018848:	ff800737          	lui	a4,0xff800
8001884c:	fff70713          	addi	a4,a4,-1 # ff7fffff <__clzsi2+0x7f7e736b>
80018850:	00e7f7b3          	and	a5,a5,a4
80018854:	01d79713          	slli	a4,a5,0x1d
80018858:	00355513          	srli	a0,a0,0x3
8001885c:	00a76533          	or	a0,a4,a0
80018860:	7ff00713          	li	a4,2047
80018864:	0037d793          	srli	a5,a5,0x3
80018868:	00e41e63          	bne	s0,a4,80018884 <__subdf3+0x744>
8001886c:	00f56533          	or	a0,a0,a5
80018870:	00000793          	li	a5,0
80018874:	00050863          	beqz	a0,80018884 <__subdf3+0x744>
80018878:	000807b7          	lui	a5,0x80
8001887c:	00000513          	li	a0,0
80018880:	00000493          	li	s1,0
80018884:	01441713          	slli	a4,s0,0x14
80018888:	7ff006b7          	lui	a3,0x7ff00
8001888c:	00c79793          	slli	a5,a5,0xc
  FP_HANDLE_EXCEPTIONS;

  return r;
}
80018890:	01c12083          	lw	ra,28(sp)
80018894:	01812403          	lw	s0,24(sp)
  FP_PACK_SEMIRAW_D (r, R);
80018898:	00d77733          	and	a4,a4,a3
8001889c:	00c7d793          	srli	a5,a5,0xc
800188a0:	01f49493          	slli	s1,s1,0x1f
800188a4:	00f767b3          	or	a5,a4,a5
800188a8:	0097e733          	or	a4,a5,s1
}
800188ac:	01012903          	lw	s2,16(sp)
800188b0:	01412483          	lw	s1,20(sp)
800188b4:	00c12983          	lw	s3,12(sp)
800188b8:	00070593          	mv	a1,a4
800188bc:	02010113          	addi	sp,sp,32
800188c0:	00008067          	ret
  FP_UNPACK_SEMIRAW_D (B, b);
800188c4:	00070793          	mv	a5,a4
800188c8:	00060513          	mv	a0,a2
  FP_SUB_D (R, A, B);
800188cc:	00080413          	mv	s0,a6
800188d0:	db5ff06f          	j	80018684 <__subdf3+0x544>
800188d4:	00000793          	li	a5,0
800188d8:	00000513          	li	a0,0
800188dc:	00000493          	li	s1,0
800188e0:	f55ff06f          	j	80018834 <__subdf3+0x6f4>
800188e4:	00000793          	li	a5,0
800188e8:	ff5ff06f          	j	800188dc <__subdf3+0x79c>
800188ec:	00000513          	li	a0,0
800188f0:	00000493          	li	s1,0
800188f4:	004007b7          	lui	a5,0x400
800188f8:	b49ff06f          	j	80018440 <__subdf3+0x300>
800188fc:	00000793          	li	a5,0
80018900:	00000513          	li	a0,0
80018904:	f51ff06f          	j	80018854 <__subdf3+0x714>

80018908 <__fixdfsi>:
  FP_DECL_EX;
  FP_DECL_D (A);
  USItype r;

  FP_INIT_EXCEPTIONS;
  FP_UNPACK_RAW_D (A, a);
80018908:	0145d713          	srli	a4,a1,0x14
8001890c:	001006b7          	lui	a3,0x100
80018910:	fff68793          	addi	a5,a3,-1 # fffff <font6x8-0x7ff0002d>
80018914:	7ff77713          	andi	a4,a4,2047
  FP_TO_INT_D (r, A, SI_BITS, 1);
80018918:	3fe00613          	li	a2,1022
  FP_UNPACK_RAW_D (A, a);
8001891c:	00b7f7b3          	and	a5,a5,a1
80018920:	01f5d593          	srli	a1,a1,0x1f
  FP_TO_INT_D (r, A, SI_BITS, 1);
80018924:	04e65e63          	bge	a2,a4,80018980 <__fixdfsi+0x78>
80018928:	41d00613          	li	a2,1053
8001892c:	00e65a63          	bge	a2,a4,80018940 <__fixdfsi+0x38>
80018930:	80000537          	lui	a0,0x80000
80018934:	fff54513          	not	a0,a0
80018938:	00a58533          	add	a0,a1,a0
8001893c:	00008067          	ret
80018940:	00d7e7b3          	or	a5,a5,a3
80018944:	43300693          	li	a3,1075
80018948:	40e686b3          	sub	a3,a3,a4
8001894c:	01f00613          	li	a2,31
80018950:	02d64063          	blt	a2,a3,80018970 <__fixdfsi+0x68>
80018954:	bed70713          	addi	a4,a4,-1043
80018958:	00e797b3          	sll	a5,a5,a4
8001895c:	00d55533          	srl	a0,a0,a3
80018960:	00a7e533          	or	a0,a5,a0
80018964:	02058063          	beqz	a1,80018984 <__fixdfsi+0x7c>
80018968:	40a00533          	neg	a0,a0
8001896c:	00008067          	ret
80018970:	41300513          	li	a0,1043
80018974:	40e50533          	sub	a0,a0,a4
80018978:	00a7d533          	srl	a0,a5,a0
8001897c:	fe9ff06f          	j	80018964 <__fixdfsi+0x5c>
80018980:	00000513          	li	a0,0
  FP_HANDLE_EXCEPTIONS;

  return r;
}
80018984:	00008067          	ret

80018988 <__fixunsdfsi>:
  FP_DECL_EX;
  FP_DECL_D (A);
  USItype r;

  FP_INIT_EXCEPTIONS;
  FP_UNPACK_RAW_D (A, a);
80018988:	0145d713          	srli	a4,a1,0x14
8001898c:	00100837          	lui	a6,0x100
80018990:	fff80793          	addi	a5,a6,-1 # fffff <font6x8-0x7ff0002d>
80018994:	7ff77713          	andi	a4,a4,2047
  FP_TO_INT_D (r, A, SI_BITS, 0);
80018998:	3fe00613          	li	a2,1022
  FP_UNPACK_RAW_D (A, a);
8001899c:	00b7f7b3          	and	a5,a5,a1
{
800189a0:	00050693          	mv	a3,a0
  FP_UNPACK_RAW_D (A, a);
800189a4:	01f5d593          	srli	a1,a1,0x1f
  FP_TO_INT_D (r, A, SI_BITS, 0);
800189a8:	04e65a63          	bge	a2,a4,800189fc <__fixunsdfsi+0x74>
800189ac:	41f00613          	li	a2,1055
800189b0:	40b60633          	sub	a2,a2,a1
800189b4:	fff58513          	addi	a0,a1,-1
800189b8:	04c75463          	bge	a4,a2,80018a00 <__fixunsdfsi+0x78>
800189bc:	00000513          	li	a0,0
800189c0:	04059063          	bnez	a1,80018a00 <__fixunsdfsi+0x78>
800189c4:	43300513          	li	a0,1075
800189c8:	40e50533          	sub	a0,a0,a4
800189cc:	01f00613          	li	a2,31
800189d0:	0107e7b3          	or	a5,a5,a6
800189d4:	00a64c63          	blt	a2,a0,800189ec <__fixunsdfsi+0x64>
800189d8:	bed70713          	addi	a4,a4,-1043
800189dc:	00e797b3          	sll	a5,a5,a4
800189e0:	00a6d533          	srl	a0,a3,a0
800189e4:	00a7e533          	or	a0,a5,a0
800189e8:	00008067          	ret
800189ec:	41300513          	li	a0,1043
800189f0:	40e50533          	sub	a0,a0,a4
800189f4:	00a7d533          	srl	a0,a5,a0
800189f8:	00008067          	ret
800189fc:	00000513          	li	a0,0
  FP_HANDLE_EXCEPTIONS;

  return r;
}
80018a00:	00008067          	ret

80018a04 <__floatsidf>:
#include "soft-fp.h"
#include "double.h"

DFtype
__floatsidf (SItype i)
{
80018a04:	ff010113          	addi	sp,sp,-16
80018a08:	00112623          	sw	ra,12(sp)
80018a0c:	00812423          	sw	s0,8(sp)
80018a10:	00912223          	sw	s1,4(sp)
  FP_DECL_D (A);
  DFtype a;

  FP_FROM_INT_D (A, i, SI_BITS, USItype);
80018a14:	08050663          	beqz	a0,80018aa0 <__floatsidf+0x9c>
80018a18:	41f55793          	srai	a5,a0,0x1f
80018a1c:	00a7c433          	xor	s0,a5,a0
80018a20:	40f40433          	sub	s0,s0,a5
80018a24:	01f55493          	srli	s1,a0,0x1f
80018a28:	00040513          	mv	a0,s0
80018a2c:	268000ef          	jal	ra,80018c94 <__clzsi2>
80018a30:	41e00713          	li	a4,1054
80018a34:	00a00793          	li	a5,10
80018a38:	40a70733          	sub	a4,a4,a0
80018a3c:	04a7c863          	blt	a5,a0,80018a8c <__floatsidf+0x88>
80018a40:	00b00793          	li	a5,11
80018a44:	40a787b3          	sub	a5,a5,a0
80018a48:	01550513          	addi	a0,a0,21 # 80000015 <__clzsi2+0xfffe7381>
80018a4c:	00f457b3          	srl	a5,s0,a5
80018a50:	00a41433          	sll	s0,s0,a0
80018a54:	00048513          	mv	a0,s1
  FP_PACK_RAW_D (a, A);
80018a58:	00c79793          	slli	a5,a5,0xc
80018a5c:	01471713          	slli	a4,a4,0x14
80018a60:	00c7d793          	srli	a5,a5,0xc
80018a64:	01f51513          	slli	a0,a0,0x1f
80018a68:	00f767b3          	or	a5,a4,a5

  return a;
}
80018a6c:	00c12083          	lw	ra,12(sp)
  FP_PACK_RAW_D (a, A);
80018a70:	00a7e733          	or	a4,a5,a0
}
80018a74:	00040513          	mv	a0,s0
80018a78:	00812403          	lw	s0,8(sp)
80018a7c:	00412483          	lw	s1,4(sp)
80018a80:	00070593          	mv	a1,a4
80018a84:	01010113          	addi	sp,sp,16
80018a88:	00008067          	ret
  FP_FROM_INT_D (A, i, SI_BITS, USItype);
80018a8c:	ff550513          	addi	a0,a0,-11
80018a90:	00a417b3          	sll	a5,s0,a0
80018a94:	00048513          	mv	a0,s1
80018a98:	00000413          	li	s0,0
80018a9c:	fbdff06f          	j	80018a58 <__floatsidf+0x54>
80018aa0:	00000713          	li	a4,0
80018aa4:	00000793          	li	a5,0
80018aa8:	ff1ff06f          	j	80018a98 <__floatsidf+0x94>

80018aac <__floatunsidf>:
#include "soft-fp.h"
#include "double.h"

DFtype
__floatunsidf (USItype i)
{
80018aac:	ff010113          	addi	sp,sp,-16
80018ab0:	00812423          	sw	s0,8(sp)
80018ab4:	00112623          	sw	ra,12(sp)
80018ab8:	00050413          	mv	s0,a0
  FP_DECL_D (A);
  DFtype a;

  FP_FROM_INT_D (A, i, SI_BITS, USItype);
80018abc:	06050263          	beqz	a0,80018b20 <__floatunsidf+0x74>
80018ac0:	1d4000ef          	jal	ra,80018c94 <__clzsi2>
80018ac4:	41e00713          	li	a4,1054
80018ac8:	00a00793          	li	a5,10
80018acc:	40a70733          	sub	a4,a4,a0
80018ad0:	04a7c063          	blt	a5,a0,80018b10 <__floatunsidf+0x64>
80018ad4:	00b00793          	li	a5,11
80018ad8:	40a787b3          	sub	a5,a5,a0
80018adc:	01550513          	addi	a0,a0,21
80018ae0:	00f457b3          	srl	a5,s0,a5
80018ae4:	00a41433          	sll	s0,s0,a0
  FP_PACK_RAW_D (a, A);

  return a;
}
80018ae8:	00c12083          	lw	ra,12(sp)
80018aec:	00040513          	mv	a0,s0
  FP_PACK_RAW_D (a, A);
80018af0:	00c79793          	slli	a5,a5,0xc
}
80018af4:	00812403          	lw	s0,8(sp)
  FP_PACK_RAW_D (a, A);
80018af8:	01471713          	slli	a4,a4,0x14
80018afc:	00c7d793          	srli	a5,a5,0xc
80018b00:	00f766b3          	or	a3,a4,a5
}
80018b04:	00068593          	mv	a1,a3
80018b08:	01010113          	addi	sp,sp,16
80018b0c:	00008067          	ret
  FP_FROM_INT_D (A, i, SI_BITS, USItype);
80018b10:	ff550793          	addi	a5,a0,-11
80018b14:	00f417b3          	sll	a5,s0,a5
80018b18:	00000413          	li	s0,0
80018b1c:	fcdff06f          	j	80018ae8 <__floatunsidf+0x3c>
80018b20:	00000793          	li	a5,0
80018b24:	00000713          	li	a4,0
80018b28:	fc1ff06f          	j	80018ae8 <__floatunsidf+0x3c>

80018b2c <__mulsi3>:
/* Our RV64 64-bit routine is equivalent to our RV32 32-bit routine.  */
# define __muldi3 __mulsi3
#endif

FUNC_BEGIN (__muldi3)
  mv     a2, a0
80018b2c:	00050613          	mv	a2,a0
  li     a0, 0
80018b30:	00000513          	li	a0,0
.L1:
  andi   a3, a1, 1
80018b34:	0015f693          	andi	a3,a1,1
  beqz   a3, .L2
80018b38:	00068463          	beqz	a3,80018b40 <__mulsi3+0x14>
  add    a0, a0, a2
80018b3c:	00c50533          	add	a0,a0,a2
.L2:
  srli   a1, a1, 1
80018b40:	0015d593          	srli	a1,a1,0x1
  slli   a2, a2, 1
80018b44:	00161613          	slli	a2,a2,0x1
  bnez   a1, .L1
80018b48:	fe0596e3          	bnez	a1,80018b34 <__mulsi3+0x8>
  ret
80018b4c:	00008067          	ret

80018b50 <__muldi3>:
# define __multi3 __muldi3
#endif

DWtype
__multi3 (DWtype u, DWtype v)
{
80018b50:	00050313          	mv	t1,a0
80018b54:	ff010113          	addi	sp,sp,-16
80018b58:	00060513          	mv	a0,a2
80018b5c:	00068893          	mv	a7,a3
80018b60:	00112623          	sw	ra,12(sp)
  const DWunion uu = {.ll = u};
  const DWunion vv = {.ll = v};
  DWunion w;
  UWtype u_low = uu.s.low;
80018b64:	00030613          	mv	a2,t1
  UWtype v_low = vv.s.low;
80018b68:	00050693          	mv	a3,a0
  UWtype u_low_msb;
  UWtype w_low = 0;
  UWtype new_w_low;
  UWtype w_high = 0;
  UWtype w_high_tmp = 0;
80018b6c:	00000713          	li	a4,0
  UWtype w_high = 0;
80018b70:	00000793          	li	a5,0
  UWtype w_low = 0;
80018b74:	00000813          	li	a6,0
  do
    {
      new_w_low = w_low + u_low;
      w_high_tmp2x = w_high_tmp << 1;
      w_high_tmp += w_high;
      if (v_low & 1)
80018b78:	0016fe13          	andi	t3,a3,1
      w_high_tmp2x = w_high_tmp << 1;
80018b7c:	00171e93          	slli	t4,a4,0x1
      if (v_low & 1)
80018b80:	000e0c63          	beqz	t3,80018b98 <__muldi3+0x48>
      new_w_low = w_low + u_low;
80018b84:	01060e33          	add	t3,a2,a6
	{
	  carry = new_w_low < w_low;
80018b88:	010e3833          	sltu	a6,t3,a6
      w_high_tmp += w_high;
80018b8c:	00e787b3          	add	a5,a5,a4
	  w_low = new_w_low;
	  w_high = carry + w_high_tmp;
80018b90:	00f807b3          	add	a5,a6,a5
	  w_low = new_w_low;
80018b94:	000e0813          	mv	a6,t3
	}
      u_low_msb = (u_low >> ((sizeof (UWtype) * 8) - 1));
80018b98:	01f65713          	srli	a4,a2,0x1f
      v_low >>= 1;
80018b9c:	0016d693          	srli	a3,a3,0x1
      u_low <<= 1;
      w_high_tmp = u_low_msb | w_high_tmp2x;
80018ba0:	00eee733          	or	a4,t4,a4
80018ba4:	00161613          	slli	a2,a2,0x1
    }
  while (v_low);
80018ba8:	fc0698e3          	bnez	a3,80018b78 <__muldi3+0x28>

  w.s.low = w_low;
  w.s.high = w_high;

  if (uu.s.high)
80018bac:	00058663          	beqz	a1,80018bb8 <__muldi3+0x68>
    w.s.high = w.s.high + __muluw3(vv.s.low, uu.s.high);
80018bb0:	f7dff0ef          	jal	ra,80018b2c <__mulsi3>
80018bb4:	00a787b3          	add	a5,a5,a0

  if (vv.s.high)
80018bb8:	00088a63          	beqz	a7,80018bcc <__muldi3+0x7c>
    w.s.high += __muluw3(uu.s.low, vv.s.high);
80018bbc:	00030513          	mv	a0,t1
80018bc0:	00088593          	mv	a1,a7
80018bc4:	f69ff0ef          	jal	ra,80018b2c <__mulsi3>
80018bc8:	00f507b3          	add	a5,a0,a5

  return w.ll;
}
80018bcc:	00c12083          	lw	ra,12(sp)
80018bd0:	00080513          	mv	a0,a6
80018bd4:	00078593          	mv	a1,a5
80018bd8:	01010113          	addi	sp,sp,16
80018bdc:	00008067          	ret

80018be0 <__divsi3>:
  li    t0, -1
  beq   a1, t0, .L20
#endif

FUNC_BEGIN (__divdi3)
  bltz  a0, .L10
80018be0:	06054063          	bltz	a0,80018c40 <__umodsi3+0x10>
  bltz  a1, .L11
80018be4:	0605c663          	bltz	a1,80018c50 <__umodsi3+0x20>

80018be8 <__udivsi3>:
  /* Since the quotient is positive, fall into __udivdi3.  */

FUNC_BEGIN (__udivdi3)
  mv    a2, a1
80018be8:	00058613          	mv	a2,a1
  mv    a1, a0
80018bec:	00050593          	mv	a1,a0
  li    a0, -1
80018bf0:	fff00513          	li	a0,-1
  beqz  a2, .L5
80018bf4:	02060c63          	beqz	a2,80018c2c <__udivsi3+0x44>
  li    a3, 1
80018bf8:	00100693          	li	a3,1
  bgeu  a2, a1, .L2
80018bfc:	00b67a63          	bgeu	a2,a1,80018c10 <__udivsi3+0x28>
.L1:
  blez  a2, .L2
80018c00:	00c05863          	blez	a2,80018c10 <__udivsi3+0x28>
  slli  a2, a2, 1
80018c04:	00161613          	slli	a2,a2,0x1
  slli  a3, a3, 1
80018c08:	00169693          	slli	a3,a3,0x1
  bgtu  a1, a2, .L1
80018c0c:	feb66ae3          	bltu	a2,a1,80018c00 <__udivsi3+0x18>
.L2:
  li    a0, 0
80018c10:	00000513          	li	a0,0
.L3:
  bltu  a1, a2, .L4
80018c14:	00c5e663          	bltu	a1,a2,80018c20 <__udivsi3+0x38>
  sub   a1, a1, a2
80018c18:	40c585b3          	sub	a1,a1,a2
  or    a0, a0, a3
80018c1c:	00d56533          	or	a0,a0,a3
.L4:
  srli  a3, a3, 1
80018c20:	0016d693          	srli	a3,a3,0x1
  srli  a2, a2, 1
80018c24:	00165613          	srli	a2,a2,0x1
  bnez  a3, .L3
80018c28:	fe0696e3          	bnez	a3,80018c14 <__udivsi3+0x2c>
.L5:
  ret
80018c2c:	00008067          	ret

80018c30 <__umodsi3>:
FUNC_END (__udivdi3)

FUNC_BEGIN (__umoddi3)
  /* Call __udivdi3(a0, a1), then return the remainder, which is in a1.  */
  move  t0, ra
80018c30:	00008293          	mv	t0,ra
  jal   __udivdi3
80018c34:	fb5ff0ef          	jal	ra,80018be8 <__udivsi3>
  move  a0, a1
80018c38:	00058513          	mv	a0,a1
  jr    t0
80018c3c:	00028067          	jr	t0
FUNC_END (__umoddi3)

  /* Handle negative arguments to __divdi3.  */
.L10:
  neg   a0, a0
80018c40:	40a00533          	neg	a0,a0
  bgez  a1, .L12      /* Compute __udivdi3(-a0, a1), then negate the result.  */
80018c44:	0005d863          	bgez	a1,80018c54 <__umodsi3+0x24>
  neg   a1, a1
80018c48:	40b005b3          	neg	a1,a1
  j     __udivdi3     /* Compute __udivdi3(-a0, -a1).  */
80018c4c:	f9dff06f          	j	80018be8 <__udivsi3>
.L11:                 /* Compute __udivdi3(a0, -a1), then negate the result.  */
  neg   a1, a1
80018c50:	40b005b3          	neg	a1,a1
.L12:
  move  t0, ra
80018c54:	00008293          	mv	t0,ra
  jal   __udivdi3
80018c58:	f91ff0ef          	jal	ra,80018be8 <__udivsi3>
  neg   a0, a0
80018c5c:	40a00533          	neg	a0,a0
  jr    t0
80018c60:	00028067          	jr	t0

80018c64 <__modsi3>:
FUNC_END (__divdi3)

FUNC_BEGIN (__moddi3)
  move   t0, ra
80018c64:	00008293          	mv	t0,ra
  bltz   a1, .L31
80018c68:	0005ca63          	bltz	a1,80018c7c <__modsi3+0x18>
  bltz   a0, .L32
80018c6c:	00054c63          	bltz	a0,80018c84 <__modsi3+0x20>
.L30:
  jal    __udivdi3    /* The dividend is not negative.  */
80018c70:	f79ff0ef          	jal	ra,80018be8 <__udivsi3>
  move   a0, a1
80018c74:	00058513          	mv	a0,a1
  jr     t0
80018c78:	00028067          	jr	t0
.L31:
  neg    a1, a1
80018c7c:	40b005b3          	neg	a1,a1
  bgez   a0, .L30
80018c80:	fe0558e3          	bgez	a0,80018c70 <__modsi3+0xc>
.L32:
  neg    a0, a0
80018c84:	40a00533          	neg	a0,a0
  jal    __udivdi3    /* The dividend is hella negative.  */
80018c88:	f61ff0ef          	jal	ra,80018be8 <__udivsi3>
  neg    a0, a1
80018c8c:	40b00533          	neg	a0,a1
  jr     t0
80018c90:	00028067          	jr	t0

80018c94 <__clzsi2>:
  count_leading_zeros (ret, x);
80018c94:	000107b7          	lui	a5,0x10
80018c98:	02f57a63          	bgeu	a0,a5,80018ccc <__clzsi2+0x38>
80018c9c:	0ff00793          	li	a5,255
80018ca0:	00a7b7b3          	sltu	a5,a5,a0
80018ca4:	00379793          	slli	a5,a5,0x3
80018ca8:	80000737          	lui	a4,0x80000
80018cac:	02000693          	li	a3,32
80018cb0:	40f686b3          	sub	a3,a3,a5
80018cb4:	00f55533          	srl	a0,a0,a5
80018cb8:	6d070793          	addi	a5,a4,1744 # 800006d0 <__clzsi2+0xfffe7a3c>
80018cbc:	00a78533          	add	a0,a5,a0
80018cc0:	00054503          	lbu	a0,0(a0)
}
80018cc4:	40a68533          	sub	a0,a3,a0
80018cc8:	00008067          	ret
  count_leading_zeros (ret, x);
80018ccc:	01000737          	lui	a4,0x1000
80018cd0:	01000793          	li	a5,16
80018cd4:	fce56ae3          	bltu	a0,a4,80018ca8 <__clzsi2+0x14>
80018cd8:	01800793          	li	a5,24
80018cdc:	fcdff06f          	j	80018ca8 <__clzsi2+0x14>
