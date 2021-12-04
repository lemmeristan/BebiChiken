
main.out:     file format elf32-littleriscv


Disassembly of section .text:

00400000 <_swag-0x4>:
#include "sdkdefs.h"

li	a4,2
  400000:	00200713          	li	a4,2

00400004 <_swag>:
_swag:
lui	a5,0xc0002
  400004:	c00027b7          	lui	a5,0xc0002
sw	a4,0(a5)
  400008:	00e7a023          	sw	a4,0(a5) # c0002000 <main+0xbfc01d0c>
sw	a4,4(a5)
  40000c:	00e7a223          	sw	a4,4(a5)
//addi a4, a4, 1
j _swag
  400010:	ff5ff06f          	j	400004 <_swag>
//lui	a4,0xc0001
//li a3, '+'
//sw a3, 1(a4)

//.align 7
j _start 
  400014:	1f00006f          	j	400204 <_start>
  400018:	00000013          	nop
  40001c:	00000013          	nop
  400020:	00000013          	nop
  400024:	00000013          	nop
  400028:	00000013          	nop
  40002c:	00000013          	nop
  400030:	00000013          	nop
  400034:	00000013          	nop
  400038:	00000013          	nop
  40003c:	00000013          	nop
  400040:	00000013          	nop
  400044:	00000013          	nop
  400048:	00000013          	nop
  40004c:	00000013          	nop
  400050:	00000013          	nop
  400054:	00000013          	nop
  400058:	00000013          	nop
  40005c:	00000013          	nop
  400060:	00000013          	nop
  400064:	00000013          	nop
  400068:	00000013          	nop
  40006c:	00000013          	nop
  400070:	00000013          	nop
  400074:	00000013          	nop
  400078:	00000013          	nop
  40007c:	00000013          	nop
  400080:	00000013          	nop
  400084:	00000013          	nop
  400088:	00000013          	nop
  40008c:	00000013          	nop
  400090:	00000013          	nop
  400094:	00000013          	nop
  400098:	00000013          	nop
  40009c:	00000013          	nop
  4000a0:	00000013          	nop
  4000a4:	00000013          	nop
  4000a8:	00000013          	nop
  4000ac:	00000013          	nop
  4000b0:	00000013          	nop
  4000b4:	00000013          	nop
  4000b8:	00000013          	nop
  4000bc:	00000013          	nop
  4000c0:	00000013          	nop
  4000c4:	00000013          	nop
  4000c8:	00000013          	nop
  4000cc:	00000013          	nop
  4000d0:	00000013          	nop
  4000d4:	00000013          	nop
  4000d8:	00000013          	nop
  4000dc:	00000013          	nop
  4000e0:	00000013          	nop
  4000e4:	00000013          	nop
  4000e8:	00000013          	nop
  4000ec:	00000013          	nop
  4000f0:	00000013          	nop
  4000f4:	00000013          	nop
  4000f8:	00000013          	nop
  4000fc:	00000013          	nop
.align 8
j _external_interrupt
  400100:	1ec0006f          	j	4002ec <_external_interrupt>
  400104:	00000013          	nop
  400108:	00000013          	nop
  40010c:	00000013          	nop
  400110:	00000013          	nop
  400114:	00000013          	nop
  400118:	00000013          	nop
  40011c:	00000013          	nop
  400120:	00000013          	nop
  400124:	00000013          	nop
  400128:	00000013          	nop
  40012c:	00000013          	nop
  400130:	00000013          	nop
  400134:	00000013          	nop
  400138:	00000013          	nop
  40013c:	00000013          	nop
  400140:	00000013          	nop
  400144:	00000013          	nop
  400148:	00000013          	nop
  40014c:	00000013          	nop
  400150:	00000013          	nop
  400154:	00000013          	nop
  400158:	00000013          	nop
  40015c:	00000013          	nop
  400160:	00000013          	nop
  400164:	00000013          	nop
  400168:	00000013          	nop
  40016c:	00000013          	nop
  400170:	00000013          	nop
  400174:	00000013          	nop
  400178:	00000013          	nop
  40017c:	00000013          	nop
  400180:	00000013          	nop
  400184:	00000013          	nop
  400188:	00000013          	nop
  40018c:	00000013          	nop
  400190:	00000013          	nop
  400194:	00000013          	nop
  400198:	00000013          	nop
  40019c:	00000013          	nop
  4001a0:	00000013          	nop
  4001a4:	00000013          	nop
  4001a8:	00000013          	nop
  4001ac:	00000013          	nop
  4001b0:	00000013          	nop
  4001b4:	00000013          	nop
  4001b8:	00000013          	nop
  4001bc:	00000013          	nop
  4001c0:	00000013          	nop
  4001c4:	00000013          	nop
  4001c8:	00000013          	nop
  4001cc:	00000013          	nop
  4001d0:	00000013          	nop
  4001d4:	00000013          	nop
  4001d8:	00000013          	nop
  4001dc:	00000013          	nop
  4001e0:	00000013          	nop
  4001e4:	00000013          	nop
  4001e8:	00000013          	nop
  4001ec:	00000013          	nop
  4001f0:	00000013          	nop
  4001f4:	00000013          	nop
  4001f8:	00000013          	nop
  4001fc:	00000013          	nop
.align 8
j _misaligned_instruction
  400200:	0f00006f          	j	4002f0 <_misaligned_instruction>

00400204 <_start>:

.globl _start
_start:

la sp, __stack_top
  400204:	ffc10117          	auipc	sp,0xffc10
  400208:	dfc10113          	addi	sp,sp,-516 # 10000 <__stack_top>
#ifdef CONTINUOUS_RESTART
la ra, _start
#else
la ra, exec_done
  40020c:	00000097          	auipc	ra,0x0
  400210:	00c08093          	addi	ra,ra,12 # 400218 <exec_done>
#endif
j main
  400214:	0e00006f          	j	4002f4 <main>

00400218 <exec_done>:

exec_done:
  400218:	0000006f          	j	400218 <exec_done>
	...

004002ec <_external_interrupt>:

void _external_interrupt()
{

    //    printf("external interrupt triggered");
}
  4002ec:	00008067          	ret

004002f0 <_misaligned_instruction>:
  4002f0:	00008067          	ret

004002f4 <main>:
}

void main()
{

    HAL_GPIO_SetDirections(0xFFFFFFFF);
  4002f4:	fff00713          	li	a4,-1
  4002f8:	c00027b7          	lui	a5,0xc0002
  4002fc:	00e7a023          	sw	a4,0(a5) # c0002000 <main+0xbfc01d0c>
    HAL_GPIO_SetOutputs(0xFFFFFFFF);
    while (1)
    {
        HAL_UART_PutChar(".");
  400300:	00000693          	li	a3,0
    HAL_GPIO_SetOutputs(0xFFFFFFFF);
  400304:	00e7a223          	sw	a4,4(a5)
        HAL_UART_PutChar(".");
  400308:	0ff6f693          	zext.b	a3,a3
  40030c:	c0001737          	lui	a4,0xc0001
  400310:	00074783          	lbu	a5,0(a4) # c0001000 <main+0xbfc00d0c>
  400314:	0017f793          	andi	a5,a5,1
  400318:	fe079ce3          	bnez	a5,400310 <main+0x1c>
  40031c:	00d700a3          	sb	a3,1(a4)
  400320:	00074783          	lbu	a5,0(a4)
  400324:	0017f793          	andi	a5,a5,1
  400328:	fe0794e3          	bnez	a5,400310 <main+0x1c>
  40032c:	ff1ff06f          	j	40031c <main+0x28>
