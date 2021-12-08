
main.out:     file format elf32-littleriscv


Disassembly of section .text:

00200000 <fuck>:
#include "sdkdefs.h"


        fuck: lui	a5,0xc0001
  200000:	c00017b7          	lui	a5,0xc0001
        li	a4, 'h'
  200004:	06800713          	li	a4,104
        sw	a4,0(a5)
  200008:	00e7a023          	sw	a4,0(a5) # c0001000 <main+0xbfe00d30>
        li	a4, 'e'
  20000c:	06500713          	li	a4,101
        sw	a4,0(a5)
  200010:	00e7a023          	sw	a4,0(a5)

        li	a4, 'l'
  200014:	06c00713          	li	a4,108
        sw	a4,0(a5)
  200018:	00e7a023          	sw	a4,0(a5)
        li	a4, 'l'
  20001c:	06c00713          	li	a4,108
        sw	a4,0(a5)
  200020:	00e7a023          	sw	a4,0(a5)
        li	a4, 'o'
  200024:	06f00713          	li	a4,111
        sw	a4,0(a5)
  200028:	00e7a023          	sw	a4,0(a5)
        li	a4, '\n'
  20002c:	00a00713          	li	a4,10
        sw	a4,0(a5)
  200030:	00e7a023          	sw	a4,0(a5)
        //blah: j blah
        beq a4, a4, fuck
  200034:	fce706e3          	beq	a4,a4,200000 <fuck>

//.align 7
j _start 
  200038:	1cc0006f          	j	200204 <_start>
  20003c:	00000013          	nop
  200040:	00000013          	nop
  200044:	00000013          	nop
  200048:	00000013          	nop
  20004c:	00000013          	nop
  200050:	00000013          	nop
  200054:	00000013          	nop
  200058:	00000013          	nop
  20005c:	00000013          	nop
  200060:	00000013          	nop
  200064:	00000013          	nop
  200068:	00000013          	nop
  20006c:	00000013          	nop
  200070:	00000013          	nop
  200074:	00000013          	nop
  200078:	00000013          	nop
  20007c:	00000013          	nop
  200080:	00000013          	nop
  200084:	00000013          	nop
  200088:	00000013          	nop
  20008c:	00000013          	nop
  200090:	00000013          	nop
  200094:	00000013          	nop
  200098:	00000013          	nop
  20009c:	00000013          	nop
  2000a0:	00000013          	nop
  2000a4:	00000013          	nop
  2000a8:	00000013          	nop
  2000ac:	00000013          	nop
  2000b0:	00000013          	nop
  2000b4:	00000013          	nop
  2000b8:	00000013          	nop
  2000bc:	00000013          	nop
  2000c0:	00000013          	nop
  2000c4:	00000013          	nop
  2000c8:	00000013          	nop
  2000cc:	00000013          	nop
  2000d0:	00000013          	nop
  2000d4:	00000013          	nop
  2000d8:	00000013          	nop
  2000dc:	00000013          	nop
  2000e0:	00000013          	nop
  2000e4:	00000013          	nop
  2000e8:	00000013          	nop
  2000ec:	00000013          	nop
  2000f0:	00000013          	nop
  2000f4:	00000013          	nop
  2000f8:	00000013          	nop
  2000fc:	00000013          	nop
.align 8
j _external_interrupt
  200100:	1c80006f          	j	2002c8 <_external_interrupt>
  200104:	00000013          	nop
  200108:	00000013          	nop
  20010c:	00000013          	nop
  200110:	00000013          	nop
  200114:	00000013          	nop
  200118:	00000013          	nop
  20011c:	00000013          	nop
  200120:	00000013          	nop
  200124:	00000013          	nop
  200128:	00000013          	nop
  20012c:	00000013          	nop
  200130:	00000013          	nop
  200134:	00000013          	nop
  200138:	00000013          	nop
  20013c:	00000013          	nop
  200140:	00000013          	nop
  200144:	00000013          	nop
  200148:	00000013          	nop
  20014c:	00000013          	nop
  200150:	00000013          	nop
  200154:	00000013          	nop
  200158:	00000013          	nop
  20015c:	00000013          	nop
  200160:	00000013          	nop
  200164:	00000013          	nop
  200168:	00000013          	nop
  20016c:	00000013          	nop
  200170:	00000013          	nop
  200174:	00000013          	nop
  200178:	00000013          	nop
  20017c:	00000013          	nop
  200180:	00000013          	nop
  200184:	00000013          	nop
  200188:	00000013          	nop
  20018c:	00000013          	nop
  200190:	00000013          	nop
  200194:	00000013          	nop
  200198:	00000013          	nop
  20019c:	00000013          	nop
  2001a0:	00000013          	nop
  2001a4:	00000013          	nop
  2001a8:	00000013          	nop
  2001ac:	00000013          	nop
  2001b0:	00000013          	nop
  2001b4:	00000013          	nop
  2001b8:	00000013          	nop
  2001bc:	00000013          	nop
  2001c0:	00000013          	nop
  2001c4:	00000013          	nop
  2001c8:	00000013          	nop
  2001cc:	00000013          	nop
  2001d0:	00000013          	nop
  2001d4:	00000013          	nop
  2001d8:	00000013          	nop
  2001dc:	00000013          	nop
  2001e0:	00000013          	nop
  2001e4:	00000013          	nop
  2001e8:	00000013          	nop
  2001ec:	00000013          	nop
  2001f0:	00000013          	nop
  2001f4:	00000013          	nop
  2001f8:	00000013          	nop
  2001fc:	00000013          	nop
.align 8
j _misaligned_instruction
  200200:	0cc0006f          	j	2002cc <_misaligned_instruction>

00200204 <_start>:

.globl _start
_start:

la sp, __stack_top
  200204:	ffe10117          	auipc	sp,0xffe10
  200208:	dfc10113          	addi	sp,sp,-516 # 10000 <__stack_top>
#ifdef CONTINUOUS_RESTART
la ra, _start
#else
la ra, exec_done
  20020c:	00000097          	auipc	ra,0x0
  200210:	00c08093          	addi	ra,ra,12 # 200218 <exec_done>
#endif
j main
  200214:	0bc0006f          	j	2002d0 <main>

00200218 <exec_done>:

exec_done:
  200218:	0000006f          	j	200218 <exec_done>
	...

002002c8 <_external_interrupt>:

void _external_interrupt()
{

    //    printf("external interrupt triggered");
}
  2002c8:	00008067          	ret

002002cc <_misaligned_instruction>:
  2002cc:	00008067          	ret

002002d0 <main>:
  2002d0:	00008067          	ret
  2002d4:	0000                	unimp
	...
