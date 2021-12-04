#include "sdkdefs.h"

li	a4,2
_swag:
lui	a5,0xc0002
sw	a4,0(a5)
sw	a4,4(a5)
//addi a4, a4, 1
j _swag

//lui	a4,0xc0001
//li a3, '+'
//sw a3, 1(a4)

//.align 7
j _start 
.align 8
j _external_interrupt
.align 8
j _misaligned_instruction

.globl _start
_start:

la sp, __stack_top
#ifdef CONTINUOUS_RESTART
la ra, _start
#else
la ra, exec_done
#endif
j main

exec_done:
    j exec_done