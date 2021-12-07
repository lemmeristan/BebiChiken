#include "sdkdefs.h"


        start: lui	a5,0xc0001
        li	a4, 'h'
        //blah: j blah
        sw	a4,0(a5)
        li	a4, 'e'
        sw	a4,0(a5)
        li	a4, 'l'
        sw	a4,0(a5)
        li	a4, 'l'
        sw	a4,0(a5)
        li	a4, 'o'
        sw	a4,0(a5)
        li	a4, '\n'
        sw	a4,0(a5)
        
        j start

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