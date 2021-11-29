#include "sdkdefs.h"

.align 7
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