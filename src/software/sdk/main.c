#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>

//#include <string.h>

#include "hal/gpio/gpio.h"
#include "hal/uart/uart.h"
//#include "hal/timebase/timebase.h"

// interrupts not implemented

void _external_interrupt()
{

    //    printf("external interrupt triggered");
}

void _misaligned_instruction()
{
}

void main()
{

    HAL_GPIO_SetDirections(0xFFFFFFFF);
    HAL_GPIO_SetOutputs(0xFFFFFFFF);
    while (1)
    {
        HAL_UART_PutChar(".");
        // HAL_Timebase_DelayMS(100);
    }
}