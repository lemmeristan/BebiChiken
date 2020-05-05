#include "uart.h"

#ifndef OPTIMIZE_STACK
void HAL_UART_WriteBuffer(uint8_t *src, uint32_t length) {
    for(uint32_t i=0; i<length; i++) {
        while(*PTR_UART_STATUS & FLAG_UART_TX_FIFO_BUSY);
        *PTR_UART_TX_DATA = *(src+i);
    }
}

void HAL_UART_PutChar(char character)
{
    while(*PTR_UART_STATUS & FLAG_UART_TX_FIFO_BUSY);
    *PTR_UART_TX_DATA = (uint8_t) character;
}

void HAL_UART_PutLine(char *line) {
  while(*line != (char) 0) {
    HAL_UART_PutChar(*line);
    line++;
  }
}

#endif