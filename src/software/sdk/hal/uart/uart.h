#ifndef UART_H
    #include <stdint.h>

    #define UART_H
    #define ADDR_UART_BASE      0xC0001000

    #define PTR_UART_STATUS     (volatile uint8_t *) ADDR_UART_BASE
    #define PTR_UART_TX_DATA    (volatile uint8_t *) (ADDR_UART_BASE+1)
    #define PTR_UART_RX_DATA    (volatile uint8_t *) (ADDR_UART_BASE+2)

    #define FLAG_UART_TX_FIFO_BUSY  1<<0

#ifndef OPTIMIZE_STACK
    void HAL_UART_WriteBuffer(uint8_t *src, uint32_t length);
    void HAL_UART_PutChar(char character);
    void HAL_UART_PutLine(char *line);

#else
    #define writeToUart(src, length) for(uint32_t i=0; i<length; i++) { \
        while(*PTR_UART_STATUS & FLAG_UART_TX_FIFO_BUSY); \
        *PTR_UART_TX_DATA = *(src+i); \
    } \

    #define HAL_UART_PutChar(character) while(*PTR_UART_STATUS & FLAG_UART_TX_FIFO_BUSY); *PTR_UART_TX_DATA = (uint8_t) character;

    #define HAL_UART_PutLine(line) char* ptr = line; while(*ptr != (char) 0) {HAL_UART_PutChar(*ptr); ptr++;} 

#endif

#endif
