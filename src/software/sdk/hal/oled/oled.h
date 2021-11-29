#ifndef OLED_H
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h> // for NULL
#include "../spi/spi.h"
#include "../gpio/gpio.h"
#include "../timebase/timebase.h"

#define OLED_CS (1 << 0)
#define OLED_DC (1 << 1)
#define OLED_RES (1 << 2)
#define OLED_VCCEN (1 << 3)
#define OLED_PMODEN (1 << 4)

#define Set_Column_Address 0x15
#define Set_Row_Address 0x75

// Internal Font size settings
#define NORMAL 0
#define WIDE 1
#define HIGH 2
#define WH 3
#define WHx36 4
#define X_width 6
#define Y_height 8
#define width 96 - 1  // Max X axial direction in screen
#define height 64 - 1 // Max Y axial direction in screen

#define x_offset 0
#define y_offset 0

void FontSizeConvert(int *lpx, int *lpy);
void HAL_OLED_Init();
void HAL_OLED_DrawBitmap(uint8_t col_start, uint8_t row_start, uint8_t col_end, uint8_t row_end, uint8_t *data);
void HAL_OLED_SetPixel(uint8_t col, uint8_t row, uint16_t color);
void HAL_OLED_PutChar(char value);
void HAL_OLED_ClearScreen();
void HAL_OLED_DrawGFXBuffer();

#define OLED_H
#endif