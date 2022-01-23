#ifndef I2C_H
#include <stdint.h>
#include <stdbool.h>

#define I2C_H
#define ADDR_I2C_BASE 0xC0004000

#define PTR_I2C_DATA (volatile uint8_t *)ADDR_I2C_BASE
#define PTR_I2C_ACK (volatile uint8_t *)(ADDR_I2C_BASE + 1)
#define PTR_I2C_PINS (volatile uint8_t *)(ADDR_I2C_BASE + 2)
#define PTR_I2C_DIVIDER (volatile uint16_t *)(ADDR_I2C_BASE + 4)

#define PIN_SCL (1 << 0)
#define PIN_SDA (1 << 1)

#ifndef OPTIMIZE_STACK
void HAL_I2C_WriteByte(uint8_t value);
uint8_t HAL_I2C_ReadByte();
void HAL_I2C_SetDivider(uint16_t value);
uint8_t HAL_I2C_ReadACK();
void HAL_I2C_WriteACK(bool value);
void HAL_I2C_SetPins(uint8_t value);
uint8_t HAL_I2C_GetPins();

#else
#define HAL_I2C_WriteByte(value) *PTR_I2C_DATA = value & 0xFF;
#define HAL_I2C_ReadByte() *PTR_I2C_DATA
#define HAL_I2C_SetDivider(value) *PTR_I2C_DIVIDER = value & 0xFF;
#define HAL_I2C_ReadACK() *PTR_I2C_ACK
#define HAL_I2C_WriteACK(value) *PTR_I2C_ACK = (value == true) ? 0 : 1;
#define HAL_I2C_SetPins(value) *PTR_I2C_PINS = value;
#define HAL_I2C_GetPins *PTR_I2C_PINS
#endif

#endif
