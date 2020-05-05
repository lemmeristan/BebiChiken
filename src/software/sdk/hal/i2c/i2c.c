#include "i2c.h"

#ifndef OPTIMIZE_STACK
    void HAL_I2C_WriteByte(uint8_t value){
        *PTR_I2C_DATA = value;
    }

    uint8_t HAL_I2C_ReadByte(){
        return *PTR_I2C_DATA;
    }

    void HAL_I2C_SetDivider(uint16_t value){
        *PTR_I2C_DIVIDER = value;
    }

    uint8_t HAL_I2C_ReadACK(){
        return *PTR_I2C_ACK;
    }
    void HAL_I2C_WriteACK(bool value){
        *PTR_I2C_ACK = (value==true)?0:1;
    }
    void HAL_I2C_SetPins(uint8_t value){
        *PTR_I2C_PINS = value;
    }
    uint8_t HAL_I2C_GetPins(){
        return *PTR_I2C_PINS;
    }
#endif