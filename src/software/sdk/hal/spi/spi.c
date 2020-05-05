#include "spi.h"

#ifndef OPTIMIZE_STACK
void HAL_SPI_WriteByte(uint8_t value){
    *PTR_SPI_DATA = value;
}

uint8_t HAL_SPI_ReadByte(){
    return *PTR_SPI_DATA;
}

uint8_t HAL_SPI_ReadByteWithAck() {
    return *PTR_SPI_DATA_WITH_ACK;
}

void HAL_SPI_SetDivider(uint8_t value){
    *PTR_SPI_DIVIDER = value;
}

void HAL_SPI_SetConfig(bool open_drain_sck, bool open_drain_mosi, bool sck_idle_level, bool mosi_idle_level, bool passthrough_sck){
    *PTR_SPI_CONFIG = (open_drain_sck?FLAG_OPEN_DRAIN_SCK:0) 
    | (open_drain_mosi?FLAG_OPEN_DRAIN_MOSI:0)
    | (sck_idle_level?FLAG_IDLE_SCK:0)
    | (mosi_idle_level?FLAG_IDLE_MOSI:0)
    | (passthrough_sck?FLAG_PASSTHROUGH_SCK:0);
}
#endif