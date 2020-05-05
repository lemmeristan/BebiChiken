#ifndef SPI_H
    #include <stdint.h>
    #include <stdbool.h> 

    #define SPI_H
    #define ADDR_SPI_BASE      0xC0003000

    #define PTR_SPI_DATA        (volatile uint8_t *) ADDR_SPI_BASE
    #define PTR_SPI_DIVIDER       (volatile uint8_t *) (ADDR_SPI_BASE+1)
    #define PTR_SPI_CONFIG       (volatile uint8_t *) (ADDR_SPI_BASE+2)
    #define PTR_SPI_DATA_WITH_ACK       (volatile uint8_t *) (ADDR_SPI_BASE+3)


    #define FLAG_OPEN_DRAIN_SCK (1 << 1)
    #define FLAG_OPEN_DRAIN_MOSI (1 << 2)
    #define FLAG_IDLE_SCK (1 << 3)
    #define FLAG_IDLE_MOSI (1 << 4)
    #define FLAG_PASSTHROUGH_SCK (1 << 5)

#ifndef OPTIMIZE_STACK
    void HAL_SPI_WriteByte(uint8_t value);
    uint8_t HAL_SPI_ReadByte();
    uint8_t HAL_SPI_ReadByteWithAck();
    void HAL_SPI_SetDivider(uint8_t value);
    void HAL_SPI_SetConfig(bool open_drain_sck, bool open_drain_mosi, bool sck_idle_level, bool mosi_idle_level, bool passthrough_sck);
#else
    #define HAL_SPI_WriteByte(value) *PTR_SPI_DATA = value & 0xFF;
    #define HAL_SPI_ReadByte() *PTR_SPI_DATA
    #define HAL_SPI_ReadByteWithAck() *PTR_SPI_DATA_WITH_ACK

    #define HAL_SPI_SetDivider(value) *PTR_SPI_DIVIDER = value & 0xFF;
    #define HAL_SPI_SetConfig(open_drain_sck, open_drain_mosi, sck_idle_level, mosi_idle_level, passthrough_sck) *PTR_SPI_CONFIG = (open_drain_sck?FLAG_OPEN_DRAIN_SCK:0) \
    | (open_drain_mosi?FLAG_OPEN_DRAIN_MOSI:0) \
    | (sck_idle_level?FLAG_IDLE_SCK:0) \
    | (mosi_idle_level?FLAG_IDLE_MOSI:0) \
    | (passthrough_sck?FLAG_PASSTHROUGH_SCK:0);
#endif

#endif
