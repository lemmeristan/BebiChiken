#ifndef TIMEBASE_H
    #define TIMEBASE_H

    #include <stdint.h>

    #define ADDR_TIMEBASE_BASE      0xC0000000
    #define PTR_TIMEBASE_NANOS     (volatile uint32_t *) (ADDR_TIMEBASE_BASE)
    #define PTR_TIMEBASE_SEC       (volatile uint32_t *) (ADDR_TIMEBASE_BASE+4)
    #define PTR_TIMEBASE_DELAY_NS  (volatile uint32_t *) (ADDR_TIMEBASE_BASE+8)

    uint32_t HAL_Timebase_GetSeconds();
    uint32_t HAL_Timebase_GetNanos();
    void HAL_Timebase_DelayNS(uint32_t ns);
    void HAL_Timebase_DelayUS(uint32_t us);
    void HAL_Timebase_DelayMS(uint32_t ms);

#endif
