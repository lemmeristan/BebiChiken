#include "timebase.h"

uint32_t HAL_Timebase_GetSeconds() {
    return *PTR_TIMEBASE_SEC;
}

uint32_t HAL_Timebase_GetNanos() {
    return *PTR_TIMEBASE_NANOS;
}

void HAL_Timebase_DelayNS(uint32_t ns) {
    *PTR_TIMEBASE_DELAY_NS = ns;
}

void HAL_Timebase_DelayUS(uint32_t us) {
    for(uint32_t i=0; i<us; i++)
        HAL_Timebase_DelayNS(1000);
}

void HAL_Timebase_DelayMS(uint32_t ms) {
    for(uint32_t i=0; i<ms; i++)
        HAL_Timebase_DelayNS(1000000);
}