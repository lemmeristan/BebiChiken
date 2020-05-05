#include "gpio.h"

#ifndef OPTIMIZE_STACK
void HAL_GPIO_SetDirections(uint32_t directions){
    *PTR_GPIO_DIR = directions;
}
void HAL_GPIO_SetOutputs(uint32_t values){
    *PTR_GPIO_DATA = values;
}
uint32_t HAL_GPIO_GetInputs(){
    return *PTR_GPIO_DATA;
}
#endif