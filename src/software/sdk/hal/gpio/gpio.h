#ifndef GPIO_H
    #include <stdint.h>

    #define GPIO_H
    #define ADDR_GPIO_BASE      0xC0002000

    #define PTR_GPIO_DIR        (volatile uint32_t *) ADDR_GPIO_BASE
    #define PTR_GPIO_DATA       (volatile uint32_t *) (ADDR_GPIO_BASE+4)

#ifndef OPTIMIZE_STACK
    void HAL_GPIO_SetDirections(uint32_t directions);
    void HAL_GPIO_SetOutputs(uint32_t values);
    uint32_t HAL_GPIO_GetInputs();
#else
    #define HAL_GPIO_SetDirections(directions) *PTR_GPIO_DIR = directions
    #define HAL_GPIO_SetOutputs(values) *PTR_GPIO_DATA = values
    #define HAL_GPIO_GetInputs() *PTR_GPIO_DATA
#endif

#endif
