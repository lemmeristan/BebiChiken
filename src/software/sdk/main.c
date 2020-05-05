#include <stdint.h>
#include <stdio.h> 
#include <stdarg.h>

//#include <string.h>

#include "hal/gpio/gpio.h"
#include "hal/spi/spi.h"
#include "hal/uart/uart.h"
#include "hal/timebase/timebase.h"
#include "hal/oled/oled.h"
#include "hal/i2c/i2c.h"

#include "printf/printf/printf.h"
#include "memmgr/memmgr.h"


// peripherals
#include "peripherals/BME680_driver/bme680.h"


void _putchar(char character)
{
    #ifdef DEBUG_UART
    HAL_UART_PutChar(character);
    #endif

    #ifdef DEBUG_OLED
    HAL_OLED_PutChar(character);
    #endif
}


int printf(const char* s, ...)
{
  char out[256];
  va_list vl;
  va_start(vl, s);
  vsnprintf(out, 255, s, vl);
  va_end(vl);
//  HAL_UART_PutLine(out);

  for(uint8_t i=0; out[i] != (char) 0; i++)
    _putchar(out[i]);

#ifdef DEBUG_OLED
    HAL_OLED_DrawGFXBuffer();
#endif

  return 0;
}

// interrupts not implemented

void _external_interrupt() {

//    printf("external interrupt triggered");
}

void _misaligned_instruction() {

}

void memset(void *dest, int c, size_t len) {
    for(int i=0; i<len; i++)
        *(uint8_t*) (dest+i) = c;
}

void memcpy(void *dest, void *src, size_t len) {
    for(size_t i=0; i<len; i++)
        *(uint8_t *) dest = *(uint8_t *) src;
}

void HAL_OLED_PutLine(const char* line) {
  while(*line != (char) 0) {
    HAL_OLED_PutChar(*line);
    line++;
  }
}

int HAL_OLED_printf(const char* s, ...)
{
  char out[256];
  va_list vl;
  va_start(vl, s);
  vsnprintf(out, 255, s, vl);
  va_end(vl);
  HAL_OLED_PutLine(out);
}



void HAL_I2C_StartCondition() {
    HAL_I2C_SetPins(PIN_SCL | PIN_SDA);
    HAL_Timebase_DelayUS(5);
    HAL_I2C_SetPins(PIN_SCL);
    HAL_Timebase_DelayUS(5);
}

void HAL_I2C_StopCondition() {
    HAL_I2C_SetPins(PIN_SCL);
    HAL_Timebase_DelayUS(5);
    HAL_I2C_SetPins(PIN_SCL | PIN_SDA);
    HAL_Timebase_DelayUS(5);
}

int16_t BME680_ReadRegister(uint8_t reg) {
    HAL_I2C_StartCondition();
    HAL_I2C_WriteByte((0x76 << 1) | 0); // write
    if(HAL_I2C_ReadACK() == 1)
        return -1;

    HAL_I2C_WriteByte(reg); // register address
    if(HAL_I2C_ReadACK() == 1)
        return -2;

    HAL_I2C_WriteACK(false);

    //HAL_I2C_StopCondition();
    
    HAL_I2C_StartCondition();
    HAL_I2C_WriteByte((0x76 << 1) | 1); // read
    if(HAL_I2C_ReadACK() == 1)
        return -3;

    uint8_t res = HAL_I2C_ReadByte();
    //HAL_I2C_WriteACK(true);

    HAL_I2C_WriteACK(false);
    HAL_I2C_WriteACK(true);


    HAL_I2C_StopCondition();
    return res;
}

int8_t BME680_WriteRegister(uint8_t reg, uint8_t value) {
    HAL_I2C_StartCondition();
    HAL_I2C_WriteByte((0x76 << 1) | 0); // write
    if(HAL_I2C_ReadACK() == 1)
        return -1;

    HAL_I2C_WriteByte(reg); // register address
    if(HAL_I2C_ReadACK() == 1)
        return -2;

    HAL_I2C_WriteByte(value); // register data
    if(HAL_I2C_ReadACK() == 1)
        return -3;

    HAL_I2C_WriteACK(false);

    HAL_I2C_StopCondition();
    return 0;
}

int8_t BME680_Init() {
    HAL_I2C_SetDivider(0xFF);
    HAL_I2C_SetPins(PIN_SCL | PIN_SDA);
    return 0;


    int8_t res = BME680_WriteRegister(0xE0, 0xB6); // soft reset
    if(res < 0)
        printf("\n\rSoft reset result: %x", res);
    else
        HAL_Timebase_DelayMS(5);
    return res;
}


void user_delay_ms(uint32_t period)
{
    HAL_Timebase_DelayMS(period);
}

int8_t user_spi_read(uint8_t dev_id, uint8_t reg_addr, uint8_t *reg_data, uint16_t len)
{
    int8_t rslt = 0; /* Return 0 for Success, non-zero for failure */

    /*
     * The parameter dev_id can be used as a variable to select which Chip Select pin has
     * to be set low to activate the relevant device on the SPI bus
     */

    /*
     * Data on the bus should be like
     * |----------------+---------------------+-------------|
     * | MOSI           | MISO                | Chip Select |
     * |----------------+---------------------|-------------|
     * | (don't care)   | (don't care)        | HIGH        |
     * | (reg_addr)     | (don't care)        | LOW         |
     * | (don't care)   | (reg_data[0])       | LOW         |
     * | (....)         | (....)              | LOW         |
     * | (don't care)   | (reg_data[len - 1]) | LOW         |
     * | (don't care)   | (don't care)        | HIGH        |
     * |----------------+---------------------|-------------|
     */

    return rslt;
}

int8_t user_spi_write(uint8_t dev_id, uint8_t reg_addr, uint8_t *reg_data, uint16_t len)
{
    int8_t rslt = 0; /* Return 0 for Success, non-zero for failure */

    /*
     * The parameter dev_id can be used as a variable to select which Chip Select pin has
     * to be set low to activate the relevant device on the SPI bus
     */

    /*
     * Data on the bus should be like
     * |---------------------+--------------+-------------|
     * | MOSI                | MISO         | Chip Select |
     * |---------------------+--------------|-------------|
     * | (don't care)        | (don't care) | HIGH        |
     * | (reg_addr)          | (don't care) | LOW         |
     * | (reg_data[0])       | (don't care) | LOW         |
     * | (....)              | (....)       | LOW         |
     * | (reg_data[len - 1]) | (don't care) | LOW         |
     * | (don't care)        | (don't care) | HIGH        |
     * |---------------------+--------------|-------------|
     */

    return rslt;
}

int8_t user_i2c_read(uint8_t dev_id, uint8_t reg_addr, uint8_t *reg_data, uint16_t len) // correct, don't change
{
 
    int16_t res;

    while(len) {
        res = BME680_ReadRegister(reg_addr);
        if(res < 0) {
            printf("\n\r\n\rRead from addr %x (length %x)", reg_addr, len);
            printf("\n\rRead error: reg_addr = %x, result = %x", reg_addr, res);
            return res;
        }
        *reg_data = (uint8_t) res;
        reg_data++;
        reg_addr++;
        len--;
    }

    return 0;
}

int8_t user_i2c_write(uint8_t dev_id, uint8_t reg_addr, uint8_t *reg_data, uint16_t len) // correct, don't change
{   
    int8_t res = 0;

    while(len) {
        res = BME680_WriteRegister(reg_addr, *reg_data);

        if(res < 0) {
            printf("\n\r\n\rWrite to addr %x (length %d)", reg_addr, len);
            printf("\n\rWrite error: reg_addr = %02x, data = %02x, error = %d", reg_addr, *reg_data, res);
            return res;
        }

        reg_data++;
        //reg_addr++;
        len--;
    }
}



void main() {
#ifdef DEBUG_OLED
    HAL_OLED_Init();
    printf("OLED display initialized.");
#endif

    if(BME680_Init() == 0) {
    struct bme680_dev gas_sensor;

    gas_sensor.dev_id = BME680_I2C_ADDR_PRIMARY;
    gas_sensor.intf = BME680_I2C_INTF;
    gas_sensor.read = user_i2c_read;
    gas_sensor.write = user_i2c_write;
    gas_sensor.delay_ms = user_delay_ms;
    /* amb_temp can be set to 25 prior to configuring the gas sensor 
     * or by performing a few temperature readings without operating the gas sensor.
     */
    gas_sensor.amb_temp = 25;


    int8_t rslt = BME680_OK;
    rslt = bme680_init(&gas_sensor);


    // set up measurement parameters
    uint8_t set_required_settings;

    /* Set the temperature, pressure and humidity settings */
    gas_sensor.tph_sett.os_hum = BME680_OS_2X;
    gas_sensor.tph_sett.os_pres = BME680_OS_4X;
    gas_sensor.tph_sett.os_temp = BME680_OS_8X;
    gas_sensor.tph_sett.filter = BME680_FILTER_SIZE_3;

    /* Set the remaining gas sensor settings and link the heating profile */
    gas_sensor.gas_sett.run_gas = BME680_ENABLE_GAS_MEAS;
    /* Create a ramp heat waveform in 3 steps */
    gas_sensor.gas_sett.heatr_temp = 320; /* degree Celsius */
    gas_sensor.gas_sett.heatr_dur = 750; /* milliseconds */

    /* Select the power mode */
    /* Must be set before writing the sensor configuration */
    gas_sensor.power_mode = BME680_FORCED_MODE; 

    /* Set the required sensor settings needed */
    set_required_settings = BME680_OST_SEL | BME680_OSP_SEL | BME680_OSH_SEL | BME680_FILTER_SEL 
        | BME680_GAS_SENSOR_SEL;

    /* Set the desired sensor configuration */
    rslt = bme680_set_sensor_settings(set_required_settings,&gas_sensor);
    //printf("\n\rbme680_set_sensor_settings: %d", rslt);

    /* Set the power mode */
    rslt = bme680_set_sensor_mode(&gas_sensor);
    //printf("\n\rbme680_set_sensor_mode: %d", rslt);

        /* Get the total measurement duration so as to sleep or wait till the
     * measurement is complete */
    uint16_t meas_period;
    bme680_get_profile_dur(&meas_period, &gas_sensor);
    //printf("\n\rbme680_get_profile_dur: %d, meas_period: %d", rslt, meas_period);

    struct bme680_field_data data;
    while(1)
    {
        user_delay_ms(meas_period); /* Delay till the measurement is ready */

        rslt = bme680_get_sensor_data(&data, &gas_sensor);


#ifdef DEBUG_OLED
        HAL_OLED_ClearScreen();
#endif

        printf("\n\rTemperat.: %3d C", data.temperature / 100);
        printf("\n\rHumidity: %4d %%", data.humidity / 1000);
        printf("\n\rPressu.: %3d hPa", data.pressure / 100);

        /* Avoid using measurements from an unstable heating setup */
        if(data.status & BME680_GASM_VALID_MSK)
            if(data.gas_resistance < 50)
                printf("\n\rIAQ: good");
            else if(data.gas_resistance < 100)
                printf("\n\rIAQ: average");
            else if(data.gas_resistance < 150)
                printf("\n\rIAQ: little bad");
            else if(data.gas_resistance < 200)
                printf("\n\rIAQ: bad");
            else if(data.gas_resistance < 250)
                printf("\n\rIAQ: worse");
            else if(data.gas_resistance < 300)
                printf("\n\rIAQ: average");
            else
                printf("\n\rIAQ: very bad");

            //printf("\n\rGas resistance: %d Ohm", data.gas_resistance);



        /* Trigger the next measurement if you would like to read data out continuously */
        if (gas_sensor.power_mode == BME680_FORCED_MODE) {
            rslt = bme680_set_sensor_mode(&gas_sensor);
        }
    }


    }
    else
        printf("BME680 could not be initialized.");
}