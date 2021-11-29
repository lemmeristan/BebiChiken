#include "oled.h"

uint8_t _x1;
uint8_t _x2;
uint8_t _y1;
uint8_t _y2;
uint8_t char_x;
uint8_t char_y;
uint8_t chr_size;
uint8_t cwidth; // character's width
uint8_t cvert;  // character's height
uint8_t externalfont;
uint16_t Char_Color;    // text color
uint16_t BGround_Color; // background color
uint16_t TBorder_Color; // border of text color

static const char font6x8[0x60][6] = {
    {0x00, 0x00, 0x00, 0x00, 0x00, 0x00}, /*SPC */
    {0x00, 0x00, 0x5F, 0x00, 0x00, 0x00}, /* !  */
    {0x04, 0x03, 0x04, 0x03, 0x00, 0x00}, /* "  */
    {0x28, 0x7E, 0x14, 0x3F, 0x0A, 0x00}, /* #  */
    {0x24, 0x2A, 0x7F, 0x2A, 0x12, 0x00}, /* $  */
    {0x23, 0x13, 0x08, 0x64, 0x62, 0x00}, /* %  */
    {0x30, 0x4E, 0x59, 0x26, 0x50, 0x00}, /* &  */
    {0x00, 0x00, 0x02, 0x01, 0x00, 0x00}, /* '  */
    {0x00, 0x00, 0x1C, 0x22, 0x41, 0x00}, /* (  */
    {0x41, 0x22, 0x1C, 0x00, 0x00, 0x00}, /* )  */
    {0x22, 0x14, 0x08, 0x14, 0x22, 0x00}, /* *  */
    {0x08, 0x08, 0x3E, 0x08, 0x08, 0x00}, /* +  */
    {0x50, 0x30, 0x00, 0x00, 0x00, 0x00}, /* ,  */
    {0x08, 0x08, 0x08, 0x08, 0x08, 0x00}, /* -  */
    {0x60, 0x60, 0x00, 0x00, 0x00, 0x00}, /* .  */
    {0x20, 0x10, 0x08, 0x04, 0x02, 0x00}, /* /  */
    {0x3E, 0x51, 0x49, 0x45, 0x3E, 0x00}, /* 0  */
    {0x00, 0x42, 0x7F, 0x40, 0x00, 0x00}, /* 1  */
    {0x62, 0x51, 0x49, 0x49, 0x46, 0x00}, /* 2  */
    {0x22, 0x41, 0x49, 0x49, 0x36, 0x00}, /* 3  */
    {0x18, 0x14, 0x12, 0x7F, 0x10, 0x00}, /* 4  */
    {0x2F, 0x45, 0x45, 0x45, 0x39, 0x00}, /* 5  */
    {0x3E, 0x49, 0x49, 0x49, 0x32, 0x00}, /* 6  */
    {0x01, 0x61, 0x19, 0x05, 0x03, 0x00}, /* 7  */
    {0x36, 0x49, 0x49, 0x49, 0x36, 0x00}, /* 8  */
    {0x26, 0x49, 0x49, 0x49, 0x3E, 0x00}, /* 9  */
    {0x00, 0x36, 0x36, 0x00, 0x00, 0x00}, /* :  */
    {0x00, 0x56, 0x36, 0x00, 0x00, 0x00}, /* ;  */
    {0x00, 0x08, 0x14, 0x22, 0x41, 0x00}, /* <  */
    {0x14, 0x14, 0x14, 0x14, 0x14, 0x00}, /* =  */
    {0x41, 0x22, 0x14, 0x08, 0x00, 0x00}, /* >  */
    {0x02, 0x01, 0x59, 0x09, 0x06, 0x00}, /* ?  */
    {0x3E, 0x41, 0x5D, 0x55, 0x2E, 0x00}, /* @  */
    {0x60, 0x1C, 0x13, 0x1C, 0x60, 0x00}, /* A  */
    {0x7F, 0x49, 0x49, 0x49, 0x36, 0x00}, /* B  */
    {0x3E, 0x41, 0x41, 0x41, 0x22, 0x00}, /* C  */
    {0x7F, 0x41, 0x41, 0x22, 0x1C, 0x00}, /* D  */
    {0x7F, 0x49, 0x49, 0x49, 0x41, 0x00}, /* E  */
    {0x7F, 0x09, 0x09, 0x09, 0x01, 0x00}, /* F  */
    {0x1C, 0x22, 0x41, 0x49, 0x3A, 0x00}, /* G  */
    {0x7F, 0x08, 0x08, 0x08, 0x7F, 0x00}, /* H  */
    {0x00, 0x41, 0x7F, 0x41, 0x00, 0x00}, /* I  */
    {0x20, 0x40, 0x40, 0x40, 0x3F, 0x00}, /* J  */
    {0x7F, 0x08, 0x14, 0x22, 0x41, 0x00}, /* K  */
    {0x7F, 0x40, 0x40, 0x40, 0x00, 0x00}, /* L  */
    {0x7F, 0x04, 0x18, 0x04, 0x7F, 0x00}, /* M  */
    {0x7F, 0x04, 0x08, 0x10, 0x7F, 0x00}, /* N  */
    {0x3E, 0x41, 0x41, 0x41, 0x3E, 0x00}, /* O  */
    {0x7F, 0x09, 0x09, 0x09, 0x06, 0x00}, /* P  */
    {0x3E, 0x41, 0x51, 0x21, 0x5E, 0x00}, /* Q  */
    {0x7F, 0x09, 0x19, 0x29, 0x46, 0x00}, /* R  */
    {0x26, 0x49, 0x49, 0x49, 0x32, 0x00}, /* S  */
    {0x01, 0x01, 0x7F, 0x01, 0x01, 0x00}, /* T  */
    {0x3F, 0x40, 0x40, 0x40, 0x3F, 0x00}, /* U  */
    {0x03, 0x1C, 0x60, 0x1C, 0x03, 0x00}, /* V  */
    {0x0F, 0x70, 0x0F, 0x70, 0x0F, 0x00}, /* W  */
    {0x41, 0x36, 0x08, 0x36, 0x41, 0x00}, /* X  */
    {0x01, 0x06, 0x78, 0x02, 0x01, 0x00}, /* Y  */
    {0x61, 0x51, 0x49, 0x45, 0x43, 0x00}, /* Z  */
    {0x00, 0x00, 0x7F, 0x41, 0x41, 0x00}, /* [  */
    {0x15, 0x16, 0x7C, 0x16, 0x11, 0x00}, /* \  */
    {0x41, 0x41, 0x7F, 0x00, 0x00, 0x00}, /* ]  */
    {0x00, 0x02, 0x01, 0x02, 0x00, 0x00}, /* ^  */
    {0x40, 0x40, 0x40, 0x40, 0x40, 0x00}, /* _  */
    {0x00, 0x01, 0x02, 0x00, 0x00, 0x00}, /* `  */
    {0x00, 0x20, 0x54, 0x54, 0x78, 0x00}, /* a  */
    {0x00, 0x7F, 0x44, 0x44, 0x38, 0x00}, /* b  */
    {0x00, 0x38, 0x44, 0x44, 0x28, 0x00}, /* c  */
    {0x00, 0x38, 0x44, 0x44, 0x7F, 0x00}, /* d  */
    {0x00, 0x38, 0x54, 0x54, 0x18, 0x00}, /* e  */
    {0x00, 0x04, 0x3E, 0x05, 0x01, 0x00}, /* f  */
    {0x00, 0x08, 0x54, 0x54, 0x3C, 0x00}, /* g  */
    {0x00, 0x7F, 0x04, 0x04, 0x78, 0x00}, /* h  */
    {0x00, 0x00, 0x7D, 0x00, 0x00, 0x00}, /* i  */
    {0x00, 0x40, 0x40, 0x3D, 0x00, 0x00}, /* j  */
    {0x00, 0x7F, 0x10, 0x28, 0x44, 0x00}, /* k  */
    {0x00, 0x01, 0x7F, 0x00, 0x00, 0x00}, /* l  */
    {0x7C, 0x04, 0x7C, 0x04, 0x78, 0x00}, /* m  */
    {0x00, 0x7C, 0x04, 0x04, 0x78, 0x00}, /* n  */
    {0x00, 0x38, 0x44, 0x44, 0x38, 0x00}, /* o  */
    {0x00, 0x7C, 0x14, 0x14, 0x08, 0x00}, /* p  */
    {0x00, 0x08, 0x14, 0x14, 0x7C, 0x00}, /* q  */
    {0x00, 0x7C, 0x08, 0x04, 0x04, 0x00}, /* r  */
    {0x00, 0x48, 0x54, 0x54, 0x24, 0x00}, /* s  */
    {0x00, 0x04, 0x3E, 0x44, 0x40, 0x00}, /* t  */
    {0x00, 0x3C, 0x40, 0x40, 0x7C, 0x00}, /* u  */
    {0x00, 0x7C, 0x20, 0x10, 0x0C, 0x00}, /* v  */
    {0x1C, 0x60, 0x1C, 0x60, 0x1C, 0x00}, /* w  */
    {0x00, 0x6C, 0x10, 0x10, 0x6C, 0x00}, /* x  */
    {0x00, 0x4C, 0x50, 0x30, 0x1C, 0x00}, /* y  */
    {0x00, 0x44, 0x64, 0x54, 0x4C, 0x00}, /* z  */
    {0x00, 0x08, 0x36, 0x41, 0x41, 0x00}, /* {  */
    {0x00, 0x00, 0x7F, 0x00, 0x00, 0x00}, /* |  */
    {0x41, 0x41, 0x36, 0x08, 0x00, 0x00}, /* }  */
    {0x08, 0x04, 0x08, 0x10, 0x08, 0x00}, /* ~  */
    {0x00, 0x00, 0x00, 0x00, 0x00, 0x00}  /*null*/
};

static uint16_t gfxbuffer[height + 1][width + 1];

void FontSizeConvert(int *lpx, int *lpy)
{
    switch (chr_size)
    {
    case WIDE:
        *lpx = 2;
        *lpy = 1;
        break;
    case HIGH:
        *lpx = 1;
        *lpy = 2;
        break;
    case WH:
        *lpx = 2;
        *lpy = 2;
        break;
    case WHx36:
        *lpx = 6;
        *lpy = 6;
        break;
    case NORMAL:
    default:
        *lpx = 1;
        *lpy = 1;
        break;
    }
}

void SetFontSize(uint8_t Csize)
{
    chr_size = Csize;
}

void SetFontColor(uint16_t foreground, uint16_t background, uint16_t border)
{
    Char_Color = foreground;
    BGround_Color = background;
    TBorder_Color = border;
}

void HAL_OLED_DrawGFXBuffer()
{
    HAL_GPIO_SetOutputs(OLED_RES | OLED_PMODEN | OLED_VCCEN);

    uint8_t spicommands[6] = {Set_Column_Address, 0, width, Set_Row_Address, 0, height};
    for (uint8_t i = 0; i < 6; i++)
    {
        HAL_SPI_WriteByte(spicommands[i]);
    }

    HAL_GPIO_SetOutputs(OLED_DC | OLED_RES | OLED_PMODEN | OLED_VCCEN);

    for (uint32_t y = 0; y <= height; y++)
        for (uint32_t x = 0; x <= width; x++)
        {
            HAL_SPI_WriteByte(gfxbuffer[y][x] >> 8);
            HAL_SPI_WriteByte(gfxbuffer[y][x] >> 0);
        }
    HAL_GPIO_SetOutputs(OLED_CS | OLED_RES | OLED_PMODEN | OLED_VCCEN);
    HAL_Timebase_DelayMS(5);
}

void HAL_OLED_ClearScreen()
{
    for (uint32_t x = 0; x <= width; x++)
        for (uint32_t y = 0; y <= height; y++)
            gfxbuffer[y][x] = 0; // black
    HAL_OLED_DrawBitmap(0, 0, 95, 63, NULL);
    char_x = x_offset;
    char_y = y_offset;
}

void HAL_OLED_Init()
{
    HAL_SPI_SetDivider(100); // 1 MHz
    HAL_SPI_SetConfig(false, false, true, false, false);

    HAL_GPIO_SetOutputs(OLED_CS | OLED_RES | OLED_PMODEN);
    HAL_GPIO_SetDirections(OLED_CS | OLED_DC | OLED_RES | OLED_VCCEN | OLED_PMODEN);
    HAL_Timebase_DelayMS(20);
    HAL_GPIO_SetOutputs(OLED_CS | OLED_PMODEN);
    HAL_Timebase_DelayUS(3);
    HAL_GPIO_SetOutputs(OLED_CS | OLED_RES | OLED_PMODEN);
    HAL_Timebase_DelayUS(3);
    HAL_GPIO_SetOutputs(OLED_RES | OLED_PMODEN);

    const uint8_t spicommands[44] = {
        // 10 bytes
        0xFD, 0x12, // unlock
        0xAE,       // turn display off
        0xA0, 0x42, // set remap and display formats
                    // [0] = 0 -> horizontal address increment
                    // [1] = 1 ->
                    // [2] = 0 ->

        // [3] = 0 ->
        // [4] = 1 ->  COM Scan Direction Remap
        // [5] = 1 ->

        // [6] = 1 ->
        // [7] = 0 ->
        0xA1, 0x00, // set display start Line to the top line
        0xA2, 0x00, // set display offset to no vertical offset
        0xA4,       // normal display with no color inversion or forcing the pixels on/off

        // 28 bytes
        0xA8, 0x40, // the multiplex ratio command and the single byte value
        0xAD, 0x8E, // the master configuration command and the required single byte value of 0x8E
        0xB0, 0x0B, // disable power saving mode
        0xB1, 0x31, // phase length
        0xB3, 0xF0, // clock ratio and oscillator frequency
        0x8A, 0x64, // color A 2nd precharge speed
        0x8B, 0x78, // color B 2nd precharge speed
        0x8C, 0x64, // color C 2nd precharge speed
        0xBB, 0x3A, // precharge voltage (45%)
        0xBE, 0x3E, // VCOMH Deselect level (83%)
        0x87, 0x06, // master current attenuation factor
        0x81, 0x91, // Color A contrast
        0x82, 0x50, // Color B contrast
        0x83, 0x7D, // Color C contrast

        // 6 bytes
        0x2E,                        // disable scrolling
        0x25, 0x00, 0x00, 0x5F, 0x3F // clear command and the five bytes representing the area to clear (5)
    };

    for (uint8_t i = 0; i < 44; i++)
    {
        HAL_SPI_WriteByte(spicommands[i]);
    }
    HAL_GPIO_SetOutputs(OLED_RES | OLED_PMODEN | OLED_VCCEN);
    HAL_Timebase_DelayMS(25);
    HAL_SPI_WriteByte(0xAF); // turn display on
    HAL_Timebase_DelayMS(100);
    HAL_GPIO_SetOutputs(OLED_CS | OLED_RES | OLED_PMODEN | OLED_VCCEN);

    SetFontSize(NORMAL);
    SetFontColor(0xFFFF, /*0xFCA0*/ 0, 0); // white font on black background
    HAL_OLED_ClearScreen();
}

void HAL_OLED_DrawBitmap(uint8_t col_start, uint8_t row_start, uint8_t col_end, uint8_t row_end, uint8_t *data)
{
    HAL_GPIO_SetOutputs(OLED_RES | OLED_PMODEN | OLED_VCCEN);

    uint8_t spicommands[6] = {0x15, col_start, col_end, 0x75, row_start, row_end};
    for (uint8_t i = 0; i < 6; i++)
    {
        HAL_SPI_WriteByte(spicommands[i]);
    }

    HAL_GPIO_SetOutputs(OLED_DC | OLED_RES | OLED_PMODEN | OLED_VCCEN);

    for (uint32_t i = 0; i < (((col_end - col_start + 1) * (row_end - row_start + 1)) << 1); i++)
    {
        HAL_SPI_WriteByte(BGround_Color); // HAL_SPI_WriteByte(*(data+i));
    }
    HAL_GPIO_SetOutputs(OLED_CS | OLED_RES | OLED_PMODEN | OLED_VCCEN);

    HAL_Timebase_DelayMS(5);
}

#define fixrow(y) ((y & 1) == 0) ? (y >> 1) : (y + 31 - (y >> 1))

void HAL_OLED_SetPixel(uint8_t col, uint8_t row, uint16_t color)
{
    /*
    if ((col>width)||(row>height)) return ;

    uint8_t cmd[7]= {Set_Column_Address,col,col,Set_Row_Address,row,row};

    HAL_GPIO_SetOutputs(OLED_RES | OLED_PMODEN | OLED_VCCEN);
    for(uint8_t i=0; i<7; i++) {
        HAL_SPI_WriteByte(cmd[i]);
    }
    HAL_GPIO_SetOutputs(OLED_DC | OLED_RES | OLED_PMODEN | OLED_VCCEN);
    HAL_SPI_WriteByte(color >> 8);
    HAL_SPI_WriteByte(color & 0xFF);
    HAL_GPIO_SetOutputs(OLED_CS | OLED_RES | OLED_PMODEN | OLED_VCCEN);
    */

    uint8_t temp = row >> 1;
    if ((row & 1) == 0)
        gfxbuffer[fixrow(row)][col] = color;
    else
        gfxbuffer[fixrow(row)][col] = color;

    //     gfxbuffer[row][col] = color;
}

void HAL_OLED_PutChar(char value)
{
    /*if(value == '\n') {
        char_x = x_offset;
        if (char_y + Y_height > height) // go back to first line
            char_y = y_offset;
        else
            char_y = char_y + Y_height;
    }
    if (char_x + X_width > width) { // go to next line
        char_x = x_offset;
        char_y = char_y + Y_height;
        if (char_y > height) {
            char_y = y_offset;
        }
    }*/

    if ((value == '\n') || (char_x > width))
    { // new line, carriage return
        char_x = x_offset;
        char_y = char_y + Y_height;
    }

    if (char_y > height)
        char_y = y_offset; // return to line 0

    if ((value < 31) || (value > 127))
        return; // test char range

    for (uint8_t i = 0; i < X_width; i++)
        for (uint8_t j = 0; j < Y_height; j++)
            gfxbuffer[fixrow(char_y + j)][char_x + i] = BGround_Color;

    for (uint8_t i = 0; i < X_width; i++)
    {
        uint8_t temp = font6x8[value - 32][i];

        for (uint8_t j = 0; j < Y_height; j++)
        {
            //HAL_OLED_SetPixel(char_x+i, char_y+j,  (temp & 1) ? Char_Color : BGround_Color);

            uint8_t x = char_x + i, y = char_y + j;
            if (temp & 1)
            {
                gfxbuffer[fixrow(char_y + j)][char_x + i] = Char_Color;

#ifdef OLED_TEXTBORDER
                if ((char_x + i > x_offset) && (char_x + i < width) && (char_y + j > y_offset) && (char_y + j < height))
                {
                    if (gfxbuffer[fixrow(y - 1)][x - 1] == BGround_Color)
                        gfxbuffer[fixrow(y - 1)][x - 1] = TBorder_Color;
                    if (gfxbuffer[fixrow(y - 1)][x] == BGround_Color)
                        gfxbuffer[fixrow(y - 1)][x] = TBorder_Color;
                    if (gfxbuffer[fixrow(y - 1)][x + 1] == BGround_Color)
                        gfxbuffer[fixrow(y - 1)][x + 1] = TBorder_Color;

                    if (gfxbuffer[fixrow(y)][x - 1] == BGround_Color)
                        gfxbuffer[fixrow(y)][x - 1] = TBorder_Color;
                    if (gfxbuffer[fixrow(y)][x + 1] == BGround_Color)
                        gfxbuffer[fixrow(y)][x + 1] = TBorder_Color;

                    if (gfxbuffer[fixrow(y + 1)][x - 1] == BGround_Color)
                        gfxbuffer[fixrow(y + 1)][x - 1] = TBorder_Color;
                    if (gfxbuffer[fixrow(y + 1)][x] == BGround_Color)
                        gfxbuffer[fixrow(y + 1)][x] = TBorder_Color;
                    if (gfxbuffer[fixrow(y + 1)][x + 1] == BGround_Color)
                        gfxbuffer[fixrow(y + 1)][x + 1] = TBorder_Color;
                }
#endif
            }

            temp >>= 1;
        }
    }

    char_x += X_width;
}