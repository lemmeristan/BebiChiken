MEMORY
{
  ram : ORIGIN = 0x0000000, LENGTH = 64K
  rom : ORIGIN = 0x400000, LENGTH = 64K
}

PROVIDE(_ram_start = ORIGIN(ram));
PROVIDE(__stack_top = ORIGIN(ram) + LENGTH(ram));

OUTPUT_ARCH( "riscv" )

ENTRY(_start)

SECTIONS
{
/*  .header :
  {
    *(.header .header.*)
    . = ALIGN(16);
    PROVIDE(_eheader = ABSOLUTE(.));
  } > ram
*/

  .rodata :
  {
    *(.rodata .rodata.*)
  } >ram

  .data :
  {
    *(.data .data.*)
    . = ALIGN(8);
    PROVIDE(_edata = ABSOLUTE(.));
  } >ram

  .bss :
  {
    PROVIDE(_bss = ABSOLUTE(.));
    *(.bss .bss.*)
    *(COMMON)
    . = ALIGN(8);
    PROVIDE(_ebss = ABSOLUTE(.));
  } >ram


  .text :
  {
    KEEP(*(.text.start))
    *(.text .text.*)
    . = ALIGN(8);
    PROVIDE(_etext = ABSOLUTE(.));
  } >rom

  /*. = ALIGN(64);*/
  PROVIDE(_end = ABSOLUTE(.));

}