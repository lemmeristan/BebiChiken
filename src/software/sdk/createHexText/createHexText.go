package main

import (
	"bufio"
	"encoding/binary"
	"fmt"
	"io"
	"os"
)

func main() {
	f, err := os.Open("content.bin")

	if err != nil {
		panic(err)
	}

	b1 := make([]byte, 1)

	initRAMf, err := os.Create("RAM.txt")
	if err != nil {
		panic(err)
	}
	w := bufio.NewWriter(initRAMf)

	for i := 0; i < 0x10000; i++ {
		_, err := f.Read(b1)
		if err != nil {
			panic(err)
		}

		//fmt.Printf("%02X\n", b1[0])
		_, err = w.WriteString(fmt.Sprintf("%02X\n", b1[0]))
		if err != nil {
			panic(err)
		}
	}
	w.Flush()

	ROMf, err := os.Create("ROM.txt")
	if err != nil {
		panic(err)
	}
	w = bufio.NewWriter(ROMf)

	b1 = make([]byte, 4)

	for {
		_, err = f.Read(b1)

		if err != nil {
			if err == io.EOF {
				break
			} else {
				panic(err)
			}
		}

		_, err = w.WriteString(fmt.Sprintf("%08X\n", binary.LittleEndian.Uint32(b1)))
		if err != nil {
			panic(err)
		}

	}

	w.Flush()

	// -----------------------------------------

	/*ROMf, err = os.Create("ROM.txt")
	if err != nil {
		panic(err)
	}
	w = bufio.NewWriter(ROMf)

	for _, v := range initialRAM {
		for i := 0; i < 4; i++ {
			_, err = w.WriteString(fmt.Sprintf("%02X\n", (v>>(i*8))&0xFF))
			if err != nil {
				panic(err)
			}
		}
	}
	w.Flush()*/

}
