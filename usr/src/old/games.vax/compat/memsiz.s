.data 0
	.asciz "	memsiz.s	1.1	82/05/12	"
.text 0
	.set MEMSIZ,0x10000
	.text
	.globl _longspace
	.globl _shortspace
	.globl _bytespace
# set aside pdp11 memory space in nonprotected text segment at loc 0
_longspace:
_shortspace:
_bytespace:
	.space MEMSIZ
# put the memory size in a global variable for other uses
	.globl _memsiz
_memsiz:.long _memsiz
