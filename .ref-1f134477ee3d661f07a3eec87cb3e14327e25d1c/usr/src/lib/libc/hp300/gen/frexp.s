#NO_APP
gcc_compiled.:
.text
	.even
.globl _frexp
_frexp:
	link a6,#0
	moveml #0x3000,sp@-
	fmoved a6@(8),fp0
	movel a6@(16),a0
	fjeq L2
	fmoved fp0,sp@-
	movel sp@+,d0
	movel sp@+,d1
	bfextu d0{#1:#11},d2
	addl #-1022,d2
	movel d2,a0@
	movel #1022,d3
	bfins d3,d0{#1:#11}
	jra L1
L2:
	clrl a0@
	clrl d0
	clrl d1
L1:
	moveml a6@(-8),#0xc
	unlk a6
	rts
