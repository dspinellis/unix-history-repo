/ C library -- getchar

.globl	_getchar
.globl	_fin

.globl	getc

.data
_getchar:
	1f
.text
1:
	jsr	r5,getc; _fin
	bcs	1f
	tst	r0
	beq	1b
	rts	pc
1:
	clr	r0
	rts	pc

.bss
_fin:	.=.+518.

