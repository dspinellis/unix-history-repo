/ C library -- putchar

/ char = putchar(char)

/ buffer output if fout is not 1

.globl	_putchar
.globl	_fout
.globl	_flush

.globl	putc
.globl	flush

.data
_putchar:
	1f
.text
1:
	mov	2(sp),r0
	tst	_fout
	bne	1f
	mov	$1,_fout
1:
	jsr	r5,putc; _fout
	movb	3(sp),r0
	beq	1f
	jsr	r5,putc; _fout
1:
	cmp	_fout,$1
	bne	1f
	jsr	r5,flush; _fout
1:
	mov	2(sp),r0
	rts	pc

.data
_flush:
	1f
.text
1:
	jsr	r5,flush; _fout
	rts	pc

.bss
_fout:	.=.+518.

