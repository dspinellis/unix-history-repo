/	hash routine for diff
/	effectively spreads the string out into 7-bit
/	bytes, then sums the result 1's-complement
/	by 16-bit bytes and adds 1 to avoid zero answer

/	r4 hash accumulator
/	r3 current offset
/	r2 first time flag
.globl	_readhash
.globl getc

.data
_readhash:
	mov	2(sp),0f
	mov	r5,-(sp)
	mov	r4,-(sp)
	mov	r3,-(sp)
	mov	r2,-(sp)
	mov	$1,r4
	mov	r4,r2
	clr	r3
1:
	jsr	r5,getc; 0: 0
	bes	2f
	clr	r2
	cmp	r0,$'\n
	beq	1f
	ashc	$-16.,r0
	ashc	r3,r0
	add	r0,r4
	adc	r4
	add	r1,r4
	adc	r4
	add	$7,r3
	cmp	r3,$16.
	blt	1b
	sub	$16.,r3
	br	1b
2:
	tst	r2
	bne	2f
	mov	$2,r0
	sys	write; 3f; 4f-3f
2:
	clr	r4
1:
	mov	r4,r0
	mov	(sp)+,r2
	mov	(sp)+,r3
	mov	(sp)+,r4
	mov	(sp)+,r5
	rts	pc
3:
	<incomplete line omitted\n>
4:
.even
