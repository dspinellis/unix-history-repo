.globl putch,obuild
.globl putdec

putdec:
	tst	r0
	bge	1f
	cmp	r0,$100000
	beq	2f
	mov	r0,-(sp)
	mov	$'-,r0
	jsr	pc,putch
	mov	(sp)+,r0
	neg	r0
1:
	alsc	$-16.,r0
	dvd	$10.,r0
	beq	1f
	mov	r1,-(sp)
	jsr	pc,1b
	mov	(sp)+,r1
1:
	mov	r1,r0
	add	$'0,r0
	jmp	putch
2:
	mov	$1f,r0
	jmp	obuild
1:
		<-32768\0>;.even
