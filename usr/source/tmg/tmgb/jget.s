f = r5
.globl j,n
.globl input
.globl classtab
.globl jget

jget:
	inc	jgetc
3:
	mov	j(f),r1
	mov	r1,r0
	bic	$inpt-1,r0
	bic	r0,r1
	cmp	r0,inpr
	beq	1f
	inc	readc
	mov	r0,inpr
	mov	input,r0
	sys	seek
inpr:		1;0
	sys	read;inpb;inpt
2:
	cmp	r0,$inpt
	bge	1f
	clrb	inpb(r0)
	inc	r0
	br	2b
1:
	movb	inpb(r1),r0
	asl	r0
	bit	n(f),classtab(r0)
	beq	1f
	inc	j(f)
	inc	r1
	cmp	r1,$inpt
	blt	1b
	br	3b
1:
	asr	r0
	rts	pc
inpt = 128.
.bss
inpb:	. = .+inpt
.data
jgetc:	0
readc:	0
