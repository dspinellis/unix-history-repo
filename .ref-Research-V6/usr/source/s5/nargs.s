/ C library -- nargs

/ WARNING: this routine does not work
/ with user I&D space separate.
/ Moreover, due to a design botch in
/ the 11/45, it cannot be made to work
/ by adding mfpi instructions.

.globl	_nargs

_nargs:
	mov	r5,-(sp)
	mov	2(r5),r1		/ pc of caller of caller
	mov	sp,r5
	clr	r0
	cmp	-4(r1),jsrsd
	bne	8f
	mov	$2,r0
8:
	cmp	(r1),tsti
	bne	1f
	add	$2,r0
	br	2f
1:
	cmp	(r1),cmpi
	bne	1f
	add	$4,r0
	br	2f
1:
	cmp	(r1),addi
	bne	1f
	add	2(r1),r0
	br	2f
1:
	cmp	(r1),jmpi
	bne	1f
	add	2(r1),r1
	add	$4,r1
	br	8b
1:
	cmpb	1(r1),bri+1
	bne	2f
	mov	r0,-(sp)
	mov	(r1),r0
	swab	r0
	ash	$-7,r0
	add	r0,r1
	add	$2,r1
	mov	(sp)+,r0
	br	8b
2:
	asr	r0
	mov	(sp)+,r5
	rts	pc

.data
jsrsd:	jsr	pc,*$0
tsti:	tst	(sp)+
cmpi:	cmp	(sp)+,(sp)+
addi:	add	$0,sp
jmpi:	jmp	0
bri:	br	.
