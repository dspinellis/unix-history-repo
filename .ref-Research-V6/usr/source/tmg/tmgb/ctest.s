f = r5
.globl j
.globl classtab,jget
.globl ctest
.globl putcstr
.globl succ

ctest:
	inc	ctestc
	mov	r0,-(sp)
	jsr	pc,jget
	asl	r0
	bit	*(sp)+,classtab(r0)
	clc
	beq	1f
	asr	r0
	jsr	pc,putcstr
	inc	j(f)
	sec
1:
	rts	pc

.data
ctestc:	0
