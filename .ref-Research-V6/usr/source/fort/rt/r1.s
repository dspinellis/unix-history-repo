/
/

/ r1 -- fortran runtime

.globl	gas1
.globl	gas2
.globl	gas4
.globl	gas8
.globl	stop
.globl	lval
.globl	rval4
.globl	rval8
.globl	do1
.globl	do12
.globl	do14
.globl	do2
.globl	do22
.globl	do24
.globl	goto
.globl	cgoto
.globl	cagoto
.globl	agoto

.globl	rerr

gas8:
	mov	8.(sp),r0
	mov	(sp)+,(r0)+
	mov	(sp)+,(r0)+
	br	1f

gas1:
	movb	(sp)+,*(sp)+
	jmp	*(r4)+

gas2:
	mov	(sp)+,*(sp)+
	jmp	*(r4)+

gas4:
	mov	4(sp),r0
1:
	mov	(sp)+,(r0)+
	mov	(sp)+,(r0)
	tst	(sp)+
	jmp	*(r4)+

stop:
	sys	exit

lval:
	mov	(r4)+,-(sp)
	jmp	*(r4)+

rval8:
	mov	(r4)+,r0
	add	$8.,r0
	mov	-(r0),-(sp)
	mov	-(r0),-(sp)
	mov	-(r0),-(sp)
	mov	-(r0),-(sp)
	jmp	*(r4)+

rval4:
	mov	(r4)+,r0
	mov	2(r0),-(sp)
	mov	(r0),-(sp)
	jmp	*(r4)+

do22:
	mov	(sp)+,r1
	br	1f

do12:
	mov	$1,r1
1:
	mov	(sp)+,r2
	mov	(sp)+,r0
	add	r1,(r0)
	cmp	(r0),r2
	bgt	goto
	tst	(r4)+
	jmp	*(r4)+

do24:
do2:
	mov	(sp)+,r2
	mov	(sp)+,r1
	br	1f

do14:
do1:
	clr	r2
	mov	$1,r1
1:
	mov	4(sp),r0
	add	r1,2(r0)
	adc	r2
	add	r2,(r0)
	cmp	(r0)+,(sp)+
	bgt	1f
	blt	2f
	cmp	(r0),(sp)
	bhi	1f
2:
	cmp	(sp)+,(sp)+
	tst	(r4)+
	jmp	*(r4)+

1:
	cmp	(sp)+,(sp)+
goto:
	mov	(r4),r4
	jmp	*(r4)+

cgoto:
	tst	(sp)+
	mov	(sp)+,r0
	asl	r0
	add	r4,r0
1:
	tst	(r4)+
	beq	gotoe
	cmp	r0,r4
	bne	1b
	mov	-(r0),r4
	jmp	*(r4)+
cagoto:
	tst	(sp)+
	mov	(sp)+,r0
1:
	tst	(r4)
	beq	gotoe
	cmp	r0,(r4)+
	bne	1b
	mov	r0,r4
	jmp	*(r4)+

agoto:
	tst	(sp)+
	mov	(sp)+,r4
	jmp	*(r4)+

gotoe:
	jsr	r5,rerr; 14.

