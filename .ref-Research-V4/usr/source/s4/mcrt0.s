/ C runtime startoff including monitoring

cbufs	= 150.

.globl	_monitor
.globl	_sbrk
.globl	_main
.globl	_exit
.globl	_etext

.comm	savr5,2

start:
	setd
	mov	sp,r0
	mov	(r0),-(sp)
	tst	(r0)+
	mov	r0,2(sp)

	mov	$_etext,r1
	sub	$eprol,r1
	add	$7,r1
	ash	$-3,r1
	bic	$!17777,r1
	mov	$cbufs,-(sp)
	add	$3*[cbufs+1],r1
	mov	r1,-(sp)
	asl	r1
	mov	r1,-(sp)
	jsr	pc,_sbrk
	tst	(sp)+
	cmp	r0,$-1
	beq	9f
	mov	r0,-(sp)
	add	$6,r0
	mov	r0,countbase
	mov	$_etext,-(sp)
	mov	$eprol,-(sp)
	jsr	pc,_monitor
	add	$10.,sp
	jsr	pc,_main
	cmp	(sp)+,(sp)+
	jsr	pc,_exit

9:
	mov	$1,r0
	sys	write; 8f; 9f-8f

.data; 8: <No space for monitor buffer\n>; 9:.even; .text

_exit:
	mov	r5,-(sp)
	mov	sp,r5
	clr	-(sp)
	jsr	pc,_monitor
	tst	(sp)+
	mov	4(r5),r0
	sys	exit

.globl	mrsave
.globl	rsave
.globl	rretrn

mrsave:
	mov	*(r5)+,r1
	bne	1f
	mov	countbase,r1
	add	$6,countbase
	mov	r5,(r1)+
	mov	r1,*-2(r5)
1:
	inc	2(r1)
	bne	rsave
	inc	(r1)

rsave:
	mov	r5,r0
	mov	sp,r5
	mov	r4,-(sp)
	mov	r3,-(sp)
	mov	r2,-(sp)
	sub	(r0)+,sp
	jmp	(r0)

rretrn:
	sub	$6,r5
	mov	r5,sp
	mov	(sp)+,r2
	mov	(sp)+,r3
	mov	(sp)+,r4
	mov	(sp)+,r5
	rts	pc

.bss
countbase:	.=.+2
.text

eprol:
