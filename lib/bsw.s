/ crt -- c runtime routines

.globl	retrn
.globl	bswitch
.globl	_getchar
.globl	_putchar
.globl	_flush
.globl	_fin
.globl	_fout

.globl	_main
.globl	getc
.globl	putc
.globl	flush

start:
	mov	$mq,r4
	mov	sp,r0
	mov	(r0),-(sp)
	tst	(r0)+
	mov	r0,2(sp)
	jsr	pc,*_main
	clr	r0
	sys	exit

retrn:
	mov	r5,sp
	mov	(sp)+,r5
	rts	pc

_getchar:.+2
1:
	jsr	r5,getc; _fin
	bcs	1f
	tst	r0
	beq	1b
	rts	pc
1:
	clr	r0
	rts	pc

_putchar:.+2
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

_flush:
	.+2
	jsr	r5,flush; _fout
	rts	pc

bswitch:
	mov	*(sp),r1
	mov	r1,base
	mov	(r1),r2		/ low
	mov	(r1)+,r3	/ hi
	mov	$1,-(sp)	/ n
	tst	(r1)+
	bne	1f
	4			/ n = 0
1:
	tst	2(r1)
	beq	1f
	cmp	(r1),r2
	bhis	2f
	mov	(r1),r2
2:
	cmp	(r1)+,r3
	blo	2f
	mov	-2(r1),r3
2:
	tst	(r1)+
	inc	(sp)
	br	1b
1:
	sub	r2,r3		/ spread
	asl	(sp)
	inc	r3
	cmp	r3,(sp)
	blo	initd		/ if spread < 2*ncase direct

initb:
	clr	(sp)		/ flag
	mov	base,r2
	mov	r2,r3
	add	$4,r3
1:
	cmp	r3,r1
	bhis	1f
	cmp	(r2)+,(r3)+
	blos	2f
	mov	-(r2),-(sp)
	mov	-(r3),(r2)+
	mov	(sp)+,(r3)+
	mov	(r2),-(sp)
	mov	(r3),(r2)
	mov	(sp)+,(r3)
	inc	(sp)
2:
	cmp	(r2)+,(r3)+
	br	1b
1:
	tst	(sp)
	bne	initb
	cmp	(sp)+,(r1)+
	mov	r1,-(sp)
1:
	mov	-(r1),2(r1)
	cmp	r1,base
	bhi	1b
	mov	(sp)+,(r1)
	mov	$binary+4,r3
	br	init

initd:
	mov	(sp)+,r3
	mov	r3,twon
	mov	r3,twona
	mov	r2,-(sp)		/ low
	mov	r3,-(sp)		/ 2*n
1:
	mov	base,r3
2:
	cmp	(r3)+,r2
	beq	2f
	tst	(r3)+
	cmp	r3,r1
	blo	2b
	mov	(r1),-(sp)		/ no match, default
	br	3f
2:
	mov	(r3),-(sp)		/ match
3:
	inc	r2
	dec	twon
	bne	1b
	add	$4,r1
	add	$2,twona
1:
	mov	(sp)+,-(r1)
	dec	twona
	bne	1b
	mov	$direct+4,r3

init:
	mov	(sp)+,r2
	mov	-(r3),-(r2)
	mov	-(r3),-(r2)
	jmp	(r2)

direct:
	jsr	pc,*$1f


/	lowval; nval
/	l1; l2; ...; ln
/	ld

1:
	mov	*(sp)+,r1
	sub	(r1)+,r0	/ low limit
	cmp	r0,(r1)+	/ n cases
	blo	1f
	mov	-(r1),r0	/ default
1:
	asl	r0
	add	r0,r1
	jmp	*(r1)+

binary:
	jsr	pc,*$1f

/	end
/	v1;l1; v2;l2; ...; vn;ln
/end:	ld

1:
	mov	*(sp)+,r1	/ low
	mov	(r1)+,r2	/ high
	mov	(r2),-(sp)	/ default
	br	1f

llo:
	mov	r3,r2
	br	1f
lhi:
	mov	r3,r1
	add	$4,r1
1:
	mov	r2,r3
	sub	r1,r3
	beq	1f
	asr	r3
	bic	$3,r3
	add	r1,r3
	cmp	r0,(r3)
	bhi	lhi
	blo	llo
	tst	(sp)+
	jmp	*2(r3)
1:
	rts	pc

.bss
base:	.=.+2
twon:	.=.+2
twona:	.=.+2
_fin:	.=.+518.
_fout:	.=.+518.

