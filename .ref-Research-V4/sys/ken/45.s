/ machine language assist
/ PDP-11/45

.globl	_fubyte, _subyte, _nofault
.globl	_fuword, _suword
wait = 1
rti = 2
reset = 5
spl = 230
mfpi = 6500^tst
mtpi = 6600^tst
PS = 177776
_fubyte:
	mov	2(sp),r1
	bic	$1,r1
	jsr	pc,gword
	cmp	r1,2(sp)
	beq	1f
	swab	r0
1:
	bic	$!377,r0
	rts	pc

_subyte:
	mov	2(sp),r1
	bic	$1,r1
	jsr	pc,gword
	mov	r0,-(sp)
	cmp	r1,4(sp)
	beq	1f
	movb	6(sp),1(sp)
	br	2f
1:
	movb	6(sp),(sp)
2:
	mov	(sp)+,r0
	jsr	pc,pword
	clr	r0
	rts	pc

_fuword:
	mov	2(sp),r1
	jsr	pc,gword
	rts	pc

_suword:
	mov	2(sp),r1
	mov	4(sp),r0
	jsr	pc,pword
	rts	pc

gword:
	mov	PS,-(sp)
	spl	7
	mov	_nofault,-(sp)
	mov	$err,_nofault
	mfpi	(r1)
	mov	(sp)+,r0
	br	1f

pword:
	mov	PS,-(sp)
	spl	7
	mov	_nofault,-(sp)
	mov	$err,_nofault
	mov	r0,-(sp)
	mtpi	(r1)
1:
	mov	(sp)+,_nofault
	mov	(sp)+,PS
	rts	pc

err:
	mov	(sp)+,_nofault
	mov	(sp)+,PS
	tst	(sp)+
	mov	$-1,r0
	rts	pc

.globl	_copyin, _copyout
_copyin:
	jsr	pc,copsu
1:
	mfpi	(r0)+
	mov	(sp)+,(r1)+
	sob	r2,1b
	br	2f

_copyout:
	jsr	pc,copsu
1:
	mov	(r0)+,-(sp)
	mtpi	(r1)+
	sob	r2,1b
2:
	mov	(sp)+,_nofault
	clr	r0
	rts	pc

copsu:
	mov	(sp)+,r3
	mov	2(sp),r0
	mov	4(sp),r1
	mov	6(sp),r2
	asr	r2
	mov	_nofault,-(sp)
	mov	$1f,_nofault
	jmp	(r3)

1:
	mov	(sp)+,_nofault
	mov	$-1,r0
	rts	pc

.bss
_nofault:.=.+2
.text

.globl	_idle
_idle:
	mov	PS,-(sp)
	spl	0
	wait
	mov	(sp)+,PS
	rts	pc

.globl	_savu, _retu, _u
_u = 140000
KISA6 = 172354
_savu:
	spl	7
	mov	(sp)+,r1
	mov	(sp),r0
	mov	sp,(r0)+
	mov	r5,(r0)+
	spl	0
	jmp	(r1)

_retu:
	spl	7
	mov	(sp)+,r1
	mov	(sp),KISA6
	mov	$_u,r0
	mov	(r0)+,sp
	mov	(r0)+,r5
	spl	0
	jmp	(r1)


.globl	_spl0, _spl4, _spl5, _spl6, _spl7
_spl0:
	spl	0
	rts	pc

_spl4:
	spl	4
	rts	pc

_spl5:
	spl	5
	rts	pc

_spl6:
	spl	6
	rts	pc

_spl7:
	spl	7
	rts	pc

.globl	_copyseg
SISA0 = 172240
SISA1 = 172242
SISD0 = 172200
SISD1 = 172202
_copyseg:
	mov	2(sp),SISA0
	mov	4(sp),SISA1
	clr	r0
	mov	$8192.,r1
	mov	$32.,r2
	mov	PS,-(sp)
	mov	$10340,PS
1:
	mfpi	(r0)+
	mtpi	(r1)+
	sob	r2,1b
	mov	(sp)+,PS
	rts	pc

.globl	_clearseg
_clearseg:
	mov	2(sp),SISA0
	clr	r0
	mov	$32.,r1
	mov	PS,-(sp)
	mov	$10340,PS
1:
	clr	-(sp)
	mtpi	(r0)+
	sob	r1,1b
	mov	(sp)+,PS
	rts	pc

.globl	_dpadd
_dpadd:
	mov	2(sp),r0
	add	4(sp),2(r0)
	adc	(r0)
	rts	pc

.globl	_dpcmp
_dpcmp:
	mov	2(sp),r0
	mov	4(sp),r1
	sub	6(sp),r0
	sub	8(sp),r1
	sbc	r0
	bge	1f
	cmp	r0,$-1
	bne	2f
	cmp	r1,$-512.
	bhi	3f
2:
	mov	$-512.,r0
	rts	pc
1:
	bne	2f
	cmp	r1,$512.
	blo	3f
2:
	mov	$512.,r1
3:
	mov	r1,r0
	rts	pc

.globl	start, _edata, _etext, _data, _end, _main
KISA0 = 172340
KISD0 = 172300
SSR0 = 177572

start:
	bit	$1,SSR0
	bne	start			/ loop if re-entry
	reset

/ initialize systems segments

	mov	$KISA0,r0
	mov	$KISD0,r1
	mov	$200,r4
	clr	r2
	mov	$6,r3
1:
	mov	r2,(r0)+
	mov	$77406,(r1)+		/ 4k rw
	add	r4,r2
	sob	r3,1b

/ initialize user segmant

	mov	$_end+63.,r2
	ash	$-6,r2
	bic	$!1777,r2
	mov	r2,(r0)+		/ ksr6 = sysu
	mov	$3406,(r1)+		/ rw (3400 = (USIZE-1)<<8)

/ initialize io segment
/ set up counts on super segments

	mov	$7600,(r0)+		/ ksr7 = IO
	mov	$77406,(r1)+		/ rw 4k
	mov	$6,SISD0
	mov	$6,SISD1

/ get a sp and start segmentation

	mov	$_u+512.,sp
	inc	SSR0

/ clear bss

	mov	$_edata,r0
1:
	clr	(r0)+
	cmp	r0,$_end
	blo	1b

/ clear user block

	mov	$_u,r0
1:
	clr	(r0)+
	cmp	r0,$_u+512.
	blo	1b

/ set up previous mode and call main

	mov	$30000,PS
	jsr	pc,_main
	mov	$170000,-(sp)
	clr	-(sp)
	rti

.globl	_ldiv
_ldiv:
	mov	2(sp),r0
	mov	4(sp),r1
	div	6(sp),r0
	rts	pc

.globl	_lrem
_lrem:
	mov	2(sp),r0
	mov	4(sp),r1
	div	6(sp),r0
	mov	r1,r0
	rts	pc

.globl retrn
retrn:
	mov	r5,sp
	mov	(sp)+,r5
	rts	pc

.globl	sswitch
sswitch:
	mov	(sp)+,r1
1:
	mov	(r1)+,r2
	beq	1f
	cmp	(r1)+,r0
	bne	1b
	jmp	(r2)
1:
	jmp	*(r1)+
