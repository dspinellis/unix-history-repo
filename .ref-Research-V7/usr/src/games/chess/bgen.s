/ generate moves

.globl	_bgen

.globl	_pval, _board, _dir
.globl	_flag, _lmp, _bkpos
.globl	_eppos
.globl	_value

uleft	= 04040
uright	= 04004
dleft	= 00440
dright	= 00404
left	= 00040
right	= 00004
up	= 04000
down	= 00400
u2r1	= 06004
u1r2	= 04006
d1r2	= 00406
d2r1	= 00604
d2l1	= 00640
d1l2	= 00460
u1l2	= 04060
u2l1	= 06040
rank2	= 00200
rank7	= 02000

_bgen:
	mov	$_dir+126.,r4
	mov	$_board+126.,r3
	mov	_lmp,r2
	mov	$63.,r1
0:
	mov	(r3),r0
	ble	1f
	asl	r0
	jmp	*2f-2(r0)

2:
	pawn
	knight
	bishop
	rook
	queen
	king

pawn:
	bit	$dleft,(r4)
	bne	2f
	tst	2*7.(r3)
	bge	3f
	jsr	r5,btry; 0; 7.*2
3:
	mov	r1,r0
	add	$7,r0
	cmp	r0,_eppos
	bne	2f
	jsr	r5,btry; 0; -1*2
2:
	bit	$dright,(r4)
	bne	2f
	tst	9.*2(r3)
	bge	3f
	jsr	r5,btry; 0; 2*9.
3:
	mov	r1,r0
	add	$9,r0
	cmp	r0,_eppos
	bne	2f
	jsr	r5,btry; 0; 2*1
2:
	tst	2*8.(r3)
	bne	1f
	jsr	r5,btry; 0; 2*8.
	bit	$rank7,(r4)
	beq	1f
	tst	2*16.(r3)
	bne	1f
	jsr	r5,btry; 0; 16.*2
	br	1f

knight:
	jsr	r5,btry; u2r1; -15.*2
	jsr	r5,btry; u1r2; -6.*2
	jsr	r5,btry; d1r2; 10.*2
	jsr	r5,btry; d2r1; 17.*2
	jsr	r5,btry; d2l1; 15.*2
	jsr	r5,btry; d1l2; 6.*2
	jsr	r5,btry; u1l2; -10.*2
	jsr	r5,btry; u2l1; -17.*2
	br	1f


1:
	cmp	-(r4),-(r3)
	dec	r1
	bpl	0b
	mov	r2,_lmp
	rts	pc

bishop:
	jsr	r5,bslide; uleft; -9.*2
	jsr	r5,bslide; uright; -7.*2
	jsr	r5,bslide; dleft; 7.*2
	jsr	r5,bslide; dright; 9.*2
	br	1b

rook:
	jsr	r5,bslide; up; -8.*2
	jsr	r5,bslide; down; 8.*2
	jsr	r5,bslide; left; -1.*2.
	jsr	r5,bslide; right; 1.*2
	br	1b
queen:
	jsr	r5,bslide; uleft; -9.*2
	jsr	r5,bslide; uright; -7.*2
	jsr	r5,bslide; dleft; 7.*2
	jsr	r5,bslide; dright; 9.*2
	jsr	r5,bslide; up; -8.*2
	jsr	r5,bslide; left; -1.*2
	jsr	r5,bslide; right; 1.*2
	jsr	r5,bslide; down; 8.*2
	br	1b

king:
	jsr	r5,btry; uleft; -9.*2
	jsr	r5,btry; uright; -7.*2
	jsr	r5,btry; dleft; 7.*2
	jsr	r5,btry; dright; 9.*2
	jsr	r5,btry; up; -8.*2
	jsr	r5,btry; left; -1.*2
	jsr	r5,btry; right; 1.*2
	jsr	r5,btry; down; 8.*2
	br	1b

btry:
	bit	(r5)+,(r4)
	bne	1f
	mov	r3,r0
	add	(r5),r0
	mov	(r0),r0
	bgt	1f
	asl	r0
	mov	_pval+12.(r0),(r2)
	sub	_value,(r2)+
	mov	(r5)+,r0
	asr	r0
	add	r1,r0
	movb	r0,(r2)+
	movb	r1,(r2)+
	rts	r5
1:
	tst	(r5)+
	rts	r5

bslide:
	mov	r4,-(sp)
	mov	r3,-(sp)
1:
	bit	(r5)+,(r4)
	bne	1f
	add	(r5),r3
	add	(r5),r4
	mov	(r3),r0
	bgt	1f
	blt	2f
	clr	(r2)
	sub	_value,(r2)+
	mov	r3,r0
	sub	$_board,r0
	asr	r0
	movb	r0,(r2)+
	movb	r1,(r2)+
	tst	-(r5)
	br	1b
2:
	asl	r0
	mov	_pval+12.(r0),(r2)
	sub	_value,(r2)+
	mov	r3,r0
	sub	$_board,r0
	asr	r0
	movb	r0,(r2)+
	movb	r1,(r2)+
1:
	tst	(r5)+
	mov	(sp)+,r3
	mov	(sp)+,r4
	rts	r5
