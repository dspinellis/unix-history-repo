/ generate moves

.globl	_wgen

.globl	_pval, _board, _dir
.globl	_flag, _lmp, _wkpos
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

_wgen:
	mov	$_dir+126.,r4
	mov	$_board+126.,r3
	mov	_lmp,r2
	mov	$63.,r1
0:
	mov	(r3),r0
	bge	1f
	asl	r0
	jmp	*2f(r0)

	king
	queen
	rook
	bishop
	knight
	pawn
2:

pawn:
	bit	$uleft,(r4)
	bne	2f
	tst	-2*9.(r3)
	ble	3f
	jsr	r5,wtry; 0; -9.*2
3:
	mov	r1,r0
	sub	$9,r0
	cmp	r0,_eppos
	bne	2f
	jsr	r5,wtry; 0; -1*2
2:
	bit	$uright,(r4)
	bne	2f
	tst	-7.*2(r3)
	ble	3f
	jsr	r5,wtry; 0; -7.*2
3:
	mov	r1,r0
	sub	$7,r0
	cmp	r0,_eppos
	bne	2f
	jsr	r5,wtry; 0; 2*1
2:
	tst	-2*8.(r3)
	bne	1f
	jsr	r5,wtry; 0; -2*8.
	bit	$rank2,(r4)
	beq	1f
	tst	-2*16.(r3)
	bne	1f
	jsr	r5,wtry; 0; -16.*2
	br	1f

knight:
	jsr	r5,wtry; u2r1; -15.*2
	jsr	r5,wtry; u1r2; -6.*2
	jsr	r5,wtry; d1r2; 10.*2
	jsr	r5,wtry; d2r1; 17.*2
	jsr	r5,wtry; d2l1; 15.*2
	jsr	r5,wtry; d1l2; 6.*2
	jsr	r5,wtry; u1l2; -10.*2
	jsr	r5,wtry; u2l1; -17.*2
	br	1f


1:
	cmp	-(r4),-(r3)
	dec	r1
	bpl	0b
	mov	r2,_lmp
	rts	pc

bishop:
	jsr	r5,wslide; uleft; -9.*2
	jsr	r5,wslide; uright; -7.*2
	jsr	r5,wslide; dleft; 7.*2
	jsr	r5,wslide; dright; 9.*2
	br	1b

rook:
	jsr	r5,wslide; up; -8.*2
	jsr	r5,wslide; down; 8.*2
	jsr	r5,wslide; left; -1.*2.
	jsr	r5,wslide; right; 1.*2
	br	1b
queen:
	jsr	r5,wslide; uleft; -9.*2
	jsr	r5,wslide; uright; -7.*2
	jsr	r5,wslide; dleft; 7.*2
	jsr	r5,wslide; dright; 9.*2
	jsr	r5,wslide; up; -8.*2
	jsr	r5,wslide; left; -1.*2
	jsr	r5,wslide; right; 1.*2
	jsr	r5,wslide; down; 8.*2
	br	1b

king:
	jsr	r5,wtry; uleft; -9.*2
	jsr	r5,wtry; uright; -7.*2
	jsr	r5,wtry; dleft; 7.*2
	jsr	r5,wtry; dright; 9.*2
	jsr	r5,wtry; up; -8.*2
	jsr	r5,wtry; left; -1.*2
	jsr	r5,wtry; right; 1.*2
	jsr	r5,wtry; down; 8.*2
	br	1b

wtry:
	bit	(r5)+,(r4)
	bne	1f
	mov	r3,r0
	add	(r5),r0
	mov	(r0),r0
	blt	1f
	asl	r0
	mov	_value,(r2)
	sub	_pval+12.(r0),(r2)+
	mov	(r5)+,r0
	asr	r0
	add	r1,r0
	movb	r0,(r2)+
	movb	r1,(r2)+
	rts	r5
1:
	tst	(r5)+
	rts	r5

wslide:
	mov	r4,-(sp)
	mov	r3,-(sp)
1:
	bit	(r5)+,(r4)
	bne	1f
	add	(r5),r3
	add	(r5),r4
	mov	(r3),r0
	blt	1f
	bgt	2f
	mov	_value,(r2)+
	mov	r3,r0
	sub	$_board,r0
	asr	r0
	movb	r0,(r2)+
	movb	r1,(r2)+
	tst	-(r5)
	br	1b
2:
	asl	r0
	mov	_value,(r2)
	sub	_pval+12.(r0),(r2)+
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
