.globl	_bmove, _bremove
.globl	_board, _pval, _amp, _flag, _eppos, _value, _bkpos
.globl	_game

_bmove:
	mov	_amp,r4
	movb	2(sp),r3		/ to
	movb	3(sp),r2		/ from
	mov	_value,(r4)+
	mov	_flag,(r4)+
	mov	_eppos,(r4)+
	mov	r2,(r4)+
	mov	r3,(r4)+
	asl	r2			/ from as a word index
	asl	r3			/ to as word index
	mov	_board(r3),r0
	mov	r0,(r4)+
	beq	1f
	asl	r0
	sub	_pval+12.(r0),_value
1:
	mov	_board(r2),r0
	mov	r0,_board(r3)
	clr	_board(r2)
	mov	$-1,_eppos
	asl	r0
	ble	error
	jmp	*0f-2(r0)		/ type of man
0:
	pmove
	nmove
	bmove
	rmove
	qmove
	kmove

error:
	3

pmove:
	sub	r3,r2
	bge	1f
	neg	r2
1:
	cmp	r2,$2*1			/ ep capture
	bne	1f
	clr	_board(r3)
	mov	$1,_board+[2*8.](r3)
	mov	$4,(r4)+
	mov	r4,_amp
	rts	pc
1:
	cmp	r2,$2*16.		/ double move
	bne	1f
	movb	3(sp),r2
	add	$8,r2
	mov	r2,_eppos
	br	move
1:
	cmp	r3,$40.*2
	blt	move
	add	$25.,_value
	cmp	r3,$48.*2
	blt	move
	add	$50.,_value
	cmp	r3,$56.*2		/ queen promotion
	blt	move
	add	$625.,_value
	mov	$5,_board(r3)
	mov	$5,(r4)+
	mov	r4,_amp
	rts	pc

rmove:
	cmp	r2,$2*7.
	bne	1f
	bic	$10,_flag
	br	move
1:
	tst	r2
	bne	move
	bic	$20,_flag
	br	move

kmove:
	asr	r3
	mov	r3,_bkpos
	bic	$30,_flag
	cmp	r2,$2*4.
	bne	2f
	cmp	r3,$6			/ kingside castle
	bne	1f
	inc	_value
	mov	$4,_board+[2*5.]
	clr	_board+[2*7.]
	mov	$2,(r4)+
	mov	r4,_amp
	rts	pc
1:
	cmp	r3,$2			/ queenside castle
	bne	2f
	inc	_value
	mov	$4,_board+[2*3.]
	clr	_board+[2*0.]
	mov	$3,(r4)+
	mov	r4,_amp
	rts	pc
2:					/ king move
	tst	_game
	bne	1f
	sub	$2,_value
1:
	clr	(r4)+
	mov	r4,_amp
	rts	pc

qmove:
	tst	_game
	bne	move
	dec	_value
	br	move

nmove:
bmove:
move:
	mov	$1,(r4)+
	mov	r4,_amp
	rts	pc

_bremove:
	mov	_amp,r4
	mov	-(r4),r0
	mov	-(r4),r1
	mov	-(r4),r3
	mov	-(r4),r2
	mov	-(r4),_eppos
	mov	-(r4),_flag
	mov	-(r4),_value
	mov	r4,_amp
	asl	r2
	asl	r3
	mov	_board(r3),_board(r2)
	mov	r1,_board(r3)
	asl	r0
	jmp	*0f(r0)
0:
	movek
	movex
	moveo
	moveoo
	movep
	moveq

movek:
	asr	r2
	mov	r2,_bkpos

movex:
	rts	pc

moveo:
	mov	$4,_board+[2*7.]
	clr	_board+[2*5]
	mov	$4,_bkpos
	rts	pc

moveoo:
	mov	$4,_board+[2*0]
	clr	_board+[2*3]
	mov	$4,_bkpos;
	rts	pc

movep:
	mov	$1,_board(r2)
	clr	_board+[2*8.](r3)
	rts	pc

moveq:
	mov	$1,_board(r2)
	rts	pc
