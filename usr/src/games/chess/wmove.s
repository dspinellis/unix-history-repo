.globl	_wmove, _wremove
.globl	_board, _pval, _amp, _flag, _eppos, _value, _wkpos
.globl	_game

_wmove:
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
	bge	error
	jmp	*0f(r0)		/ type of man
	kmove
	qmove
	rmove
	bmove
	nmove
	pmove
0:
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
	mov	$-1,_board-16.(r3)
	mov	$4,(r4)+
	mov	r4,_amp
	rts	pc
1:
	cmp	r2,$2*16.		/ double move
	bne	1f
	movb	3(sp),r2
	sub	$8,r2
	mov	r2,_eppos
	br	move
1:
	cmp	r3,$24.*2
	bge	move
	sub	$25.,_value
	cmp	r3,$16.*2
	bge	move
	sub	$50.,_value
	cmp	r3,$8.*2		/ queen promotion
	bge	move
	sub	$625.,_value
	mov	$-5,_board(r3)
	mov	$5,(r4)+
	mov	r4,_amp
	rts	pc

rmove:
	cmp	r2,$2*63.
	bne	1f
	bic	$1,_flag
	br	move
1:
	cmp	r2,$2*56.
	bne	move
	bic	$2,_flag
	br	move

kmove:
	asr	r3
	mov	r3,_wkpos
	bic	$3,_flag
	cmp	r2,$2*60.
	bne	2f
	cmp	r3,$62.			/ kingside castle
	bne	1f
	dec	_value
	mov	$-4,_board+[2*61.]
	clr	_board+[2*63.]
	mov	$2,(r4)+
	mov	r4,_amp
	rts	pc
1:
	cmp	r3,$58.			/ queenside castle
	bne	2f
	dec	_value
	mov	$-4,_board+[2*59.]
	clr	_board+[2*56.]
	mov	$3,(r4)+
	mov	r4,_amp
	rts	pc
2:					/ king move
	tst	_game
	bne	1f
	add	$2,_value
1:
	clr	(r4)+
	mov	r4,_amp
	rts	pc

qmove:
	tst	_game
	bne	move
	inc	_value
	br	move

nmove:
bmove:
move:
	mov	$1,(r4)+
	mov	r4,_amp
	rts	pc

_wremove:
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
	mov	r2,_wkpos

movex:
	rts	pc

moveo:
	mov	$-4,_board+[2*63.]
	clr	_board+[2*61.]
	mov	$60.,_wkpos
	rts	pc

moveoo:
	mov	$-4,_board+[2*56.]
	clr	_board+[2*59.]
	mov	$60.,_wkpos;
	rts	pc

movep:
	mov	$-1,_board(r2)
	clr	_board-[2*8.](r3)
	rts	pc

moveq:
	mov	$-1,_board(r2)
	rts	pc
