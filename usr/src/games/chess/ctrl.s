/ list pieces controlling a square

.globl	_attack

.globl	_dir, _board
.globl	_attacv

none	= 12345
uleft	= 04040;
uright	= 04004;
dleft	= 00440;
dright	= 00404;
left	= 00040;
right	= 00004;
up	= 04000;
down	= 00400;
u2r1	= 06004;
u1r2	= 04006;
d1r2	= 00406;
d2r1	= 00604;
d2l1	= 00640;
d1l2	= 00460;
u1l2	= 04060;
u2l1	= 06040;

_attack:
	mov	2(sp),r0
	asl	r0
	mov	$_attacv,r4

	jsr	r5,patt
		u2r1
		-15.*2
		2; -2
	jsr	r5,patt
		u1r2
		-6.*2
		2; -2
	jsr	r5,patt
		d2r1
		17.*2
		2; -2
	jsr	r5,patt
		d2l1
		15.*2
		2; -2
	jsr	r5,patt
		d1l2
		6.*2
		2; -2
	jsr	r5,patt
		u1l2
		-10.*2
		2; -2
	jsr	r5,patt
		u2l1
		-17.*2
		2; -2

	jsr	r5,satt
		uleft; -9.*2
		1
		3; -3; 5; -5
	jsr	r5,satt
		uright; -7.*2
		1
		3; -3; 5; -5
	jsr	r5,satt
		dleft; 7.*2
		-1
		3; -3; 5; -5
	jsr	r5,satt
		dright; 9.*2
		-1
		3; -3; 5; -5
	jsr	r5,satt
		up; -8.*2
		none
		4; -4; 5; -5
	jsr	r5,satt
		left; -1.*2
		none
		4; -4; 5; -5
	jsr	r5,satt
		right; 1.*2
		none
		4; -4; 5; -5
	jsr	r5,satt
		down; 8.*2
		none
		4; -4; 5; -5
	clr	(r4)+
	rts	pc

patt:
	bit	(r5)+,_dir(r0)
	bne	1f
	mov	r0,r1
	add	(r5)+,r1
	jsr	pc,look
	jsr	pc,look
	rts	r5
1:
	add	$6,r5
	rts	r5

satt:
	mov	r5,-(sp)
	bit	(r5)+,_dir(r0)
	bne	1f
	mov	r0,r1
	add	(r5)+,r1
	jsr	pc,look			/ pawn
	mov	r0,r1
2:
	mov	(sp),r5
	bit	(r5)+,_dir(r1)
	bne	1f
	add	(r5)+,r1
	tst	_board(r1)
	beq	2b
	tst	(r5)+
	mov	r4,-(sp)
	jsr	pc,look
	jsr	pc,look
	jsr	pc,look
	jsr	pc,look
	cmp	(sp)+,r4
	bne	2b
1:
	mov	(sp)+,r5
	add	$14.,r5
	rts	r5

look:
	cmp	(r5)+,_board(r1)
	bne	1f
	mov	-2(r5),(r4)+
1:
	rts	pc
