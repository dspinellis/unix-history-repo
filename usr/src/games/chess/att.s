/ does white/black attack position?

.globl	_battack
.globl	_wattack

.globl	_dir, _board

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

_battack:
	mov	2(sp),r0
	asl	r0
	mov	_dir(r0),r1
	mov	$2,r2
	bit	$u2r1,r1
	bne	1f
	cmp	_board+[-15.*2](r0),r2
	beq	2f
1:
	bit	$u1r2,r1
	bne	1f
	cmp	_board+[-6.*2](r0),r2
	beq	2f
1:
	bit	$d1r2,r1
	bne	1f
	cmp	_board+[+10.*2](r0),r2
	beq	2f
1:
	bit	$d2r1,r1
	bne	1f
	cmp	_board+[+17.*2](r0),r2
	beq	2f
1:
	bit	$d2l1,r1
	bne	1f
	cmp	_board+[+15.*2](r0),r2
	beq	2f
1:
	bit	$d1l2,r1
	bne	1f
	cmp	_board+[+6.*2](r0),r2
	beq	2f
1:
	bit	$u1l2,r1
	bne	1f
	cmp	_board+[-10.*2](r0),r2
	beq	2f
1:
	bit	$u2l1,r1
	bne	1f
	cmp	_board+[-17.*2](r0),r2
	beq	2f
1:
	jsr	r5,badiag; uleft; -9.*2
	jsr	r5,badiag; uright; -7.*2
	jsr	r5,badiag; dleft; 7.*2
	jsr	r5,badiag; dright; 9.*2
	jsr	r5,barank; up; -8.*2
	jsr	r5,barank; left; -1.*2
	jsr	r5,barank; right; 1.*2
	jsr	r5,barank; down; 8.*2

	bit	$uleft,_dir(r0)
	bne	1f
	cmp	_board-18.(r0),$1		/ pawn?
	beq	2f
1:
	bit	$uright,_dir(r0)
	bne	1f
	cmp	_board-14.(r0),$1
	bne	1f
2:
	clr	r0
	rts	pc
1:
	mov	$1,r0
	rts	pc

badiag:
	mov	r0,r1
	mov	(r5)+,r2
	mov	(r5)+,r3
	bit	r2,_dir(r1)
	bne	1f
	add	r3,r1
	mov	_board(r1),r4
	beq	2f
	cmp	r4,$3
	beq	9f
	cmp	r4,$5
	beq	9f
	cmp	r4,$6
	beq	9f
1:
	rts	r5
2:
	bit	r2,_dir(r1)
	bne	2f
	add	r3,r1
	mov	_board(r1),r4
	beq	2b
	cmp	r4,$3
	beq	9f
	cmp	r4,$5
	beq	9f
2:
	rts	r5

barank:
	mov	r0,r1
	mov	(r5)+,r2
	mov	(r5)+,r3
	bit	r2,_dir(r1)
	bne	1f
	add	r3,r1
	mov	_board(r1),r4
	beq	2f
	cmp	r4,$4
	beq	9f
	cmp	r4,$5
	beq	9f
	cmp	r4,$6
	beq	9f
1:
	rts	r5
2:
	bit	r2,_dir(r1)
	bne	2f
	add	r3,r1
	mov	_board(r1),r4
	beq	2b
	cmp	r4,$4
	beq	9f
	cmp	r4,$5
	beq	9f
2:
	rts	r5

9:
	mov	(sp)+,r5
	clr	r0
	rts	pc

_wattack:
	mov	2(sp),r0
	asl	r0
	mov	_dir(r0),r1
	mov	$-2,r2
	bit	$u2r1,r1
	bne	1f
	cmp	_board+[-15.*2](r0),r2
	beq	2f
1:
	bit	$u1r2,r1
	bne	1f
	cmp	_board+[-6.*2](r0),r2
	beq	2f
1:
	bit	$d1r2,r1
	bne	1f
	cmp	_board+[+10.*2](r0),r2
	beq	2f
1:
	bit	$d2r1,r1
	bne	1f
	cmp	_board+[+17.*2](r0),r2
	beq	2f
1:
	bit	$d2l1,r1
	bne	1f
	cmp	_board+[+15.*2](r0),r2
	beq	2f
1:
	bit	$d1l2,r1
	bne	1f
	cmp	_board+[+6.*2](r0),r2
	beq	2f
1:
	bit	$u1l2,r1
	bne	1f
	cmp	_board+[-10.*2](r0),r2
	beq	2f
1:
	bit	$u2l1,r1
	bne	1f
	cmp	_board+[-17.*2](r0),r2
	beq	2f
1:
	jsr	r5,wadiag; uleft; -9.*2
	jsr	r5,wadiag; uright; -7.*2
	jsr	r5,wadiag; dleft; 7.*2
	jsr	r5,wadiag; dright; 9.*2
	jsr	r5,warank; up; -8.*2
	jsr	r5,warank; left; -1.*2
	jsr	r5,warank; right; 1.*2
	jsr	r5,warank; down; 8.*2

	bit	$dleft,_dir(r0)
	bne	1f
	cmp	_board+14.(r0),$-1		/ pawn?
	beq	2f
1:
	bit	$dright,_dir(r0)
	bne	1f
	cmp	_board+18.(r0),$-1
	bne	1f
2:
	clr	r0
	rts	pc
1:
	mov	$1,r0
	rts	pc

wadiag:
	mov	r0,r1
	mov	(r5)+,r2
	mov	(r5)+,r3
	bit	r2,_dir(r1)
	bne	1f
	add	r3,r1
	mov	_board(r1),r4
	beq	2f
	cmp	r4,$-3
	beq	9f
	cmp	r4,$-5
	beq	9f
	cmp	r4,$-6
	beq	9f
1:
	rts	r5
2:
	bit	r2,_dir(r1)
	bne	2f
	add	r3,r1
	mov	_board(r1),r4
	beq	2b
	cmp	r4,$-3
	beq	9f
	cmp	r4,$-5
	beq	9f
2:
	rts	r5

warank:
	mov	r0,r1
	mov	(r5)+,r2
	mov	(r5)+,r3
	bit	r2,_dir(r1)
	bne	1f
	add	r3,r1
	mov	_board(r1),r4
	beq	2f
	cmp	r4,$-4
	beq	9f
	cmp	r4,$-5
	beq	9f
	cmp	r4,$-6
	beq	9f
1:
	rts	r5
2:
	bit	r2,_dir(r1)
	bne	2f
	add	r3,r1
	mov	_board(r1),r4
	beq	2b
	cmp	r4,$-4
	beq	9f
	cmp	r4,$-5
	beq	9f
2:
	rts	r5

9:
	mov	(sp)+,r5
	clr	r0
	rts	pc
