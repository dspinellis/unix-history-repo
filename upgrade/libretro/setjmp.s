/ C library -- setjmp, longjmp

/	longjmp(a)
/ will generate a "return" from
/ the last call to
/	setjmp(a)
/ by restoring sp, r5, pc from `a'
/ and doing a return.
/

.globl	_setjmp
.globl	_longjmp
.globl	csv, cret

_setjmp:
	mov	2(sp),r0
	mov	r5,(r0)+
	mov	sp,(r0)+
	mov	(sp),(r0)
	clr	r0
	rts	pc

_longjmp:
	jsr	r5,csv
	mov	4(r5),r0
1:
	cmp	(r5),(r0)
	beq	1f
	mov	(r5),r5
	bne	1b
/ panic -- r2-r4 lost
	mov	(r0)+,r5
	mov	(r0)+,sp
	mov	(r0),(sp)
	rts	pc
1:
	mov	4(r0),2(r5)
	jmp	cret

