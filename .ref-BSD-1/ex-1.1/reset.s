/ C library -- reset, setexit

/	reset(x)
/ will generate a "return" from
/ the last call to
/	setexit()
/ by restoring sp, r5
/ and doing a return.
/ The returned value is x; on the original
/ call the returned value is 0.
/
/ useful for going back to the main loop
/ after a horrible error in a lowlevel
/ routine.

.globl	_setexit
.globl	_getexit, _resexit
.globl	_reset
.globl	csv, cret

_setexit:
	mov	r5,sr5
	mov	(sp),spc
	mov	sp,ssp
	clr	r0
	rts	pc

_getexit:
	mov	2(sp),r0
	mov	sr5,(r0)+
	mov	ssp,(r0)+
	mov	spc,(r0)+
	rts	pc

_resexit:
	mov	2(sp),r0
	mov	(r0)+,sr5
	mov	(r0)+,ssp
	mov	(r0)+,spc
	rts	pc

_reset:
	jsr	r5,csv
	mov	4(r5),r0
1:
	cmp	(r5),sr5
	beq	1f
	mov	(r5),r5
	bne	1b
/ panic -- r2-r4 lost
	mov	ssp,sp
	mov	sr5,r5
	mov	spc,(sp)
	rts	pc
1:
	mov	spc,2(r5)
	jmp	cret

.bss
sr5:	.=.+2
spc:	.=.+2
ssp:	.=.+2
