/ C library -- reset, setexit

/	reset()
/ will generate a "return" from
/ the last call to
/	setexit()
/ by restoring sp, r5
/ and doing a return.
/
/ useful for going back to the main loop
/ after a horrible error in a lowlevel
/ routine.

.globl	_setexit
.globl	_reset

_setexit:
	mov	sp,ssp
	mov	r5,sr5
	mov	(sp),spc
	rts	pc

_reset:
	mov	ssp,sp
	mov	sr5,r5
	mov	spc,(sp)
	rts	pc

.bss
ssp:	.=.+2
sr5:	.=.+2
spc:	.=.+2

