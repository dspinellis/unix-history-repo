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
.globl	csv, cret

_setexit:
	jsr	r5,csv
	mov	r5,sr5
	mov	2(r5),spc
	jmp	cret

_reset:
	mov	sr5,r5
	mov	spc,2(r5)
	jmp	cret

.bss
sr5:	.=.+2
spc:	.=.+2
