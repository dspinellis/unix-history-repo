/ double ldexp(number, exp)
/ double number
/  -- returns number * 2^exp

.globl	_ldexp
.globl	csv, cret

ERANGE = 34.

_ldexp:
	jsr	r5,csv
	movf	4(r5),fr0
	movei	fr0,r0
	add	12.(r5),r0
	movie	r0,fr0
	cfcc
	bvc	1f
	bmi	2f
	movf	huge,fr0
	br	3f
2:
	movf	huge,fr0
	negf	fr0
3:
	mov	$ERANGE,_errno
1:
	jmp	cret

	.comm	_errno,2
	.data
huge:	077777; 0177776; 0177777; 0177777
