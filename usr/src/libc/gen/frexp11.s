/  double frexp(x, ip)
/  double x; int *ip;
/  returns a fractional part 1/16 <= |value| < 1
/ and stores an exponent so x = value * 2^(*ip)

.globl	_frexp
.globl	csv, cret

_frexp:
	jsr	r5,csv
	movf	4(r5),fr0
	movei	fr0,r0
	clr	r1
	movie	r1,fr0
	mov	r0,*12.(r5)
	jmp	cret
