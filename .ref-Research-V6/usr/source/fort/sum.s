/	example of UNIX fortran
/	calling interface to machine code
/	this example is a function that
/	returns the single precision 
/	sum of all of its single precision arguments.
/	for example:
/		f = sum(1.,2.,3.)
/	sets f to 6.

.globl	sum.				/ defination of entry
.globl	retrn				/ reference of return

sum.:					/ entry point
	value				/ location of return value
	.+2				/ pointer to execution code
	setf				/ no d/f i/l modes guaranteed
	mov	*2(sp),r0		/ arg count
	mov	r3,r1			/ r3 points to arg list
	tst	(r1)+			/ zeroth arg is old r3

	clrf	fr0			/ start of actual function
1:
	addf	*(r1)+,fr0		/ add in each argument
	sob	r0,1b			/ for each argument

	movf	fr0,value		/ make returned value available
	jmp	retrn			/ actual return

.bss
value:	.=.+4				/ space for return value

/ synopsis:
/	1. save registers r3, sp
/	2. arg list (pointers to values)
/	   begins at 2(r3)
/	3. entry name is name of function
/	   followed by "."
/	4. first word after entry point is
/	   location of return value. this is
/	   true for both functions and subroutines
/	5. second word after entry point is
/	   pointer to pdp-11 code body
/	6. return is expedited by a jump to
/	   the globl routine "retrn"
