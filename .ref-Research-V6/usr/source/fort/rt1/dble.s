/
/

/ dble fortran function

.globl	dble.

.globl	rval4p
.globl	lval
.globl	r4r8
.globl	gas8
.globl	retrn
.globl	temp

dble.:	temp
	lval; temp
	rval4p; 2
	r4r8
	gas8
	retrn
