/
/

/ float fortran function

.globl	float.

.globl	rval4p
.globl	lval
.globl	gas4
.globl	i4r4
.globl	retrn
.globl	temp

float.:	temp
	lval; temp
	rval4p; 2
	i4r4
	gas4
	retrn
