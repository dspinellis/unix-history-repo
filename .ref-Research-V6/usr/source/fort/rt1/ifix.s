/
/

/ ifix & int fortran functions

.globl	ifix.
.globl	int.

.globl	lval
.globl	r4i4
.globl	gas4
.globl	rval4p
.globl	retrn
.globl	temp

int.:
ifix.:	temp
	lval; temp
	rval4p; 2
	r4i4
	gas4
	retrn
