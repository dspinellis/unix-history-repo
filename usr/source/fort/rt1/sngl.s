/
/

/ sngl fortran function

.globl	sngl.

.globl	rval8p
.globl	lval
.globl	r8r4
.globl	gas4
.globl	retrn
.globl	temp

sngl.:	temp
	lval; temp
	rval8p; 2
	r8r4
	gas4
	retrn
