#include "DEFS.h"

	.data
	.align	2
Lbig:	.double	0d2.14748364800000000000e+09
	.text

/*
 * VAX conversions overflow politely,
 * but we'll be conservative just in case someone is trapping overflow.
 */
ENTRY(__fixunsdfsi, 0)
	cmpd	4(ap),Lbig
	jgeq	1f
	cvtdl	4(ap),r0
	ret

1:	subd3	Lbig,4(ap),r0
	cvtdl	r0,r0
	addl2	$2147483648,r0
	ret
