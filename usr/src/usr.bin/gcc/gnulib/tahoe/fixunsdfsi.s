#include "DEFS.h"

	.data
	.align	2
Lbig:	.long	0x50000000, 0x00000000 # .double 2147483648
	.text

ENTRY(__fixunsdfsi, 0)
	cmpd2	4(fp),Lbig
	jgeq	1f
	ldd	4(fp)
	cvdl	r0
	ret

1:	ldd	4(fp)
	subd	Lbig
	cvdl	r0
	addl2	$2147483648,r0
	ret
