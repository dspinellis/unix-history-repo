#include "DEFS.h"

ENTRY(__lshrsi3, 0)
	movl	4(ap),r0
	subl3	8(ap),$32,r1
	extzv	8(ap),r1,r0,r0
	ret
