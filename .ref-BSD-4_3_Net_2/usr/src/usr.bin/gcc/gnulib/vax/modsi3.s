#include "DEFS.h"

ENTRY(__modsi3, 0)
	divl3	8(ap),4(ap),r0
	mull2	8(ap),r0
	subl3	r0,4(ap),r0
	ret
