#include "DEFS.h"

ENTRY(__umulsi3, 0)
	emul	8(fp),4(fp),$0,r0
	movl	r1,r0
	ret
