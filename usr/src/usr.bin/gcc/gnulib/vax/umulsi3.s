#include "DEFS.h"

ENTRY(__umulsi3, 0)
	mull3	8(ap),4(ap),r0
	ret
