#include "DEFS.h"

ENTRY(__lshlsi3, 0)
	ashl	8(ap),4(ap),r0
	ret
