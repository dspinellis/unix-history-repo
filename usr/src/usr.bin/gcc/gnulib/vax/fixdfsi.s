#include "DEFS.h"

ENTRY(__fixdfsi, 0)
	cvtdl	4(ap),r0
	ret
