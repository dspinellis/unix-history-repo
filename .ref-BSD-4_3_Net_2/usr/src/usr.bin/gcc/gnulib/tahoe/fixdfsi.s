#include "DEFS.h"

ENTRY(__fixdfsi, 0)
	ldd	4(fp)
	cvdl	r0
	ret#1
