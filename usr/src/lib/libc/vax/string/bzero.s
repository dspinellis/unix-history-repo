/* @(#)bzero.s	4.1 %G% */
/* bzero(base, length) */

#include "DEFS.h"

ENTRY(bzero)
	movc5	$0, (r0), $0, 8(ap), *4(ap)
	ret
