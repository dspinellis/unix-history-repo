/* @(#)htonl.s	4.1 %G% */
/* netorder = htonl(hostorder) */

#include "DEFS.h"

ENTRY(htonl)
	rotl	$-8,4(ap),r0
	insv	r0,$16,$8,r0
	movb	7(ap),r0
	ret
