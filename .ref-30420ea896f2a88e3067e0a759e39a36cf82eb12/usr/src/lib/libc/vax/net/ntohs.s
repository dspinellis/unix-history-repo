/* @(#)ntohs.s	4.1 %G% */
/* hostorder = ntohs(netorder) */

#include "DEFS.h"

ENTRY(ntohs)
	rotl	$8,4(ap),r0
	movb	5(ap),r0
	movzwl	r0,r0
	ret
