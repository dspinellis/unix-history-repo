/* @(#)open.c	4.1 (Berkeley) 1/1/83 */

#include "SYS.h"

ENTRY(open)
	pushl	4(ap)
	CALL(1,_fixf)
	movl	r0,4(ap)
	chmk	$SYS_open
	jcs 	err
	ret
err:	
	jmp 	cerror
