/* @(#)link.c	4.1 (Berkeley) 1/1/83 */

#include "SYS.h"

ENTRY(link)
	pushl	4(ap)
	CALL(1,_fixf)
	movl	r0,4(ap)
	pushl	8(ap)
	CALL(1,_fixf2)
	movl	r0,8(ap)
	chmk	$SYS_link
	jcs 	err
	ret
err:
	jmp	cerror
