/* @(#)chdir.c	4.1 (Berkeley) 1/1/83 */

#include "SYS.h"

ENTRY(chdir)
	pushl	4(ap)
	CALL(1,_savfile)
	movl	r0,4(ap)
	chmk	$SYS_chdir
	jcs 	err
	ret
err:
	jmp 	cerror
