/*	reset.c	4.1	83/06/30	*/

/*
 * Backwards compatible setexit/reset.
 */
#include <setjmp.h>

static	jmp_buf save;

setexit()
{
	
	return (setjmp(save));
}

reset(x)
{

	longjmp(save, x);
}
