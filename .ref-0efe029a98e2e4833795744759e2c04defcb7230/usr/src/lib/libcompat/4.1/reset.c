/*	reset.c	4.2	90/06/23	*/

/*
 * This can't be written in C.  You have to longjmp from a context
 * below (stackwise) the call to setjmp:
 *
 *	/* test reset/setexit *(/
 *	main()
 *	{
 *		int i = setexit();
 *		printf("i=%d\n", i);
 *		if (i == 0)
 *			reset(1);
 *	}
 *
 * The above prints `longjmp botch' and dumps core.
 */

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
