/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)failed.c	1.1 */

/*
 *   FAILED.C
 *
 *   Programmer:  D. A. Lambeth
 *
 *        Owner:  D. A. Lambeth
 *
 *         Date:  April 17, 1980
 *
 *
 *
 *   FAILED (S1, S2)
 *
 *        Program Failure.  Print an error diagnostic containing
 *        the strings S1 and S2, and longjmp to an error-handling
 *        routine.
 *
 *
 *
 *   See Also:  SETJMP(3C)
 */

/*
 *   FAILED (S1, S2)
 *
 *        char *S1;
 *
 *        char *S2;
 *
 *   Print an error message of the format
 *
 *        S1 : S2
 *
 *   at the stderr file.
 *
 *   Longjmp to the location errshell, which must have been
 *   established previously by a call to setjmp.
 *
 *   Note that the return value from setjmp will always be
 *   '1'.
 */

#include <stdio.h>
#include <setjmp.h>

extern jmp_buf errshell;
extern char colon[], newline[];

void	failed(s1,s2)
char	*s1, *s2;
{
	write (2, s1, strlen(s1));
	if (s2)
	{
		write (2, colon, strlen(colon));
		write (2, s2, strlen(s2));
	}
	write (2, newline, strlen(newline));
	longjmp(errshell, 1);
}
