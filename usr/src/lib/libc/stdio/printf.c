/* @(#)printf.c	4.1 (Berkeley) 12/21/80 */
#include	<stdio.h>

printf(fmt, args)
char *fmt;
{
	_doprnt(fmt, &args, stdout);
	return(ferror(stdout)? EOF: 0);
}
