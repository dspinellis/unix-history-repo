/* @(#)fprintf.c	4.1 (Berkeley) %G% */
#include	<stdio.h>

fprintf(iop, fmt, args)
FILE *iop;
char *fmt;
{
	_doprnt(fmt, &args, iop);
	return(ferror(iop)? EOF: 0);
}
