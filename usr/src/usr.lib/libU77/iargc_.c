/*
char id_iargc[] = "@(#)iargc_.c	1.1";
 *
 * return the number of args on the command line following the command name
 *
 * calling sequence:
 *	nargs = iargc()
 * where:
 *	nargs will be set to the number of args
 */

extern int xargc;

long iargc_()
{
	return ((long)(xargc - 1));
}
