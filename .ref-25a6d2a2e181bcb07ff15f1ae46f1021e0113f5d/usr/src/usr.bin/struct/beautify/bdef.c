#ifndef lint
static char sccsid[] = "@(#)bdef.c	4.1	(Berkeley)	%G%";
#endif not lint

#define xxtop	100		/* max size of xxstack */
int xxindent, xxval, newflag, xxmaxchars, xxbpertab;
int xxlineno;		/* # of lines already output */
int xxstind, xxstack[xxtop], xxlablast, xxt;
