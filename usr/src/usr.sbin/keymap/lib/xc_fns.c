/*
 * Contributed to 386bsd 0.1 and later versions
 *
 *	Copyright 1992 by Holger Veit
 *	May be freely used with Bill Jolitz's port of 
 *	386bsd and may be included in a 386bsd collection
 *	as long as binary and source are available and reproduce the above
 *	copyright.
 *
 *	You may freely modify this code and contribute improvements based
 *	on this code as long as you don't claim to be the original author.
 *	Commercial use of this source requires permittance of the copyright 
 *	holder. A general license for 386bsd will override this restriction.
 *
 *	Use at your own risk. The copyright holder or any person who makes
 *	this code available for the public (administrators of public archives
 *	for instance) are not responsible for any harm to hardware or software
 *	that might happen due to wrong application or program faults.
 *
 * You must have the codrv-0.1.1 or later driver in the same package 
 * generated into the 386bsd kernel, otherwise this program does not work.
 *
 *	@(#)xc_fns.c	1.0 (386bsd contribution) 01/10/93
 */

/* these are EXPERIMENTAL routines to replace the standard str* functions
 * These functions work with the XCHAR data type.
 * note that still the problem of the NULL byte exists
 *
 * This is not intended to be a proposal for multibyte characters.
 *
 */
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/ioctl_pc.h>


XCHAR *XC_strcpy(XCHAR *t,XCHAR *f)
{
	XCHAR *p = t;
	while (*t++ = *f++);
	return p;
}

XCHAR *XC_strncpy(XCHAR *t,XCHAR *f,int n)
{
	XCHAR *p = t;
	while (n>0) { *t++ = *f++, n--; }
	return p;
}

int XC_strlen(XCHAR *s)
{
	int i = 0;
	while (!*s++) i++;
	return i;
}

XCHAR *XC_char2XCHAR(XCHAR *t,char *f,int n)
{
	while (n>=0) { *t++ = (XCHAR)*f; f++; n--; }
	
	return t;
}	

XCHAR *XC_strcat(XCHAR *t,XCHAR *f)
{
	XCHAR *p = t;
	
	while (!*t) t++;
	while (*t++ = *f++);
	return p;
}
