/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)convert.c	1.1 */

/*
 *   CONVERT.C
 *
 *
 *   LTOU (STR1, STR2)
 *        Copy STR1 to STR2, changing lower case to upper case.
 *
 *   UTOL (STR1, STR2)
 *        Copy STR1 to STR2, changing upper case to lower case.
 *
 */

#ifdef KSHELL
#include	"shtype.h"
#else
#include	<ctype.h>
#endif	/* KSHELL */

/* 
 *   LTOU (STR1, STR2)
 *        char *STR1;
 *        char *STR2;
 *
 *   Copy STR1 to STR2, converting uppercase alphabetics to
 *   lowercase.  STR2 should be big enough to hold STR1.
 *
 *   STR1 and STR2 may point to the same place.
 *
 */

void ltou(str1,str2)
char *str1,*str2;
{
	register char *s,*d;
	for(s=str1,d=str2;*s;s++,d++)
	{
		if(islower(*s))
			*d = toupper(*s);
		else
			*d = *s;
	}
	*d = 0;
}


/*
 *   UTOL (STR1, STR2)
 *        char *STR1;
 *        char *STR2;
 *
 *   Copy STR1 to STR2, converting lowercase alphabetics to
 *   uppercase.  STR2 should be big enough to hold STR1.
 *
 *   STR1 and STR2 may point to the same place.
 *
 */

void utol(str1,str2)
char *str1,*str2;
{
	register char *s,*d;
	for(s=str1,d=str2;*s;s++,d++)
	{
		if(isupper(*s))
			*d = tolower(*s);
		else
			*d = *s;
	}
	*d = 0;
}

