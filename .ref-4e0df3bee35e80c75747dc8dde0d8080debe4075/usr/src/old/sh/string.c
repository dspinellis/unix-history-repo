/*	string.c	4.1	82/05/07	*/

#
/*
 * UNIX shell
 *
 * S. R. Bourne
 * Bell Telephone Laboratories
 *
 */

#include	"defs.h"


/* ========	general purpose string handling ======== */


STRING	movstr(a,b)
	REG STRING	a, b;
{
	WHILE *b++ = *a++ DONE
	return(--b);
}

INT	any(c,s)
	REG CHAR	c;
	STRING		s;
{
	REG CHAR d;

	WHILE d = *s++
	DO	IF d==c
		THEN	return(TRUE);
		FI
	OD
	return(FALSE);
}

INT	cf(s1, s2)
	REG STRING s1, s2;
{
	WHILE *s1++ == *s2
	DO	IF *s2++==0
		THEN	return(0);
		FI
	OD
	return(*--s1 - *s2);
}

INT	length(as)
	STRING as;
{
	REG STRING s;

	IF s=as THEN WHILE *s++ DONE FI
	return(s-as);
}
