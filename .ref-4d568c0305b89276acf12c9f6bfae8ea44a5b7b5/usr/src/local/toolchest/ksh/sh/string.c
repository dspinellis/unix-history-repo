/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/*
 * string processing routines for Korn shell
 *
 */

#include	"defs.h"
#ifdef MULTIBYTE
#include	"national.h"
#endif /* MULTIBYTE */

/* The following routines are defined by this module */
char	*itos();
char	*movstr();
char	*substitute();
char	*simple();
void	trim();

/* The following routines are referenced by this module */
extern char	*utos();
extern char	*strrchr();

/*
 * converts integer n into an unsigned decimal string
 */

char *itos(n)
int n;
{
#ifdef pdp11
	return(utos((long)n,10));
#else
	return(utos((unsigned long)n,10));
#endif /* pdp11 */
}


/*
 * look for the substring <old> in <string> and replace with <new>
 * The new string is put on top of the stack
 */

char *substitute(string,old,new,newstring)
char *string;
char *old;
char *new;
char *newstring;
{
	register char *sp = string;
	register char *dp;
	register char *cp;
	char *savesp = NIL;
	dp = newstring;
	if(*sp==0)
		return(NIL);
	if(*(cp=old) == 0)
		goto found;
	do
	{
	/* skip to first character which matches start of old */
		while(*sp && (savesp==sp || *sp != *cp))
		{
#ifdef MULTIBYTE
			/* skip a whole character at a time */
			int c = *sp;
			c = echarset(c);
			c = in_csize(c) + (c>=2);
			while(c-- > 0)
#endif /* MULTIBYTE */
			*dp++ = *sp++;
		}
		if(*sp == 0)
			return(NIL);
		savesp = sp;
	        for(;*cp;cp++)
		{
			if(*cp != *sp++)
				break;
		}
		if(*cp==0)
		/* match found */
			goto found;
		sp = savesp;
		cp = old;
	}
	while(*sp);
	return(NIL);

found:
	/* copy new */
	dp = movstr(new,dp);
	/* copy rest of string */
	movstr(sp,dp);
	return(newstring);
}

/*
 * given a pathname return the base name
 */

char *simple(name)
register char *name;
{
	char *start = name;
	while (*name)
		if ((*name++ == '/') && *name)	/* don't trim trailing / */
			start = name;
	return (start);
}

/*
 * TRIM(at)
 * Remove quote bit from characters in at and eliminate quoted nulls.
 */

void	trim(at)
char *	at;
{
	register char *sp = at;
	register char *dp;
	register int c;
	if(sp)
	{
		dp = sp;
		while(c= *sp++)
		{
			if(c == ESCAPE)
				c = *sp++;
			if(c)
				*dp++ = c;
		}
		*dp = 0;
	}
}

/*
 * copy string a to string b and return a pointer to the end of the string
 */

char *movstr(a,b)
register char *a,*b;
{
	while(*b++ = *a++);
	return(--b);
}

