/*
 * Copyright (c) 1987 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)fortran.c	5.4 (Berkeley) 6/1/90";
#endif /* not lint */

#include <ctags.h>
#include <string.h>

char	*lbp;				/* line buffer pointer */

PF_funcs()
{
	register bool	pfcnt;		/* pascal/fortran functions found */
	register char	*cp;
	char	tok[MAXTOKEN],
		*gettoken();

	for (pfcnt = NO;;) {
		lineftell = ftell(inf);
		if (!fgets(lbuf,sizeof(lbuf),inf))
			return(pfcnt);
		++lineno;
		lbp = lbuf;
		if (*lbp == '%')	/* Ratfor escape to fortran */
			++lbp;
		for (;isspace(*lbp);++lbp);
		if (!*lbp)
			continue;
		switch (*lbp | ' ') {	/* convert to lower-case */
		case 'c':
			if (cicmp("complex") || cicmp("character"))
				takeprec();
			break;
		case 'd':
			if (cicmp("double")) {
				for (;isspace(*lbp);++lbp);
				if (!*lbp)
					continue;
				if (cicmp("precision"))
					break;
				continue;
			}
			break;
		case 'i':
			if (cicmp("integer"))
				takeprec();
			break;
		case 'l':
			if (cicmp("logical"))
				takeprec();
			break;
		case 'r':
			if (cicmp("real"))
				takeprec();
			break;
		}
		for (;isspace(*lbp);++lbp);
		if (!*lbp)
			continue;
		switch (*lbp | ' ') {
		case 'f':
			if (cicmp("function"))
				break;
			continue;
		case 'p':
			if (cicmp("program") || cicmp("procedure"))
				break;
			continue;
		case 's':
			if (cicmp("subroutine"))
				break;
		default:
			continue;
		}
		for (;isspace(*lbp);++lbp);
		if (!*lbp)
			continue;
		for (cp = lbp + 1;*cp && intoken(*cp);++cp);
		if (cp = lbp + 1)
			continue;
		*cp = EOS;
		(void)strcpy(tok,lbp);
		getline();			/* process line for ex(1) */
		pfnote(tok,lineno);
		pfcnt = YES;
	}
	/*NOTREACHED*/
}

/*
 * cicmp --
 *	do case-independent strcmp
 */
cicmp(cp)
	register char	*cp;
{
	register int	len;
	register char	*bp;

	for (len = 0,bp = lbp;*cp && (*cp &~ ' ') == (*bp++ &~ ' ');
	    ++cp,++len);
	if (!*cp) {
		lbp += len;
		return(YES);
	}
	return(NO);
}

static
takeprec()
{
	for (;isspace(*lbp);++lbp);
	if (*lbp == '*') {
		for (++lbp;isspace(*lbp);++lbp);
		if (!isdigit(*lbp))
			--lbp;			/* force failure */
		else
			while (isdigit(*++lbp));
	}
}
