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
static char sccsid[] = "@(#)lisp.c	5.4 (Berkeley) 6/1/90";
#endif /* not lint */

#include <ctags.h>
#include <string.h>

extern char	*lbp;			/* pointer shared with fortran */

/*
 * lisp tag functions
 * just look for (def or (DEF
 */
l_entries()
{
	register int	special;
	register char	*cp,
			savedc;
	char	tok[MAXTOKEN];

	for (;;) {
		lineftell = ftell(inf);
		if (!fgets(lbuf,sizeof(lbuf),inf))
			return;
		++lineno;
		lbp = lbuf;
		if (!cicmp("(def"))
			continue;
		special = NO;
		switch(*lbp | ' ') {
		case 'm':
			if (cicmp("method"))
				special = YES;
			break;
		case 'w':
			if (cicmp("wrapper") || cicmp("whopper"))
				special = YES;
		}
		for (;!isspace(*lbp);++lbp);
		for (;isspace(*lbp);++lbp);
		for (cp = lbp;*cp && *cp != '\n';++cp);
		*cp = EOS;
		if (special) {
			if (!(cp = index(lbp,')')))
				continue;
			for (;cp >= lbp && *cp != ':';--cp);
			if (cp < lbp)
				continue;
			lbp = cp;
			for (;*cp && *cp != ')' && *cp != ' ';++cp);
		}
		else
			for (cp = lbp + 1;
			    *cp && *cp != '(' && *cp != ' ';++cp);
		savedc = *cp;
		*cp = EOS;
		(void)strcpy(tok,lbp);
		*cp = savedc;
		getline();
		pfnote(tok,lineno);
	}
	/*NOTREACHED*/
}
