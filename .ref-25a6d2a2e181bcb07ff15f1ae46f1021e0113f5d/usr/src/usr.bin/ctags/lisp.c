/*
 * Copyright (c) 1987 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)lisp.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <ctags.h>
#include <strings.h>

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
