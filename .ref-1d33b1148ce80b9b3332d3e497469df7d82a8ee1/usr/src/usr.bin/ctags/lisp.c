/*
 * Copyright (c) 1987, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)lisp.c	8.3 (Berkeley) %G%";
#endif /* not lint */

#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>

#include "ctags.h"

/*
 * lisp tag functions
 * just look for (def or (DEF
 */
void
l_entries()
{
	int	special;
	char	*cp;
	char	savedc;
	char	tok[MAXTOKEN];

	for (;;) {
		lineftell = ftell(inf);
		if (!fgets(lbuf, sizeof(lbuf), inf))
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
		for (; !isspace(*lbp); ++lbp)
			continue;
		for (; isspace(*lbp); ++lbp)
			continue;
		for (cp = lbp; *cp && *cp != '\n'; ++cp)
			continue;
		*cp = EOS;
		if (special) {
			if (!(cp = strchr(lbp, ')')))
				continue;
			for (; cp >= lbp && *cp != ':'; --cp)
				continue;
			if (cp < lbp)
				continue;
			lbp = cp;
			for (; *cp && *cp != ')' && *cp != ' '; ++cp)
				continue;
		}
		else
			for (cp = lbp + 1;
			    *cp && *cp != '(' && *cp != ' '; ++cp)
				continue;
		savedc = *cp;
		*cp = EOS;
		(void)strcpy(tok, lbp);
		*cp = savedc;
		getline();
		pfnote(tok, lineno);
	}
	/*NOTREACHED*/
}
