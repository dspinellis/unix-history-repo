/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)stab.c	6.1 (Berkeley) %G%";
#endif /* not lint */

# include "sendmail.h"

/*
**  STAB -- manage the symbol table
**
**	Parameters:
**		name -- the name to be looked up or inserted.
**		type -- the type of symbol.
**		op -- what to do:
**			ST_ENTER -- enter the name if not
**				already present.
**			ST_FIND -- find it only.
**
**	Returns:
**		pointer to a STAB entry for this name.
**		NULL if not found and not entered.
**
**	Side Effects:
**		can update the symbol table.
*/

# define STABSIZE	400

static STAB	*SymTab[STABSIZE];

STAB *
stab(name, type, op)
	char *name;
	int type;
	int op;
{
	register STAB *s;
	register STAB **ps;
	register int hfunc;
	register char *p;
	extern char lower();

	if (tTd(36, 5))
		printf("STAB: %s %d ", name, type);

	/*
	**  Compute the hashing function
	**
	**	We could probably do better....
	*/

	hfunc = type;
	for (p = name; *p != '\0'; p++)
		hfunc = (((hfunc << 7) | lower(*p)) & 077777) % STABSIZE;

	if (tTd(36, 9))
		printf("(hfunc=%d) ", hfunc);

	ps = &SymTab[hfunc];
	while ((s = *ps) != NULL && (strcasecmp(name, s->s_name) || s->s_type != type))
		ps = &s->s_next;

	/*
	**  Dispose of the entry.
	*/

	if (s != NULL || op == ST_FIND)
	{
		if (tTd(36, 5))
		{
			if (s == NULL)
				printf("not found\n");
			else
			{
				long *lp = (long *) s->s_class;

				printf("type %d val %lx %lx %lx %lx\n",
					s->s_type, lp[0], lp[1], lp[2], lp[3]);
			}
		}
		return (s);
	}

	/*
	**  Make a new entry and link it in.
	*/

	if (tTd(36, 5))
		printf("entered\n");

	/* make new entry */
	s = (STAB *) xalloc(sizeof *s);
	bzero((char *) s, sizeof *s);
	s->s_name = newstr(name);
	makelower(s->s_name);
	s->s_type = type;

	/* link it in */
	*ps = s;

	return (s);
}
