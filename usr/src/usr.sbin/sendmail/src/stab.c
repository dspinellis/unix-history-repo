# include "sendmail.h"

SCCSID(@(#)stab.c	3.12		%G%);

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
	extern bool sameword();
	register int hfunc;
	register char *p;
	extern char lower();

# ifdef DEBUG
	if (Debug > 4)
		printf("STAB: %s %d ", name, type);
# endif DEBUG

	/*
	**  Compute the hashing function
	**
	**	We could probably do better....
	*/

	hfunc = type;
	for (p = name; *p != '\0'; p++)
		hfunc = (((hfunc << 7) | lower(*p)) & 077777) % STABSIZE;

# ifdef DEBUG
	if (Debug > 5)
		printf("(hfunc=%d) ", hfunc);
# endif DEBUG

	ps = &SymTab[hfunc];
	while ((s = *ps) != NULL && (!sameword(name, s->s_name) || s->s_type != type))
		ps = &s->s_next;

	/*
	**  Dispose of the entry.
	*/

	if (s != NULL || op == ST_FIND)
	{
# ifdef DEBUG
		if (Debug > 4)
		{
			if (s == NULL)
				printf("not found\n");
			else
				printf("type %d val %lx\n", s->s_type, s->s_class);
		}
# endif DEBUG
		return (s);
	}

	/*
	**  Make a new entry and link it in.
	*/

# ifdef DEBUG
	if (Debug > 4)
		printf("entered\n");
# endif DEBUG

	/* make new entry */
	s = (STAB *) xalloc(sizeof *s);
	clear((char *) s, sizeof *s);
	s->s_name = newstr(name);
	makelower(s->s_name);
	s->s_type = type;

	/* link it in */
	*ps = s;

	return (s);
}
