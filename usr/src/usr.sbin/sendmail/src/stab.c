# include "sendmail.h"

static char SccsId[] = "@(#)stab.c	3.4	%G%";

/*
**  STAB -- manage the symbol table
**
**	Parameters:
**		name -- the name to be looked up or inserted.
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
**
**	Notes:
**		Obviously, this deserves a better algorithm.  But
**		for the moment......
*/

static STAB	*SymTab;

STAB *
stab(name, op)
	char *name;
	int op;
{
	register STAB *s = SymTab;
	register STAB **ps = &SymTab;
	extern bool sameword();

# ifdef DEBUG
	if (Debug > 4)
		printf("STAB: %s ", name);
# endif DEBUG

	while (s != NULL && !sameword(name, s->s_name))
	{
		ps = &s->s_next;
		s = s->s_next;
	}
	if (s != NULL || op == ST_FIND)
	{
# ifdef DEBUG
		if (Debug > 4)
		{
			if (s == NULL)
				printf("not found\n");
			else
				printf("type %d class %x\n", s->s_type, s->s_class);
		}
# endif DEBUG
		return (s);
	}

# ifdef DEBUG
	if (Debug > 4)
		printf("entered\n");
# endif DEBUG

	/* make new entry */
	s = (STAB *) xalloc(sizeof *s);
	s->s_name = newstr(name);
	makelower(s->s_name);
	s->s_type = 0;
	s->s_class = 0;
	s->s_next = NULL;

	/* and link it in */
	*ps = s;

	return (s);
}
