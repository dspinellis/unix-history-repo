#ifndef lint
static	char *sccsid = "@(#)lookup.c	4.1 (Berkeley) 83/09/27";
#endif

#include "defs.h"

/*
 * Define a variable from a command line argument.
 */
define(name)
	char *name;
{
	register char *cp, *s;
	register struct block *bp, *value;

	if (debug)
		printf("define(%s)\n", name);

	cp = index(name, '=');
	if (cp == NULL || cp[1] == '\0')
		value = NULL;
	else if (cp[1] != '(') {
		*cp++ = '\0';
		value = makeblock(NAME, cp);
	} else {
		bp = NULL;
		*cp++ = '\0';
		do
			cp++;
		while (*cp == ' ' || *cp == '\t');
		for (s = cp; ; s++) {
			switch (*s) {
			case ')':
				*s = '\0';
			case '\0':
				break;
			case ' ':
			case '\t':
				*s++ = '\0';
				while (*s == ' ' || *s == '\t')
					s++;
				if (*s == ')')
					*s = '\0';
				break;
			default:
				continue;
			}
			if (bp == NULL)
				value = bp = makeblock(NAME, cp);
			else {
				bp->b_next = makeblock(NAME, cp);
				bp = bp->b_next;
			}
			if (*s == '\0')
				break;
			cp = s;
		}
	}
	bp = makeblock(VAR, name);
	bp->b_args = value;
	(void) lookup(bp->b_name, bp, 1);
}

static struct block *hashtab[HASHSIZE];

/*
 * Lookup name in the table and return a pointer to it.
 * Insert == 0 - just do lookup, return NULL if not found.
 * insert == 1 - insert name with value, error if already defined.
 * insert == 2 - replace name with value if not entered with insert == 1.
 */

struct block *
lookup(name, value, insert)
	char *name;
	struct block *value;
	int insert;
{
	register unsigned n;
	register char *cp;
	register struct block *b, *f;

	if (debug)
		printf("lookup(%s, %x, %d)\n", name, value, insert);

	n = 0;
	for (cp = name; *cp; )
		n += *cp++;
	n %= HASHSIZE;

	for (b = hashtab[n]; b != NULL; b = b->b_next) {
		if (strcmp(name, b->b_name))
			continue;
		if (insert) {
			if (b->b_type == NAME) {
				warn("%s redefined\n", name);
				f = b->b_args;
				b->b_args = value->b_args;
				value->b_args = f;
			} else if (value->b_type == VAR)
				fatal("%s redefined\n", name);
			while (f = value->b_next) {
				value->b_next = f->b_next;
				free(f->b_name);
				free(f);
			}
			free(value->b_name);
			free(value);
		}
		return(b);
	}

	if (!insert)
		fatal("%s not defined", name);

	value->b_next = hashtab[n];
	hashtab[n] = value;
	return(value);
}

/*
 * Make a block for lists of variables, commands, etc.
 */
struct block *
makeblock(type, name)
	int type;
	register char *name;
{
	register char *cp;
	register struct block *bp;

	bp = ALLOC(block);
	if (bp == NULL)
		fatal("ran out of memory\n");
	bp->b_type = type;
	bp->b_next = bp->b_args = NULL;
	if (type == NAME || type == VAR) {
		bp->b_name = cp = (char *) malloc(strlen(name) + 1);
		if (cp == NULL)
			fatal("ran out of memory\n");
		while (*cp++ = *name++)
			;
	} else
		bp->b_name = NULL;
	return(bp);
}
