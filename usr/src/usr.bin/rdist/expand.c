#ifndef lint
static	char *sccsid = "@(#)expand.c	4.1 (Berkeley) 83/09/07";
#endif

#include "defs.h"

static struct block *hashtab[HASHSIZE];
static int nhashed = 0;

/*
 * Lookup name in the table and return a pointer to it.
 * if insert, insert or replace name with value.
 */

struct block *
lookup(name, insert, value)
	char *name;
	int insert;
	struct block *value;
{
	register unsigned n;
	register char *cp;
	register struct block *b;

	if (debug)
		printf("lookup(%s, %d, %x)\n", name, insert, value);

	n = 0;
	for (cp = name; *cp; )
		n += *cp++;
	n %= HASHSIZE;

	for (b = hashtab[n]; b != NULL; b = b->b_next) {
		if (strcmp(name, b->b_name))
			continue;
		if (insert) {
			warn("%s redefined\n", name);
			b->b_args = value->b_args;
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

char	*index();

/*
 * Take a list of names and expand any macros found.
 */
struct block *
expand(list)
	struct block *list;
{
	register struct block *prev, *bp, *tp;
	register char *cp, *s;
	register int n;
	char *var, *tail;
	int c;

	for (prev = NULL, bp = list; bp != NULL; prev = bp, bp = bp->b_next) {
	again:
		cp = index(bp->b_name, '$');
		if (cp == NULL || cp != bp->b_name && cp[-1] == '\\')
			continue;
		*cp++ = '\0';
		if (*cp == '\0')
			fatal("no variable name after '$'");
		if (*cp == '{') {
			cp++;
			if ((tail = index(cp, '}')) == NULL)
				fatal("missing '}'");
			*tail++ = c = '\0';
			if (*cp == '\0')
				fatal("no variable name after '$'");
		} else {
			tail = cp + 1;
			c = *tail;
			*tail = '\0';
		}
		tp = lookup(cp, 0, NULL);
		if ((tp = tp->b_args) != NULL) {
			struct block *first = tp;

			if (prev == NULL)
				list = tp;
			else
				prev->b_next = tp;
			if (c)
				*tail = c;
			makestr(tp, bp->b_name, tail);
			while (tp->b_next != NULL) {
				tp = tp->b_next;
				makestr(tp, bp->b_name, tail);
			}
			tp->b_next = bp->b_next;
			free(bp->b_name);
			free(bp);
			bp = first;
			goto again;
		} else {
			if (prev == NULL)
				list = tp = list->b_next;
			else
				prev->b_next = tp = bp->b_next;
			free(bp->b_name);
			free(bp);
			if (tp != NULL) {
				bp = tp;
				goto again;
			}
			break;
		}
	}
	return(list);
}

/*
 * Concat head, bp->b_name, and tail
 */
makestr(bp, head, tail)
	struct block *bp;
	char *head, *tail;
{
	register int n;
	register char *cp;

	if (!*head && !*tail)
		return;
	n = strlen(bp->b_name) + strlen(head) + strlen(tail) + 1;
	cp = (char *) malloc(n);
	if (cp == NULL)
		fatal("ran out of memory");
	sprintf(cp, "%s%s%s", head, bp->b_name, tail);
	free(bp->b_name);
	bp->b_name = cp;
}

/*
 * If there are any Shell meta characters in the name,
 * expand into a list, after searching directory
 * For now, only do ~name.
 */
shexpand(buf, file)
	char buf[];
	register char *file;
{
	register char *s1, *s2, *s3;
	register struct passwd *pw;
	extern char *homedir;

	if (*file != '~') {
		strcpy(buf, file);
		return;
	}
	file++;
	if (*file == '\0' || *file == '/') {
		s2 = homedir;
		s3 = file;
	} else {
		for (s3 = file; *s3 && *s3 != '/'; s3++)
			;
		if (*s3 == '/')
			*s3 = '\0';
		else
			s3 = NULL;
		setpwent();
		pw = getpwnam(file);
		if (pw == NULL) {
			error("unknown user %s\n", file);
			if (s3 != NULL)
				*s3 = '/';
			return;
		}
		if (s3 != NULL)
			*s3 = '/';
		s2 = pw->pw_dir;
	}
	for (s1 = buf; *s1++ = *s2++; )
		;
	if (s3 == NULL)
		return;
	s1--;
	while (*s1++ = *s3++)
		;
}
