/* Copyright (c) 1984 Regents of the University of California */

#ifndef lint
static char sccsid[] = "@(#)main.c	1.1	(Berkeley)	%G%";
#endif not lint

#include <stdio.h>
#include <ctype.h>
#include "pc2.h"

main(argc, argv)
	int argc;
	char *argv[];
{
	register struct pats *pp, **hp;
	register char *cp, *lp;
	register char *bufp;
	int size;
	extern char *index();

	if (argc > 1)
		freopen(argv[1], "r", stdin);
	if (argc > 2)
		freopen(argv[2], "w", stdout);
	/*
	 * set up the hash table
	 */
	for (pp = language_ptab; pp->name[0] != '\0'; pp++) {
		hp = hash(pp->name, &size);
		pp->size = size;
		pp->next = *hp;
		*hp = pp;
	}
	for (pp = libc_ptab; pp->name[0] != '\0'; pp++) {
		hp = hash(pp->name, &size);
		pp->size = size;
		pp->next = *hp;
		*hp = pp;
	}
	for (pp = machine_ptab; pp->name[0] != '\0'; pp++) {
		hp = hash(pp->name, &size);
		pp->size = size;
		pp->next = *hp;
		*hp = pp;
	}
	/*
	 * check each line and replace as appropriate
	 */
	buftail = bufhead = 0;
	bufp = line[0];
	while (fgets(bufp, MAXLINELEN, stdin)) {
		lp = index(bufp, LABELCHAR);
		if (lp != NULL) {
			bufp = newline();
			if (*++lp == '\n') {
				emptyqueue();
				continue;
			}
			strcpy(bufp, lp);
			*lp++ = '\n';
			*lp = '\0';
			emptyqueue();
		}
		for (cp = bufp; isspace(*cp); cp++)
			/* void */;
		if ((cp = doreplaceon(cp)) == 0) {
			bufp = newline();
			continue;
		}
		for (pp = *hash(cp, &size); pp; pp = pp->next) {
			if (pp->size == size && bcmp(pp->name, cp, size) == 0) {
				expand(pp->replace);
				bufp = line[bufhead];
				break;
			}
		}
		if (!pp) {
			emptyqueue();
			fputs(bufp, stdout);
		}
	}
	emptyqueue();
	exit(0);
}

/*
 * Integrate an expansion into the assembly stream
 */
expand(replace)
	char *replace;
{
	register int curptr;
	char *nextreplace, *argv[MAXARGS];
	int argc, argreg, queueempty, mod = 0;
	char parsebuf[BUFSIZ];

	for (curptr = bufhead; curptr != buftail; ) {
		queueempty = (curptr == buftail);
		curptr = PRED(curptr);
		nextreplace = copyline(replace, line[bufhead]);
		argc = parseline(line[bufhead], argv, parsebuf);
		argreg = nextarg(argc, argv);
		if (argreg == -1)
			break;
		while (!queueempty) {
			argc = parseline(line[curptr], argv, parsebuf);
			if (ispusharg(argc, argv))
				break;
			mod |= 1 << modifies(argc, argv);
			queueempty = (curptr == buftail);
			curptr = PRED(curptr);
		}
		if (queueempty)
			break;
		replace = nextreplace;
		if (mod & (1 << argreg)) {
			(void)newline();
		} else {
			rewrite(line[curptr], argc, argv, argreg);
			mod |= 1 << argreg;
		}
	}
	emptyqueue();
	fputs(replace, stdout);
}

/*
 * Parse a line of assembly language into opcode and arguments.
 */
parseline(linep, argv, linebuf)
	char *linep;
	char *argv[];
	char *linebuf;
{
	register char *bufp = linebuf, *cp = linep;
	register int argc = 0;

	for (;;) {
		/*
		 * skip over white space
		 */
		while (isspace(*cp))
			cp++;
		if (*cp == '\0')
			return (argc);
		/*
		 * copy argument
		 */
		if (argc == MAXARGS - 1) {
			fprintf(stderr, "instruction too long->%s", linep);
			return (argc);
		}
		argv[argc++] = bufp;
		while (!isspace(*cp) && *cp != ',' && *cp != COMMENTCHAR)
			*bufp++ = *cp++;
		*bufp++ = '\0';
		if (*cp == COMMENTCHAR)
			return (argc);
		if (*cp == ',')
			cp++;
	}
}

/*
 * Copy a newline terminated string.
 * Return pointer to character following last character copied.
 */
char *
copyline(from, to)
	register char *from, *to;
{

	while (*from != '\n')
		*to++ = *from++;
	*to++ = *from++;
	*to = '\0';
	return (from);
}

/*
 * open space for next line in the queue
 */
char *
newline()
{
	bufhead = SUCC(bufhead);
	if (bufhead == buftail) {
		fputs(line[buftail], stdout);
		buftail = SUCC(buftail);
	}
	return (line[bufhead]);
}

/*
 * empty the queue by printing out all its lines.
 */
emptyqueue()
{
	while (buftail != bufhead) {
		fputs(line[buftail], stdout);
		buftail = SUCC(buftail);
	}
}

/*
 * Compute the hash of a string.
 * Return the hash and the size of the item hashed
 */
struct pats **
hash(cp, size)
	char *cp;
	int *size;
{
	register char *cp1 = cp;
	register int hash;

	hash = 1;
	while (*cp1 && *cp1 != '\n')
		hash += (int)*cp1++;
	*size = cp1 - cp + 1;
	hash &= HSHSIZ - 1;
	return (&hashhdr[hash]);
}
