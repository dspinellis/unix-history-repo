/*
 * Copyright (c) 1980 The Regents of the University of California.
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
char copyright[] =
"@(#) Copyright (c) 1980 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)symorder.c	5.4 (Berkeley) 6/1/90";
#endif /* not lint */

/*
 * symorder - reorder symbol table
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <a.out.h>

#define SPACE 100

struct	nlist order[SPACE];

char	*savestr(), *index(), *malloc();
struct	exec exec;
off_t	sa;
struct	stat stb;
int	nsym = 0;
int	symfound = 0;
char	*strings;
char	*newstrings;
struct	nlist *symtab;
struct	nlist *newtab;
int	symsize;
char	asym[BUFSIZ];

main(argc, argv)
	char **argv;
{
	register char *ns;
	register struct nlist *symp;
	register struct nlist *p;
	register FILE *f;
	register int i;
	int n, o;

	if (argc != 3) {
		fprintf(stderr, "Usage: symorder orderlist file\n");
		exit(1);
	}
	if ((f = fopen(argv[1], "r")) == NULL) {
		perror(argv[1]);
		exit(1);
	}
	for (p = order; fgets(asym, sizeof asym, f) != NULL; p++, nsym++) {
		for (i = 0; asym[i] && asym[i] != '\n'; i++)
			continue;
		if (asym[i] == '\n')
			asym[i] = 0;
		p->n_un.n_name = savestr(asym);
	}
	fclose(f);
	if ((f = fopen(argv[2], "r")) == NULL)
		perror(argv[2]), exit(1);
	if ((o = open(argv[2], 1)) < 0)
		perror(argv[2]), exit(1);
	if ((fread(&exec, sizeof exec, 1, f)) != 1 || N_BADMAG(exec)) {
		fprintf(stderr, "symorder: %s: bad format\n", argv[2]);
		exit(1);
	}
	if (exec.a_syms == 0) {
		fprintf(stderr, "symorder: %s is stripped\n", argv[2]);
		exit(1);
	}
	fstat(fileno(f), &stb);
	if (stb.st_size < N_STROFF(exec)+sizeof(off_t)) {
		fprintf(stderr, "symorder: %s is in old format or truncated\n",
		    argv[2]);
		exit(1);
	}
	sa = N_SYMOFF(exec);
	fseek(f, sa, 0);
	n = exec.a_syms;
	symtab = (struct nlist *)malloc(n);
	if (symtab == (struct nlist *)0) {
		fprintf(stderr, "symorder: Out of core, no space for symtab\n");
		exit(1);
	}
	if (fread((char *)symtab, 1, n, f) != n) {
		fprintf(stderr, "symorder: Short file "); perror(argv[2]);
		exit(1);
	}
	if (fread((char *)&symsize, sizeof (int), 1, f) != 1 ||
	    symsize <= 0) {
		fprintf(stderr, "symorder: No strings "); perror(argv[2]);
		exit(1);
	}
	strings = malloc(symsize);
	if (strings == (char *)0) {
		fprintf(stderr,"symorder: Out of core, no space for strings\n");
		exit(1);
	}
	/*
	 * Need to subtract four from symsize here since
	 * symsize includes itself, and we've already read
	 * it.  (6/30/85 chris@maryland)
	 */
	if (fread(strings, 1, symsize - 4, f) != symsize - 4) {
		fprintf(stderr, "symorder: Truncated strings "); 
		perror(argv[2]);
		exit(1);
	}

	newtab = (struct nlist *)malloc(n);
	if (newtab == (struct nlist *)0) {
		fprintf(stderr,
		    "symorder: Out of core, no space for new symtab\n");
		exit(1);
	}
	i = n / sizeof (struct nlist);
	reorder(symtab, newtab, i);
	free((char *)symtab);
	symtab = newtab;

	newstrings = malloc(symsize);
	if (newstrings == (char *)0) {
		fprintf(stderr,
		    "symorder: Out of core, no space for newstrings\n");
		exit(1);
	}
	ns = newstrings;
	for (symp = symtab; --i >= 0; symp++) {
		if (symp->n_un.n_strx == 0)
			continue;
		symp->n_un.n_strx -= sizeof (int);
		if ((unsigned)symp->n_un.n_strx >= symsize) {
			fprintf(stderr,"symorder: Corrupted string pointers\n");
			exit(1);
		}
		strcpy(ns, &strings[symp->n_un.n_strx]);
		symp->n_un.n_strx = (ns - newstrings) + sizeof (int);
		ns = index(ns, 0) + 1;
		if (ns > &newstrings[symsize]) {
			fprintf(stderr, "symorder: Strings grew longer!\n");
			exit(1);
		}
	}

	lseek(o, sa, 0);
	if (write(o, (char *)symtab, n) != n) {
		fprintf(stderr, "symorder: Write failed "); perror(argv[2]);
		exit(1);
	}
	if (write(o, (char *)&symsize, sizeof (int)) != sizeof (int)) {
		fprintf(stderr, "symorder: Write failed "); perror(argv[2]);
		exit(1);
	}
	if (write(o, newstrings, symsize - 4) != symsize - 4) {
		fprintf(stderr, "symorder: Write failed "); perror(argv[2]);
		exit(1);
	}
	if ((i = nsym - symfound) > 0) {
		fprintf(stderr, "symorder: %d symbol%s not found:\n",
		    i, i == 1 ? "" : "s");
		for (i = 0; i < nsym; i++) {
			if (order[i].n_value == 0)
				printf("%s\n", order[i].n_un.n_name);
		}
	}
	exit(0);
}

reorder(st1, st2, n)
	register struct nlist *st1, *st2;
	register n;
{
	register struct nlist *stp = st2 + nsym;
	register i;

	while (--n >= 0) {
		i = inlist(st1);
		if (i == -1)
			*stp++ = *st1++;
		else
			st2[i] = *st1++;
	}
}

inlist(p)
	register struct nlist *p;
{
	register char *nam;
	register struct nlist *op;

	if (p->n_type & N_STAB)
		return (-1);
	if (p->n_un.n_strx == 0)
		return (-1);

	nam = &strings[p->n_un.n_strx - sizeof(int)];
	if (nam >= &strings[symsize]) {
		fprintf(stderr, "symorder: corrupt symtab\n");
		exit(1);
	}

	for (op = &order[nsym]; --op >= order; ) {
		if (strcmp(op->n_un.n_name, nam) != 0)
			continue;
		if (op->n_value == 0) {
			op->n_value++;
			symfound++;
		}
		return (op - order);
	}
	return (-1);
}

#define	NSAVETAB	4096
char	*savetab;
int	saveleft;

char *
savestr(cp)
	register char *cp;
{
	register int len;

	len = strlen(cp) + 1;
	if (len > saveleft) {
		saveleft = NSAVETAB;
		if (len > saveleft)
			saveleft = len;
		savetab = (char *)malloc(saveleft);
		if (savetab == 0) {
			fprintf(stderr,
			    "symorder: ran out of memory (savestr)\n");
			exit(1);
		}
	}
	strncpy(savetab, cp, len);
	cp = savetab;
	savetab += len;
	saveleft -= len;
	return (cp);
}
