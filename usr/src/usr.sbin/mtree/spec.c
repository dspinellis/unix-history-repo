/*
 * Copyright (c) 1989 The Regents of the University of California.
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
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)spec.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include "mtree.h"

extern ENTRY *root;			/* root of the tree */
int lineno;				/* current spec line number */
mode_t dmode;
mode_t fmode;

spec()
{
	register char *p;
	ENTRY *centry, *last;
	INFO info;
	int ch, ignore;
	char buf[2048], *emalloc();

	info.flags = 0;
	last = NULL;
	for (lineno = 1; fgets(buf, sizeof(buf), stdin); ++lineno) {
		if (!(p = index(buf, '\n'))) {
			(void)fprintf(stderr,
			    "mtree: line %d too long.\n", lineno);
			while ((ch = getchar()) != '\n' && ch != EOF);
			continue;
		}
		*p = '\0';
		for (p = buf; *p && isspace(*p); ++p);
		if (!*p || *p == '#')
			continue;

		/* grab file name, "$", "set", or "unset" */
		if (!(p = strtok(buf, "\n\t ")))
			specerr();

		ignore = 0;
		if (p[0] == '/')
			switch(p[1]) {
			case 'i':
				ignore = 1;
				if (!(p = strtok((char *)NULL, "\t ")))
					specerr();
				break;
			case 's':
				if (strcmp(p + 1, "set"))
					break;
				if (!(p = strtok((char *)NULL, "\t ")))
					specerr();
				set(p, &info, 1);
				continue;
			case 'u':
				if (strcmp(p + 1, "unset"))
					break;
				if (!(p = strtok((char *)NULL, "\t ")))
					specerr();
				unset(p, &info);
				continue;
			}

		if (index(p, '/')) {
			(void)fprintf(stderr,
			    "mtree: file names may not contain slashes.\n");
			specerr();
		}

		if (!(info.flags&F_TYPE)) {
			(void)fprintf(stderr, "mtree: no type set.\n");
			specerr();
		}

		if (!strcmp(p, "..")) {
			/* don't go up, if haven't gone down */
			if (!root)
				noparent();
			if (last->info.type != F_DIR || last->flags&F_DONE) {
				if (last == root)
					noparent();
				last = last->parent;
			}
			last->flags |= F_DONE;
			continue;
		}

		centry = (ENTRY *)emalloc(sizeof(ENTRY));
		if (!(centry->name = strdup(p)))
			nomem();
		centry->info = info;
		centry->info.st_mode = info.type == F_DIR ? dmode : fmode;
		centry->flags = ignore;
		while (p = strtok((char *)NULL, "\t "))
			set(p, &centry->info, 0);

		if (!root) {
			last = root = centry;
			root->parent = root;
		} else if (last->info.type == F_DIR && !(last->flags&F_DONE)) {
			centry->parent = last;
			last = last->child = centry;
		} else {
			centry->parent = last->parent;
			last = last->next = centry;
		}
	}
}

noparent()
{
	(void)fprintf(stderr, "mtree: no parent node.\n");
	specerr();
}
