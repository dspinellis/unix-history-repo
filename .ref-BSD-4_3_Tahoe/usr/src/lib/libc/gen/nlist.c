/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)nlist.c	5.2 (Berkeley) 3/9/86";
#endif LIBC_SCCS and not lint

#include <sys/types.h>
#include <a.out.h>
#include <stdio.h>

/*
 * nlist - retreive attributes from name list (string table version)
 */
nlist(name, list)
	char *name;
	struct nlist *list;
{
	register struct nlist *p, *q;
	register char *s1, *s2;
	register n, m;
	int maxlen, nreq;
	FILE *f;
	FILE *sf;
	off_t sa;		/* symbol address */
	off_t ss;		/* start of strings */
	struct exec buf;
	struct nlist space[BUFSIZ/sizeof (struct nlist)];

	maxlen = 0;
	for (q = list, nreq = 0; q->n_un.n_name && q->n_un.n_name[0]; q++, nreq++) {
		q->n_type = 0;
		q->n_value = 0;
		q->n_desc = 0;
		q->n_other = 0;
		n = strlen(q->n_un.n_name);
		if (n > maxlen)
			maxlen = n;
	}
	f = fopen(name, "r");
	if (f == NULL)
		return (-1);
	fread((char *)&buf, sizeof buf, 1, f);
	if (N_BADMAG(buf)) {
		fclose(f);
		return (-1);
	}
	sf = fopen(name, "r");
	if (sf == NULL) {
		/* ??? */
		fclose(f);
		return(-1);
	}
	sa = N_SYMOFF(buf);
	ss = sa + buf.a_syms;
	n = buf.a_syms;
	fseek(f, sa, 0);
	while (n) {
		m = sizeof (space);
		if (n < m)
			m = n;
		if (fread((char *)space, m, 1, f) != 1)
			break;
		n -= m;
		for (q = space; (m -= sizeof(struct nlist)) >= 0; q++) {
			char nambuf[BUFSIZ];

			if (q->n_un.n_strx == 0 || q->n_type & N_STAB)
				continue;
			fseek(sf, ss+q->n_un.n_strx, 0);
			fread(nambuf, maxlen+1, 1, sf);
			for (p = list; p->n_un.n_name && p->n_un.n_name[0]; p++) {
				s1 = p->n_un.n_name;
				s2 = nambuf;
				while (*s1) {
					if (*s1++ != *s2++)
						goto cont;
				}
				if (*s2)
					goto cont;
				p->n_value = q->n_value;
				p->n_type = q->n_type;
				p->n_desc = q->n_desc;
				p->n_other = q->n_other;
				if (--nreq == 0)
					goto alldone;
				break;
		cont:		;
			}
		}
	}
alldone:
	fclose(f);
	fclose(sf);
	return (nreq);
}
