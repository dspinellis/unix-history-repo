/* @(#)nlist.c	4.1 (Berkeley) 12/21/80 */
#include <sys/types.h>
#include <pagsiz.h>
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
	register n, m, i, nreq;
	FILE *f;
	off_t sa;		/* symbol address */
	off_t ss;		/* start of strings */
	struct exec buf;
	struct nlist space[BUFSIZ/sizeof (struct nlist)];
	int maxlen;

	maxlen = 0;
	for (q = list, nreq = 0; q->n_un.n_name && q->n_un.n_name[0]; q++, nreq++) {
		q->n_type = 0;
		q->n_value = 0;
		q->n_desc = 0;
		q->n_other = 0;
		i = strlen(q->n_un.n_name);
		if (i > maxlen)
			maxlen = i;
	}
	f = fopen(name, "r");
	if (f == NULL)
		return (NULL);
	fread((char *)&buf, sizeof buf, 1, f);
	if (N_BADMAG(buf)) {
		close(f);
		return (-1);
	}
	sa = N_SYMOFF(buf);
	ss = sa + buf.a_syms;
	n = buf.a_syms;
	while (n) {
		m = sizeof (space);
		if (n < m)
			m = n;
		fseek(f, sa, 0);
		i = fread((char *)space, m, 1, f);
		sa += m;
		n -= m;
		for (q = space; (m -= sizeof(struct nlist)) >= 0; q++) {
			char nambuf[BUFSIZ];

			if (q->n_un.n_strx == 0 || q->n_type & N_STAB)
				continue;
			fseek(f, ss+q->n_un.n_strx, 0);
			fread(nambuf, maxlen+1, 1, f);
			for (p = list; p->n_un.n_name[0]; p++) {
				i = 0;
				while (p->n_un.n_name[i]) {
					if (p->n_un.n_name[i] != nambuf[i])
						goto cont;
					i++;
				}
				if (nambuf[i])
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
	return (0);
}
