/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)nlist.c	5.7 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/file.h>
#include <a.out.h>
#include <stdio.h>
#include <unistd.h>

typedef struct nlist NLIST;
#define	_strx	n_un.n_strx
#define	_name	n_un.n_name
#define	ISVALID(p)	(p->_name && p->_name[0])

nlist(name, list)
	char *name;
	NLIST *list;
{
	register NLIST *p, *s;
	struct exec ebuf;
	FILE *fstr, *fsym;
	NLIST nbuf;
	off_t strings_offset, symbol_offset, symbol_size, lseek();
	int entries, len, maxlen;
	char sbuf[256];

	entries = -1;

	if (!(fsym = fopen(name, "r")))
		return(-1);
	if (fread((char *)&ebuf, sizeof(struct exec), 1, fsym) != 1 ||
	    N_BADMAG(ebuf))
		goto done1;

	symbol_offset = N_SYMOFF(ebuf);
	symbol_size = ebuf.a_syms;
	strings_offset = symbol_offset + symbol_size;
	if (fseek(fsym, symbol_offset, SEEK_SET))
		goto done1;

	if (!(fstr = fopen(name, "r")))
		goto done1;

	/*
	 * clean out any left-over information for all valid entries.
	 * Type and value defined to be 0 if not found; historical
	 * versions cleared other and desc as well.  Also figure out
	 * the largest string length so don't read any more of the
	 * string table than we have to.
	 */
	for (p = list, entries = maxlen = 0; ISVALID(p); ++p, ++entries) {
		p->n_type = 0;
		p->n_other = 0;
		p->n_desc = 0;
		p->n_value = 0;
		if ((len = strlen(p->_name)) > maxlen)
			maxlen = len;
	}
	if (++maxlen > sizeof(sbuf)) {		/* for the NULL */
		(void)fprintf(stderr, "nlist: symbol too large.\n");
		entries = -1;
		goto done2;
	}

	for (s = &nbuf; symbol_size; symbol_size -= sizeof(NLIST)) {
		if (fread((char *)s, sizeof(NLIST), 1, fsym) != 1)
			goto done2;
		if (!s->_strx || s->n_type&N_STAB)
			continue;
		if (fseek(fstr, strings_offset + s->_strx, SEEK_SET))
			goto done2;
		(void)fread(sbuf, sizeof(sbuf[0]), maxlen, fstr);
		for (p = list; ISVALID(p); p++)
			if (!strcmp(p->_name, sbuf)) {
				p->n_value = s->n_value;
				p->n_type = s->n_type;
				p->n_desc = s->n_desc;
				p->n_other = s->n_other;
				if (!--entries)
					goto done2;
			}
	}
done2:	(void)fclose(fstr);
done1:	(void)fclose(fsym);
	return(entries);
}
