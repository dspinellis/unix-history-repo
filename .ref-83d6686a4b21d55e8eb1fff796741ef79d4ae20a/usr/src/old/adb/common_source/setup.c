#ifndef lint
static char sccsid[] = "@(#)setup.c	5.1 (Berkeley) %G%";
#endif

/*
 * adb - routines to read a.out and core files at startup
 */

#include "defs.h"
#include <sys/file.h>

static struct exec filhdr;

off_t	lseek();
char	*malloc();

/* NB. the following works only with letter (alpha) variables */
#define	setavar(name, value) (var[(name) - 'a' + 10] = (value))

setsym()
{
	register struct nlist *sp;
	int strsize;
	char *strtab;
	off_t loc, dbase;

	txtmap.ufd = symfile.fd = getfile(1);
	if (read(symfile.fd, (char *)&filhdr, sizeof(filhdr)) != sizeof(filhdr) ||
	    N_BADMAG(filhdr)) {
		bzero((char *)&filhdr, sizeof(filhdr));
		txtmap.m1.e = -(addr_t)1;
		return;
	}
	loc = filhdr.a_text + filhdr.a_data;
	txtmap.m1.f = txtmap.m2.f = N_TXTOFF(filhdr);
	switch ((int)filhdr.a_magic) {

	case OMAGIC:
		/* text map 1 is empty; map 2 goes from 0 to loc */
		txtmap.m2.b = dbase = 0;
		txtmap.m2.e = loc;
		break;

	case ZMAGIC:
	case NMAGIC:
		/* text map 1 maps text segment, map 2 maps data */
		txtmap.m1.e = filhdr.a_text;
		txtmap.m2.b = dbase = roundup(filhdr.a_text, CLBYTES);
		txtmap.m2.e = dbase + filhdr.a_data;
		txtmap.m2.f += txtmap.m1.e;
		break;
	}

	/* save data segment base in variable b */
	setavar('b', dbase);

	if (filhdr.a_syms != 0) {
		loc = N_SYMOFF(filhdr);
		symtab = (struct nlist *)malloc((u_int)filhdr.a_syms);
		if (symtab == NULL)
			goto nospace;
		esymtab = &symtab[filhdr.a_syms / sizeof(struct nlist)];
		(void) lseek(symfile.fd, loc, L_SET);

#define	rd(a, n) \
	if (read(symfile.fd, (char *)(a), (int)(n)) != (n)) \
		goto readerr

		rd(symtab, filhdr.a_syms);
		rd(&strsize, sizeof(strsize));
		/*
		 * offsets in the string table are relative to the offset
		 * of the number we just read; we adjust for it here.
		 */
		strsize -= sizeof(strsize);
		if ((strtab = malloc((u_int)strsize)) == NULL)
			goto nospace;
		rd(strtab, strsize);
		for (sp = symtab; sp < esymtab; sp++) {
			if (sp->n_un.n_strx == 0)
				continue;
			sp->n_un.n_strx -= sizeof(strsize);
			if ((u_long)sp->n_un.n_strx >= strsize) {
				adbprintf("bad string index %D in symtab\n",
				    (expr_t)sp->n_un.n_strx);
				sp->n_un.n_name = "";
			} else
				sp->n_un.n_name = strtab + sp->n_un.n_strx;
		}
#undef rd
	}
	if (INKERNEL(filhdr.a_entry)) {
		txtmap.m1.b += KERNTEXTOFF;
		txtmap.m1.e += KERNTEXTOFF;
		txtmap.m2.b += KERNTEXTOFF;
		txtmap.m2.e += KERNTEXTOFF;
	}
	return;

readerr:
	prints("Error reading symbol|string table (old format a.out?)\n");
	exit(1);
	/* NOTREACHED */

nospace:
	prints("Not enough space for symbol|string table\n");
	exit(1);
	/* NOTREACHED */
}

setcore()
{
	off_t stacksize;

	datmap.m1.e = -(addr_t)1;
	if ((datmap.ufd = corefile.fd = getfile(2)) < 0)
		goto ret;
	if (kernel && INKERNEL(filhdr.a_entry) && getkcore()) {
		kcore = 1;
		goto ret;
	}
	if (read(corefile.fd, (char *)&u, ctob(UPAGES)) != ctob(UPAGES) ||
	    !udot()) {
		adbprintf("not core file = %s\n", corefile.name);
		goto ret;
	}
	signo = u.u_arg[0];
	sigcode = u.u_code;
	filhdr.a_text = ctob(u.u_tsize);
	filhdr.a_data = ctob(u.u_dsize);
	stacksize = ctob(u.u_ssize);
	switch ((int)filhdr.a_magic) {

	case OMAGIC:
		datmap.m1.b = 0;
		datmap.m1.e = filhdr.a_text + filhdr.a_data;
		datmap.m2.f = ctob(UPAGES) + datmap.m1.e;
		break;

	case NMAGIC:
	case ZMAGIC:
		datmap.m1.b = roundup(filhdr.a_text, CLBYTES);
		datmap.m1.e = datmap.m1.b + filhdr.a_data;
		datmap.m2.f = ctob(UPAGES) + filhdr.a_data;
		break;
	}
	/* save (possibly new) data segment base, and save stack size */
	setavar('b', datmap.m1.b);
	setavar('s', stacksize);
	datmap.m1.f = ctob(UPAGES);
	datmap.m2.b = KERNBASE - ctob(UPAGES) - stacksize;
	datmap.m2.e = KERNBASE - ctob(UPAGES);
ret:
	u.u_ar0 = (int *)((caddr_t)&u + ctob(UPAGES));	/* XXX */
	setavar('d', filhdr.a_data);
	setavar('e', filhdr.a_entry);
	setavar('m', filhdr.a_magic);
	setavar('t', filhdr.a_text);
}
