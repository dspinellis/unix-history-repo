/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kdb_sym.c	7.3 (Berkeley) 5/3/90
 */

/*
 * adb - symbol table routines
 */
#include "../kdb/defs.h"
#include <stab.h>

/*
 * Initialize the symbol table.
 */
kdbsetsym(sym, esym, strtab, strsize)
	char *sym, *esym, *strtab;
{
	register struct nlist *sp;

	kdbsymtab = (struct nlist *)sym, kdbesymtab = (struct nlist *)esym;
	for (sp = kdbsymtab; sp < kdbesymtab; sp++)
		if (sp->n_un.n_strx) {
			if (sp->n_un.n_strx > strsize) {
				kdbprintf("setsym: Bad string table index (%d)\n",
				    sp->n_un.n_strx);
				sp->n_un.n_strx = 0;	/* XXX */
				continue;
			}
			sp->n_un.n_name = strtab + sp->n_un.n_strx;
		}
}

/*
 * Lookup a symbol by name.
 */
struct nlist *
kdblookup(symstr)
	char *symstr;
{
	register struct nlist *sp;

	kdbcursym = 0;
	if (kdbsymtab)
	for (sp = kdbsymtab; sp < kdbesymtab; sp++)
		/* SHOULD DO SOME OF EQSYM INLINE TO SAVE TIME */
		if ((sp->n_type&N_STAB)==0 && kdbeqsym(sp->n_un.n_name, symstr, '_'))
			return(kdbcursym = sp);
	return (0);
}

/*
 * Find the closest symbol to val, and return
 * the difference between val and the symbol found.
 * Leave a pointer to the symbol found as cursym.
 */
kdbfindsym(val, type)
	register long val;
	int type;
{
	register diff;
	register struct nlist *sp;

	kdbcursym = 0;
	diff = MAXINT;
	if (type == NSYM || kdbsymtab == 0)
		return (diff);
	for (sp = kdbsymtab; sp < kdbesymtab; sp++) {
		if (sp->n_type&N_STAB || (sp->n_type&N_EXT)==0)
			continue;
		if (val - sp->n_value < diff && val >= sp->n_value) {
			diff = val - sp->n_value;
			kdbcursym = sp;
			if (diff == 0)
				break;
		}
	}
	return (diff);
}

/*
 * Advance cursym to the next local variable.
 * Leave its value in localval as a side effect.
 * Return 0 at end of file.
 */
kdblocalsym(cframe)
	long cframe;
{
	register int type;
	register struct nlist *sp;

	if (kdbcursym)
	for (sp = kdbcursym; ++sp < kdbesymtab; ) {
		type = sp->n_type;
		if (sp->n_un.n_name[0] =='_' || type == N_FN)
			return (0);
		switch (type) {

		case N_TEXT:
		case N_TEXT|N_EXT:
		case N_DATA:
		case N_DATA|N_EXT:
		case N_BSS:
		case N_BSS|N_EXT:
			kdblocalval = sp->n_value;
			kdbcursym = sp;
			return (1);

		case N_LSYM:
			kdblocalval = cframe - sp->n_value;
			kdbcursym = sp;
			return (1);

		case N_PSYM:
		case N_ABS:
			kdblocalval = cframe + sp->n_value;
			kdbcursym = sp;
			return (1);
		}
	}
	kdbcursym = 0;
	return (0);
}

/*
 * Print value v and then the string s.
 * If v is not zero, then we look for a nearby symbol
 * and print name+offset if we find a symbol for which
 * offset is small enough.
 *
 * For values which are just into kernel address space
 * that they match exactly or that they be more than maxoff
 * bytes into kernel space.
 */
kdbpsymoff(v, type, s)
	register long v;
	int type;
	char *s;
{
	register w;

	if (v) 
		w = kdbfindsym(v, type);
	if (v==0 || w >= kdbmaxoff)
		kdbprintf(LPRMODE, v);
	else {
		kdbprintf("%s", kdbcursym->n_un.n_name);
		if (w)
			kdbprintf(OFFMODE, w);
	}
	kdbprintf(s);
}

/*
 * Print value v symbolically if it has a reasonable
 * interpretation as name+offset.  If not, print nothing.
 * Used in printing out registers $r.
 */
kdbvalpr(v, idsp)
	long v;
{
	register off_t d;

	d = kdbfindsym(v, idsp);
	if (d >= kdbmaxoff)
		return;
	kdbprintf("%s", kdbcursym->n_un.n_name);
	if (d)
		kdbprintf(OFFMODE, d);
}
