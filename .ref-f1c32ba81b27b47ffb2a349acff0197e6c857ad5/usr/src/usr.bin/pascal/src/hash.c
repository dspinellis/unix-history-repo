/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)hash.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "whoami.h"
#include "0.h"
#include "tree_ty.h"		/* must be included for yy.h */
#include "yy.h"

/*
 * The definition for the segmented hash tables.
 */
struct ht {
	int	*ht_low;
	int	*ht_high;
	int	ht_used;
} htab[MAXHASH];

/*
 * This is the array of keywords and their
 * token values, which are hashed into the table
 * by inithash.
 */
struct kwtab yykey[] = {
	"and",		YAND,
	"array",	YARRAY,
	"begin",	YBEGIN,
	"case",		YCASE,
	"const",	YCONST,
	"div",		YDIV,
	"do",		YDO,
	"downto",	YDOWNTO,
	"else",		YELSE,
	"end",		YEND,
	"file",		YFILE,
	"for",		YFOR,
	"forward",	YFORWARD,
	"function",	YFUNCTION,
	"goto",		YGOTO,
	"if",		YIF,
	"in",		YIN,
	"label",	YLABEL,
	"mod",		YMOD,
	"nil",		YNIL,
	"not",		YNOT,
	"of",		YOF,
	"or",		YOR,
	"packed",	YPACKED,
	"procedure",	YPROCEDURE,
	"program",	YPROG,
	"record",	YRECORD,
	"repeat",	YREPEAT,
	"set",		YSET,
	"then",		YTHEN,
	"to",		YTO,
	"type",		YTYPE,
	"until",	YUNTIL,
	"var",		YVAR,
	"while",	YWHILE,
	"with",		YWITH,
	0,		0,	 /* the following keywords are non-standard */
	"oct",		YOCT,
	"hex",		YHEX,
	"external",	YEXTERN,
	0
};

char *lastkey;

/*
 * Inithash initializes the hash table routines
 * by allocating the first hash table segment using
 * an already existing memory slot.
 */
#ifndef PI0
inithash()
#else
inithash(hshtab)
	int *hshtab;
#endif
{
	register int *ip;
#ifndef PI0
	static int hshtab[HASHINC];
#endif

	htab[0].ht_low = hshtab;
	htab[0].ht_high = &hshtab[HASHINC];
	for (ip = ((int *)yykey); *ip; ip += 2)
		hash((char *) ip[0], 0)[0] = ((int) ip);
	/*
	 * If we are not running in "standard-only" mode,
	 * we load the non-standard keywords.
	 */
	if (!opt('s'))
		for (ip += 2; *ip; ip += 2)
			hash((char *) ip[0], 0)[0] = ((int) ip);
	lastkey = (char *)ip;
}

/*
 * Hash looks up the s(ymbol) argument
 * in the string table, entering it if
 * it is not found. If save is 0, then
 * the argument string is already in
 * a safe place. Otherwise, if hash is
 * entering the symbol for the first time
 * it will save the symbol in the string
 * table using savestr.
 */
int *hash(s, save)
	char *s;
	int save;
{
	register int *h;
	register i;
	register char *cp;
	int *sym;
	struct cstruct *temp;
	struct ht *htp;
	int sh;

	/*
	 * The hash function is a modular hash of
	 * the sum of the characters with the sum
	 * doubled before each successive character
	 * is added.
	 */
	cp = s;
	if (cp == NIL)
		cp = token;	/* default symbol to be hashed */
	i = 0;
	while (*cp)
		i = i*2 + *cp++;
	sh = (i&077777) % HASHINC;
	cp = s;
	if (cp == NIL)
		cp = token;
	/*
	 * There are as many as MAXHASH active
	 * hash tables at any given point in time.
	 * The search starts with the first table
	 * and continues through the active tables
	 * as necessary.
	 */
	for (htp = htab; htp < &htab[MAXHASH]; htp++) {
		if (htp->ht_low == NIL) {
			cp = (char *) pcalloc(sizeof ( int * ), HASHINC);
			if (cp == 0) {
				yerror("Ran out of memory (hash)");
				pexit(DIED);
			}
			htp->ht_low = ((int *)cp);
			htp->ht_high = htp->ht_low + HASHINC;
			cp = s;
			if (cp == NIL)
				cp = token;
		}
		h = htp->ht_low + sh;
		/*
		 * quadratic rehash increment
		 * starts at 1 and incremented
		 * by two each rehash.
		 */
		i = 1;
		do {
			if (*h == 0) {
				if (htp->ht_used > (HASHINC * 3)/4)
					break;
				htp->ht_used++;
				if (save != 0) {
					*h = (int) savestr(cp);
				} else
					*h = ((int) s);
				return (h);
			}
			sym = ((int *) *h);
			if (sym < ((int *) lastkey) && sym >= ((int *) yykey))
				sym = ((int *) *sym);
			temp = ((struct cstruct *) sym);
			if (temp->pchar == *cp && pstrcmp((char *) sym, cp) == 0)
				return (h);
			h += i;
			i += 2;
			if (h >= htp->ht_high)
				h -= HASHINC;
		} while (i < HASHINC);
	}
	yerror("Ran out of hash tables");
	pexit(DIED);
	return (NIL);

}
