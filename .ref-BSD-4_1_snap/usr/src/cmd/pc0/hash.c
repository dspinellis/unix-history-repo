/* Copyright (c) 1979 Regents of the University of California */

static	char sccsid[] = "@(#)hash.c 1.2 11/24/80";

#include "whoami.h"
#include "0.h"
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
	"assert",	YASSERT,
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
	"oct",		YOCT,	 /* non-standard Pascal */
	"hex",		YHEX,	 /* non-standard Pascal */
	"external",	YEXTERN, /* non-standard Pascal */
	0
};

char	*lastkey	= &yykey[sizeof yykey/sizeof yykey[0]];

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
	for (ip = yykey; *ip; ip += 2)
		hash(ip[0], 0)[0] = ip;
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
			cp = (char *) calloc(sizeof ( int * ), HASHINC);
			if (cp == 0) {
				yerror("Ran out of memory (hash)");
				pexit(DIED);
			}
			htp->ht_low = cp;
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
					*h = s;
				return (h);
			}
			sym = *h;
			if (sym < lastkey && sym >= yykey)
				sym = *sym;
			if (sym->pchar == *cp && strcmp(sym, cp) == 0)
				return (h);
			h += i;
			i += 2;
			if (h >= htp->ht_high)
				h -= HASHINC;
		} while (i < HASHINC);
	}
	yerror("Ran out of hash tables");
	pexit(DIED);
}
