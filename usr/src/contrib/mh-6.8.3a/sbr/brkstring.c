/* brkstring.c - break string into an array of strings */

#include "../h/mh.h"


static char *broken[MAXARGS+1];	/* array of substring start addresses */
static brkany();

char  **brkstring (strg, brksep, brkterm)
register char  *strg;
register char  *brksep,
               *brkterm;
{
    register int    bi;
    register char   c,
                   *sp;

    sp = strg;			/* scan string, replacing separators with
				   zeroes */

    for (bi = 0; bi < MAXARGS; bi++) {
				/* and entering start addrs in "broken" */
	while (brkany (c = *sp, brksep))
	    *sp++ = 0;
	if (!c || brkany (c, brkterm)) {
	    *sp = 0;
	    broken[bi] = 0;
	    return broken;	/* terminator found, finish up */
	}

	broken[bi] = sp;	/* set next start addr */
	while ((c = *++sp) && !brkany (c, brksep) && !brkany (c, brkterm))
		continue;
    }
    broken[MAXARGS] = 0;	/* reached limit of MAXARGS substrings */

    return broken;
}


static  brkany (chr, strg)	/* returns 1 if chr in strg, 0 otherwise  */
register char   chr,
               *strg;
{
    register char  *sp;

    if (strg)
	for (sp = strg; *sp; sp++)
	    if (chr == *sp)
		return 1;
    return 0;
}
