#define NSTR 25
char **brkstring(strg,brksep,brkterm)	/* returns pointer to static table of substring ptrs */
char *strg;
char *brksep, *brkterm;
{

	register char c, *bp, *sp;
	static char *broken[NSTR+1];	/* static array of substring start addresses */
	int bi;

	sp = strg;	/* scan string, replacing separators with zeroes */

	for (bi=0; bi<NSTR; bi++) {	/* and entering start addrs in "broken" */
		while (brkany(c = *sp, brksep)) *sp++ = 0;
		if (!c || brkany(c, brkterm)) {
			*sp=0;
			broken[bi] = 0;
			return(broken);		/* terminator found, finish up */
		}

		broken[bi] = sp;	/* set next start addr */

		while ((c = *++sp) && !brkany(c,brksep) && !brkany(c,brkterm));

	}
	broken[NSTR] = 0;		/* reached limit of NSTR substrings */
	return (broken);
}

brkany (chr,strg)	/* returns 1 if chr in strg, 0 otherwise */
char chr,*strg;
{
	register char *sp;

	for (sp=strg; *sp; sp++)
		if (chr == *sp) return (1);
	return (0);
}
