#define NSTR 25

/*
 * Parse the given string into a number of sub strings separated
 * by characters in brksep.  Terminate on brkterm or 0.
 */

char **
brkstring(strg, brksep, brkterm)
	char *strg;
	char *brksep, *brkterm;
{
	register int c;
	register char *cp, **ap;
	static char *broken[NSTR+1];

	cp = strg;
	ap = broken;
	for (;;) {
		if (ap-broken > NSTR) {
			broken[NSTR] = 0;
			return(broken);
		}
		while (brkany(*cp, brksep))
			cp++;
		if (*cp == 0 || *cp == brkterm) {
done:
			*ap = 0;
			return(broken);
		}
		*ap++ = cp;
		while (!brkany(*cp, brksep) && *cp && *cp != brkterm)
			cp++;
		if (*cp == 0 || *cp == brkterm) {
			*cp = 0;
			goto done;
		}
		*cp++ = 0;
	}
}

brkany (chr,strg)	/* returns 1 if chr in strg, 0 otherwise */
char chr,*strg;
{
	register char *sp;

	for (sp=strg; *sp; sp++)
		if (chr == *sp) return (1);
	return (0);
}
