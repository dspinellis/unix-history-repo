/* @(#)syskludge.c	4.1 (Berkeley) 1/1/83 */

#include <sys/param.h>

/*
 * Prefix table.
 * If a prefix is "xyz", for example,
 * then any file Spool/xyz... is mapped to Spool/xyz/xyz... .
 * The first prefix found is used, so D.foo should preceed D. in table.
 * Should be tuned on a per-system basis.
 *
 * Each prefix must be a subdirectory of Spool, owned by uucp!
 */

static char *prefix[] = {
	"D.decvax",
	"D.ucsfcgl",
	"D.npois",
	"D.ihnss",
	"D.cbosgd",
	"D.cbosg",
	"D.ucbvaxB",
	"D.ucbvax",
	"D.ucb",
	"LOG.",
	"LTMP.",
	"STST.",
	"D.",		/* "D." subdirectory (remember the "."!) */
	"C.",		/* "C." subdirectory */
	"X.",
	0
};

/*
 * filename mapping kludges to put uucp work files in other directories.
 */

char	fn1[MAXPATHLEN], fn2[MAXPATHLEN];	/* remapped filename areas */
char	sfn[MAXPATHLEN];	/* most recent filename passed to chdir */

extern	char *Spool;

char	*strcpy(), *strcat();

/*
 * return (possibly) remapped string s,
 * using buffer area t if necessary.
 */
static char *
_dofix(s, t)
register char *s, *t;
{
	register char **p;
	register int n;
	char *os;

	os = s;
	t[0] = '\0';

	/* if s begins with Spool/, copy that to t and advance s */
	if (strncmp(s, Spool, n = strlen(Spool)) == 0 && s[n] == '/') {
		strcpy(t, Spool);
		strcat(t, "/");
		s += n + 1;
	}
	else
		if (strcmp(sfn, Spool))
			return(os);

	/* look for first prefix which matches, and make subdirectory */
	for (p = &prefix[0]; *p; p++) {
		if (strncmp(s, *p, n = strlen(*p))==0 && s[n] && s[n] != '/') {
			strcat(t, *p);
			strcat(t, "/");
			strcat(t, s);
			return(t);
		}
	}
	return(os);
}

char *
_fixf(s)
char *s;
{
	return(_dofix(s, fn1));
}

char *
_fixf2(s)
char *s;
{
	return(_dofix(s, fn2));
}


/*
 * save away filename
 */
char *
_savfile(s)
char *s;
{
	return(strcpy(sfn, s));
}
