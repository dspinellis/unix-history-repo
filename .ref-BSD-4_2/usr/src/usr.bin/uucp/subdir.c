#ifndef lint
static char sccsid[] = "@(#)subdir.c	5.1 (Berkeley) 7/2/83";
#endif

#include "uucp.h"
#ifdef	UUDIR
/*
 * By Tom Truscott, March 1983
 * THIS VERSION OF SYSKLUDGE IS FOR USE ONLY
 * WITH THE 'UUDIR' VERSION OF UUCP.
 *
 * There once was a separate 'uudir' package to retrofit
 * versions of uucp, but that is no longer recommended.
 *
 * Prefix table.
 * If a prefix is "abc", for example,
 * then any file Spool/abc... is mapped to Spool/abc/abc... .
 * The first prefix found is used, so D.foo should preceed D. in table.
 *
 * Each prefix must be a subdirectory of Spool, owned by uucp!
 * Remember: use cron to uuclean these directories daily,
 * and check them manual every now and then.  Beware complacency!
 */

static char *prefix[] = {
	DLocalX,	/* Outbound 'xqt' request files */
	DLocal,		/* Outbound data files */
	"D.",		/* Other "D." files (remember the "."!) */
	"C.",		/* "C." subdirectory */
	"X.",		/* "X." subdirectory */
	"TM.",		/* Temporaries for inbound files */
	0
};

/*
 * filename mapping kludges to put uucp work files in other directories.
 */

#define	BUFLEN	50
/* assert(strlen(Spool)+1+14+1+14 <= BUFLEN) */

static	char fn1[BUFLEN], fn2[BUFLEN];	/* remapped filename areas */
static	int	inspool;		/* true iff working dir is Spool */

/*
 * return (possibly) remapped string s
 */
char *
SubFile(as)
char *as;
{
	register char *s, **p;
	register int n;
	static char *tptr = NULL;

	/* Alternate buffers so "link(subfile(a), subfile(b))" works */
	if (tptr != fn1)
		tptr = fn1;
	else
		tptr = fn2;

	s = as;
	tptr[0] = '\0';

	/* if s begins with Spool/, copy that to tptr and advance s */
	if (strncmp(s, Spool, n = strlen(Spool)) == 0 && s[n] == '/') {
		if (!inspool) {
			strcpy(tptr, Spool);
			strcat(tptr, "/");
		}
		s += n + 1;
	}
	else
		if (!inspool)
			return(as);

	/* look for first prefix which matches, and make subdirectory */
	for (p = &prefix[0]; *p; p++) {
		if (strncmp(s, *p, n = strlen(*p))==0 && s[n] && s[n] != '/') {
			strcat(tptr, *p);
			strcat(tptr, "/");
			strcat(tptr, s);
			return(tptr);
		}
	}
	return(as);
}

/*
 * save away filename
 */
SubChDir(s)
register char *s;
{
	inspool = (strcmp(s, Spool) == 0);
	return(chdir(s));
}

/*
 * return possibly corrected directory for searching
 */
char *
SubDir(d, pre)
register char *d, pre;
{
	if (strcmp(d, Spool) == 0)
		if (pre == CMDPRE)
			return("/usr/spool/uucp/C.");
		else if (pre == XQTPRE)
			return("/usr/spool/uucp/X.");
	return(d);
}
#else
static int subdir_here;		/* quiet 'ranlib' command */
#endif
