static char *sccsid = "@(#)newsfuncs.c	1.1\t1/23/83";

/*
 * funcs - functions used by both inews and readnews.
 */

#include <sys/types.h>
#include <ctype.h>
#include "structs.h"
#include "newsgate.h"
#include "bnews.h"

extern char *SYSTEM;
char bfr[WDLEN];

/*
 * Trap interrupts.
 */
onsig(n)
int n;
{
	/*
	 * Most UNIX systems reset caught signals to SIG_DFL.
	 * This bad design requires that the trap be set again here.
	 * Unfortunately, if the signal recurs before the trap is set,
	 * the program will die, possibly leaving the lock in place.
	 */
	signal(n, onsig);
}

/*
 * Append NGDELIM to string.
 */
ngcat(s)
register char *s;
{
	if (*s) {
		while (*s++);
		s -= 2;
		if (*s++ == NGDELIM)
			return;
	}
	*s++ = NGDELIM;
	*s = '\0';
}

/*
 * News group matching.
 *
 * nglist is a list of newsgroups.
 * sublist is a list of subscriptions.
 * sublist may have "meta newsgroups" in it.
 * All fields are NGDELIM separated,
 * and there is an NGDELIM at the end of each argument.
 *
 * Currently implemented glitches:
 * sublist uses 'all' like shell uses '*', and '.' like shell '/'.
 * If subscription X matches Y, it also matches Y.anything.
 */
ngmatch(nglist, sublist)
register char *nglist, *sublist;
{
	register char *n, *s;
	register int rc;

	rc = FALSE;
	for (n = nglist; *n != '\0' && rc == FALSE;) {
		for (s = sublist; *s != '\0';) {
			if (*s != NEGCHAR)
				rc |= ptrncmp(s, n);
			else
				rc &= ~ptrncmp(s+1, n);
			while (*s++ != NGDELIM);
		}
		while (*n++ != NGDELIM);
	}
	return(rc);
}

/*
 * Compare two newsgroups for equality.
 * The first one may be a "meta" newsgroup.
 */
ptrncmp(ng1, ng2)
register char *ng1, *ng2;
{
	while (*ng1 != NGDELIM) {
		if (ng1[0]=='a' && ng1[1]=='l' && ng1[2]=='l') {
			ng1 += 3;
			while (*ng2 != NGDELIM && *ng2 != '.')
				if (ptrncmp(ng1, ng2++))
					return(TRUE);
			return (ptrncmp(ng1, ng2));
		} else if (*ng1++ != *ng2++)
			return(FALSE);
	}
	return (*ng2 == '.' || *ng2 == NGDELIM);
}

/*
 * Remove newsgroups in 'a' not subscribed to by 'b'.
 */
ngsquash(ap, bp)
register char *ap, *bp;
{
	register char *tp;
	char tbuf[WDLEN];

	/* replace NGDELIM by '\0' in a */
	for (tp = ap; *tp != '\0'; tp++)
		if (*tp == NGDELIM)
			*tp = '\0';
	/* ap = building, tp = checking. */
	tp = ap;
	while (*tp != '\0') {
		ngcat(strcpy(tbuf, tp));
		if (ngmatch(tbuf, bp)) {
			while ((*ap++ = *tp++) != '\0')
				;
			ap[-1] = NGDELIM;
		} else
			while (*tp++ != '\0');
	}
	*ap = '\0';
}


/*
 * Strip trailing newlines, blanks, and tabs from 's'.
 * Return TRUE if newline was found, else FALSE.
 */
nstrip(s)
register char *s;
{
	register char *p;
	register int rc;

	rc = FALSE;
	p = s;
	while (*p)
		if (*p++ == '\n')
			rc = TRUE;
	while (--p >= s && (*p == '\n' || *p == ' ' || *p == '\t'));
	*++p = '\0';
	return(rc);
}

/*
 * Delete trailing NGDELIM.
 */
ngdel(s)
register char *s;
{
	if (*s++) {
		while (*s++);
		s -= 2;
		if (*s == NGDELIM)
			*s = '\0';
	}
}

/*
 * Return the ptr in sp at which the character c appears;
 * NULL if not found
 *
 * These are the v7 index and rindex routines, stolen for portability.
 * (Some Unix systems call them strchr and strrchr, notably PWB 2.0
 * and its derivitives such as Unix/TS 2.0, Unix 3.0, etc.)  Others,
 * like v6, don't have them at all.
 */

char *
index(sp, c)
register char *sp, c;
{
	do {
		if (*sp == c)
			return(sp);
	} while (*sp++);
	return(NULL);
}

/*
 * Return the ptr in sp at which the character c last
 * appears; NULL if not found
 */

char *
rindex(sp, c)
register char *sp, c;
{
	register char *r;

	r = NULL;
	do {
		if (*sp == c)
			r = sp;
	} while (*sp++);
	return(r);
}
static	FILE	*sysfile;

char *fldget(q, p)
register char *q, *p;
{
	while (*p && *p != ':') {
		if (*p == '\\' && p[1]==':')
			p++;
		*q++ = *p++;
	}
	*q = '\0';
	return(p);
}

/*
 * Local open routine.
 */
FILE *
xfopen(name, mode)
register char *name, *mode;
{
	register FILE *fp;

	if ((fp = fopen(name, mode)) == NULL) {
		sprintf(bfr, "Cannot open %s (%s)", name, mode);
		xerror(bfr);
	}
	return(fp);
}

time_t
cgtdate(datestr)
char *datestr;
{
	time_t	i;
	char	junk[40],month[40],day[30],time[60],year[50];

	if ((i = getdate(datestr, (struct timeb *) NULL)) >= 0)
		return i;
	sscanf(datestr, "%s %s %s %s %s", junk, month, day, time, year);
	sprintf(bfr, "%s %s, %s %s", month, day, year, time);
	return getdate(bfr, (struct timeb *) NULL);
}

lcase(s)
register char *s;
{
	register char *ptr;

	for (ptr = s; *ptr; ptr++)
		if (isupper(*ptr))
			*ptr = tolower(*ptr);
}

ohwrite(hp, fp)
register struct hbuf *hp;
register FILE *fp;
{
	ngdel(strcpy(bfr, hp->nbuf));
	fprintf(fp, "A%s\n%s\n%s!%s\n%s\n%s\n", hp->ident, bfr, SYSTEM, hp->path, hp->subdate, hp->title);
}

static int hascaught = 0;
static catchintr()
{
	hascaught = 1;
	printf("\n");
	fflush(stdout);
}

xerror(message)
char *message;
{
	printf("expire: %s.\n", message);
	fflush(stdout);
	exit(1);
}
