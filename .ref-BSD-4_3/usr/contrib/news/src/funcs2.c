/*
 * This software is Copyright (c) 1985 by Rick Adams.
 *
 * Permission is hereby granted to copy, reproduce, redistribute or
 * otherwise use this software as long as: there is no monetary
 * profit gained specifically from the use or reproduction or this
 * software, it is not sold, rented, traded or otherwise marketed, and
 * this copyright notice is included prominently in any copy
 * made.
 *
 * The author make no claims as to the fitness or correctness of
 * this software for any use whatsoever, and it is provided as is. 
 * Any use of this software is at the user's own risk.
 *
 *
 * funcs2 - functions used by both inews and readnews.
 */

#ifdef SCCSID
static char	*SccsId = "@(#)funcs2.c	1.13	1/17/86";
#endif /* SCCSID */

#include "params.h"

#ifdef SunIII
#ifndef INTERNET
#define	INTERNET
#endif /* !INTERNET */
#endif /* SunIII */

/*LINTLIBRARY*/

/*
 * Get user name and home directory.
 */
getuser()
{
	static int flag = TRUE;
	register struct passwd *p;

	if (flag) {
		if ((p = getpwuid(uid)) == NULL)
			xerror("Cannot get user's name");
		if ( username == NULL || username[0] == 0)
			username = AllocCpy(p->pw_name);
		userhome = AllocCpy(p->pw_dir);
		flag = FALSE;
	}
	(void) strcpy(header.path, username);
}

static	FILE	*sysfile;

char *fldget();

static int sfline;

/*
 * Open SUBFILE.
 */
s_openr()
{
	sysfile = xfopen(SUBFILE, "r");
	sfline = 0;
}

/*
 * Read SUBFILE.
 */
s_read(sp)
register struct srec *sp;
{
	register char *p;
	register int  c;
	char *e;
	int chop_spaces = 0;
again:
	p = bfr;
        /*
         * Read  the  SUBFILE  (/usr/lib/news/sys)  from   the   current
	 * position  to  the  first  unescaped newline.  If a newline is
	 * escaped with a backslash (\) continue reading but throw  away
	 * the backslash and newline; read the next line skipping spaces
	 * and tabs until the first non-space/tab character, then  start
	 * looking   for   a   newline   again.   Skipping  the  leading
	 * spaces/tabs after a escaped newline  keeps  the  news  groups
	 * together.  If  a  line  begins  with a newline, just skip it.
	 */
	for (e=p+LBUFLEN; p < e && (c=getc(sysfile)) != EOF; p++) {
		*p = c;
		if (c == '\n') {
			sfline++;
			if (p == bfr || p[-1] != '\\') {
				p[1] = '\0';
				break;
			} else {
				chop_spaces++;
				p -= 2;
			}
		} else if (chop_spaces) {
			if (c == '\t' || c == ' ')
				p--;
			else
				chop_spaces = 0;
		}
	}
	if (c == EOF) {
		return FALSE;
	}
	p = bfr;
	if (*p == '\n')
		goto again;	     /* skip newlines */
	if (!nstrip(p))
		xerror("SUBFILE (%s) line %d too long.", SUBFILE, sfline);
	if (*p == '#')
		goto again;
	sp->s_xmit[0] = '\0';
	sp->s_flags[0] = '\0';
	sp->s_nosend = (char *)0;

	p = fldget(sp->s_name, p);
	if (*p++ == '\0')
		xerror("Bad SUBFILE (%s) line %d.", SUBFILE, sfline);
/*
 * A sys file line reading "ME" means the name of the local system.
 */
	if (strcmp(sp->s_name, "ME") == 0)
		(void) strcpy(sp->s_name, FULLSYSNAME);
	e = index(sp->s_name, '/');
	if (e) {
		*e++ = '\0';
		sp->s_nosend = e;
	}
	p = fldget(sp->s_nbuf, p);
	lcase(sp->s_nbuf);
	if (*p++ == '\0')
		return TRUE;

	p = fldget(sp->s_flags, p);
	if (*p++ == '\0')
		return TRUE;

	(void) fldget(sp->s_xmit, p);
	return TRUE;
}

char *
fldget(q, p)
register char *q, *p;
{
	while (*p && *p != ':') {
		if (*p == '\\' && p[1]==':')
			p++;
		*q++ = *p++;
	}
	*q = '\0';
	return p;
}

/*
 * Find the SUBFILE record for a system.
 */
s_find(sp, system)
register struct srec *sp;
char *system;
{
	s_openr();
	while (s_read(sp))
		if (strncmp(system, sp->s_name, SNLN) == 0) {
			s_close();
			return TRUE;
		}
	s_close();
	return FALSE;
}

/*
 * Close sysfile.
 */
s_close()
{
	(void) fclose(sysfile);
}

time_t
cgtdate(datestr)
char *datestr;
{
	char	junk[40],month[40],day[30],tod[60],year[50];
	static time_t lasttime;
	static char lastdatestr[BUFLEN] = "";

	if ( lastdatestr[0] && strcmp(datestr, lastdatestr) == 0)
		return lasttime;
	lasttime = getdate(datestr, (struct timeb *)NULL);
	if (lasttime < 0 &&
	  sscanf(datestr, "%s %s %s %s %s", junk, month, day, tod, year) == 5) {
		(void) sprintf(bfr, "%s %s, %s %s", month, day, year, tod);
		lasttime = getdate(bfr, (struct timeb *)NULL);
	}
	strncpy(lastdatestr, datestr, BUFLEN);
	return lasttime;
}

lcase(s)
register char *s;
{
	register char *ptr;

	for (ptr = s; *ptr; ptr++)
		if (isupper(*ptr))
			*ptr = tolower(*ptr);
}

/*
 * Return a compact representation of the person who posted the given
 * message.  A sender or internet name will be used, otherwise
 * the last part of the path is used preceded by an optional ".."
 */
char *
tailpath(hp)
struct hbuf *hp;
{
	char *p, *r;
	static char resultbuf[BUFLEN];
	char pathbuf[PATHLEN];
	char *malloc();

	/*
	 * This only happens for articles posted by old news software
	 * in non-internet format.
	 */
	resultbuf[0] = '\0';
	(void) strncpy(pathbuf, hp->path, PATHLEN);
	p = index(pathbuf, ' ');
	if (p)
		*p = '\0';	/* Chop off trailing " (name)" */
	r = rindex(pathbuf, '!');
	if (r == 0) {
		r = pathbuf;
	} else {
		while (r > pathbuf && *--r != '!')
			;
		if (r > pathbuf) {
			r++;
			(void) strcpy(resultbuf, "..!");
		}
	}
	(void) strcat(resultbuf, r);
	return resultbuf;
}

/*
 * arpadate is like ctime(3) except that the time is returned in
 * an acceptable ARPANET time format instead of ctime format.
 */
char *
arpadate(longtime)
time_t *longtime;
{
	register char *p, *q, *ud;
	register int i;
	static char b[40];
	extern struct tm *gmtime();
	extern char *asctime();

	/*  Get current time. This will be used resolve the timezone. */
	ud = asctime(gmtime(longtime));

	/*  Crack the UNIX date line in a singularly unoriginal way. */
	q = b;

#ifdef notdef
/* until every site installs the fix to getdate.y, the day
   of the week can cause time warps */
	p = &ud[0];		/* Mon */
	*q++ = *p++;
	*q++ = *p++;
	*q++ = *p++;
	*q++ = ','; *q++ = ' ';
#endif

	p = &ud[8];		/* 16 */
	if (*p == ' ')
		p++;
	else
		*q++ = *p++;
	*q++ = *p++; *q++ = ' ';

	p = &ud[4];		/* Sep */
	*q++ = *p++; *q++ = *p++; *q++ = *p++; *q++ = ' ';

	p = &ud[22];		/* 1979 */
	*q++ = *p++; *q++ = *p++; *q++ = ' ';

	p = &ud[11];		/* 01:03:52 */
	for (i = 8; i > 0; i--)
		*q++ = *p++;

	*q++ = ' ';
	*q++ = 'G';		/* GMT */
	*q++ = 'M';
	*q++ = 'T';
	*q = '\0';

	return b;
}

char *
replyname(hptr)
struct hbuf *hptr;
{
	register char *ptr;
	static char tbuf[PATHLEN];

	ptr = hptr->path;
	if (prefix(ptr, FULLSYSNAME) &&
		index(NETCHRS, ptr[strlen(FULLSYSNAME)]))
		ptr = index(ptr, '!') + 1;
#ifdef INTERNET
	if (hptr->from[0])
		ptr = hptr->from;
	if (hptr->replyto[0])
		ptr = hptr->replyto;
#endif
	(void) strcpy(tbuf, ptr);
	ptr = index(tbuf, '(');
	if (ptr) {
		while (ptr[-1] == ' ')
			ptr--;
		*ptr = 0;
	}
#ifdef	SunIII
	if (ptr = rindex(tbuf, '.')) {
		if (prefix(++ptr, "OZ")) {
			/* some people only allow it in lower case ... */
			strcpy(ptr, "oz");
			return tbuf;
		}
		if (prefix(ptr, "UUCP") || prefix(ptr, "ARPA") ||
		    prefix(ptr, "DEC") || prefix(ptr, "CSNET")) {
			strcat(tbuf, "@munnari.oz");	/* via sun to munnari */
			return tbuf;
		}
	}
	/*
	 * must(?) have come from a uucp site, lets look see if path passes
	 * through munnari, and if so delete the fake uucp path after that.
	 */
	for (ptr = tbuf ;; ptr++) {
		if (prefix(ptr, "munnari!")) {
			strcpy(tbuf, ptr+8);
			break;
		}
		ptr = index(ptr, '!');
		if (ptr == (char *)0)
			break;
	}
	/*
	 * now, just send the address we have left to munnari, and
	 * hope that something sensible will be done with it there.
	 * (This works in more cases than you'd think ...)
	 */
	strcat(tbuf, "@munnari.oz");
#else /* !SunIII */
#ifndef INTERNET
	/*
	 * Play games stripping off multiple berknet
	 * addresses (a!b!c:d:e => a!b!d:e) here.
	 */
	for (ptr=tbuf; *ptr; ptr++) {
		register char *ptr2;

		if (index(NETCHRS, *ptr) && *ptr == ':' &&
		    (ptr2=index(ptr+1, ':')))
			(void) strcpy(ptr, ptr2);
	}
#endif /* !INTERNET */
#endif /* SunIII */
	return tbuf;
}

#ifdef DBM
typedef struct {
	char *dptr;
	int dsize;
} datum;
#endif /* DBM */

/*
 * Given an article ID, find the line in the history file that mentions it.
 * Return the text of the line, or NULL if not found.  A pointer to a
 * static area is returned.
 */
char *
findhist(artid)
char *artid;
{
	static char lbuf[256];
	char oidbuf[BUFSIZ];
	FILE *hfp;
	register char *p;
#ifdef DBM
	datum lhs, rhs;
	datum fetch();
	long fpos; /* We have to use an explicit variable to insure alignment */
#else /* !DBM */
	char *histfile();
#endif /* !DBM */

	/* Try to understand old artid's as well.  Assume .UUCP domain. */
	if (artid[0] != '<') {
		p = index(artid, '.');
		if (p)
			*p++ = '\0';
		(void) sprintf(oidbuf, "<%s@%s.UUCP>", p, artid);
		if (p)
			*--p = '.';
	} else
		(void) strcpy(oidbuf, artid);
	lcase(oidbuf);
#ifdef DBM
	initdbm(ARTFILE);
	lhs.dptr = oidbuf;
	lhs.dsize = strlen(lhs.dptr) + 1;
	rhs = fetch(lhs);
	if (rhs.dptr == NULL)
		return NULL;
	hfp = xfopen(ARTFILE, "r");
	/* The bcopy is NECESSARY to insure alignment on some machines */
	bcopy(rhs.dptr, (char *)&fpos, sizeof (long));
	fseek(hfp, fpos, 0);
#else /* !DBM */
	hfp = xfopen(histfile(oidbuf), "r");
#endif /* !DBM */
	while (fgets(lbuf, BUFLEN, hfp) != NULL) {
		p = index(lbuf, '\t');
		if (p == NULL)
			p = index(lbuf, '\n');
		*p = 0;
		if (strcmp(lbuf, artid) == 0 || strcmp(lbuf, oidbuf) == 0) {
			(void) fclose(hfp);
			*p = '\t';
			*(lbuf + strlen(lbuf) - 1) = 0;	/* zap the \n */
			return lbuf;
		}
#ifdef DBM
		break;
#endif /* DBM */
	}
	(void) fclose(hfp);
	return NULL;
}

/*
 * Hunt up the article "artid", and return the newsgroup/artnum
 * where it can be found.
 */
char *
findfname(artid)
char *artid;
{
	char *line, *p, *q;
	char *findhist();
	static char fname[BUFLEN];

	line = findhist(artid);
	if (line) {
		/* Look for it stored as an article, where it should be */
		p = index(line, '\t');
		p = index(p+1, '\t');
		p++;
		if (*p) {
			q = index(p, ' ');
			if (q)
				*q = 0;
			(void) strcpy(fname, p);
			return fname;
		}
	}
	return NULL;
}

/*
 * Hunt up the article "artid", fopen it for read, and return a
 * file descriptor to it.  We look everywhere we can think of.
 */
FILE *
hfopen(artid)
char *artid;
{
	char *p;
	char *findhist();
	FILE *rv = NULL;
	char fname[BUFLEN];

	p = findfname(artid);
	if (p) {
		(void) strcpy(fname, dirname(p));
		rv = fopen(fname, "r");	/* NOT xfopen! */
		if (rv == NULL)
			xerror("Cannot hfopen article %s", artid);
	}
	return rv;
}

#ifdef DBM
/*
** Avoid problems of multiple dbminit calls.
*/
initdbm(name)
char *name;
{
	static int called = 0;

	if (called != 0)
		return;
	called = 1;
	(void) dbminit(name);
}
#endif

#ifndef BSD4_2
/*
 * move n bytes from a to b
 */
bcopy(a, b, n)
register char *a, *b;
register n;
{
	while (--n >= 0)
		*b++ = *a++;
}
#endif

#if !defined(BSD4_2) && !defined(BSD4_1C)
rename(from,to)
register char *from, *to;
{
	(void) unlink(to);
	if (link(from, to) < 0)
		return -1;

	(void) unlink(from);
	return 0;
}
#endif /* !BSD4_2 && ! BSD4_1C */

#ifndef DBM
/*
** Generate the appropriate history subfile name
*/
char *
histfile(hline)
char *hline;
{
	char *p;
	char chr;	/* least significant digit of article number */
	static char subfile[BUFLEN];

	p = strchr(hline, '@');
	if (p != NULL && p > hline)
		chr = *(p - 1);
	else
		chr = '0';
	if (!isdigit(chr))
		chr = '0';
	sprintf(subfile, "%s.d/%c", ARTFILE, chr);
	if (access(subfile, 04) < 0)
		return(ARTFILE);
	return(subfile);
}
#endif /* !DBM */
