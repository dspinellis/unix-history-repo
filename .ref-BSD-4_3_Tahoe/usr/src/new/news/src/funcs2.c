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
static char	*SccsId = "@(#)funcs2.c	1.22	10/15/87";
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
		if ( username == NULL || username[0] == 0 ||
			STRCMP(username, "Unknown") == 0)
			username = AllocCpy(p->pw_name);
		userhome = AllocCpy(p->pw_dir);
		flag = FALSE;
	}
	(void) strcpy(header.path, username);
}

/* no sys file on clients via nntp */
#ifndef SERVER
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
	while (*p == ' ' || *p == '\t') /* skip leading white space */
		p++;
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
	if (STRCMP(sp->s_name, "ME") == 0)
		(void) strcpy(sp->s_name, LOCALPATHSYSNAME);
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
		if (STRNCMP(system, sp->s_name, SNLN) == 0) {
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
#endif /* SERVER */

extern struct timeb Now;

time_t
cgtdate(datestr)
char *datestr;
{
	char	junk[40],month[40],day[30],tod[60],year[50];
	static time_t lasttime;
	static char lastdatestr[BUFLEN] = "";

	if ( lastdatestr[0] && STRCMP(datestr, lastdatestr) == 0)
		return lasttime;
	lasttime = getdate(datestr, &Now);
	if (lasttime < 0 &&
	  sscanf(datestr, "%s %s %s %s %s", junk, month, day, tod, year) == 5) {
		(void) sprintf(bfr, "%s %s, %s %s", month, day, year, tod);
		lasttime = getdate(bfr, &Now);
		if (lasttime < 0) {
			logerr("Unparsable date \"%s\"", datestr);
			datestr = "now";	/* better than nothing */
			lasttime = Now.time;
		}
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
	if (PREFIX(ptr, PATHSYSNAME) &&
		index(NETCHRS, ptr[strlen(PATHSYSNAME)]))
		ptr = index(ptr, '!') + 1;
#ifdef INTERNET
	if (hptr->from[0])
		ptr = hptr->from;
	if (hptr->replyto[0])
		ptr = hptr->replyto;
#else /* !INTERNET */
	if (hptr->replyto[0] && !index(hptr->replyto, '@'))
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
		if (PREFIX(++ptr, "OZ")) {
			/* some people only allow it in lower case ... */
			strcpy(ptr, "oz");
			return tbuf;
		}
		if (PREFIX(ptr, "UUCP") || PREFIX(ptr, "ARPA") ||
		    PREFIX(ptr, "DEC") || PREFIX(ptr, "CSNET")) {
			strcat(tbuf, "@munnari.oz");	/* via sun to munnari */
			return tbuf;
		}
	}
	/*
	 * must(?) have come from a uucp site, lets look see if path passes
	 * through munnari, and if so delete the fake uucp path after that.
	 */
	for (ptr = tbuf ;; ptr++) {
		if (PREFIX(ptr, "munnari!")) {
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
#else	/* INTERNET */
	{
	char mbuf[BUFLEN], modadd[BUFLEN];
	FILE *mfd;
	/* Let's find a path to the backbone */
	sprintf(mbuf, "%s/mailpaths", LIB);
	mfd = xfopen(mbuf, "r");
	do {
		if (fgets(mbuf, sizeof mbuf, mfd) == NULL)
			xerror("Can't find internet in %s/mailpaths",
				LIB);
	} while (!PREFIX(mbuf, "internet"));
	if (sscanf(mbuf, "%*s %s", modadd) != 1)
		xerror("backbone address corrupted");
	(void) fclose(mfd);
	(void)strcpy(mbuf, tbuf);
	/* If we are lucky, there is no ! or @ in the forward address */
	if (strpbrk(modadd, "!@") == NULL) {
		sprintf(tbuf, modadd, mbuf);
	} else {
		char *cp = index(mbuf, '@');
		if (index(modadd, '@') == NULL && cp) {
			/* we have to rearrange the address so no @ are in it */
			char atbuf[BUFLEN];
			*cp++ = '\0';
			sprintf(atbuf, "%s!%s", cp, mbuf);
			sprintf(tbuf, modadd, atbuf);
		} else if (cp) {
			/* some days you don't get lucky. presume the % hack */
			*cp = '%';
			sprintf(tbuf, modadd, mbuf);
		}
	}
	}
#endif /* INTERNET */
#endif /* !SunIII */
	return tbuf;
}


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
#ifdef SERVER
	char workspace[256];
	struct tm *tm;
	long clock;
#else /* !SERVER */
#ifdef DBM
	datum lhs, rhs;
	datum fetch();
	long fpos; /* We have to use an explicit variable to insure alignment */
#else /* !DBM */
	char *histfile();
#endif /* !DBM */
#endif /* !SERVER */
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
#ifdef SERVER
	(void) sprintf(lbuf,"STAT %s",oidbuf);
	put_server(lbuf);
	(void) get_server(workspace,sizeof(workspace));
	if (*workspace != CHAR_OK)
		return NULL;
	(void) sprintf(lbuf,"XHDR xref %s",oidbuf);
	put_server(lbuf);
	(void) get_server(workspace,sizeof(workspace));	/* get response */
	if (*workspace != CHAR_OK)
		return NULL;		/* old style nntp */
	(void) get_server(workspace,sizeof(workspace)); /* get header line */
	sync_server();	/* get rid of the rest of it */
	p = index(workspace,' ');
	p++;

	if (*p == '(') {	/* there is no xref line */
		long s,sm;
		FILE * af;
		char n[100], buf[100], *name;
		(void) sprintf(lbuf,"XHDR newsgroups %s",oidbuf);
		put_server(lbuf);
		(void) get_server(workspace,sizeof(workspace));
		if (*workspace != CHAR_OK)
			return NULL;
		(void) get_server(workspace,sizeof(workspace));
		sync_server();
		if ((name = index(workspace,' ')) == NULL)
			return NULL;
		name++;
		/* now we fetch the line from the active file */
		af = xfopen(ACTIVE, "r");
		while (fgets(buf, sizeof(buf), af) != NULL) {
			if (sscanf(buf, "%s %ld %ld", n, &s, &sm) == 3 &&
			     STRCMP(n, name) == 0) {
				break;
			}
		}
		(void) fclose(af);
		/* now we ask for a message ids in that newsgroup */
		if (set_group(name) == NULL)
			return NULL;
		(void) sprintf(lbuf, "XHDR message-id %d-%d", sm, s);
		put_server(lbuf);
		(void) get_server(workspace,sizeof(workspace));
		if (*workspace != CHAR_OK)
			return NULL;
		while (	get_server(workspace,sizeof(workspace)) >= 0) {
			if (*workspace == '.'  && strlen(workspace) == 1) 
				return NULL;
			if (strindex(workspace,oidbuf) > -1)
				break;
		}
		sync_server();
		*(index(workspace,' ')) = '\0';
		(void) sprintf(lbuf, "%s/%s", n, workspace);
		bzero(workspace,sizeof(workspace));
		strcpy(workspace, lbuf);
	} else {
		bzero(lbuf, sizeof(lbuf));
		strcpy(lbuf, p);
		while (*p != '\0' && (p = index(lbuf,':')) != NULL) {
			*p = '/';
			p++;
		}
		strcpy(workspace, lbuf);
	}
	p = &workspace[0];
	time(&clock);		
	tm = localtime(&clock);
#ifdef USG
	sprintf(lbuf, "%s\t%2.2d/%2.2d/%d %2.2d:%2.2d\t%s",
#else /* !USG */
	sprintf(lbuf, "%s\t%02d/%02d/%d %02d:%02d\t%s",
#endif /* !USG */
	oidbuf,tm->tm_mon,tm->tm_mday,tm->tm_year,tm->tm_hour,tm->tm_min,p);
	return lbuf;		/* not really the same, but close */
#else	/* !SERVER */
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
		if (STRCMP(lbuf, artid) == 0 || STRCMP(lbuf, oidbuf) == 0) {
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
#endif	/* !SERVER */
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
#ifdef SERVER
	if ((rv = getartbyid(p)) != NULL) {
		strcpy(fname, article_name());
		(void) fclose(rv);
		rv = NULL;
	}
	else
		xerror("Cannot hfopen article %s", artid);
#else 	/* !SERVER */
		(void) strcpy(fname, dirname(p));
#endif	/* !SERVER */
		rv = fopen(fname, "r");	/* NOT xfopen! */
		if (rv == NULL)
			xerror("Cannot hfopen article %s", artid);
	}
#ifdef SERVER
	(void) unlink(fname);
#endif /* !SERVER */
	return rv;
}
#ifndef SERVER
# ifdef DBM
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
# endif /* DBM */
#endif	/* !SERVER */

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

#if !defined(BSD4_2)
rename(from,to)
register char *from, *to;
{
	(void) unlink(to);
	if (link(from, to) < 0)
		return -1;

	(void) unlink(from);
	return 0;
}
#endif /* !BSD4_2 */

#ifndef DBM
/*
** Generate the appropriate history subfile name
*/
char *
histfile(hline)
char *hline;
{
	char chr;	/* least significant digit of article number */
	static char subfile[BUFLEN];

	chr = findhfdigit(hline);
	sprintf(subfile, "%s.d/%c", ARTFILE, chr);
	return subfile;
}

findhfdigit(fn)
char *fn;
{
	register char *p;
	register int chr;

	p = index(fn, '@');
	if (p != NULL && p > fn)
		chr = *(p - 1);
	else
		chr = '0';
	if (!isdigit(chr))
		chr = '0';
	return chr;
}
#endif /* !DBM */

#ifdef VMS
/*
 * These functions open an article with one level of indirection,
 * to support symbolic links. xart_open exits if the open fails.
 */
FILE *
xart_open (filename,mode)
char *filename,*mode;
{
	FILE *fp = art_open (filename, mode);
	extern int errno;
	if (fp == NULL)
		xerror("Cannot open article %s (%s): %s\n",
			 filename, mode, errmsg(errno));
	return fp;
}

FILE *
art_open (filename,mode)
char *filename,*mode;
{
	char linkfile[BUFSIZ];
	FILE *fp;

	if ((fp = fopen (filename, mode)) == NULL)
		return NULL;
	if (fgets (linkfile, BUFSIZ, fp) == NULL || linkfile[0] != '/') {
		rewind (fp);
		return fp;
	}
/* Chase the symbolic link. */
	(void) fclose (fp);
	if ((fp = fopen (linkfile, mode)) == NULL)
/* Clean up dangling link, if we have the power. Ignore error if we don't. */
		(void) unlink (filename);
	return fp;
}
#endif /* VMS */

/*
 * Generate the name of the person responsible for posting this article,
 * in order to check that two articles were posted by the same person.
 */
char *
senderof(hp)
struct hbuf *hp;
{
	register char *q, *tp;
	char *tailpath();
	static char senderbuf[BUFLEN];

	if (hp->sender[0])
		tp = hp->sender;
	else if (hp->from[0])
		tp = hp->from;
	else
		tp = tailpath(hp);

	(void) strncpy(senderbuf, tp, BUFLEN);
	/* Remove full name */
	q = index(senderbuf, ' ');
	if (q)
		*q = '\0';

	return senderbuf;
}
