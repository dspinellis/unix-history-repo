/*
 * funcs - functions used by both inews and readnews.
 */

static char *SccsId = "@(#)funcs.c	2.10	6/24/83";

#include "params.h"

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
	char tbuf[BUFLEN];

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
 * Exec the shell.
 * This version resets uid, gid, and umask.
 * Called with fsubr(ushell, s, NULL)
 */
/* ARGSUSED */
ushell(s, dummy)
char *s, *dummy;
{
	umask(savmask);
	setgid(gid);
	setuid(uid);
	xshell(s);
}

/*
 * Exec the shell.
 * This version restricts PATH to bin and /usr/bin.
 * Called with fsubr(pshell, s, NULL)
 */
extern char	**environ;

/* ARGSUSED */
pshell(s, dummy)
char *s, *dummy;
{
	static char *penv[] = { SYSPATH, NULL };
	register char **ep, *p;
	register int found;

	found = FALSE;
	for (ep = environ; p = *ep; ep++) {
		if (strncmp(p, "PATH=", 5) == 0) {
			*ep = penv[0];
			found = TRUE;
		}
	}
	if (!found)
		environ = &penv[0];
	xshell(s);
}

/*
 * Exec the shell.
 */
xshell(s)
char *s;
{
	execl(SHELL, SHELL, "-c", s, 0);
	xerror("No shell!");
}

/*
 * Fork and call a subroutine with two args.
 * Return pid without waiting.
 */
fsubr(f, s1, s2)
int (*f)();
char *s1, *s2;
{
	register int pid;

	while ((pid = fork()) == -1)
		sleep(1);
	if (pid == 0) {
		(*f)(s1, s2);
		exit(0);
	}
	return(pid);
}

/*
 * Wait on a child process.
 */
fwait(pid)
register int pid;
{
	register int w;
	int status;
	void (*onhup)(), (*onint)();

	onint = (void (*)()) signal(SIGINT, SIG_IGN);
	onhup = (void (*)()) signal(SIGHUP, SIG_IGN);
	while ((w = wait(&status)) != pid && w != -1)
		;
	if (w == -1)
		status = -1;
	signal(SIGINT, onint);
	signal(SIGHUP, onhup);
	return(status);
}

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
		if (username[0] == 0)
			strcpy(username, p->pw_name);
		strcpy(userhome, p->pw_dir);
		flag = FALSE;
	}
	strcpy(header.path, username);
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

char *fldget();

/*
 * Open SUBFILE.
 */
s_openr()
{
	sysfile = xfopen(SUBFILE, "r");
}

/*
 * Read SUBFILE.
 */
s_read(sp)
register struct srec *sp;
{
	register char *p;
again:
	p = bfr;
	if (fgets(p, LBUFLEN, sysfile) == NULL)
		return(FALSE);
	if (!nstrip(p))
		xerror("SUBFILE line too long.");
	if (*p == '#')
		goto again;
	sp->s_xmit[0] = '\0';
	sp->s_flags[0] = '\0';

	p = fldget(sp->s_name, p);
	if (*p++ == '\0')
		xerror("Bad SUBFILE line.");
/*
 * A sys file line reading "ME" means the name of the local system.
 */
	if (strcmp(sp->s_name, "ME") == 0)
		strcpy(sp->s_name, FULLSYSNAME);
	p = fldget(sp->s_nbuf, p);
	lcase(sp->s_nbuf);
	ngcat(sp->s_nbuf);
	if (*p++ == '\0')
		return(TRUE);

	p = fldget(sp->s_flags, p);
	if (*p++ == '\0')
		return(TRUE);

	fldget(sp->s_xmit, p);
	return(TRUE);
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
	return(p);
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
			return(TRUE);
		}
	s_close();
	return(FALSE);
}

/*
 * Close sysfile.
 */
s_close()
{
	fclose(sysfile);
}

/*
 * Local open routine.
 */
FILE *
xfopen(name, mode)
register char *name, *mode;
{
	register FILE *fp;
	char	*fname;

	if ((fp = fopen(name, mode)) == NULL) {
		fname = rindex(name, '/');
		/*
		 * IHCC users only see the "filename" that was in trouble, not the
		 * whole path.  (for security!)
		 */
#ifdef IHCC
		sprintf(bfr, "Cannot open %s (%s)", ++fname, mode);
#else
		sprintf(bfr, "Cannot open %s (%s)", name, mode);
#endif
		xerror(bfr);
	}
	/* kludge for setuid not being honored for root */
	if ((uid == 0) && (duid != 0) && ((mode == "a") || (mode == "w")))
		chown(name, duid, dgid);
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
	fprintf(fp, "A%s\n%s\n%s!%s\n%s\n%s\n", hp->oident, bfr, FULLSYSNAME, hp->path, hp->subdate, hp->title);
}

static int hascaught = 0;
static catchintr()
{
	hascaught = 1;
	printf("\n");
	fflush(stdout);
}

/*
 * Print a recorded message warning the poor luser what he is doing
 * and demand that he understands it before proceeding.  Only do
 * this for newsgroups listed in LIBDIR/recording.
 */
recording(ngrps)
char *ngrps;
{
	char recbuf[100];
	FILE *fd;
	char nglist[100], fname[100];
	char lngrps[100];
	char *oldsig;
	int  c, n, yes;

	sprintf(recbuf, "%s/%s", LIB, "recording");
	fd = fopen(recbuf, "r");
	if (fd == NULL)
		return 0;
	strcpy(lngrps, ngrps);
	ngcat(lngrps);
	while ((fgets(recbuf, sizeof recbuf, fd)) != NULL) {
		sscanf(recbuf, "%s %s", nglist, fname);
		ngcat(nglist);
		if (ngmatch(lngrps, nglist)) {
			fclose(fd);
			if (fname[0] == '/')
				strcpy(recbuf, fname);
			else
				sprintf(recbuf, "%s/%s", LIB, fname);
			fd = fopen(recbuf, "r");
			if (fd == NULL)
				return 0;
			while ((c = getc(fd)) != EOF)
				putc(c, stderr);
			hascaught = 0;
			oldsig = (char *) signal(SIGINT, catchintr);
			fprintf(stderr, "Do you understand this?  Hit <return> to proceed, <BREAK> to abort: ");
			n = read(2, recbuf, 100);
			c = recbuf[0];
			yes = (c=='y' || c=='Y' || c=='\n' || c=='\n' || c==0);
			signal(SIGINT, oldsig);
			if (hascaught || n <= 0 || !yes)
				return -1;
		}
	}
	return 0;
}

/*
 * Return a compact representation of the person who posted the given
 * message.  A sender or internet name will be used, otherwise
 * the last part of the path is used preceeded by an optional ".."
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
	strcpy(pathbuf, hp->path);
	p = index(pathbuf, ' ');
	if (p)
		*p = '\0';	/* Chop off trailing " (name)" */
	r = rindex(pathbuf, '!');
	if (r == 0) {
		r = pathbuf;
	}
	else {
		while (r > pathbuf && *--r != '!')
			;
		if (r > pathbuf) {
			r++;
			strcpy(resultbuf, "..!");
		}
	}
	strcat(resultbuf, r);
	return resultbuf;
}

/*
 * Generate the name of the person responsible for posting this article,
 * in order to check that two articles were posted by the same person.
 */
char *
senderof(hp)
struct hbuf *hp;
{
	char *q, *tp;

	if (hp->sender[0])
		tp = hp->sender;
	else if (hp->from[0])
		tp = hp->from;
	else
		tp = tailpath(hp);
	
	/* Remove full name */
	q = index(tp, ' ');
	if (q)
		*q = '\0';

	q = malloc(strlen(tp) + 1);
	strcpy(q, tp);
	return q;
}

/*
 * Returns 1 iff addr looks like a valid internet address
 * (as opposed to a routing path).
 * The current check insists on *@*.* as a format.
 */
goodinternet(addr)
register char *addr;
{
	register char *at, *dot;

	at = index(addr, '@');
	if (at == NULL)
		return 0;
	dot = index(at, '.');
	if (dot == NULL)
		return 0;
	/*
	 * A more thorough check would insist on only alphanumerics
	 * and dots to the right of the @.
	 */
	return 1;
}

rwaccess(fname)
char *fname;
{
	int fd;

	fd = open(fname, 2);
	if (fd < 0)
		return 0;
	close(fd);
	return 1;
}

exists(fname)
char *fname;
{
	int fd;

	fd = open(fname, 0);
	if (fd < 0)
		return 0;
	close(fd);
	return 1;
}

prefix(full, pref)
register char *full, *pref;
{
	while (*full++ == *pref++)
		;
	if (*--pref == 0)
		return 1;
	else
		return 0;
}

char *
dirname(ngname)
char *ngname;
{
	static char rbuf[100];
	register char *p;

	sprintf(rbuf, "%s/%s", SPOOL, ngname);
#ifdef UPWARDCOMPAT
	/* First check the old style name. */
	if (exists(rbuf))
		return rbuf;
#endif

	/* Use the new style name for all new stuff. */
	for (p=rbuf+strlen(SPOOL); *p; p++)
		if (*p == '.')
			*p = '/';
	return rbuf;
}

#ifdef notdef
char *
dotname(ngname)
char *ngname;
{
	static char rbuf[100];
	register char *p;

#ifdef UPWARDCOMPAT
	/* First check the old style name. */
	sprintf(rbuf, "%s/.%s", SPOOL, ngname);
	if (exists(rbuf))
		return rbuf;
#endif

	/* Use the new style name for all new stuff. */
	sprintf(rbuf, "%s/%s", SPOOL, ngname);
	for (p=rbuf+strlen(SPOOL); *p; p++)
		if (*p == '.')
			*p = '/';
	strcat(rbuf, "/bounds");
	return rbuf;
}
#endif

/*
 * Return TRUE iff ngname is a valid newsgroup name, active
 * or inactive.
 */
validng(ngname)
char *ngname;
{
	return exists(dirname(ngname));
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
	char *cp;
	register int i;
	static char b[40];
	struct timeb t;
	extern struct tm *localtime();
	extern char *ctime();
	extern struct timeb *ftime();
#ifdef USG
	struct tm *bp;
	extern char *tzname[];
#else
	extern char *timezone();
#endif

	/*  Get current time. This will be used resolve the timezone. */
	ud = ctime(longtime);
	ftime(&t);

	/*  Crack the UNIX date line in a singularly unoriginal way. */
	q = b;

	p = &ud[0];		/* Mon */
	*q++ = *p++;
	*q++ = *p++;
	*q++ = *p++;
	*q++ = ','; *q++ = ' ';

	p = &ud[8];		/* 16 */
	if (*p == ' ')
		p++;
	else
		*q++ = *p++;
	*q++ = *p++; *q++ = '-';

	p = &ud[4];		/* Sep */
	*q++ = *p++; *q++ = *p++; *q++ = *p++; *q++ = '-';

	p = &ud[22];		/* 1979 */
	*q++ = *p++; *q++ = *p++; *q++ = ' ';

	p = &ud[11];		/* 01:03:52 */
	for (i = 8; i > 0; i--)
		*q++ = *p++;

				/* -PST or -PDT */
#ifdef USG
	bp = localtime(&t.time);
	p = tzname[bp->tm_isdst];
#else
	p = timezone(t.timezone, localtime(&t.time)->tm_isdst);
#endif
	if (p[3] != '\0') {
		/* hours from GMT */
		p += 3;
		*q++ = *p++;
		if (p[1] == ':')
			*q++ = '0';
		else
			*q++ = *p++;
		*q++ = *p++; p++; *q++ = *p++; *q++ = *p++;
	} else {
		*q++ = ' '; *q++ = *p++; *q++ = *p++; *q++ = *p++;
	}
	*q = '\0';

	return (b);
}

char *
replyname(hptr)
struct hbuf *hptr;
{
	register char *ptr;
	static char tbuf[PATHLEN];

	ptr = hptr->path;
	if (prefix(ptr, FULLSYSNAME))
		ptr = index(ptr, '!') + 1;
#ifdef INTERNET
	if (hptr->from[0])
		ptr = hptr->from;
	if (hptr->replyto[0])
		ptr = hptr->replyto;
#endif
	strcpy(tbuf, ptr);
	ptr = index(tbuf, '(');
	if (ptr) {
		while (ptr[-1] == ' ')
			ptr--;
		*ptr = 0;
	}
#ifndef INTERNET
	/*
	 * Play games stripping off multiple berknet
	 * addresses (a!b!c:d:e => a!b!d:e) here.
	 */
	for (ptr=tbuf; *ptr; ptr++)
		if (index(NETCHRS, *ptr) && *ptr == ':' && index(ptr+1, ':'))
			strcpy(ptr, index(ptr+1, ':'));
#endif
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
	char *p;

	/* Try to understand old artid's as well.  Assume .UUCP domain. */
	if (artid[0] != '<') {
		p = index(artid, '.');
		if (p)
			*p++ = '\0';
		sprintf(oidbuf, "<%s@%s.UUCP>", p, artid);
		if (p)
			*--p = '.';
	} else
		strcpy(oidbuf, artid);
	hfp = xfopen(ARTFILE, "r");
	while (fgets(lbuf, BUFLEN, hfp) != NULL) {
		p = index(lbuf, '\t');
		if (p == NULL)
			p = index(lbuf, '\n');
		*p = 0;
		if (strcmp(lbuf, artid) == 0 || strcmp(lbuf, oidbuf) == 0) {
			fclose(hfp);
			*p = '\t';
			*(lbuf + strlen(lbuf) - 1) = 0;	/* zap the \n */
			return(lbuf);
		}
	}
	fclose(hfp);
	return(NULL);
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
	FILE *rv;
	static char fname[256];

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
			strcpy(fname, p);
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
	char fname[256];

	p = findfname(artid);
	if (p) {
		strcpy(fname, dirname(p));
		rv = fopen(fname, "r");	/* NOT xfopen! */
		if (rv != NULL)
			return rv;
	}

	xerror("Cannot hfopen article %s", artid);
}
