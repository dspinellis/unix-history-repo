/*
 * ifuncs - functions used by inews.
 */

static char *SccsId = "@(#)ifuncs.c	2.21	3/31/83";

#include "iparams.h"

/*
 * Transmit this article to all interested systems.
 */

#ifdef u370
static struct srec srec;
static struct hbuf h;
#endif

broadcast()
{
	register char *nptr, *hptr;
	register FILE *fp;
#ifndef u370
	struct srec srec;
	struct hbuf h;
#endif

	/* h is a local copy of the header we can scribble on */
	fp = xfopen(ARTICLE, "r");
	if (hread(&h, fp, TRUE) == NULL)
		xerror("Cannot reread article");
	fclose(fp);
	if (h.distribution[0])
		strcpy(h.nbuf, h.distribution);
	ngcat(h.nbuf);

	 /* break path into list of systems. */
	hptr = nptr = h.path;
	while (*hptr != '\0') {
		if (index(NETCHRS, *hptr)) {
			*hptr++ = '\0';
			nptr = hptr;
		} else
			hptr++;
	}
	*nptr = '\0';

	/* loop once per system. */
	lock();
	s_openr();
	while (s_read(&srec)) {
		if (strncmp(srec.s_name, FULLSYSNAME, SNLN) == 0)
			continue;
		hptr = h.path;
		while (*hptr != '\0') {
			if (strncmp(srec.s_name, hptr, SNLN) == 0)
				goto contin;
			while (*hptr++ != '\0')
				;
		}
		if (ngmatch(h.nbuf, srec.s_nbuf)) {
			transmit(&srec, xfopen(ARTICLE, "r"), 1);
		}
	contin:;
	}
	s_close();
	unlock();
}

/*
 * Transmit file to system.
 */
#define PROC 0004
transmit(sp, ifp, maynotify)
register struct srec *sp;
register FILE *ifp;
int maynotify;
{
	register FILE *ofp;
	register int c;
	register char *ptr;
	struct hbuf hh;
	char TRANS[BUFLEN];
	char *argv[20];
	register int pid, fd;
	extern char firstbufname[];

/* A:	afmt: the other machine runs an A news, so we xmit in A format */
	int afmt = (index(sp->s_flags, 'A') != NULL);
/* B:	use B format (this is the default - don't use this letter elsewise). */
/* F:	append name to file */
	int appfile = (index(sp->s_flags, 'F') != NULL);
/* L:	local: don't send the article unless it was generated locally */
	int local = (index(sp->s_flags, 'L') != NULL);
/* N:	notify: don't send the article, just tell him we have it */
	int notify = maynotify && (index(sp->s_flags, 'N') != NULL);
/* S:	noshell: don't fork a shell to execute the xmit command */
	int noshell = (index(sp->s_flags, 'S') != NULL);
/* U:	useexist: use the -c option to uux to use the existing copy */
	int useexist = (index(sp->s_flags, 'U') != NULL);

	if (local && mode == PROC)
		return;
#ifdef DEBUG
	printf("Transmitting to '%s'\n", sp->s_name);
#endif
	if (!appfile && !useexist) {
		if (hread(&hh, ifp, TRUE) == NULL) {
			fprintf(stderr, "Bad header, not transmitting\n");
			log("Bad header, not transmitting %s re %s to %s",
				hh.ident, hh.title, sp->s_name);
			return;
		}
		/* Taken out for obscure reasons - see the standard.
		ngsquash(hh.nbuf, sp->s_nbuf);
		*/
		if (hh.nbuf[0] == '\0') {
			printf("Article not subscribed to by %s\n", sp->s_name);
			return;
		}
		sprintf(TRANS, "%s/trXXXXXX", SPOOL);
	}

	if (notify) {
		char oldid[50];
		sprintf(hh.title, "ihave %s %s", hh.ident, FULLSYSNAME);
		sprintf(hh.nbuf, "to.%s.ctl", sp->s_name);
		strcpy(oldid, hh.ident);
		getident(&hh);
		log("tell %s about %s, notif. id %s",
			sp->s_name, oldid, hh.ident);
	} else
		log("xmit article %s to %s",
			hh.ident, sp->s_name);

	if (appfile) {
		if (firstbufname[0] == '\0')
			xerror("No file name to xmit from");
		ofp = fopen(sp->s_xmit, "a");
		if (ofp == NULL)
			xerror("Cannot append to %s", sp->s_xmit);
		fprintf(ofp, "%s\n", firstbufname);
		fclose(ofp);
		return;
	}
	else
#ifdef UXMIT
	if (useexist) {
		if (firstbufname[0] == '\0')
			xerror("No file name to xmit from");
		if (*sp->s_xmit == '\0')
			sprintf(bfr, UXMIT, sp->s_name, firstbufname);
		else
			sprintf(bfr, sp->s_xmit, firstbufname);
	} else
#endif
	{
		ofp = xfopen(mktemp(TRANS), "w");
		if (afmt)
			ohwrite(&hh, ofp);
		else 
			hwrite(&hh, ofp);
		if (!notify)
			while ((c = getc(ifp)) != EOF)
				putc(c, ofp);
		fclose(ifp);
		fclose(ofp);
		if (*sp->s_xmit == '\0')
			sprintf(bfr, DFTXMIT, sp->s_name, TRANS);
		else
			sprintf(bfr, "(%s) < %s", sp->s_xmit, TRANS);
	}

	/* At this point, the command to be executed is in bfr. */
	if (noshell) {
		if (pid = fork())
			fwait(pid);
		else {
			close(0);
			open(TRANS, 0);
			ptr = sp->s_xmit;
			for (pid = 0; pid < 19; pid++) {
				while (isspace(*ptr))
					*ptr++ = 0;
				argv[pid] = ptr;
				while (!isspace(*++ptr) && *ptr)
					;
				if (!*ptr)
					break;
			}
			argv[++pid] = 0;
			execv(sp->s_xmit, argv);
			xerror("Can't execv\n");
		}
	} else
		system(bfr);
	if (!appfile && !useexist)
		unlink(TRANS);
}

typedef struct {
	char *dptr;
	int dsize;
} datum;

/*
 * Return TRUE if we have seen this file before, else FALSE.
 */
history(hp)
struct hbuf *hp;
{
	register FILE *hfp;
	register char *p;
	datum lhs, rhs;
	datum fetch();

#ifdef DEBUG
	fprintf(stderr,"history(%s)\n", hp->ident);
#endif
	idlock(hp->ident);
#ifdef DBM
	dbminit(ARTFILE);
	lhs.dptr = hp->ident;
	lhs.dsize = strlen(lhs.dptr) + 1;
	rhs = fetch(lhs);
	if (rhs.dptr)
		return(TRUE);
#else
	hfp = xfopen(ARTFILE, "r");
	while (fgets(bfr, BUFLEN, hfp) != NULL) {
		p = index(bfr, '\t');
		if (p == NULL)
			p = index(bfr, '\n');
		if (p != NULL)	/* can happen if nulls in file */
			*p = 0;
		if (strcmp(bfr, hp->ident)==0 ||
				hp->oident[0] && strcmp(bfr, hp->oident)==0) {
			fclose(hfp);
			idunlock();
#ifdef DEBUG
			fprintf(stderr,"history returns true\n");
#endif
			return(TRUE);
		}
	}
	fclose(hfp);
#endif
	addhist(hp->ident);
	addhist("\t");
#ifdef DEBUG
	fprintf(stderr,"history returns false\n");
#endif
	return(FALSE);
}

static char histline[256];	/* Assumed initially zero */

addhist(msg)
char *msg;
{
	strcat(histline, msg);
}

savehist()
{
	register FILE *hfp;
	datum lhs, rhs;
	long fpos;
	register char *p;

	hfp = xfopen(ARTFILE, "a");
	fpos = ftell(hfp);
	fprintf(hfp, "%s\n", histline);
	fclose(hfp);
#ifdef DBM
	/* We assume that history has already been called, calling dbminit. */
	p = index(histline, '\t');
	if (p)
		*p = 0;
	lhs.dptr = histline;
	lhs.dsize = strlen(lhs.dptr) + 1;
	rhs.dptr = (char *) &fpos;
	rhs.dsize = sizeof fpos;
	store(lhs, rhs);
#endif
	histline[0] = 0;
	idunlock();
}

/*
 * Save partial news.
 */
newssave(fd, dummy)
FILE *fd, *dummy;
{
	register FILE *tofd, *fromfd;
	char sfname[BUFLEN];
	register int c;
	struct hbuf h;
	time_t tim;

	if (fd == NULL)
		fromfd = xfopen(INFILE, "r");
	else
		fromfd = fd;
	umask(savmask);
	setgid(gid);
	setuid(uid);

	sprintf(sfname, "%s/%s", userhome, PARTIAL);
	if ((tofd = fopen(sfname, "a")) == NULL)
		xerror("Cannot save partial news");
	time(&tim);
	fprintf(tofd, "----- News saved at %s\n", arpadate(&tim));
	while ((c = getc(fromfd)) != EOF)
		putc(c, tofd);
	fclose(fromfd);
	fclose(tofd);
	printf("News saved in %s\n", sfname);
	xxit(0);
}

/*
 * Handle dates in header.
 */

dates(hp)
struct hbuf *hp;
{
	long edt;

	time(&hp->rectime);
	nstrip(strcpy(hp->recdate, arpadate(&hp->rectime)));
	if (*hp->subdate) {
		if (cgtdate(hp->subdate) < 0) {
			log("Bad sub date '%s'", hp->subdate);
			xerror("Cannot parse submittal date");
		}
	} else
		strcpy(hp->subdate, hp->recdate);
	if (*hp->expdate) {
		if ((edt = cgtdate(hp->expdate)) < 0)
			xerror("Cannot parse expiration date");
		nstrip(strcpy(hp->expdate, arpadate(&edt)));
	} else {
		defexp = TRUE;
		/*
		 * Default is now applied in expire.c
		hp->exptime = hp->rectime + DFLTEXP;
		nstrip(strcpy(hp->expdate, arpadate(&hp->exptime)));
		*/
	}
}

/*
 *	Exit and cleanup.
 */
xxit(status)
int status;
{
	unlink(INFILE);
	unlink(ARTICLE);
	while (lockcount > 0)
		unlock();
	idunlock();
	exit(status);
}

xerror(message, arg1, arg2)
char *message;
int arg1, arg2;
{
	char buffer[128];

	fflush(stdout);
	sprintf(buffer, message, arg1, arg2);
	fprintf(stderr, "inews: %s.\n", buffer);
	log(buffer);
	xxit(1);
}

#ifdef	VMS

#define	SUBLOCK	"/tmp/netnews.lck.1"

/*
 * Newsystem locking.
 * These routines are different for VMS because we can not
 * effectively simulate links, and VMS supports multiple
 * version numbers of files
 */
lock()
{
	register int i;
	register int fd;

	if (lockcount++ == 0) {
		i = DEADTIME;
		while ((fd = creat(SUBLOCK, 0444)) < 0) {
			if (--i < 0) {
				unlink(SUBLOCK);
				log("News system locked up");
			}
			if (i < -3)
				xerror("Unable to unlock news system");
			sleep((unsigned)1);
		}
		close(fd);
	}
}

unlock()
{
	if (--lockcount == 0)
		unlink(SUBLOCK);
}

#else	VMS

/*
 * Newsystem locking.
 */

lock()
{
	register int i;

	if (lockcount++ == 0) {
		i = DEADTIME;
		while (link(SUBFILE, LOCKFILE)) {
			if (--i < 0)
				xerror("News system locked up");
			sleep((unsigned)1);
		}
	}
}

unlock()
{
	if (--lockcount == 0)
		unlink(LOCKFILE);
}
#endif	VMS

char lockname[80];
idlock(str)
char *str;
{
	register int i;
	char tempname[80];
	long now;
	struct stat sbuf;
	int fd;

#ifdef	VMS
	sprintf(lockname, "/tmp/%s.l.1", str);
	if ((fd = creat(lockname, 0444)) < 0) {
#else	VMS
	sprintf(tempname, "/tmp/LTMP.%d", getpid());
	sprintf(lockname, "/tmp/L%s", str);
#ifdef FOURTEENMAX
	lockname[5 /* /tmp/ */ + 14] = '\0';
#endif
	close(creat(tempname, 0666));
	while (link(tempname, lockname)) {
#endif	VMS
		time(&now);
		i = stat(lockname, &sbuf);
		if (i < 0) {
			xerror("Directory permission problem in /tmp");
		}
		if (sbuf.st_mtime + 10*60 < now) {
			unlink(lockname);
			log("Article %s locked up", str);
			continue;
		}
		log("waiting on lock for %s", lockname);
		sleep((unsigned)60);
	}
#ifdef VMS
	close(fd);
#else
	unlink(tempname);
#endif
	unlink(tempname);
}

idunlock()
{
	unlink(lockname);
}

/*
 * Put a unique name into header.ident.
 */
getident(hp)
struct hbuf *hp;
{
	long seqn;
	register FILE *fp;

	lock();
	fp = xfopen(SEQFILE, "r");
	fgets(bfr, BUFLEN, fp);
	fclose(fp);
	seqn = atol(bfr) + 1;
#ifdef	VMS
	unlink(SEQFILE);
#endif	VMS
	fp = xfopen(SEQFILE, "w");
	fprintf(fp, "%ld\n", seqn);
	fclose(fp);
	unlock();
	sprintf(hp->ident, "<%ld@%s%s>", seqn, FULLSYSNAME, MYDOMAIN);
}

/*
 * Log the given message, with printf strings and parameters allowed,
 * on the log file, if it can be written.  The date and an attempt at
 * figuring out the remote system name are also logged.
 */
log(fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9)
char *fmt;
{
	FILE *logfile;
	char msg[256];
	char *logtime, *p, *q;
	char rmtsys[256];
	char c;
	long t;

	if (header.relayversion[0]) {
		for (p=header.relayversion; p; p=index(p+1, 's'))
			if (strncmp(p, "site ", 5) == 0)
				break;
		if (p == NULL)
			goto crackpath;
		p += 4;
		while (*p == ' ' || *p == '\t')
			p++;
		for (q=p; *q && *q!=' ' && *q != '\t'; q++)
			;
		c = *q;
		strcpy(rmtsys, p);
		*q = c;
	} else {
crackpath:
		strcpy(rmtsys, header.path);
		p = index(rmtsys, '!');
		if (p == NULL)
			p = index(rmtsys, ':');
		if (p)
			*p = 0;
		else {
			p = rindex(rmtsys, '@');
			if (p)
				strcpy(rmtsys, p+1);
			else
				strcpy(rmtsys, "local");
		}
	}

	time(&t);
	logtime = ctime(&t);
	logtime[16] = 0;
	logtime += 4;

	sprintf(msg, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9);

	lock();
	if (access(logfname, 0)) {
		unlock(0);
		return;
	}
	logfile = fopen(logfname, "a");
	if (logfile == NULL) {
		unlock(0);
		return;
	}
	fprintf(logfile, "%s %s\t%s\n", logtime, rmtsys, msg);
	fclose(logfile);
	unlock();
}

/*
 * Check if header.nbuf contains only valid newsgroup names;
 * exit with error if not valid.
 *
 * a == TRUE means header.nbuf is subscription list
 * a == FALSE means header.nbuf is newsgroup list
 */

ngfcheck(a)
int a;
{
	char ngcheck[NGFSIZ];	/* Hold NGFILE newsgroups */
	char tbuf[BUFLEN];	/* hold single header.nbuf news group */
	register char *s1, *s2;
	register FILE *f;

	s1 = ngcheck;
	f = xfopen(NGFILE, "r");
	while (fgets(bfr, BUFLEN, f) != NULL) {
		for (s2 = bfr; *s2 != '\0' &&
		    *s2 != ' ' && *s2 != '\t' &&
		    *s2 != ':' && *s2 != '\n';) {
			if (s1 >= &ngcheck[NGFSIZ-2])
				xerror("NGFILE too long");
			*s1++ = *s2++;
		}
		*s1++ = NGDELIM;
	}
	*s1 = '\0';
	fclose(f);
	for (s1 = header.nbuf; *s1 != '\0';) {
		if (*s1 == NEGCHAR)
			s1++;
		s2 = tbuf;
		while ((*s2++ = *s1++) != NGDELIM)
			if (s1[-1] == ':')
				xerror("Newsgroup cannot contain ':'");
		*s2 = '\0';
		s2 = tbuf;
		if (!ngmatch(s2, ngcheck) && (!a || !ngmatch(ngcheck, s2))) {
			ngdel(s2);
			sprintf(bfr, "Bad news group \"%s\"", s2);
			newssave(stdin, NULL);
			xerror(bfr);
		}
	}
}

/*
 * Figure out who posted the article (which is locally entered).
 * The results are placed in the header structure hp.
 */
gensender(hp, logname)
struct hbuf *hp;
char *logname;
{
	char *fn;
	static char buf[100];
	char buf2[100];
	char *fullname(), *getenv();
	char *p;
	int fd;

	fn = getenv("NAME");

	if (fn == NULL) {
		sprintf(buf, "%s/%s", getenv("HOME"), ".name");
		fd = open(buf, 0);
		if (fd >= 0) {
			read(fd, buf2, sizeof buf2);
			close(fd);
			if (buf2[0] >= 'A')
				fn = buf2;
			for (p=fn; *p; p++)
				if (*p < ' ')
					*p = 0;
		}
	}

	if (fn == NULL)
		fn = fullname(logname);

	sprintf(hp->path, "%s", logname);
	sprintf(hp->from, "%s@%s%s (%s)", logname, FULLSYSNAME, MYDOMAIN, fn);
}

/*
 * Trap interrupts.
 */
onsig(n)
int n;
{
	static int numsigs = 0;
	/*
	 * Most UNIX systems reset caught signals to SIG_DFL.
	 * This bad design requires that the trap be set again here.
	 * Unfortunately, if the signal recurs before the trap is set,
	 * the program will die, possibly leaving the lock in place.
	 */
	if (++numsigs > 100) {
		log("readnews ran away looping on signal %d", n);
		xxit(1);
	}
	signal(n, onsig);
	sigtrap = n;
}

/*
 * If the stdin begins with "#", we assume we have been fed a batched
 * shell script which looks like this:
 *	#! rnews 1234
 *	article with 1234 chars
 *	#! rnews 4321
 *	article with 4321 chars
 *
 * In this case we just exec the unbatcher and let it unpack and call us back.
 *
 * Note that there is a potential security hole here.  If the batcher is
 * /bin/sh, someone could ship you arbitrary stuff to run as shell commands.
 * The main protection you have is that the effective uid will be news, not
 * uucp and not the super user.  (That, plus the fact that BATCH is set to
 * "unbatch" as the system is distributed.)  If you want to run a batched link
 * and you are security concious, do not use /bin/sh as the unbatcher.
 * the thing to do is to change BATCH in your localize.sh file from /bin/sh
 * to some restricted shell which can only run rnews.
 */
checkbatch()
{
	int c;

#ifdef BATCH
	c = getc(stdin);
	ungetc(c, stdin);
	clearerr(stdin);
	if (c == '#') {
		reset_stdin();
		execl(BATCH, "news-unpack", 0);
		xerror("Unable to exec shell to unpack news.\n");
	}
#endif
}

/*
 * We've already done a read on stdin, and we want to seek back to the
 * beginning.  We want the real file descriptor (beyond buffers) to
 * reflect the true beginning.  Do whatever is necessary.
 */
reset_stdin()
{
	register FILE *ofd;
	register int c;
	char *ofdname;

	/* First try to seek back - if so, it's a cheap way back. */
	if (lseek(0, 0L, 0) == 0)
		return;

	/* Can't seek, so have to copy input to a file and use that. */
	ofdname = "/tmp/inewsXXXXX";
	mktemp(ofdname);
	ofd = fopen(ofdname, "w");
	while ((c=getc(stdin)) != EOF)
		putc(c, ofd);
	fclose(stdin);
	fclose(ofd);

	/* Now for a few lower level hacks to reopen stdin and make
	 * absolutely sure that the right fd's are done for the exec.
	 */
	close(0);		/* to make sure stdin is really closed. */
	open(ofdname, 0);	/* returns zero */
	unlink(ofdname);	/* to avoid cleaning it up later. */
}
