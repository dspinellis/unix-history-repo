/*
 * This software is Copyright (c) 1986 by Rick Adams.
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
 * ifuncs - functions used by inews.
 */

#ifdef SCCSID
static char	*SccsId = "@(#)ifuncs.c	2.66	10/7/87";
#endif /* SCCSID */

#include "iparams.h"

extern long	localize();

/*LINTLIBRARY*/

/*
 * Transmit this article to all interested systems.
 */

#ifdef u370
static struct srec srec;
#endif /* u370 */

static struct hbuf h, hh;

#ifdef MULTICAST
#define	MAXMCAST	20
#define	MAXMCS		10

struct multicast {
	char mc_name[SBUFLEN];		/* "multi-cast" name */
	short mc_syscnt;
	char mc_tosys[MAXMCAST][SBUFLEN];
} mcast[MAXMCS];

static int mccount;
#endif /* MULTICAST */

long lseek();

#ifndef DBM
char *histfile();
#endif /* !DBM */

#ifdef VMS
/*
 * For VMS/Eunice there are no links: article was moved to firstbufname
 * before broadcast is reached.  So we read it from there.
 */
extern char firstbufname[];
#endif

#ifndef GENERICPATH
/*ARGSUSED*/
#endif /* !GENERICPATH */
broadcast(is_rnews)
int is_rnews;
{
	register char *hptr;
	register char *sptr;
	register FILE *fp;
#ifndef u370
	struct srec srec;
#endif
	char sentbuf[LBUFLEN];
	int nsent = 0;
	char *sentsys;
#ifdef GENERICPATH
	int len;
#endif /* GENERICPATH */

	/* h is a local copy of the header we can scribble on */
#ifdef VMS
	fp = xfopen (firstbufname, "r");
#else
	fp = xfopen(ARTICLE, "r");
#endif
	if (hread(&h, fp, TRUE) == NULL)
		xerror("Cannot reread article");
	(void) fclose(fp);

	(void) strcpy(sentbuf, h.ident);
	(void) strcat(sentbuf, " sent to ");
	sentsys = index(sentbuf, '\0');
	nsent = 0;
	/* break path into list of systems. */
	hptr = h.path;
#ifdef GENERICPATH
	if (!is_rnews && 
		STRNCMP(PATHSYSNAME, h.path, (len = strlen(PATHSYSNAME))) == 0
		&& index(NETCHRS, h.path[len]))
		(void) strcpy(h.path, &(h.path[len+1]));
#endif /* GENERICPATH */
	sptr = hptr = h.path;
	while ((hptr=strpbrk(hptr, NETCHRS)) != NULL) {
		*hptr++ = '\0';
		sptr = hptr;
	}
	*sptr = '\0';

#ifdef MULTICAST
	mccount = 0;
#endif /* MULTICAST */

	/* loop once per system. */
	s_openr();
	while (s_read(&srec)) {
		char *dist = h.distribution;
		if (STRNCMP(srec.s_name, LOCALPATHSYSNAME, SNLN) == 0)
			continue;
		if (sptr = srec.s_nosend) {
			while (*sptr) {
				while (*sptr && *sptr != ',')
					sptr++;
				if (*sptr == ',')
					*sptr++ = '\0';
			}
			*++sptr = '\0';
		}
		hptr = h.path;
		while (*hptr != '\0') {
			if (STRNCMP(srec.s_name, hptr, SNLN) == 0)
				goto contin;
			if (sptr = srec.s_nosend) {
				while (*sptr != '\0') {
					if (STRNCMP(sptr, hptr, SNLN) == 0)
						goto contin;
					while (*sptr++)
						;
				}
			}
			while (*hptr++ != '\0')
				;
		}
		if (!ngmatch(h.nbuf, srec.s_nbuf))
			continue;
		if (*dist == '\0')
			dist = "world";
		if (!ngmatch(dist, srec.s_nbuf) && !ngmatch(srec.s_nbuf, dist))
			    continue;

		if (nsent) {
			hptr = sentsys;
			while ((sptr = index(hptr, ',')) != NULL) {
				*sptr = '\0';
				if (STRCMP(hptr, srec.s_name) == 0) {
					*sptr = ',';
					goto contin;
				}
				*sptr++ = ',';
				for (hptr = sptr; isspace(*hptr); hptr++)
					;
			}
			if (STRCMP(hptr, srec.s_name) == 0)
				continue;
		}
		/* now we've found a system to send this article to */
#ifdef MULTICAST
		if (index(srec.s_flags, 'M')) {
			/* do a "multi-cast" transmit */
			register struct multicast *m;

			if (strlen(srec.s_name) >= SBUFLEN ||
			    strlen(srec.s_xmit) >= SBUFLEN)
				xerror("system name too long for multicast");
			for (m = mcast; m < &mcast[mccount]; m++)
				if (STRCMP(srec.s_xmit, m->mc_name) == 0)
					break;
			if (m >= &mcast[MAXMCS])
				xerror("Too many multicasts");
			if (m == &mcast[mccount]) {
				mccount++;
				m->mc_syscnt = 0;
				strcpy(m->mc_name, srec.s_xmit);
			}
			if (m->mc_syscnt >= MAXMCAST)
				xerror("Too many systems for multicast");
			strcpy(m->mc_tosys[m->mc_syscnt++], srec.s_name);
		} else {
			register struct multicast *m;
			register char **yptr;
			char *sysptrs[MAXMCAST];
			int mc;

			mc = 0;
			for (m = mcast; m < &mcast[mccount]; m++)
				if (STRCMP(m->mc_name, srec.s_name) == 0) {
					yptr = sysptrs;
					while (mc < m->mc_syscnt)
						*yptr++ = m->mc_tosys[mc++];
					break;
				}
#ifdef VMS
			if (!transmit(&srec, xfopen(firstbufname,"r"),
#else /* !VMS */
			if (!transmit(&srec, xfopen(ARTICLE,"r"),
#endif /* !VMS */
				(STRNCMP(h.nbuf, "to.", 3) != 0),
				sysptrs, mc))
				continue;
		}
#else /* !MULTICAST */
#ifdef VMS
		if (!transmit(&srec, xfopen(firstbufname, "r"),
#else /* !VMS */
		if (!transmit(&srec, xfopen(ARTICLE, "r"),
#endif /* !VMS */
			(STRNCMP(h.nbuf, "to.", 3) != 0),
			(char **) NULL, FALSE))
				continue;
#endif /* !MULTICAST */
		if (nsent)
			(void) strcat(sentbuf, ", ");
		(void) strcat(sentbuf, srec.s_name);
		nsent++;
	contin:;
	}
	if (nsent)
		log(sentbuf);
	s_close();
}

/*
 * Transmit file to system.
 */
#define PROC 0004
#ifndef MULTICAST
/* ARGSUSED */
#endif /* !MULTICAST */
transmit(sp, ifp, maynotify, sysnames, mc)
register struct srec *sp;
register FILE *ifp;
int maynotify;
char **sysnames;
int mc;
{
	register FILE *ofp;
	register int c;
	register char *ptr;
	char TRANS[BUFLEN];
	char *argv[20];
	register int pid;
	extern char firstbufname[];

/* A:	afmt: the other machine runs an A news, so we xmit in A format */
	int afmt = (index(sp->s_flags, 'A') != NULL);
/* B:	use B format (this is the default - don't use this letter elsewise). */
/* F:	append name to file */
	int appfile = (index(sp->s_flags, 'F') != NULL);
/* L:	local: don't send the article unless it was generated locally */
	int local = ((ptr = index(sp->s_flags, 'L')) != NULL);
/* H:	interpolate history line into command, use existing file */
	int history = (index(sp->s_flags, 'H') != NULL);
/* m:	moderated: only send if group is moderated */
	int sendifmoderated = (index(sp->s_flags, 'm') != NULL);
/* u:	unmoderated: only send if group is unmoderated */
	int sendifunmoderated = (index(sp->s_flags, 'u') != NULL);
/* M:	multi-cast: this is taken care of above, but don't reuse flag */
#ifdef MULTICAST
/* O:	multi-cast only, don't send article if not multicast hosts */
	int multisend = (index(sp->s_flags, 'O') != NULL);
#endif /* MULTICAST */
/* N:	notify: don't send the article, just tell him we have it */
	int notify = maynotify && (index(sp->s_flags, 'N') != NULL);
/* S:	noshell: don't fork a shell to execute the xmit command */
	int noshell = (index(sp->s_flags, 'S') != NULL);
/* U:	useexist: use the -c option to uux to use the existing copy */
	int useexist = (index(sp->s_flags, 'U') != NULL);
/* I:	append messageid to file. implies F flag */
	int appmsgid = maynotify && (index(sp->s_flags, 'I') != NULL);

	if (notify)
		appfile = appmsgid = FALSE;

	if (local && mode == PROC) {
		local = 0;
		while (isdigit(*++ptr))
			local = local * 10 + *ptr - '0';
		for (ptr = h.path; *ptr != '\0' && local >= 0; local--)
			while (*ptr++ != '\0')
				;
		if (local < 0) {
			(void) fclose(ifp);
			return FALSE;
		}
	}

	/*
	** Do not transmit to system specified in -x flag.
	*/
	if (not_here[0] && STRCMP(not_here, sp->s_name) == 0) {
		(void) fclose(ifp);
		return FALSE;
	}

#ifdef DEBUG
	printf("Transmitting to '%s'\n", sp->s_name);
#endif /* DEBUG */

#ifdef MULTICAST
	if (multisend && mc == 0) {
		(void) fclose(ifp);
		return FALSE;
	}
#endif /* MULTICAST */

	if ((sendifmoderated && is_mod[0] == '\0') ||
	    (sendifunmoderated && is_mod[0] != '\0')) {
		fclose(ifp);
		return FALSE;
	}

	if (appmsgid || (!appfile && !useexist && !history)) {
		if (!hread(&hh, ifp, TRUE)) {
			logerr("Bad header, not transmitting %s re %s to %s",
				hh.ident, hh.title, sp->s_name);
			(void) fclose(ifp);
			return FALSE;
		}
		if (hh.nbuf[0] == '\0') {
			fprintf(stderr, "Article not subscribed to by %s\n", sp->s_name);
			(void) fclose(ifp);
			return FALSE;
		}
		(void) sprintf(TRANS, "%s/trXXXXXX", SPOOL);
	}

	if (notify) {
		char oldid[50];
		(void) sprintf(hh.title, "ihave %s %s", hh.ident, PATHSYSNAME);
		(void) strcpy(hh.ctlmsg, hh.title);
		(void) strcpy(hh.numlines, "0");
		(void) sprintf(hh.nbuf, "to.%s.ctl", sp->s_name);
		(void) strcpy(oldid, hh.ident);
		getident(&hh);
		log("tell %s about %s, notif. id %s",
			sp->s_name, oldid, hh.ident);
	}

	if (appfile || appmsgid) {
		if (firstbufname[0] == '\0') {
			extern char histline[];
			localize("junk");
			savehist(histline);
			xerror("No file name to xmit from");
		}
		if (sp->s_xmit[0] == '\0')
			sprintf(sp->s_xmit, "%s/%s%s", BATCHDIR, sp->s_name,
				appmsgid ? ".ihave" : "");
#ifdef LOGDIR
		(void) sprintf(TRANS, "%s/%s/%s", logdir(HOME), BATCHDIR, sp->s_xmit);
		ofp = fopen(TRANS, "a");
#else /* !LOGDIR */
		ofp = fopen(sp->s_xmit, "a");
#endif /* !LOGDIR */
		if (ofp == NULL)
			xerror("Cannot append to %s", sp->s_xmit);
		fprintf(ofp, "%s", appmsgid ? hh.ident : firstbufname);
#ifdef MULTICAST
		while (--mc >= 0)
			fprintf(ofp, " %s", *sysnames++);
#endif /* !MULTICAST */
		putc('\n', ofp);
		(void) fclose(ofp);
		(void) fclose(ifp);
		return TRUE;
	}
	else if (useexist) {
		if (firstbufname[0] == '\0')
			xerror("No file name to xmit from");
		if (*sp->s_xmit == '\0')
#ifdef UXMIT
			(void) sprintf(bfr, UXMIT, sp->s_name, firstbufname);
#else
			xerror("UXMIT not defined for U flag");
#endif
		else
#ifdef MULTICAST
			makeargs(bfr, sp->s_xmit, firstbufname, sysnames, mc);
#else
			(void) sprintf(bfr, sp->s_xmit, firstbufname);
#endif
		(void) fclose(ifp);
	} else if (history) {
		extern char histline[];

		if (*sp->s_xmit == '\0')
			xerror("no xmit command with H flag");
#ifdef MULTICAST
		makeargs(bfr, sp->s_xmit, histline, sysnames, mc);
#else
		(void) sprintf(bfr, sp->s_xmit, histline);
#endif
	} else {
		ofp = xfopen(mktemp(TRANS), "w");
		if (afmt) {
#ifdef OLD
			fprintf(ofp, "A%s\n%s\n%s!%s\n%s\n%s\n", oident(hh.ident), hh.nbuf, PATHSYSNAME,
				hh.path, hh.subdate, hh.title);
#else /* !OLD */
			logerr("Must have OLD defined to use A flag for xmit");
			return FALSE;
#endif /* !OLD */
		} else
			hwrite(&hh, ofp);
		if (!notify)
			while ((c = getc(ifp)) != EOF)
				putc(c, ofp);
		if (ferror(ofp))
			xerror("write failed on transmit");
		(void) fclose(ifp);
		(void) fclose(ofp);
		if (*sp->s_xmit == '\0')
			(void) sprintf(bfr, DFTXMIT, sp->s_name, TRANS);
		else
#ifdef MULTICAST
			makeargs(bfr, sp->s_xmit, TRANS, sysnames, mc);
#else /* !MULTICAST */
			(void) sprintf(bfr, sp->s_xmit, TRANS);
#endif /* !MULTICAST */
	}

	/* At this point, the command to be executed is in bfr. */
	if (noshell) {
		if (pid = vfork())
			fwait(pid);
		else {
			(void) close(0);
			(void) open(TRANS, 0);
			ptr = bfr;
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
			(void) setgid(gid);
			(void) setuid(uid);
			execvp(argv[0], argv);
			xerror("Can't execv %s", argv[0]);
		}
	} else {
		if (!history && sp->s_xmit[0] && !index(bfr, '<')) {
			char newcmd[LBUFLEN];

			(void) sprintf(newcmd, "(%s) <%s", bfr,
			    useexist ? firstbufname : TRANS);
			system(newcmd);
		} else
			system(bfr);
	}
	if (!appfile && !useexist && !history)
		(void) unlink(TRANS);
	(void) fclose(ifp);
	return TRUE;
}

#ifdef MULTICAST
makeargs(buf, cmd, arg2, sysargs, sac)
char *buf;
char *cmd;
char *arg2;
register char **sysargs;
int sac;
{
	register char *p = cmd;
	register char *q;
	register ac = 0;
	register char *b = buf;

	q = p;
	do {
		if (q = index(q, ' '))
			*q = '\0';
		if (index(p, '%')) {
			switch (++ac) {
			case 1:
				while (--sac >= 0) {
					sprintf(b, p, *sysargs++);
					b = index(b, '\0');
				}
				break;
			case 2:
				sprintf(b, p, arg2);
				b = index(b, '\0');
				break;
			default:
				if (q)
					*q = ' ';
				xerror("badly formed command: %s", cmd);
			}
		} else {
			strcpy(b, p);
			b = index(b, '\0');
		}
		if (q) {
			*q = ' ';
			p = q;
			while (isspace(*q))
				q++;
		}
	} while (q != NULL);
}
#endif /* MULTICAST */

/*
 * Return TRUE if we have seen this file before, else FALSE.
 */
history(hp)
struct hbuf *hp;
{
#ifdef DBM
	datum lhs, rhs;
	datum fetch();
#else /* !DBM */
	register FILE *hfp;
	register char *p;
#endif /* !DBM */
	char lcident[BUFLEN];
	extern char histline[];

#ifdef DEBUG
	fprintf(stderr,"history(%s)\n", hp->ident);
#endif /* DEBUG */
	/*
	 * Make the article ID case insensitive.
	 */
	(void) strcpy(lcident, hp->ident);
	lcase(lcident);

	idlock(lcident);
#ifdef DBM
	initdbm(ARTFILE);
	lhs.dptr = lcident;
	lhs.dsize = strlen(lhs.dptr) + 1;
	rhs = fetch(lhs);
	if (rhs.dptr) {
		idunlock();
		return(TRUE);
	}
#else /* !DBM */
	hfp = xfopen(histfile(lcident), "r");
	while (fgets(bfr, BUFLEN, hfp) != NULL) {
		p = index(bfr, '\t');
		if (p == NULL)
			p = index(bfr, '\n');
		if (p != NULL)	/* can happen if nulls in file */
			*p = 0;
		lcase(bfr);

		if (STRCMP(bfr, lcident) == 0) {
			(void) fclose(hfp);
			idunlock();
#ifdef DEBUG
			fprintf(stderr,"history returns true\n");
#endif /* DEBUG */
			return TRUE;
		}
	}
	(void) fclose(hfp);
#endif /* !DBM */
	histline[0] = '\0';
	addhist(hp->ident);
	addhist("\t");
#ifdef DEBUG
	fprintf(stderr,"history returns false\n");
#endif
	return FALSE;
}

char histline[PATHLEN];

addhist(msg)
char *msg;
{
	(void) strcat(histline, msg);
}

savehist(hline)
char *hline;
{
	register FILE *hfp;
	register char *p;
#ifdef DBM
	long fpos;
#endif /* !DBM */

#ifndef DBM
	if (STRCMP((p = histfile(hline)), ARTFILE) != 0) {
	/* If the history subfile is accessible */
		if ((hfp = xfopen(p, "a")) != NULL ) { /* If we can append */
			fprintf(hfp, "%s\n", hline);   /* Append */
			(void) fclose(hfp);
		} else
			logerr("Unable to append to %s: %s", p, errmsg(errno));
	} else
#endif /* !DBM */
	{
	hfp = xfopen(ARTFILE, "a");
	(void) fseek(hfp, 0L, 2); /* Unisoft 5.1 doesn't seek to EOF on 'a' */
#ifdef DBM
	fpos = ftell(hfp);
#endif /* !DBM */
	fprintf(hfp, "%s\n", hline);
	(void) fclose(hfp);
	}
#ifdef DBM
	{
	datum lhs, rhs;
	/* We assume that history has already been called, calling dbminit. */
	p = index(hline, '\t');
	if (p)
		*p = 0;
	lcase(hline);
	lhs.dptr = hline;
	lhs.dsize = strlen(lhs.dptr) + 1;
	rhs.dptr = (char *)&fpos;
	rhs.dsize = sizeof fpos;
	store(lhs, rhs);
	}
#endif /* DBM */
	idunlock();
}

/*
 * Save partial news.
 */
/* ARGSUSED */
newssave(fd, dummy)
FILE *fd;
char *dummy;
{
	register FILE *tofd, *fromfd;
	char sfname[BUFLEN];
	register int c;
	time_t tim;

	if (fd == NULL)
		fromfd = xfopen(INFILE, "r");
	else
		fromfd = fd;
	(void) umask(savmask);
	(void) setgid(gid);
	(void) setuid(uid);

	(void) sprintf(sfname, "%s/%s", userhome, PARTIAL);
	if ((tofd = fopen(sfname, "a")) == NULL)
		xerror("Cannot save partial news in %s", sfname);
	(void) time(&tim);
	fprintf(tofd, "----- News saved at %s\n", arpadate(&tim));
	while ((c = getc(fromfd)) != EOF)
		putc(c, tofd);
	(void) fclose(fromfd);
	(void) fclose(tofd);
	printf("News saved in %s\n", sfname);
	xxit(1);
}

/*
 * Handle dates in header.
 */

dates(hp)
struct hbuf *hp;
{
	time_t edt;

	if (*hp->subdate) {
		if (cgtdate(hp->subdate) < 0) {
			error("Cannot parse submittal date '%s'", hp->subdate);
		}
	} else {
		(void) time(&edt);
		(void) strcpy(hp->subdate, arpadate(&edt));
	}
}

#define LOCKSIZE	128
char lockname[LOCKSIZE];

idlock(str)
char *str;
{
	register int i;
	register char *cp, *scp;
	char tempname[LOCKSIZE];
	time_t now;
	struct stat sbuf;
	extern int errno;
#ifdef	VMS
	int fd;
/* The name here is because of the peculiar properties of version numbers
 * in Eunice.  We eliminate any troublesome characters also.
 */
	(void) sprintf(lockname, "/tmp/%.10s.l.1", str);
	for (cp = lockname; *cp; cp++)
		if (*cp == '/' || *cp == '[' || *cp == ']') *cp = '.';
	while ((fd = creat(lockname, 0444)) < 0) {
#else /* !VMS */
	(void) strcpy(tempname, "/tmp/LTMP.XXXXXX");
	(void) mktemp(tempname);
	(void) strcpy(lockname, "/tmp/L");
	i = strlen(lockname);
	cp = &lockname[i];
	scp = str - 1;
	while (i++ < LOCKSIZE && *++scp != '\0')
		if (*scp == '/')	/* slash screws up the open */
			*cp++ = '.';
		else
			*cp++ = *scp;
	*cp = '\0';
#ifdef FOURTEENMAX
	lockname[5 /* /tmp/ */ + 14] = '\0';
#endif
	i = creat(tempname, 0666);
	if (i < 0)
		xerror("Cannot creat %s: errno %d", tempname, errno);
	(void) close(i);
	while (link(tempname, lockname)) {
#endif /* !VMS */
		(void) time(&now);
		if (stat(lockname, &sbuf) < 0)
			xerror("Directory permission problem in /tmp");

		if (sbuf.st_mtime + 10*60 < now) {
			(void) unlink(lockname);
			logerr("Article %s locked up", str);
			break;
		}
		log("waiting on lock for %s", lockname);
		sleep((unsigned)60);
	}
#ifdef VMS
	(void) close(fd);
#endif
	(void) unlink(tempname);
}

idunlock()
{
	(void) unlink(lockname);
}

/*
 * Put a unique name into header.ident.
 */
getident(hp)
struct hbuf *hp;
{
	long seqn;
	register FILE *fp;
	extern char *mydomain();

	lock();
	fp = xfopen(SEQFILE, "r");
	(void) fgets(bfr, BUFLEN, fp);
	(void) fclose(fp);
	seqn = atol(bfr) + 1;
/*
 * For Eunice, this breaks if SEQFILE is not in Unix format.
 */
	fp = xfopen(SEQFILE, "r+w");
	fprintf(fp, "%ld\n", seqn);
	(void) fclose(fp);
	unlock();
	(void) sprintf(hp->ident, "<%ld@%s>", seqn, LOCALSYSNAME);
}

/*
 * Check that header.nbuf contains only valid newsgroup names;
 * exit with error if not valid.
 */
ngfcheck(isproc)
{
	register FILE *	f;
	register char *	cp;
	register int	i, j;
	register int	ngcount, okcount, havealiased;
	register int	pass;
	char *		ngs[sizeof header.nbuf / 2];
	char		uses[sizeof header.nbuf / 2];
	char		tbuf[sizeof header.nbuf];
	char		abuf[BUFLEN];

	havealiased = ngcount = 0;
	is_mod[0] = '\0';
	/*
	** Split header.nbuf into constituent newsgroups.
	** Zap "local" newsgroups of articles from remote sites.
	*/
	cp = tbuf;
	(void) strcpy(cp, header.nbuf);
	for ( ; ; ) {
		while (*cp == NGDELIM || *cp == ' ')
			++cp;
		if (*cp == '\0')
			break;
		ngs[ngcount] = cp;
		do {
			++cp;
		} while (*cp != '\0' && *cp != NGDELIM && *cp != ' ');
		if (*cp != '\0')
			*cp++ = '\0';
		/*
		** Check for local only distribution on incoming
		** newsgroups.  This might occur if someone posted to
		** general,net.unix
		*/
		if (isproc && index(ngs[ngcount], '.') == NULL &&
			index(header.nbuf, '.') != NULL) {
				logerr("Local group %s removed",
					ngs[ngcount]);
				continue;
		}
		uses[ngcount] = 1;	/* it should go in "Newsgroups" line */
		++ngcount;
	}
	/*
	** Check groups against active file.
	*/
recheck:
	okcount = 0;
	rewind(actfp); clearerr(actfp);
	while (okcount < ngcount && fgets(bfr, BUFLEN, actfp) == bfr) {
		if ((cp = index(bfr, ' ')) == NULL)
			continue;	/* strange line in input! */
		/* newsgroup 12345 12345 X */
		/*  cp +    01234567890123 */
		if (!isproc && cp[13]  == 'n')
			continue;	/* can't post to this group! */
		*cp = '\0';
		for (i = 0; i < ngcount; ++i)
			if (uses[i] >= 1 && STRCMP(bfr, ngs[i]) == 0) {
				uses[i] = 2;	/* it should be localized too */
				if (cp[13] == 'm')
					strcpy(is_mod, bfr);
				++okcount;
			}
	}
#ifdef ALWAYSALIAS
	okcount = 0;
#endif /* ALWAYSALIAS */
	/*
	** Handle groups absent from active file.
	*/
	if (havealiased == 0 && okcount < ngcount) {
		/*
		** See if remaining groups are in our alias list.
		*/
		f = xfopen(ALIASES, "r");
		while (okcount < ngcount && fscanf(f, "%s %s%*[^\n]", abuf, bfr) == 2)
			for (i = 0; i < ngcount; ++i) {
#ifndef ALWAYSALIAS
				if (uses[i] == 2)
					continue;
#endif /* ALWAYSALIAS */
				if (STRCMP(ngs[i], abuf) != 0)
					continue;
				if (isproc)
					cp = "Aliased newsgroup %s to %s";
				else
					cp = "Please change %s to %s";
				logerr(cp, abuf, bfr);
				ngs[i] = AllocCpy(bfr);
				uses[i] = 2;
				++havealiased;
				++okcount;
			}
		(void) fclose(f);
		for (i = 0; i < ngcount; ++i) {
			if (uses[i] == 2)
				continue;
			if (isproc)
				log("Unknown newsgroup %s not localized",
					ngs[i]);
			else
				logerr("Unknown newsgroup %s", ngs[i]);
#ifdef ALWAYSALIAS
			++okcount;	/* so we know to exit below */
		}
		if (!isproc && okcount > 0)
#else /* !ALWAYSALIAS */
		}
		if (!isproc)
#endif /* !ALWAYSALIAS */
			newssave(infp, (char *) NULL);
		/*
		 * Unfortunately, if you alias an unmoderated group to a
		 * moderated group, you must recheck the active file to see
		 * if the new group is moderated. Rude but necessary.
		 */
		if (havealiased)
			goto recheck;	
	}
	/*
	** Zap duplicates.
	*/
	for (i = 0; i < ngcount - 1; ++i) {
		if (uses[i] == 0)
			continue;
		for (j = i + 1; j < ngcount; ++j) {
			if (uses[j] == 0)
				continue;
			if (STRCMP(ngs[i], ngs[j]) != 0)
				continue;
			logerr("Duplicate %s removed", ngs[j]);
			if (uses[i] < uses[j])
				uses[i] = uses[j];
			uses[j] = 0;
		}
	}
	for (pass = 1; pass <= 2; ++pass) {
		register int	avail;

		if (pass == 1) {
			/*
			** Rewrite header.nbuf.
			*/
			cp = header.nbuf;
			avail = sizeof header.nbuf;
		} else {
			/*
			** Fill in nbuf.
			*/
			cp = nbuf;
			avail = sizeof nbuf;
		}
		for (i = 0; i < ngcount; ++i) {
			if (uses[i] < pass)
				continue;
			j = strlen(ngs[i]);
			if (j + 2 > avail) {
				logerr("Redone Newsgroups too long");
				break;
			}
			(void) strcpy(cp, ngs[i]);
			cp += j;
			*cp++ = (pass == 1) ? NGDELIM : '\0';
			avail -= (j + 1);
		}
		if (pass == 1) {
			if (cp == header.nbuf)
				*cp = '\0';
			else	*(cp - 1) = '\0';
		} else	*cp = '\0';
	}
	/*
	** Free aliases.
	*/
	for (i = 0; i < ngcount; ++i)
		if (ngs[i] < tbuf || ngs[i] > &tbuf[sizeof tbuf - 1])
			free(ngs[i]);
	return nbuf[0] == '\0';
}

/*
 * Figure out who posted the article (which is locally entered).
 * The results are placed in the header structure hp.
 */
gensender(hp, logname)
struct hbuf *hp;
char *logname;
{
	register char *fn, *p;
	char buf[BUFLEN];
	char *fullname(), *getenv();
	int fd, n;
	extern char *mydomain();

	if ((fn = getenv("NAME")) == NULL) {
		(void) sprintf(buf, "%s/%s", userhome, ".name");
		if ((fd = open(buf, 0)) >= 0) {
			n = read(fd, buf, sizeof buf);
			(void) close(fd);
			if (n > 0 && buf[0] >= 'A') {
				for (p = fn = buf; *p; p++)
					if (*p < ' ')
						*p = '\0';
			}
		}
	}

	if (fn == NULL)
		fn = fullname(logname);

	(void) sprintf(hp->path, "%s", logname);
	(void) sprintf(hp->from, "%s@%s (%s)", logname, FROMSYSNAME, fn);
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
		xerror("inews ran away looping on signal %d", n);
	}
	(void) signal(n, onsig);
	SigTrap = n;
}

/*
 * If the stdin begins with "#" the input is some kind of batch.  if
 * the first line is:
 *	#!cunbatch
 * or
 *	#!c7unbatch
 * then fork off a pipe to do the either a
 *	"compress -d"
 * or a
 *	"decode | compress -d"
 * and check their output for more batch headers.  They probably
 * contain a batch format that looks like this:
 *	#! rnews 1234
 *	article with 1234 chars
 *	#! rnews 4321
 *	article with 4321 chars
 * If so, then for each article, copy the indicated number of chars into
 * a temp file, fork a copy of ourselves, make its input the temp file,
 * and allow the copy to process the article.  This avoids an exec of
 * rnews for each article.
 */

checkbatch()
{
	int c;
	char *cp;

	setbuf(infp, (char *)NULL);
	while ((c = getc(infp)) == '#') {
		/* some kind of batch, investigate further */
		int i;
		char cmd[BUFLEN];
		cmd[0] = c;
		fgets(cmd + 1, BUFLEN, infp);
		if (strncmp(cmd, "#! cunbatch", 11) == 0) {
			(void) sprintf(cmd, "%s/compress", LIB);
			input_pipe(cmd, "compress", "-d", (char *) 0);
			setbuf(infp, (char *)NULL);
			continue;	/* look for the #! rnews */
		} else if (strncmp(cmd, "#! c7unbatch", 12) == 0) {
			(void) sprintf(cmd, "%s/decode | %s/compress -d",
				LIB, LIB);
			input_pipe("/bin/sh", "news-unpack", "-c", cmd);
			setbuf(infp, (char *)NULL);
			continue;	/* look for the #! rnews */
		} else if (strncmp(cmd, "#! rnews ", 9) == 0 ||
			strncmp(cmd, "! rnews ", 8) == 0) {
			/* instead of execing unbatch do it ourselves */
			register int fd, rc, wc;
			int piped[2];
			register long size, asize;
			char *tfilename;
			int pid, wpid, exstat;
#define CPBFSZ 8192
			char buf[CPBFSZ];

			tfilename = 0;
			cp = malloc((unsigned)BUFSIZ);
			if (cp != NULL)
				setbuf(infp, cp);
			do {
				while (STRNCMP(cmd, "#! rnews ", 9)) {
					fprintf(stderr, "out of sync, skipping %s\n", cmd);
					if (fgets(cmd, BUFLEN, infp) == NULL)
						exit(0);
				}
				asize = atol(cmd + 9);
				if (asize <= 0)
					xerror("checkbatch: bad batch count %ld", asize);
				fd = -1;
				size = asize;
				do {
					if (size > CPBFSZ)
						rc = CPBFSZ;
					else
						rc = size;
					rc = fread(buf, 1, rc, infp);
					if (rc <= 0)
						break;
					if (fd < 0) {
						if (rc == asize)
							break;	/* fits in buffer */
						if (!tfilename)
							tfilename = mktemp("/tmp/unbnewsXXXXXX");
						if ((fd = creat(tfilename, 0666)) < 0) {
							fprintf(stderr, "rnews: creat of \"%s\" failed",
								tfilename);
							perror(" ");
							exit(1);
						}
					}
					wc = write(fd, buf, rc);	/* write to temp file */
					if (wc != rc) {
						fprintf(stderr, "write of %d to \"%s\" returned %d",
							rc, tfilename, wc);
						perror(" ");
						exit(1);
					}
					size -= rc;
				} while (size > 0);
				if (fd >= 0)
					(void) close(fd);

				/*
				 * If we got a truncated batch, don't process
				 * the last article; it will probably be
				 * received again. 
				 */
				if ((rc < asize) && (size > 0))
					break;

				/*
				 * This differs from the old unbatcher in
				 * that we don't exec rnews, mainly because
				 * we ARE rnews.  Instead we fork off a copy
				 * of ourselves for each article and allow it
				 * to process. 
				 */
				if (rc == asize) {
					/*
					 * article fits in buffer, use a pipe
					 * instead of a temporary file. 
					 */
					if (pipe(piped) != 0)
						xerror("checkbatch: pipe() failed");
				}
				while ((pid = fork()) == -1) {
					fprintf(stderr, "fork failed, waiting...\r\n");
					sleep(60);
				}
				if (pid == 0) {
					if (rc == asize) {
						/* article fits in buffer
						 * make the output of the
						 * pipe for STDIN 
						 */
						(void) fclose(infp);
						/* redundant but why not */
						(void) close(0);
						if ((i = dup(piped[0])) != 0)
							xerror("dup() returned %d, should be 0", i);
						(void) close(piped[0]);
						(void) close(piped[1]);
						infp = fdopen(0, "r");
					} else	/* supstitute temp file as
						 * input */
						freopen(tfilename, "r", infp);
					return;	/* from checkbatch as if
						 * normal article */
				}
				/* parent of fork */
				if (rc == asize) {
					/* article fits in buffer */
					wc = write(piped[1], buf, rc);
					if (wc != rc) {
						fprintf(stderr, "write of %d to pipe returned %d",
							rc, wc);
						perror("rnews: write");
						exit(1);
					}
					(void) close(piped[0]);
					(void) close(piped[1]);
				}
				while ((wpid = wait(&exstat)) >= 0 && wpid != pid);
				(void) unlink(tfilename);
			} while (fgets(cmd, BUFLEN, infp) != NULL);
			exit(0);/* all done */

		} else {
			docmd(cmd);
			xxit(0);
		}
	}			/* while a batch */
	cp = malloc((unsigned)BUFSIZ);
	if (cp != NULL)
		setbuf(infp, cp);
	if (c != EOF)
		(void) ungetc(c, infp);
	clearerr(infp);
}

/*
 * The input requires some processing so fork and exec the indicated command
 * with its output piped to our input. 
 */
static 
input_pipe(cmd, arg0, arg1, arg2)
char *cmd, *arg0, *arg1, *arg2;
{
	int i, pid;
	int piped[2];

	if (pipe(piped) != 0) {
		perror("checkbatch: pipe() failed");
		exit(1);
	}
	fflush(stdout);
	while ((pid = vfork()) == -1) {
		perror("checkbatch: fork failed, waiting");
		sleep(60);
	}
	if (pid == 0) {		/* child process */
		/*
		 * setup a pipe such that the exec'ed process will read our
		 * input file and write to the pipe 
		 */
		(void) close(1);
		if ((i = dup(piped[1])) != 1)
			xerror("dup() returned %d, should be 1", i);
		(void) close(piped[0]);
		(void) close(piped[1]);
		execl(cmd, arg0, arg1, arg2, (char *) 0);
		perror("checkbatch");
		xerror("Unable to exec %s to unpack news.", cmd);
	} else {		/* parent process */
		/* make the output of the pipe for STDIN */
		(void) fclose(infp);
		(void) close(0);
		if ((i = dup(piped[0])) != 0)
			xerror("dup() returned %d, should be 0", i);
		(void) close(piped[0]);
		(void) close(piped[1]);
		/*
		 * there should be a way to clear any buffered input and just
		 * replace file descriptor 0 but I can't find a portable way. 
		 */
		infp = fdopen(0, "r");
	}
}

#define MAXARGS 32

docmd(p)
register char *p;
{
	char *args[MAXARGS];
	register char **ap = args;
	char path[BUFSIZ];
	char *rindex(), *cp;

	while (*p && !isspace(*p))		/* skip leading #! crud */
		p++;

	while (isspace(*p))
		p++;

	while (*p != '\0') {
		*ap++ = p;
		if (ap >= &args[MAXARGS]) {
			logerr("inews: unbatch: Too many args to %s", args[0]);
			exit(2);
		}
		while (*p && !isspace(*p))
			p++;
		if (*p)
			*p++ = '\0';
		while (isspace(*p))
			p++;
	}
	*ap = (char *)0;

	if (ap == args) {
		logerr("inews: unbatch: no command to execute");
		exit(2);
	}

	/* strip off any leading pathname in case someone gets tricky */
	cp = rindex(args[0], '/');
	if (cp++ == NULL)
		cp = args[0];

# ifdef HOME
	sprintf(path, "%s/%s/%s", logdir(HOME), LIBDIR, cp);
# else /* !HOME */
	sprintf(path, "%s/%s", LIBDIR, cp);
# endif /* HOME */

	/*
	 * "path" is absolute, no searching is needed,  we use
	 * 'execvp' solely so that sh scripts will be handled
	 */
	(void) execvp(path, args);
	perror(path);
	xxit(2);
}

/*
 *	Exit and cleanup.
 */
xxit(status)
int status;
{
	(void) unlink(INFILE);
	(void) unlink(ARTICLE);
	while (lockcount > 0)
		unlock();
	idunlock();
	exit(status);
}

rwaccess(fname)
char *fname;
{
	int fd;

	fd = open(fname, 2);
	if (fd < 0)
		return 0;
	(void) close(fd);
	return 1;
}

exists(fname)
char *fname;
{
	int fd;

	fd = open(fname, 0);
	if (fd < 0)
		return 0;
	(void) close(fd);
	return 1;
}

int	lockcount = 0;			/* no. of times we've called lock */

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
				(void) unlink(SUBLOCK);
				logerr("News system locked up");
			}
			if (i < -3)
				xerror("Unable to unlock news system");
			sleep((unsigned)1);
		}
		(void) close(fd);
	}
}

unlock()
{
	if (--lockcount == 0)
		(void) unlink(SUBLOCK); 
}

#else /* !VMS */

/*
 * Newsystem locking.
 */

#if defined(BSD4_2) || defined(LOCKF)
#ifdef LOCKF
#include <unistd.h>
#else /* !LOCKF */
#include <sys/file.h>
#endif /* !LOCKF */
static int LockFd = -1;
lock()
{
	LockFd = open(SUBFILE, 2);
	if (LockFd < 0)
		logerr("Can't open(\"%s\", 2) to lock", SUBFILE);
	/* This will sleep until the other program releases the lock */
	/* We may need to alarm out of this, but I don't think so */
#ifdef LOCKF
	if (lockf(LockFd, F_LOCK, 0L) < 0)
#else
	if (flock(LockFd, LOCK_EX) < 0)
#endif
		xerror("Can't get lock on %s: %s", SUBFILE, errmsg(errno));
}

unlock()
{
	(void) close(LockFd);
}
#else /* !BSD4_2 */
lock()
{
	register int i;
	extern int errno;

	if (lockcount++ == 0) {
		i = DEADTIME;
		while (link(SUBFILE, LOCKFILE)) {
			if (errno != EEXIST)
				break;
			if (--i < 0)
				xerror("News system locked up");
			sleep((unsigned)1);
		}
	}
}

unlock()
{
	if (--lockcount == 0)
		(void) unlink(LOCKFILE);
}
#endif /* !BSD4_2 */
#endif /* !VMS */

/* VARARGS1 */
error(message, arg1, arg2, arg3)
char *message;
long arg1, arg2, arg3;
{
	char buffer[LBUFLEN];

	fflush(stdout);
	(void) sprintf(buffer, message, arg1, arg2, arg3);
	logerr(buffer);
	xxit(mode == PROC ? 0 : 1);
}
