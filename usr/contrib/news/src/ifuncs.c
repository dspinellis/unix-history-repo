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
static char	*SccsId = "@(#)ifuncs.c	2.51	3/19/86";
#endif /* SCCSID */

#include "iparams.h"
#include <errno.h>
#include <ctype.h>

/*LINTLIBRARY*/

#define AFSIZ	4000	/* size of text in the active file for initial malloc */

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

#ifndef DBM
char *histfile();
#endif /* !DBM */

broadcast()
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

	/* h is a local copy of the header we can scribble on */
	fp = xfopen(ARTICLE, "r");
	if (hread(&h, fp, TRUE) == NULL)
		xerror("Cannot reread article");
	(void) fclose(fp);

	(void) strcpy(sentbuf, h.ident);
	(void) strcat(sentbuf, " sent to ");
	sentsys = index(sentbuf, 0);
	nsent = 0;
	/* break path into list of systems. */
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
#ifdef HIDDENNET
		if (strncmp(srec.s_name, LOCALSYSNAME, SNLN) == 0)
			continue;
#endif /* HIDDENNET */
		if (strncmp(srec.s_name, FULLSYSNAME, SNLN) == 0)
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
			if (strncmp(srec.s_name, hptr, SNLN) == 0)
				goto contin;
			if (sptr = srec.s_nosend) {
				while (*sptr != '\0') {
					if (strncmp(sptr, hptr, SNLN) == 0)
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
		if (h.distribution[0] != '\0' &&
			!ngmatch(h.distribution, srec.s_nbuf) &&
			!ngmatch(srec.s_nbuf, h.distribution))
			    continue;
		if (nsent) {
			hptr = sentsys;
			while ((sptr = index(hptr, ',')) != NULL) {
				*sptr = '\0';
				if (strcmp(hptr, srec.s_name) == 0) {
					*sptr = ',';
					goto contin;
				}
				*sptr++ = ',';
				for (hptr = sptr; isspace(*hptr); hptr++)
					;
			}
			if (strcmp(hptr, srec.s_name) == 0)
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
				if (strcmp(srec.s_xmit, m->mc_name) == 0)
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
				if (strcmp(m->mc_name, srec.s_name) == 0) {
					yptr = sysptrs;
					while (mc < m->mc_syscnt)
						*yptr++ = m->mc_tosys[mc++];
					break;
				}
			if (!transmit(&srec,xfopen(ARTICLE,"r"),1,sysptrs,mc))
				continue;
		}
#else /* !MULTICAST */
		if (!transmit(&srec, xfopen(ARTICLE, "r"), 1, (char **)0, 0))
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

#ifdef DEBUG
	printf("Transmitting to '%s'\n", sp->s_name);
#endif /* DEBUG */

#ifdef MULTICAST
	if (multisend && mc == 0) {
		(void) fclose(ifp);
		return FALSE;
	}
#endif /* MULTICAST */

	if (!appfile && !useexist && !history) {
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
		(void) sprintf(hh.title, "ihave %s %s", hh.ident, FULLSYSNAME);
		(void) sprintf(hh.nbuf, "to.%s.ctl", sp->s_name);
		(void) strcpy(oldid, hh.ident);
		getident(&hh);
		log("tell %s about %s, notif. id %s",
			sp->s_name, oldid, hh.ident);
	}

	if (appfile) {
		if (firstbufname[0] == '\0') {
			extern char histline[];
			localize("junk");
			savehist(histline);
			xerror("No file name to xmit from");
		}
		if (sp->s_xmit[0] == '\0')
			sprintf(sp->s_xmit, "%s/%s", BATCHDIR, sp->s_name);
#ifdef IHCC
		(void) sprintf(TRANS, "%s/%s/%s", logdir(HOME), BATCHDIR, sp->s_xmit);
		ofp = fopen(TRANS, "a");
#else /* !IHCC */
		ofp = fopen(sp->s_xmit, "a");
#endif /* !IHCC */
		if (ofp == NULL)
			xerror("Cannot append to %s", sp->s_xmit);
#ifdef MULTICAST
		fprintf(ofp, "%s", firstbufname);
		while (--mc >= 0)
			fprintf(ofp, " %s", *sysnames++);
		fprintf(ofp, "\n");
#else /* !MULTICAST */
		fprintf(ofp, "%s\n", firstbufname);
#endif /* !MULTICAST */
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
			fprintf(ofp, "A%s\n%s\n%s!%s\n%s\n%s\n", oident(hh.ident), hh.nbuf, FULLSYSNAME,
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
		if (pid = fork())
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
			execv(argv[0], argv);
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
	if (rhs.dptr)
		return(TRUE);
#else /* !DBM */
	hfp = xfopen(histfile(lcident), "r");
	while (fgets(bfr, BUFLEN, hfp) != NULL) {
		p = index(bfr, '\t');
		if (p == NULL)
			p = index(bfr, '\n');
		if (p != NULL)	/* can happen if nulls in file */
			*p = 0;
		lcase(bfr);

		if (strcmp(bfr, lcident) == 0) {
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
	datum lhs, rhs;
	long fpos;
	register char *p;

	hfp = xfopen(ARTFILE, "a");
	fpos = ftell(hfp);
	fprintf(hfp, "%s\n", hline);
	(void) fclose(hfp);
#ifdef DBM
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
#else /* !DBM */
	/* also append to proper history subfile */
	hfp = xfopen(histfile(hline), "a");
	fprintf(hfp, "%s\n", hline);
	(void) fclose(hfp);
#endif /* !DBM */
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
			xerror("Cannot parse submittal date '%s'", hp->subdate);
		}
	} else {
		(void) time(&edt);
		(void) strcpy(hp->subdate, arpadate(&edt));
	}
}

char lockname[80];
idlock(str)
char *str;
{
	register int i;
	char tempname[80];
	time_t now;
	struct stat sbuf;
	extern int errno;
#ifdef	VMS
	int fd;

	(void) sprintf(lockname, "/tmp/%s.l.1", str);
	if ((fd = creat(lockname, 0444)) < 0) {
#else /* !VMS */
	(void) strcpy(tempname, "/tmp/LTMP.XXXXXX");
	(void) mktemp(tempname);
	(void) sprintf(lockname, "/tmp/L%s", str);
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
		i = stat(lockname, &sbuf);
		if (i < 0) {
			xerror("Directory permission problem in /tmp");
		}
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

	lock();
	fp = xfopen(SEQFILE, "r");
	(void) fgets(bfr, BUFLEN, fp);
	(void) fclose(fp);
	seqn = atol(bfr) + 1;
#ifdef	VMS
	(void) unlink(SEQFILE);
#endif /* VMS */
	fp = xfopen(SEQFILE, "r+w");
	fprintf(fp, "%ld\n", seqn);
	(void) fclose(fp);
	unlock();
#ifdef HIDDENNET
	if (strcmp(LOCALSYSNAME, FULLSYSNAME))
		(void) sprintf(hp->ident, "<%ld@%s.%s%s>", seqn, LOCALSYSNAME, FULLSYSNAME,
		MYDOMAIN);
	else
#endif /* !HIDDENNET */
	(void) sprintf(hp->ident, "<%ld@%s%s>", seqn, FULLSYSNAME, MYDOMAIN);
}

/*
 * Check that header.nbuf contains only valid newsgroup names;
 * exit with error if not valid.
 *
 */
ngfcheck(isproc)
int isproc;
{
	register char *s1, *s2;
	register FILE *f;
	register char *os1;
	int ngroups = 1;
	unsigned int ngsize = AFSIZ;
	char tbuf[BUFLEN], *ngcheck;

	f = xfopen(ACTIVE, "r");
	ngcheck = malloc(ngsize);
	if (ngcheck == NULL)
		xerror("Can't malloc the active file");
	s1 = ngcheck;
	while (fgets(bfr, BUFLEN, f) != NULL) {
		os1 = s1;
		for(s2 = bfr; *s2 != '\0' && *s2 != ' ';) {
			if (s1 >= &ngcheck[ngsize-2]) {
				unsigned int offs = s1 - ngcheck;
				ngsize += LBUFLEN;
				ngcheck = realloc(ngcheck, ngsize);
				if (ngcheck == NULL)
					xerror("Can't realloc active file");
				s1 = ngcheck + offs;
			}
			*s1++ = *s2++;
		}
		*s1++ = '\0';
		if (isproc) /* don't check to see if can post to this group */
			continue;
		while (*s2++ != '\0' && *s2 != ' ')
			;	/* skip max article number */
		while (*s2++ != '\0' && *s2 != ' ')
			;	/* skip min article number */
		if (*s2++ != '\0' && *s2 == 'n')
			s1 = os1;	/* can't post to this group */
	}
	*s1++ = '\0';
	*s1 = '\0';
	(void) fclose(f);

	s1 = header.nbuf;
	s2 = nbuf;
	while (*s1 == NGDELIM || *s1 == ' ')
		s1++;	/* skip leading junk */
	do {
		/* there shouldn't be blanks, but give the jerk a break */
		if (*s1 == NGDELIM || *s1 == ' ') {
			*s2++ = '\0';
			while (*++s1 == NGDELIM || *s1 == ' ')
				;	/* remove extra commas */
			if (*s1 != '\0')
				ngroups++;
		} else
			*s2++ = *s1++;
	} while (*s1 != '\0');
	if (s2[-1] == NGDELIM)	/* strip trailing commas */
		s2--;
	*s2 = '\0';

	s1 = nbuf;
	while (*s1 != '\0') {	/* for each newsgroup in header */
		s2 = ngcheck;
		while (*s2 != '\0') { /* for each newsgroup in active file */
			if (strcmp(s1, s2) == 0)
				break;
			while (*s2++ != '\0')
				;
		}
		if (*s2 == '\0') {	/* not found. remove it */
			if (!isproc) {
				logerr("Invalid news group '%s'", s1);
				newssave(stdin, (char *)NULL);
			}
			/* See if it's in our alias list */
			f = xfopen(ALIASES,"r");
			while (fscanf(f,"%s %s", tbuf, bfr) == 2
				&& strcmp(s1, tbuf))
				;
			(void) fclose(f);
			if (strcmp(s1, tbuf) == 0) {
				logerr("Aliased newsgroup '%s' to '%s'", s1, bfr);
				os1 = s1;
				s1 = nbuf;
				s2 = tbuf;
				while (s1 < os1) /* copy left part */
					*s2++ = *s1++;
				s1 = bfr;
				while (*s1 != '\0') /* copy alias */
					*s2++ = *s1++;
				*s2++ = '\0';
				s1 = os1;
				os1 = nbuf + (s2 - tbuf);
				while (*s1++ != '\0') /* skip old group */
					;
				/* copy right part */
				tbufcpy(s2, s1);
				/* copy back to original buffer */
				tbufcpy(nbuf, tbuf);
				s1 = os1;
			} else {
				logerr("Unknown newsgroup '%s' removed", s1);
				s2 = s1;
				while (*s2++ != '\0')	/* skip the bad one */
					;
				tbufcpy(s1, s2);
			}
		} else { /* It's in our active file */
			os1 = s1;
			while (*s1++ != '\0')
				;
			/* check for local only distribution on incoming
			   newsgroups. This might occur if someone posted to
			   general,net.unix */
			if(isproc && ngroups > 1 && index(os1, '.') == NULL
				&& index(header.nbuf, '.') != NULL) {
				logerr("Local group '%s' removed", os1);
				tbufcpy(os1, s1);
				s1 = os1;
			}
		}
	}
	/*  remove any duplicates */
	os1 = s1 = nbuf;
	for(;;) {
		if (*s1++ == '\0') {
			if (*s1 == '\0')
				break;
			s2 = s1;
			while (*s2 != '\0') {
				if (strcmp(os1, s2) == 0) {
					logerr("Duplicate '%s' removed",os1);
					os1 = s2;
					while (*s2++ != '\0') /* skip it */
						;
					tbufcpy(os1, s2);
				} else
					while (*s2++ != '\0')
						;
			}
			os1 = s1;
			s1[-1] = '\0';
		}
	}
	if (nbuf[0] != '\0') {
		s1 = header.nbuf;
		s2 = nbuf;
		do {
			while (*s2 != '\0')
				*s1++ = *s2++;
			*s1++ = NGDELIM;
		} while (*++s2 != '\0');
		*--s1 = '\0';
		(void) free(ngcheck);
		return FALSE;
	}
	(void) free(ngcheck);
	return TRUE;
}

tbufcpy(s1, s2)
register char *s1, *s2;
{
	do {
		while (*s2 != '\0')
			*s1++ = *s2++;
		*s1++ = '\0';
	} while (*++s2 != '\0');
	*s1 = '\0';
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

	fn = getenv("NAME");

	if (fn == NULL) {
		(void) sprintf(buf, "%s/%s", userhome, ".name");
		fd = open(buf, 0);
		if (fd >= 0) {
			n = read(fd, buf, sizeof buf);
			(void) close(fd);
			if (n > 0 && buf[0] >= 'A') {
				fn = buf;
				for (p=fn; *p; p++)
					if (*p < ' ')
						*p = '\0';
			}
		}
	}

	if (fn == NULL)
		fn = fullname(logname);

	(void) sprintf(hp->path, "%s", logname);
	(void) sprintf(hp->from, "%s@%s%s (%s)", logname, FULLSYSNAME, MYDOMAIN, fn);
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
		logerr("inews ran away looping on signal %d", n);
		xxit(1);
	}
	(void) signal(n, onsig);
	SigTrap = n;
}

#ifdef BATCH
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
 * and you are security conscious, do not use /bin/sh as the unbatcher.
 * the thing to do is to change BATCH in your localize.sh file from /bin/sh
 * to some restricted shell which can only run rnews.
 */
checkbatch()
{
	int c;

	c = getc(stdin);
	if (c != EOF)
		(void) ungetc(c, stdin);
	clearerr(stdin);
	if (c == '#') {
		char unbatcher[BUFLEN];

		(void) sprintf(unbatcher, "%s/%s", LIB, BATCH);
		reset_stdin();
		execl(unbatcher, "news-unpack", (char *)0);
		xerror("Unable to exec shell to unpack news.");
	}
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
	long lseek();

	/* First try to seek back - if so, it's a cheap way back. */
	if (lseek(0, 0L, 0) == 0L)
		return;

	/* Can't seek, so have to copy input to a file and use that. */
	ofdname = "/tmp/inewsXXXXXX";
	(void) mktemp(ofdname);
	ofd = fopen(ofdname, "w");
	while ((c=getc(stdin)) != EOF)
		putc(c, ofd);
	if (ferror(ofd))
		xerror("write failed on temp file %s", ofdname);
	(void) fclose(stdin);
	(void) fclose(ofd);

	/* Now for a few lower level hacks to reopen stdin and make
	 * absolutely sure that the right fd's are done for the exec.
	 */
	(void) close(0);		/* make sure stdin is really closed. */
	(void) open(ofdname, 0);	/* should return zero */
	(void) unlink(ofdname);		/* to avoid cleaning it up later. */
}
#endif /* BATCH */

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

#ifdef BSD4_2
#include <sys/file.h>
static int LockFd = -1;
lock()
{
	LockFd = open(SUBFILE,0);
	/* This will sleep until the other program releases the lock */
	/* We may need to alarm out of this, but I don't think so */
	(void) flock(LockFd, LOCK_EX);
}

unlock()
{
	(void) flock(LockFd, LOCK_UN);
	(void) close(LockFd);
}
#else /* !BSD4_2 */
/* Why doesn't USG unix have file locking????? */
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

/*
 * Generate the name of the person responsible for posting this article,
 * in order to check that two articles were posted by the same person.
 */
char *
senderof(hp)
struct hbuf *hp;
{
	char *q, *tp;
	char *tailpath();

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

	q = malloc((unsigned)(strlen(tp) + 1));
	(void) strcpy(q, tp);
	return q;
}
