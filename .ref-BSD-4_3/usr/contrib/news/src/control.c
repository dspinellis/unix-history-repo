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
 * Control message handling code.  Deal with messages which are to be
 * acted on by netnews itself rather than by people.
 *
 * See defs.h "news_version" for the real version of netnews.
 */

#ifdef SCCSID
static char	*SccsId = "@(#)control.c	2.43	3/19/86";
#endif /* SCCSID */

#include "iparams.h"

#define eq(msg) (strcmp(msg, cargv[0]) == 0)

int cargc;
char **cargv;

FILE *hfopen();
FILE *popen(), *mhopen(), *mailhdr();

char *senderof();
#ifdef u370
static struct hbuf htmp;
#endif /* u370 */

control(h)
struct hbuf *h;
{
	register char *ctlmsgtext;

	if (strncmp(h->title, "cmsg ", 5) == 0) {
		register char *cp1, *cp2;
		cp1 = h->title;
		cp2 = h->title + 5;
		while (*cp1++ = *cp2++)
			;
	}

	if (*h->ctlmsg)
		ctlmsgtext = h->ctlmsg;
	else
		ctlmsgtext = h->title;
	log("Ctl Msg %s from %s: %s", h->nbuf, h->path, ctlmsgtext);
	/*
	 * Control messages have the standard format
	 *	command [args]
	 * much like shell commands.  Each site has the option
	 * of customizing this code to deal with control messages
	 * as they see fit, but we would like to buy back the
	 * code, ifdeffed or otherwise parameterized, to simplify
	 * the maintenence issues.
	 */
	argparse(ctlmsgtext);
	
	if (eq("cancel"))
		return c_cancel(cargc, cargv);
	else if (eq("newgroup"))
		c_newgroup(cargc, cargv);
	else if (eq("ihave"))
		c_ihave(cargc, cargv);
	else if (eq("sendme"))
		c_sendme(cargc, cargv);
	else if (eq("sendbad"))
		c_sendme(cargc, cargv);
	else if (eq("rmgroup"))
		c_rmgroup(cargc, cargv);
	else if (eq("sendsys"))
		c_sendsys(cargc, cargv);
	else if (eq("senduuname"))
		c_senduuname(cargc, cargv);
	else if (eq("version"))
		c_version(cargc, cargv);
	else if (eq("checkgroups"))
		c_checkgroups(cargc, cargv);
	else if (eq("delsub"))
		c_unimp(cargc, cargv);
	else
		c_unknown(h, ctlmsgtext);
	return 0;
}

/*
 * Parse the string str into separate words in cargc and cargv
 * as per the usual UNIX convention.  Nothing fancy here, just
 * blanks and tabs separating words.
 */
argparse(str)
char *str;
{
	static char *cavpbuf[20];
	static char cavbuf[256];
	char *nextfree = cavbuf;

	if (str == '\0')
		xerror("Control message %s has no title", header.ident);
	cargc = (*str != '\0');
	cargv = cavpbuf;
	cargv[0] = cavbuf;

	while (*str) {
		if (*str <= ' ') {
			*nextfree++ = 0;
			cargv[cargc] = nextfree;
			cargc++;
			/* skip over white space */
			while (*str != '\0' && *str <= ' ')
				str++;
			if (*str == '\0')	/* line ends in white space */
				return;
		} else
			*nextfree++ = *str++;
	}
}

/*
 * ihave <artid> <remotesys>
 * The other system is telling you it has article <artid>, in case
 * you decide you want it to transmit it to you.
 */
c_ihave(argc, argv)
char **argv;
{
	char tl[256], ng[256];
#ifndef u370
	struct hbuf htmp;
#endif /* !u370 */

	if (argc < 2)
		xerror("ihave: Not enough arguments.");
	/*
	 * Check that we haven't already seen it (history)
	 * and then send back a "sendme" message if we subscribe.
	 */
	(void) strncpy(htmp.ident, argv[1], BUFLEN);
	if (history(&htmp) == 0) {
		/* Should probably check SUBFILE here. */
		(void) sprintf(tl, "sendme %s %s", argv[1], FULLSYSNAME);
		(void) sprintf(ng, "to.%s.ctl", argv[2]);
		xmitmsg(argv[2], tl, ng);
	}
}

/*
 * sendme <artid> ... <remotesys>
 * The other system wants me to send him article <artid>.
 */
c_sendme(argc, argv)
char **argv;
{
	int i;
	FILE *fp;
	struct srec srec;
#ifndef u370
	struct hbuf htmp;
#endif /* !u370 */

	if (argc < 2)
		xerror("sendme: Not enough arguments.");
	/* Don't ask for it from myself */
	if (strncmp(FULLSYSNAME, argv[argc], SNLN) == 0)
		return;
	/* Find the sys record */
	s_openr();
	while (s_read(&srec)) {
		if (strncmp(srec.s_name, argv[argc], SNLN))
			continue;
		/* It's the right one.  Send them. */
		for (i=1; i<argc; i++) {
			fp = hfopen(argv[i]);
			htmp.unrec[0] = NULL;
			if (hread(&htmp, fp, TRUE) == NULL) {
				if (bfr[0] == '/') {
					fp = xfopen(bfr, "r");
					if (hread(&htmp, fp, TRUE) == NULL)
						xerror("Article %s is garbled.", bfr);
				} else
					xerror("Article %s is garbled.", argv[i]);
			}
			(void) fseek(fp, 0L, 0);
			if (strcmp(argv[0], "sendme") == 0) {
				/* check that other sys subscribes. */
				if (!ngmatch(htmp.nbuf, srec.s_nbuf) || 
					!(htmp.distribution[0] == '\0' ||
					ngmatch(htmp.distribution, srec.s_nbuf)))
					continue;
			}
			transmit(&srec, fp, 0, (char **)0, 0);
			/* transmit does fclose(fp) */
		}
		return;
	}
	xerror("Cannot find system %s to send article %s to.", argv[argc],
		argv[1]);
}

/*
 * newgroup <groupname>
 * A new newsgroup has been created.
 * The body of the article, if present, is a description of the
 * purpose of the newsgroup.
 *
 */
c_newgroup(argc, argv)
char **argv;
{
	FILE *fd;
	int didcreate = 0;

	if (argc < 1)
		xerror("newgroup: Not enough arguments.");
	if (validng(argv[1]))
		return;
	if (header.approved[0] == '\0')
		xerror("newgroup: %s not approved", argv[1]);

#ifndef NONEWGROUPS
#ifdef	ORGDISTRIB
	if (!strcmp(ORGDISTRIB, header.distribution)) {
		didcreate++;
#endif	/* ORGDISTRIB */
		actfp = xfopen(ACTIVE, "a");
		fprintf(actfp, "%s 00000 00001 y\n", argv[1]);
		(void) fclose(actfp);
#ifdef	ORGDISTRIB
	}
#endif	/* ORGDISTRIB */
#endif /* !NONEWGROUPS */

#ifdef NOTIFY
	fd = mailhdr((struct hbuf *)NULL, "creation of new newsgroup");
	if (fd != NULL) {
		if (didcreate) 
			fprintf(fd, "A new newsgroup called '%s' has been created by %s.\n",
			argv[1], header.path);
		else
			fprintf(fd, "%s requested that a new newsgroup called '%s' be created.\n",
			header.path, argv[1]);
		fprintf(fd, "It was approved by %s\n\n", header.approved);
#ifdef ORGDISTRIB
		    fprintf(fd, "You can accomplish this by sending a newgroup control message with a\n");
		    fprintf(fd, "distribution code of %s; in other words, by executing the command:\n", ORGDISTRIB);
		    fprintf(fd, "%s/inews -n net.news -d %s -t \"cmsg newgroup %s\"\n", 
			LIB, ORGDISTRIB, argv[1]);
#endif /* ORGDISTRIB */
		(void) mclose(fd);
	}
}
#endif /* NOTIFY */


/*
 * rmgroup <groupname>
 * An old newsgroup is being cancelled on a network wide basis.
 */
c_rmgroup(argc, argv)
char **argv;
{
	FILE *fd;
	int shouldremove = 0;

	if (argc < 1)
		xerror("rmgroup: Not enough arguments.");
	if (!validng(argv[1]))
		return;
	if (header.approved[0] == '\0')
		xerror("rmgroup: %s not approved", argv[1]);

#ifndef MANUALLY
#ifdef ORGDISTRIB
	if (!strcmp(ORGDISTRIB, header.distribution))
#endif	/* ORGDISTRIB */		
	shouldremove++;
#endif /* !MANUALLY */
#ifdef NOTIFY
	fd = mailhdr((struct hbuf *)NULL, "rmgroup control message");
	if (fd != NULL) {
		if (shouldremove) {
			fprintf(fd, "A newsgroup called '%s' has been removed by %s.\n\n",
				argv[1], header.path);
#  ifdef USG
			fprintf(fd, "You may need to remove the directory %s by hand\n",
				dirname(argv[1]));
#  endif
		} else {
			fprintf(fd, "%s has requested that newsgroup %s be removed.\n",
				header.path, argv[1]);
			fprintf(fd, "You should remove it by hand\n");
			fprintf(fd, "To do this, execute the command\n");
			fprintf(fd, "\t%s/rmgroup %s\n", LIB, argv[1]);
		}
		(void) mclose(fd);
	}
#endif /* NOTIFY */

	if (shouldremove) {
		int rc;
		/* We let the shell do all the work.
		 * See the rmgrp shell script. */
		(void) setuid(geteuid()); /* otherwise it won't rmdir the dir */
		(void) sprintf(bfr, "exec %s/rmgroup %s", LIB, argv[1]);
		rc = system(bfr);
		log("system(%s) status %d", bfr, rc);
	}
}

/*
 * cancel <artid>
 * Cancel the named article
 */
c_cancel(argc, argv)
char **argv;
{
	char *line, *p, *q, *r, *poster;
	char *findhist();
	register FILE *fp;
	char whatsisname[BUFLEN], nfilename[BUFLEN];
	time_t t;
	int su = 0;
#ifndef u370
	struct hbuf htmp;
#endif /* !u370 */

	if (argc < 1)
		xerror("cancel: Not enough arguments.");
	(void) strcpy(whatsisname, senderof(&header));
	line = findhist(argv[1]);
	if (line == NULL) {
		struct tm *tm;
		log("Can't cancel %s:  non-existent", argv[1]);
		(void) time(&t);
		tm = localtime(&t);
#ifdef USG
		sprintf(bfr,"%s\t%2.2d/%2.2d/%d %2.2d:%2.2d\tcancelled",
#else /* !USG */
		sprintf(bfr,"%s\t%02d/%02d/%d %02d:%02d\tcancelled",
#endif /* !USG */
		   argv[1], tm->tm_mon+1, tm->tm_mday, tm->tm_year, tm->tm_hour,
		   tm->tm_min);
		savehist(bfr);
		return 1;
	}

	q = index(line, '\t');
	p = index(q+1, '\t');
	if (p == NULL || *++p == '\0' || *p == '\n') {
		*q = '\0';
		log("Expired article %s", line);
		return 1;
	}
	if (strcmp(p, "cancelled") == 0) {
		*q = '\0';
		log("Already Cancelled %s", line);
		return 1;
	} else
		log("Cancelling %s", line);
	if ((uid == ROOTID||uid == 0) && strcmp(header.distribution, "local") == 0)
		su = 1;
	while (*p) {
		q = index(p, ' ');
		if (q)
			*q = '\0';
		(void) strcpy(nfilename, dirname(p));
		fp = fopen(nfilename, "r");
		if (fp == NULL) {
			log("Already Cancelled %s", line);
			return 1;
		}
		htmp.unrec[0] = NULL;
		if (hread(&htmp, fp, TRUE) == NULL) {
			if (bfr[0] == '/') {
				fp = fopen(bfr, "r");
				if (fp == NULL
					|| hread(&htmp, fp, TRUE) == NULL)
					xerror("Article is garbled.");
			} else 
				xerror("Article is garbled.");
		}
		(void) fclose(fp);
		poster = senderof(&htmp);
		/* only compare up to '.' or ' ' */
		r = index(poster,'.');
		if (r == NULL)
			r = index(poster,' ');
		if (r != NULL)
			*r = '\0';
		if (!su && strncmp(whatsisname, poster,strlen(poster))) {
			xerror("Not contributor: posted by %s, and you are %s", poster, whatsisname);
		}

		(void) unlink(nfilename);
		p = q+1;
	}
	return 0;
}

/*
 * sendsys	(no arguments)
 *
 * Mail the sys file to the person submitting the article.
 * POLICY: the contents of your sys file are public information
 * and as such, you should not change this code.  You may feel
 * free to arrange for it to manually notify you, in the event
 * that you want to do something to clean it up before it goes out.
 * Secret sites on the net are expressly frowned on.
 * 
 * The purpose of this command is for making a network map.  The
 * details of your link and which newsgroups are forwarded are not
 * important, in case you want to sanitize them.  Since the definition
 * of USENET is those sites getting net.announce, you can disable this
 * on sites not getting net articles, but if you take out the list of
 * forwarded newsgroups, and you have sites that only get local newsgroups,
 * you should make this clear, or remove those sites from what you send out.
 */
/* ARGSUSED */
c_sendsys(argc, argv)
char **argv;
{
	register FILE *f, *u;
	int c;

#ifdef NOTIFY
	f = mailhdr((struct hbuf *)NULL, "sendsys control message");
	if (f != NULL) {
		fprintf(f, "%s requested your %s/sys file.\n", header.path, LIB);
		fprintf(f, "It has been sent.\n");
		(void) mclose(f);
	}
#endif
	f = mailhdr(&header, "response to your sendsys request");
	u = fopen(SUBFILE, "r");
	if (f != NULL && u != NULL) {
		while ((c=getc(u)) != EOF)
			putc(c, f);
		(void) fclose(u);
		(void) mclose(f);
	}
}

/*
 * senduuname	(no arguments)
 *
 * Run the "uuname" command and send it back to the person who submitted
 * the article.  The purpose of this control message is for attempting to
 * make a uucp net map.
 *
 * POLICY: If you view this information as not public (because you have
 * a connection you consider secret, or know a site that considers itself
 * secret) you can feel free to change this code in whatever way is
 * appropriate, so long as it sends some response back to the sender.  If
 * you don't run uucp, this code does not make sense, and so an error
 * message (or garbage, such as "research") will be mailed back.
 *
 * If you wish to add or remove sites from the output of uuname, you
 * may wish to use the euuname.sh shell script here.
 */
/* ARGSUSED */
c_senduuname(argc, argv)
char **argv;
{
	char buf[256];
	FILE *fd, *u;
	int c;

#ifdef NOTIFY
	fd = mailhdr((struct hbuf *)NULL, "uuname control message");
	fprintf(fd, "%s requested your uuname output\n", header.path);
	(void) mclose(fd);
#endif
	fd = mailhdr(&header, "response to your senduuname request");
#ifdef UUPROG
	if (UUPROG[0] == '/')
		(void) strcpy(buf, UUPROG);
	else
		(void) sprintf(buf, "%s/%s", LIB, UUPROG);
#else
	(void) strcpy(buf, "uuname");
#endif
	u = popen(buf, "r");
	if (fd != NULL && u != NULL) {
		while ((c=getc(u)) != EOF)
			putc(c, fd);
		(void) pclose(u);
		(void) mclose(fd);
	}
}

/*
 * Send the version number to the right person.
 */
/* ARGSUSED */
c_version(argc, argv)
char **argv;
{
	register FILE *f;

	f = mailhdr(&header, "Our news version");
	if (f == NULL)
		xerror("Cannot send back error message");
	fprintf(f, "Currently running news version %s.\n\n", news_version);
	fprintf(f, "The header of your message follows:\n\n");
	(void) hwrite(&header, f);
	(void) mclose(f);
}

/*
 * Check the active file for old or missing newsgroups
 * Body of article is list of valid groups
 */
/* ARGSUSED */
c_checkgroups(argc, argv)
char **argv;
{
	int rc;

	(void) setuid(geteuid());
	/* dont change the cat %s| to < %s, it breaks some "unix" systems */
	(void) sprintf(bfr, "cat %s | %s/checkgroups %s", INFILE, LIB,
#ifdef NOTIFY
		(TELLME && *TELLME) ? TELLME : NEWSUSR );
#else /* !NOTIFY */
		NEWSUSR);
#endif /* !NOTIFY */
	rc = system(bfr);
	log("system(%s) status %d", bfr, rc);
}

/*
 * An unknown control message has been received.
 */
c_unknown(h, ctlmsgtext)
struct hbuf *h;
char *ctlmsgtext;
{
	register FILE *f;

	log("UNKNOWN Ctl Msg %s from %s", ctlmsgtext, h->path);
#ifdef NOTIFY
	f = mailhdr((struct hbuf *)NULL, "Unrecognized Control Message");
	if (f != NULL) {
		fprintf(f, "Currently running news version %s.\n\n", news_version);
		fprintf(f, "The header of the message follows:\n\n");
		(void) hwrite(h, f);
		(void) mclose(f);
	}
#endif /* NOTIFY */
}

/* ARGSUSED */
c_unimp(argc, argv)
char **argv;
{
	register FILE *f;

#ifdef NOTIFY
	f = mailhdr((struct hbuf*)NULL, "Unimplemented Control Message");
	if (f != NULL) {
		fprintf(f, "Currently running news version B %s.\n\n", news_version);
		fprintf(f, "The header of the message follows:\n\n");
		(void) hwrite(&header, f);
		(void) mclose(f);
	}
#endif /* NOTIFY */
}

xmitmsg(tosys, title, ng)
char *tosys, *title, *ng;
{
#ifndef u370
	struct hbuf htmp;
#endif /* !u370 */
	struct srec srec;
	FILE *tfp;
	char *fname;

	/* Make an article called ARTICLE */
	(void) sprintf(htmp.from, "%s@%s%s", "usenet", FULLSYSNAME, MYDOMAIN);
	(void) strcpy(htmp.path, NEWSUSR);
	(void) strcpy(htmp.nbuf, ng);
	(void) strcpy(htmp.title, title);
	(void) strcpy(htmp.ctlmsg, title);
	(void) strcpy(htmp.subdate, "");
	(void) strcpy(htmp.expdate, "");
	getident(&htmp);
	dates(&htmp);
	tfp = xfopen(fname = mktemp("/tmp/xmsgXXXXXX"), "w");
	hwrite(&htmp, tfp);
	(void) fclose(tfp);

	/* Find the sys record */
	s_openr();
	while (s_read(&srec)) {
		if (strncmp(srec.s_name, tosys, SNLN))
			continue;
		tfp = xfopen(fname, "r");
		(void) transmit(&srec, tfp, 0, (char **)0, 0);
		(void) unlink(fname);
		return;
	}
	log("Can't find sys record for %s", tosys);
	xerror("Cannot find sys record");
}

/*
 * This is a modified version of popen, made more secure.  Rather than
 * forking off a shell, you get a bare process.  You must have exactly
 * one argument, and the command must be mail (or sendmail if you have it).
 */
#define	RDR	0
#define	WTR	1
static	int	mopen_pid[20];
char *replyname();

FILE *
mhopen(hptr)
struct hbuf *hptr;
{
	int p[2];
	register myside, hisside, pid;
	char *sendto = "usenet";

	if (hptr)
		sendto = replyname(hptr);
	else {
#ifdef NOTIFY
		if (TELLME && *TELLME)
			sendto = TELLME;
#endif /* NOTIFY */
		if (sendto == NULL)
			return NULL;
	}
	verifyname(sendto);
	if(pipe(p) < 0)
		return NULL;
	myside = p[WTR];
	hisside = p[RDR];
	if((pid = fork()) == 0) {
		/* myside and hisside reverse roles in child */
		(void) close(myside);
		(void) close(0);
		(void) dup(hisside);
		(void) close(hisside);
		(void) setgid(gid);
		(void) setuid(uid);
#ifdef SENDMAIL
		execl(SENDMAIL, "sendmail", "-oi", "-oeq", sendto, (char *)NULL);
#endif /* SENDMAIL */
#ifdef MMDF
		execl(MMDF, "inews-mail", "-smuxto,cc*", (char *)NULL);
#endif /* MMDF */
		execl("/bin/mail", "mail", sendto, (char *)NULL);
		execl("/usr/bin/mail", "mail", sendto, (char *)NULL);
		execl("/usr/ucb/mail", "mail", sendto, (char *)NULL);
		_exit(1);
	}
	if(pid == -1)
		return NULL;
	mopen_pid[myside] = pid;
	(void) close(hisside);
	return(fdopen(myside, "w"));
}

mclose(ptr)
FILE *ptr;
{
	register f, r, (*hstat)(), (*istat)(), (*qstat)();
	int status;

	f = fileno(ptr);
	(void) fclose(ptr);
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);
	hstat = signal(SIGHUP, SIG_IGN);
	while((r = wait(&status)) != mopen_pid[f] && r != -1)
		;
	if(r == -1)
		status = -1;
	signal(SIGINT, istat);
	signal(SIGQUIT, qstat);
	signal(SIGHUP, hstat);
	return status;
}

/*
 * mhopen a pipe to mail, write out a std header, and return the file ptr.
 *
 * We don't include a From: field because this is probably uucp, i.e.
 * explicitly routed.  Leave it up to the recipient's mailer.
 * Always include the To: field because if we ge back failed mail, we
 * might be able to deliver it by hand if we know to wom it was addressed.
 * By convention, hptr==NULL means to send the message to the local contact person.
 */
FILE *
mailhdr(hptr, subject)
struct hbuf *hptr;
char  *subject;
{
	FILE *fp;
	time_t now;
	char *to = "usenet";

#ifdef NOTIFY
	if (TELLME && *TELLME)
		to = TELLME;
#endif /* NOTIFY */
	if (hptr)
		to = replyname(hptr);

	if ((fp = mhopen(hptr)) != NULL) {
		(void) time(&now);
		fprintf(fp, "Date: %s\n", arpadate(&now));
#ifdef MMDF
		fprintf(fp, "From: The News System <usenet@%s%s>\n",
				FULLSYSNAME, MYDOMAIN);
#endif /* MMDF */
		fprintf(fp, "To: %s\n", to);
		fprintf(fp, "Subject: %s\n", subject);
#ifdef HIDDENNET
		if (strcmp(LOCALSYSNAME, FULLSYSNAME))
			fprintf(fp, "Responding-System: %s.%s%s\n\n",
				LOCALSYSNAME, FULLSYSNAME, MYDOMAIN);
#endif /* !HIDDENNET */
			fprintf(fp, "Responding-System: %s%s\n\n",
				FULLSYSNAME, MYDOMAIN);
	}
	return fp;
}

/*
 * verify that the name mail is being sent to does not contain any
 * nasty hooks to invoke funny functions from the shell or the like.
 */
verifyname(sendto)
char *sendto;
{
	/* Be sure we DO allow alphabetics, !, :, ., -, @. *. */
	char *nasty = "\"'\\`^|;& <>/~";
	register char *p;

	if (sendto[0] <= ' ') {
		log("nasty mail name %s from %s", sendto, header.path);
		xxit(1);
	}
	for (p=sendto; *p; p++) {
		if (*p == ' ') {
			*p = 0;
			break;
		}
	}
	if (strpbrk(sendto, nasty) != NULL)
		xerror("nasty mail name %s from %s", sendto, header.path);

	for (nasty = sendto; (nasty = index(nasty, '.')) != NULL; ) {
		if (*++nasty == '.')	/* check for .. */
			xerror("nasty mail name %s from %s", sendto, header.path);
	}
}

/*
 * Checks to make sure the control message is OK to post.
 */
ctlcheck()
{
	char msg[BUFLEN];
	char *p;

	if (!is_ctl)
		return;

	if (header.ctlmsg[0])
		(void) strcpy(msg, header.ctlmsg);
	else
		(void) strcpy(msg, header.title);

	p = index(msg, ' ');
	if (p)
		*p = 0;
	
	if (strcmp(msg, "ihave") == 0) {
	} else if (strcmp(msg, "sendme") == 0) {
		return;	/* no restrictions */
	} else if (strcmp(msg, "newgroup") == 0) {
		suser();
	} else if (strcmp(msg, "rmgroup") == 0) {
		suser();
	} else if (strcmp(msg, "sendsys") == 0) {
		suser();
	} else if (strcmp(msg, "senduuname") == 0) {
		suser();
	} else if (strcmp(msg, "checkgroups") == 0) {
		suser();
	} else if (strcmp(msg, "version") == 0) {
		return;	/* no restrictions */
	} else if (strcmp(msg, "cancel") == 0) {
		return;	/* no restrictions at this level */
	} else if (strcmp(msg, "delsub") == 0) {
		if (!prefix(header.nbuf, "to.")) {
			printf("Must be in a 'to.system' newsgroup.");
			xxit(0);
		}
		return;
	} else {
		printf("Unrecognized control message - %s\n", msg);
		xxit(0);
	}
}

/* Make sure this guy is special. */
suser()
{
	if (uid == 0 || uid == ROOTID)
		return;
	/*
	 * We assume that since our real uid is the same as NEWSUSR
	 * (the euid) we were run by rootid and it did a setuid.
	 * Too bad we can't set just the effective uid like suid does.
	 */
	if (uid == geteuid())
		return;
#ifdef IHCC
	printf("Please use the command:\n\ttoolnews providers\n");
	printf("then call one of the news people.\n");
#else
	printf("Get your local netnews contact to do it for you.\n");
#endif
	xxit(0);
}
