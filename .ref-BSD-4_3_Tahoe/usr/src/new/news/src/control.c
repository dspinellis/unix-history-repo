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
static char	*SccsId = "@(#)control.c	2.55	10/7/87";
#endif /* SCCSID */

#include "iparams.h"

#define eq(msg) (msg[0] == cargv[0][0] && strcmp(msg, cargv[0]) == 0)

int cargc;
char **cargv;

FILE *hfopen();
FILE *popen(), *mhopen(), *mailhdr();

#define NCARGS	30
char *senderof();
#ifdef u370
static struct hbuf htmp;
#endif /* u370 */

/*
 * The global structure is initialized to NOTIFY as the default (if defined)
 * uid to send mail to for every state.  The following conditions are
 * dealt with (assumes NOTIFY defined):
 *
 * 1) LIB/notify exists and is empty (or contains no recognizable control
 *    message types).
 *    	Action: force TELLME = "";
 * 2) LIB/notify contains the control message name "all" and no associated
 *    address.
 *	Action: force TELLME = "";
 * 3) LIB/notify contains the control message name "all" and has an address.
 *	Action: set TELLME = AlloCpy(address);
 * 4) LIB/notify contains only some of the known control message types.
 *	Action: initialize all addresses to "" and set declared addresses
 *		to listed address.
 */


control(h)
struct hbuf *h;
{
	register char *ctlmsgtext;
	register struct msgtype *mp;

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
	
	/*
	 * We look for a match of the control message name and then
	 * set TELLME to the value parsed from the LIB/notify file
	 * (if any).
	 */
	for(mp=msgtype; mp->m_name; mp++) {
		if(eq(mp->m_name) ) {		/* hit */
#ifdef NOTIFY
			TELLME = mp->m_who_to;	/* reset whom to tell */
#endif /* NOTIFY */
			return (*mp->m_func)(cargc, cargv); /* do it */
		}
	}
	if( !mp->m_name ) {
#ifdef NOTIFY
		TELLME = NOTIFY;
#endif /* NOTIFY */
		c_unknown(h, ctlmsgtext);
	}
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
	static char *cavpbuf[NCARGS];
	static char cavbuf[256];
	char *nextfree = cavbuf;

	if (str == '\0')
		error("Control message %s has no title", header.ident);
	cargc = (*str != '\0');
	cargv = cavpbuf;
	cargv[0] = cavbuf;

	while (*str) {
		if (*str <= ' ') {
			/* skip over white space */
			while (*str != '\0' && *str <= ' ')
				str++;
			if (*str == '\0')	/* line ends in white space */
				return;
			*nextfree++ = 0;
			cargv[cargc] = nextfree;
			if (cargc++ >= NCARGS)
				xerror("Too many arguments to control message %s",
						header.ident);
		} else
			*nextfree++ = *str++;
	}
}

/*
 * ihave <artid> ... <remotesys>
 *	or
 * ihave <remotesys>
 *	with <artid>s in message body.
 *
 * The other system is telling you it has article <artid>, in case
 * you decide you want it to transmit it to you.
 * The assumption is that the other system only tells you about articles
 * in newsgroups you subscribe to.
 *
 * We turn the incoming ihave into an outgoing sendme on the fly.
 * It then gets saved in the SPOOL directory and transmitted to the
 * remote system.  (This way the sendme messages can be batched.)
 */
c_ihave(argc, argv)
register char **	argv;
{
	register int	i;
	char		list[sizeof header.title];
	extern char *	findhist();
	extern char *	mydomain();

	if (argc < 2)
		error("ihave: Too few arguments.");
	if (strncmp(PATHSYSNAME, argv[argc - 1], SNLN) == 0)
		return 0;
	list[0] = '\0';
	if (argc > 2) {
		for (i = 1; i < (argc - 1); ++i)
			if (findhist(argv[i]) == NULL) {
				(void) strcat(list, " ");
				(void) strcat(list, argv[i]);
			}
		if (list[0] == '\0')
			return 0;
	} else {
		register FILE *	outfp;
		register long	outpos, inpos;
		char		myid[256];

		outfp = xfopen(INFILE, "a");
		outpos = ftell(outfp);
		inpos = ftell(infp);
		while (ftell(infp) < outpos) {
			if (fgets(myid, sizeof myid, infp) != myid)
				error("iline: Can't reread article");
			myid[strlen(myid) - 1] = '\0';
			if (findhist(myid) == NULL)
				(void) fprintf(outfp, "%s\n", myid);
		}
		if (outpos == ftell(outfp)) {	/* if nothing is wanted */
			(void) fclose(outfp);
			(void) fseek(infp, inpos, 0);
			return 0;
		}
		(void) fclose(outfp);
		/*
		** The close and open may just be paranoia.
		*/
		(void) fclose(infp);
		infp = xfopen(INFILE, "r");
		(void) fseek(infp, outpos, 0);
	}
	/*
	** Turn the ihave into a sendme.
	*/
	(void) sprintf(header.nbuf, "to.%s.ctl", argv[argc - 1]);
	(void) sprintf(header.title, "sendme%s %s", list, PATHSYSNAME);
	(void) strcpy(header.ctlmsg, header.title);
	getident(&header);
	(void) sprintf(header.from, "%s@%s", "usenet", FROMSYSNAME);
	(void) strcpy(header.path, NEWSUSR);
	header.subdate[0] = header.expdate[0] = '\0';
	dates(&header);
	/*
	** What else of this kind should be done?
	*/
	header.organization[0] = header.distribution[0] = '\0';
	header.numlines[0] = '\0';
	for (i = 0; i < NUNREC && header.unrec[i] != NULL; ++i) {
		free(header.unrec[i]);
		header.unrec[i] = NULL;
	}
	/*
	** Note that we do *not* change the history line
	** so that if the "ihave" message comes in again it gets rejected.
	*/
	return 0;
}

/*
 * sendme <artid> ... <remotesys>
 *	or
 * sendme <remotesys>
 *	with <artid>s in message body.
 * The other system wants me to send out article <artid>.
 * Give it to them with no fuss.
 */
#ifdef MULTICAST
static int	c_mc;
static char **	c_sysnames;
#endif /* MULTICAST */
c_sendme(argc, argv)
register char **argv;
{
	struct srec	srec;

	if (argc < 2)
		error("sendme: Too few arguments.");
	if (strncmp(PATHSYSNAME, argv[argc - 1], SNLN) == 0)
		return 0;
	if (s_find(&srec, argv[argc - 1]) != TRUE)
		error("sendme: Can't find sys record for %s", argv[argc - 1]);
#ifdef MULTICAST
	c_mc = index(srec.s_flags, 'M') != 0;
	if (c_mc) {
		struct srec	trec;

		c_sysnames = &argv[argc - 1];
		if (s_find(&trec, srec.s_xmit) != TRUE)
			error("sendme: Can't find sys record for %s for %s",
				srec.s_xmit, argv[argc - 1]);
		srec = trec;
	} else	c_sysnames = NULL;
#endif /* MULTICAST */
	/* Send the articles. */
	if (argc == 2) {
		register FILE *	fp;
		char		buf[256];

		fp = xfopen(INFILE, "r");
		while (fgets(buf, sizeof buf, fp) == buf) {
			buf[strlen(buf) - 1] = '\0';	/* zap trailing '\n' */
			sendmefunc(buf, &srec);
		}
		(void) fclose(fp);
	} else { 	/* argc > 2 */
		register int	i;

		for (i = 1; i < (argc - 1); ++i)
			sendmefunc(argv[i], &srec);
	}
	return 0;
}

static
sendmefunc(id, sp)
register char *		id;
register struct srec *	sp;
{
	register FILE *	fp;
	register char *	cp;
	char		savedbufname[256];
	extern char	firstbufname[];
	extern char *	dirname();
	extern char *	findfname();

	cp = findfname(id);
	if (cp == NULL) {
		log("System %s wants unavailable article %s.",
#ifdef MULTICAST
			(c_mc ? c_sysnames[0] : sp->s_name), id);
#else /* !MULTICAST */
			sp->s_name, id);
#endif /* !MULTICAST */
		return;
	}
	cp = dirname(cp);
	fp = fopen(cp, "r");
	if (fp == NULL) {
		logerr("Article %s unopenable as %s.", id, cp);
		return;
	}
	(void) strcpy(savedbufname, firstbufname);
	(void) strcpy(firstbufname, cp);
#ifdef MULTICAST
	transmit(sp, fp, FALSE, c_sysnames, c_mc);
#else /* !MULTICAST */
	transmit(sp, fp, FALSE, (char **) NULL, 0);
#endif /* !MULTICAST */
	/* transmit closes fp */
	(void) strcpy(firstbufname, savedbufname);
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
	char abuf[BUFLEN], subjline[BUFLEN];
	int didcreate = 0;
	register char *p, *q;
# ifdef NONEWGROUPS
#  ifdef ORGDISTRIB
	/* local or ORGDISTRIB */
	int can_change = (strcmp(header.distribution, "local") == 0) ||
				(strcmp(header.distribution, ORGDISTRIB) == 0);
#  else /* ! ORGDISTRIB */
	/* local only */
	int can_change = strcmp(header.distribution, "local") == 0;
#  endif /* ORGDISTRIB */
# else /* ! NONEWGROUPS */
	int can_change = 1;	/* allow changes for all distributions */
# endif /* NONEWGROUPS */

	if (argc < 2)
		error("newgroup: Too few arguments.");

	if (header.approved[0] == '\0') {
		logerr("newgroup: %s not approved", argv[1]);
		return 1;
	}

	lock();
	/* see if it already exists */
	(void) rewind(actfp); clearerr(actfp);
	while(fgets(abuf, BUFLEN, actfp) != NULL) {
		p = abuf;
		q = argv[1];
		while (*p++ == *q++)
			;
		if (*--q == '\0' && *--p == ' ') {
			/* Now check if it's correctly moderated/unmoderated */
			while (*p++)
				;
			p -= 3;
			if (argc > 2 && strcmp(argv[2], "moderated") == 0) {
				if (*p == 'm') {
					unlock();
					return 0;
				}
				*p = 'm';
			} else {
				if (*p != 'm') {
					unlock();
					return 0;
				}
				*p = 'y';
			}
# ifdef NOTIFY
			(void) sprintf(subjline,
			"Newsgroup %s changed from %smoderated to %smoderated",
				argv[1], *p=='y' ? "" : "un",
				*p=='y' ? "un" : "");
			fd = mailhdr((struct hbuf *)NULL, subjline);
			if (fd != NULL) {
				if(can_change)
					fprintf(fd,
"%s has been changed from %smoderated to %smoderated as requested by\n%s\n",
						argv[1], *p=='y' ? "" : "un", 
						*p=='y' ? "un":"", header.path);
				else {
					fprintf(fd,
"%s\nhas requested that %s be changed from %smoderated to %smoderated\n",
						header.path, argv[1], 
						*p=='y' ? "" : "un",
						*p=='y' ? "un" : "");
#ifdef ORGDISTRIB
					fprintf(fd,
"You can accomplish this by re-creating the newsgroup with a distribution\n");
					fprintf(fd,
"of '%s' by executing the command:\n", ORGDISTRIB);
					fprintf(fd,
				"%s/inews -d %s -C %s%s\n",
						LIB, ORGDISTRIB, argv[1],
						*p=='y' ? "" : " moderated");
#else /* !ORGDISTRIB */
					fprintf(fd,
"You can accomplish this by re-creating the newsgroup by executing the command:\n");
					fprintf(fd, "%s/inews -C %s%s\n",
						LIB, argv[1],
						*p=='y' ? "" : " moderated");
#endif /* !ORGDISTRIB */
				}
				(void) mclose(fd);
			}
# endif /* NOTIFY */
# ifdef NONEWGROUPS
			/*
			 * No permission to change
			 */
			if(!can_change) {
				unlock();
				return 0;
			}
# endif /* NONEWGROUPS */
			/* The active file was wrong about the state of the
			 * group. Rewrite the active file
			 */
			(void) fseek(actfp, -2L, 1); /* back up 2 characters */
			putc(*p, actfp);
			fflush(actfp);
			if (*p != 'm')
				logerr("Newsgroup %s changed from moderated to unmoderated",
				argv[1]);
			else
				logerr("Newsgroup %s changed from unmoderated to moderated",
				argv[1]);
			unlock();
			return 0;
		}
	}

	/* It doesn't already exist, we must create it */

	if(can_change) {
		didcreate++;
		(void) fseek(actfp, 0L, 2); clearerr(actfp);
		fprintf(actfp, "%s 00000 00001 %c\n", argv[1],
			(argc > 2 && strcmp(argv[2], "moderated") == 0) 
				? 'm' : 'y');
#if defined(USG) || defined(MG1)
		/*
		 * U G L Y   K L U D G E
		 * This utter piece of tripe is the only way I know of
		 * to get around the fact that ATT BROKE standard IO
		 * in System 5.2. Basically, you can't open a file for
		 * "r+" and then try and write to it. This hack works
		 * on all "real" USG Unix systems, It will probably
		 * break on some obscure look alike that doesnt use the
		 * real ATT stdio.h
		 * also broken in WCW MG-1 42nix 2.0
		 * Don't blame me, blame ATT. stdio should have
		 * already done the following line for us, but it didn't
		 */
		actfp->_flag |= _IOWRT;
#endif /* USG */
		fflush(actfp);
	}

# ifdef NOTIFY
	(void) sprintf(subjline, "Newsgroup %s created", argv[1]);
	fd = mailhdr((struct hbuf *)NULL, subjline);
	if (fd != NULL) {
		if (didcreate) 
			fprintf(fd, 
		"A new %snewsgroup called '%s' has been created by %s.\n",
				argc > 2 ? "moderated " : "", argv[1],
				header.path);
		else {
			fprintf(fd, 
		"%s requested that a new %snewsgroup called '%s' be created.\n",
			header.path, argc > 2 ? "moderated " : "", argv[1]);
			fprintf(fd,"It was approved by %s\n\n",header.approved);
			fprintf(fd, 
		"You can accomplish this by creating the newgroup yourself\n");
#  ifdef ORGDISTRIB
			fprintf(fd,"with a distribution of '%s'.\n",
				ORGDISTRIB);
			fprintf(fd,
				"In other words, by executing the command:\n");
			fprintf(fd, "%s/inews -d %s -C %s %s\n", LIB, 
				ORGDISTRIB, argv[1], argc > 2 ? argv[2] : "");
#  else /* !ORGDISTRIB */
			fprintf(fd, "In other words, by executing the command:\n");
			fprintf(fd, "%s/inews -C %s %s\n", LIB, argv[1],
				argc > 2 ? argv[2] : "");
#  endif /* !ORGDISTRIB */
		}
		(void) mclose(fd);
	}
# endif /* NOTIFY */
	unlock();
	return 0;
}

/*
 * rmgroup <groupname>
 * An old newsgroup is being cancelled on a network wide basis.
 */
c_rmgroup(argc, argv)
char **argv;
{
	FILE *fd;
	int shouldremove = 0;
#ifdef NOTIFY
	char subjline[BUFLEN];
#endif	/* NOTIFY */

	if (argc < 2)
		error("rmgroup: Too few arguments.");
	if (!validng(argv[1]))
		return 0;
	if (header.approved[0] == '\0') {
		logerr("rmgroup: %s not approved", argv[1]);
		return 1;
	}

#ifdef MANUALLY
#ifdef ORGDISTRIB
	/*
	 * Allow local as well as organizational removals
	 */
	if (!strcmp(ORGDISTRIB, header.distribution)
	   || !strcmp("local", header.distribution))
#else	/* !ORGDISTRIB */		
	if (!strcmp("local", header.distribution))
#endif	/* !ORGDISTRIB */		
#endif /* MANUALLY */
		shouldremove++;
#ifdef NOTIFY
	sprintf(subjline, "Received rmgroup for %s", argv[1]);
	fd = mailhdr((struct hbuf *)NULL, subjline);
	if (fd != NULL) {
		if (shouldremove) {
		    fprintf(fd, "Newsgroup '%s' has been removed by %s.\n\n",
				argv[1], header.path);
#  ifdef USG
		    fprintf(fd, "You may need to remove the directory %s by hand\n",
				dirname(argv[1]));
#  endif
		} else {
		    fprintf(fd, "%s requested that newsgroup %s be removed.\n",
				header.path, argv[1]);
		    fprintf(fd, "You should remove it by hand\n");
		    fprintf(fd, "To do this, execute the command\n");
		    fprintf(fd, "\t%s/rmgroup %s\n", LIB, argv[1]);
		}
		(void) mclose(fd);
	}
#endif /* NOTIFY */

	if (shouldremove) {
		int pid, status;
		/* We let the shell do all the work.
		 * See the rmgrp shell script.
		 */
		lock();
		(void) sprintf(bfr, "%s/rmgroup", LIB);

		if (pid = fork()) {
			status = fwait(pid);
		} else {
			register int i;
			for (i =3; i<20; i++)
				if (close(i) < 0)
					break;
			(void) setuid(duid);
			execvp(bfr, argv);
		}
		unlock();
		if (status)
			log("rmgroup status %d", status);
	}
	return 0;
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

	if (argc < 2)
		error("cancel: Too few arguments.");
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
		return -1;
	}

	q = index(line, '\t');
	p = index(q+1, '\t');
	if (p == NULL || *++p == '\0' || *p == '\n') {
		*q = '\0';
		log("Expired article %s", line);
		return -1;
	}
	if (strcmp(p, "cancelled") == 0) {
		*q = '\0';
		log("Already Cancelled %s", line);
		return -1;
	} else
		log("Cancelling %s", line);
	if ((uid == ROOTID||uid == 0) && (
#ifdef ORGDISTRIB
		strcmp(header.distribution, ORGDISTRIB) == 0 ||
#endif /* ORGDISTRIB */
		strcmp(header.distribution, "local") == 0))
		su = 1;
	while (*p) {
		q = index(p, ' ');
		if (q)
			*q = '\0';
		(void) strcpy(nfilename, dirname(p));
		fp = fopen(nfilename, "r");
		if (fp == NULL) {
			log("Can't cancel %s: %s", line, errmsg(errno));
			return -1;
		}
		htmp.unrec[0] = NULL;
		if (hread(&htmp, fp, TRUE) == NULL) {
			if (bfr[0] == '/') {
				fp = fopen(bfr, "r");
				if (fp == NULL
					|| hread(&htmp, fp, TRUE) == NULL)
					error("Article is garbled.");
			} else 
				error("Article is garbled.");
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
			error("Not contributor: posted by %s, and you are %s", poster, whatsisname);
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
#endif /* NOTIFY */
	f = mailhdr(&header, "response to your sendsys request");
	u = fopen(SUBFILE, "r");
	if (f != NULL && u != NULL) {
		while ((c=getc(u)) != EOF)
			putc(c, f);
		(void) fclose(u);
		(void) mclose(f);
	}
	return 0;
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
		error("Cannot send back error message");
	fprintf(f, "Currently running news version %s.\n\n", news_version);
	fprintf(f, "The header of your message follows:\n\n");
	(void) hwrite(&header, f);
	(void) mclose(f);
	return 0;
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
	return 0;
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
	return 0;
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
	return 0;
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
		if (TELLME)
			sendto = TELLME;
#endif /* NOTIFY */
		if (sendto == NULL || *sendto == '\0')
			return NULL;
	}
	verifyname(sendto);
	if(pipe(p) < 0)
		return NULL;
	myside = p[WTR];
	hisside = p[RDR];
	if((pid = vfork()) == 0) {
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
		execl("/usr/bin/mailx", "mail", sendto, (char *)NULL);
		execl("/usr/bin/mail", "mail", sendto, (char *)NULL);
		execl("/usr/ucb/mail", "mail", sendto, (char *)NULL);
		execl("/bin/mail", "mail", sendto, (char *)NULL);
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
	extern char *mydomain();

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
		fprintf(fp, "From: The News System <usenet@%s>\n",
				FROMSYSNAME);
#endif /* MMDF */
		fprintf(fp, "To: %s\n", to);
		fprintf(fp, "Subject: %s\n", subject);
		fprintf(fp, "Responding-System: %s\n\n", LOCALSYSNAME);
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
		xerror("nasty mail name %s from %s", sendto, header.path);
	}
	for (p=sendto; *p; p++) {
		if (*p == ' ') {
			*p = 0;
			break;
		}
	}
	if (strpbrk(sendto, nasty) != NULL)
		error("nasty mail name %s from %s", sendto, header.path);

	for (nasty = sendto; (nasty = index(nasty, '.')) != NULL; ) {
		if (*++nasty == '.')	/* check for .. */
			error("nasty mail name %s from %s", sendto, header.path);
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
	
	if (strcmp(msg, "ihave") == 0 || strcmp(msg, "sendbad") == 0 ||
		strcmp(msg, "sendme") == 0) {
		return;	/* no restrictions */
	} else if (strcmp(msg, "newgroup") == 0) {
		suser();
	} else if (strcmp(msg, "rmgroup") == 0) {
		suser();
	} else if (strcmp(msg, "sendsys") == 0) {
		suser();
	} else if (strcmp(msg, "checkgroups") == 0) {
		suser();
	} else if (strcmp(msg, "version") == 0) {
		return;	/* no restrictions */
	} else if (strcmp(msg, "cancel") == 0) {
		return;	/* no restrictions at this level */
	} else if (strcmp(msg, "delsub") == 0) {
		if (!PREFIX(header.nbuf, "to.")) {
			log("Must be in a 'to.system' newsgroup.");
			xxit(0);
		}
		return;
	} else {
		log("Unrecognized control message - %s\n", msg);
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
