/*
 * readr - /bin/mail and msgs interface and associated functions.
 */

static char	*SccsId = "@(#)readr.c	2.26	5/3/83";

#include "rparams.h"

static char	lbuf[BUFLEN*2];

#define	saveart	oobit = bit;strcpy(ofilename1, filename);strcpy(ogroupdir, groupdir);hbufcp(&hbuf1, &h);ongsize = pngsize
#define NLINES(h, fp) (h.numlines[0] ? h.intnumlines : (h.intnumlines=linecnt(fp),sprintf(h.numlines, "%d", h.intnumlines), h.intnumlines))

char *tft = "/tmp/folXXXXXX";

static int	hascaught = 0;
static catchintr()
{
	hascaught = 1;
	printf("\n");
	fflush(stdout);
}

/*
 * These were made static for u370 with its buggy cc.
 * I judged it better to have one copy with no ifdefs than
 * to conditionally compile them as automatic variables
 * in readr (which they originally were).  Performance
 * considerations might warrent moving some of the simple
 * things into register variables, but I don't know what
 * breaks the u370 cc.
 */
static char goodone[BUFLEN];		/* last decent article		*/
static char ogroupdir[BUFLEN];		/* last groupdir		*/
static char address[PATHLEN];		/* for reply copy		*/
static char edcmdbuf[128];
static char folbuf[160];
static int rfq = 0;			/* for last article		*/
static long ongsize;			/* Previous ngsize		*/
static long pngsize;			/* Printing ngsize		*/
static char *bptr;			/* temp pointer.		*/
static struct srec srec;		/* srec for sys file entries	*/
static char *tfilename;			/* temporary file name 		*/
static char ofilename1[BUFLEN];		/* previous file name		*/
static struct hbuf hbuf1, hbuf2, *hptr;	/* for minusing			*/
static char *ptr1, *ptr2, *ptr3;	/* for reply manipulation	*/
static int  news = 0;
static int  abs = FALSE;		/* TRUE if we asked absolutely	*/
static char *ed, tf[100];
static struct hbuf h;			/* ditto.			*/
static int i;
static int oobit;			/* last bit, really		*/
static char *oldsig;
static int dgest = 0;
static FILE *ofp;			/* Current output file to terminal*/
static FILE *fp;			/* current article to be printed*/
static int holdup;			/* 1 iff should stop before hdr */
static int ignorenews;			/* 1 iff readnews -p > /dev/null*/
static long timelastsaved;		/* time newsrc last written out */

int catchcont();

readr()
{

#ifdef DEBUG
	fprintf(stderr, "readr()\n");
#endif
	if (aflag) {
		if (*datebuf) {
			if ((atime = cgtdate(datebuf)) == -1)
				xerror("Cannot parse date string");
		} else 
			atime = 0L;
	}

	if (pflag && ignoring())
		ignorenews = TRUE;

	if (xflag)
		uflag = 0;
	if (uflag)
		time(&timelastsaved);

	ofp = stdout;
	if (cflag && coptbuf[0] != '\0') {
		umask(022);
		mktemp(outfile);	/* get "unique" file name */
		ofp = xfopen(outfile, "w");
		umask(N_UMASK);
		cflag = FALSE;
		pflag = TRUE;
	}

	/* loop reading articles. */
	fp = NULL;
	obit = -1;
	nextng();
	for ( ;; ) {
		if (getnextart(FALSE))
			break;
#ifdef DEBUG
		printf("after getnextart, fp %x, pos %d, bit %d, group '%s', filename '%s'\n",
			fp, ftell(fp), bit, groupdir, filename);
#endif
		strcpy(goodone, filename);
		if (pflag || lflag || eflag) {
			/* This code should be gotten rid of */
			if (sigtrap) {
				qfflush(ofp);
				fprintf(ofp, "\n");
				cdump(ofp);
				_exit(0); /* kludge! drop when qfflush works */
				return;
			}
			clear(bit);
			nextbit();
			if (fp) {
				fclose(fp);
				fp = NULL;
			}
			continue;
		}
		for ( ;; ) {
			char *pp;
#ifdef	SIGCONT
			int (*ocont)();
#endif
			sigtrap = FALSE;
			if (!cflag) {
				if (rfq)
					fprintf(ofp, "Last article.  [qfr] ");
				else
					fprintf(ofp, "(%d lines) More? [ynq] ", NLINES(h, fp));
			} else
				fprintf(ofp, "? ");
			fflush(ofp);
			bptr = lbuf;
#ifdef SIGCONT
			ocont = signal(SIGCONT, catchcont);
#endif
			pp = fgets(bptr, BUFLEN, stdin);
#ifdef SIGCONT
			signal(SIGCONT, ocont);
#endif
			if (pp != NULL)
				break;
			if (!sigtrap)
				return;
#ifdef SIGCONT
			if (sigtrap != SIGCONT)
#endif
				fprintf(ofp, "\n");
		}
		nstrip(bptr);
		while (*bptr == ' ' || *bptr == '\t')
			bptr++;
		i = command();
		if (i)
			break;
	}

	if (!news)
		fprintf(stderr, "No news.\n");
	cout(ofp);
}


#define EOL() if (*bptr != '\0') { fprintf(ofp, "? for commands.\n"); return FALSE; }
/*
 * Process one command, which has already been typed in.
 */
command()
{
	char *findhist();

	switch (i = *bptr++) {

	/* No.  Go on to next article. */
	case 'n':
		EOL();
		itsbeenseen(h.ident);
		readmode = NEXT;
		if (!cflag) {
			fclose(fp);
			fp = NULL;
		}
		fprintf(ofp, "\n");
		clear(bit);
		saveart;
		nextbit();
		break;

	/* Undigestify the article. */
	case 'd':
		dgest = 1;
		/* fall through */

	/* yes: print this article, go on. */
	case 'y':
		EOL();
		/* fall through. */

	/* The user hit return.  Default is 'y' unless rfq, then it's 'q'. */
	case '\0':
		if (!bptr[-1] && rfq)
			return;
		readmode = NEXT;
		showtail(fp);
		clear(bit);
		saveart;
		nextbit();
		break;

	/*
	 * Unsubscribe to the newsgroup and go on to next group
	 */
	case 'u':
		fprintf(ofp, "To unsubscribe, use 'U'\n");
		break;

	case 'U':
		fprintf(ofp, "Unsubscribing to newsgroup: %s\n", groupdir);
		obit = -1;
		if (fp != NULL) {
			fclose(fp);
			fp = NULL;
		}
		if (cflag)
			clear(bit);
		else
			putc('\n', ofp);
		rfq = 0;
		zapng = TRUE;
		saveart;
		if (nextng()) {
			if (actdirect == BACKWARD)
				fprintf(ofp, "Can't back up.\n");
			else
				return TRUE;
		}
		break;

		/* Print the current version of news */
	case 'v':
		fprintf(ofp, "News version: %s\n", news_version);
		break;

		/* reprint the article */
	case 'p':
		EOL();
		if (!cflag)
			goto minus;
		readmode = NEXT;
		if (!cflag) {
			fclose(fp);
			fp = NULL;
			bit = last;
			putc('\n', ofp);
		}
		obit = -1;
		break;

		/* decrypt joke */
	case 'D': 
		caesar_command();
		readmode = NEXT;
		clear(bit);
		saveart;
		nextbit();
		break;

		/* write out the article someplace */
	case 's':
	case 'w':
		{
		char *grn = groupdir;
		tfilename = filename;
		if (*bptr == '-') {
			bptr++;
			grn = ogroupdir;
			if (*ofilename1)
				tfilename = ofilename1;
		}
		if (*bptr != '\0' && *bptr != ' ') {
			fprintf(ofp, "Bad file name.\n");
			break;
		}
		while (*bptr == ' ')
			bptr++;
		if (*bptr != '|' && *bptr != '/') {
			char	hetyped[BUFLEN];
			char	*boxptr;
			strcpy(hetyped, bptr);
			if (boxptr = getenv("NEWSBOX"))
				if (index(boxptr, '%'))
					sprintf(bptr, boxptr, grn);
				else
					strcpy(bptr, boxptr);
			else if (hetyped[0] == '~' && hetyped[1] == '/') {
				strcpy(hetyped, bptr+2);
				strcpy(bptr, userhome);
			} else
				strcpy(bptr, ".");
			strcat(bptr, "/");
			if (hetyped[0] != '\0')
				strcat(bptr, hetyped);
			else
				strcat(bptr, "Articles");
		}
		fwait(fsubr(save, tfilename, bptr));
		}
		break;

		/* back up  */
	case '-':
minus:
		rfq = 0;
		abs = TRUE;
		if (!*ofilename1) {
			fprintf(ofp, "Can't back up.\n");
			break;
		}
		if (cflag)
			clear(bit);
		else {
			fclose(fp);
			fp = NULL;
			putc('\n', ofp);
		}
		hbufcp(&hbuf2, &h);
		hbufcp(&h, &hbuf1);
		hbufcp(&hbuf1, &hbuf2);
		strcpy(bfr, filename);
		strcpy(filename, ofilename1);
		strcpy(ofilename1, bfr);
		obit = bit;
		if (strcmp(groupdir, ogroupdir)) {
			strcpy(bfr, groupdir);
			selectng(ogroupdir);
			strcpy(groupdir, ogroupdir);
			strcpy(ogroupdir, bfr);
			ngrp = 1;
			back();
		}
		bit = oobit;
		oobit = obit;
		obit = -1;
		getnextart(TRUE);
		return FALSE;

		/* skip forwards */
	case '+':
caseplus:
		if (*bptr == '\0')
			strcat(bptr, "1");
		rfq = 0;
		if (cflag)
			clear(bit);
		saveart;
		last = bit;
		for (i = 0; i < atoi(bptr); i++) {
			nextbit();
			if ((bit > pngsize) || (rflag && bit < 1))
				break;
		}
		if (!cflag) {
			putc('\n', ofp);
			fclose(fp);
			fp = NULL;
		}
		obit = -1;
		break;

	/* exit - time updated to that of most recently read article */
	case 'q':
		EOL();
		return TRUE;

	/* exit - no time update. */
	case 'x':
		EOL();
		xxit(0);

	/* cancel the article. */
	case 'c':
		cancel_command();
		break;

	/* escape to shell */
	case '!':
		fwait(fsubr(ushell, bptr, (char *)NULL));
		fprintf(ofp, "\n");
		hdr();
		break;

	/* mail reply */
	case 'r':
		reply_command();
		break;

	/* send to some system */
	case 'X':
		xmit_command();
		break;

	/* next newsgroup */
	case 'P':
		*bptr = '-';
	case 'N':
		if (fp != NULL) {
			fclose(fp);
			fp = NULL;
		}
		if (next_ng_command())
			return TRUE;
		break;

	/* specific no. */
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		sscanf(--bptr, "%d", &i);
		if (i == 0) {
			fprintf(ofp, "Bad article no.\n");
			break;
		}
		if (i > pngsize) {
			fprintf(ofp, "Not that many articles.\n");
			break;
		}
		readmode = SPEC;
		abs = TRUE;
		bit = i;
		obit = -1;
		if (!cflag) {
			putc('\n', ofp);
			fclose(fp);
			fp = NULL;
		}
		rfq = 0;
		break;

	/* specific message ID. */
	case '<':
		ptr1 = findhist(--bptr);
		if (ptr1 == NULL) {
			fprintf(ofp, "No such article: %s.\n", bptr);
			break;
		}
		ptr2 = index(ptr1, '\t');
		ptr3 = index(++ptr2, '\t');
		ptr2 = index(++ptr3, ' ');
		if (ptr2)
			*ptr2 = '\0';
		ptr2 = index(ptr3, '/');
		*ptr2++ = '\0';
		abs = TRUE;
		if (cflag)
			clear(bit);
		else {
			fclose(fp);
			fp = NULL;
			putc('\n', ofp);
		}
		hbufcp(&hbuf1, &h);
		saveart;
		strcpy(ogroupdir, ptr3);
		if (strcmp(groupdir, ogroupdir)) {
			strcpy(bfr, groupdir);
			selectng(ogroupdir);
			strcpy(groupdir, ogroupdir);
			strcpy(ogroupdir, bfr);
			back();
		}
		sscanf(ptr2, "%d", &bit);
		oobit = obit;
		obit = -1;
		getnextart(TRUE);
		rfq = 0;
		break;

	/* follow-up article */
	case 'f':
		followup_command();
		break;

	/* erase - pretend we haven't seen this article. */
	case 'e':
		if (rfq || *bptr == '-') {
			if (strcmp(groupdir, ogroupdir)) {
				i = bit;
				strcpy(bfr, groupdir);
				selectng(ogroupdir);
				set(oobit);
				printf("Holding article %d newsgroup %s\n", oobit, ogroupdir),
				strcpy(groupdir, ogroupdir);
				selectng(bfr);
				bit = i;
			} else {
				printf("Holding article %d\n", oobit),
				set(oobit);
			}
		} else {
			printf("Holding article %d\n", bit),
			set(bit);
			goto caseplus;	/* skip this article for now */
		}
		break;

	case 'H':
	case 'h':
		if (!hflag)
			dash(8, ofp);
		if (*bptr == '-') {
			if (oobit > 0)
				fprintf(ofp, "Article %d:\n", oobit);
			hprint(&hbuf1, ofp, 1 + (i=='H'));
		} else {
			fprintf(ofp, "Article %d of %ld: %s\n",
				rfq ? oobit : bit, pngsize, h.ident);
			hprint(&h, ofp, 1 + (i=='H'));
		}
		if (!hflag)
			dash(8, ofp);
		break;

	case '#':
		fprintf(ofp, "Article %d of %ld: newsgroup %s\n",
			rfq ? oobit : bit, pngsize, rfq ? ogroupdir : groupdir);
		break;

		/* error */
	case '?':
		help(ofp);
		break;
	default:
		fprintf(ofp, "? for commands.\n");
		break;
	}

	return FALSE;
}

cancel_command()
{
	tfilename = filename;
	hptr = &h;
	if (*bptr == '-') {
		if (*ofilename1) {
			tfilename = ofilename1;
			hptr = &hbuf1;
		}
		bptr++;
	}
	EOL();
	readmode = SPEC;
	strcpy(rcbuf, hptr->path);
	ptr1 = index(rcbuf, ' ');
	if (ptr1)
		*ptr1 = 0;
	if (uid == ROOTID)
		i = 0;		/* root gets to cancel */
	else
		i = strcmp(username, rcbuf);
	if (i != 0) {
		fprintf(ofp, "Can't cancel what you didn't write.\n");
		return;
	}
	if (!cancel(ofp, hptr, i) && hptr == &h) {
		clear(bit);
		saveart;
		nextbit();
		obit = -1;
		fp = NULL;
		if (!cflag)
			putc('\n', ofp);
	}
	if (fp != NULL)
		fclose(fp);
	fp = NULL;
}

reply_command()
{
	register char	*pathptr, *ptr;
	int edit = 1;
	char *ed;
	FILE *tfp;
	char	curberk[BUFLEN];
	char *replyname();
	char subj[100];
	char folbuf[100];
	extern char MAILPARSER[];

	hptr = &h;
	while (*bptr && index("d-", *bptr)) {
		switch (*bptr) {
		/* Followup the previous article. */
		case '-':
			hptr = &hbuf1;
			break;

		/* Don't edit the headers */
		case 'd':
			edit = 0;
			break;
		}
		bptr++;
	}
	EOL();
	if (edit && access(MAILPARSER, 1)) {
#ifdef IHCC
		fprintf(stderr, "Can't edit headers, 'recmail' missing.\n");
#else
		fprintf(stderr, "Can't edit headers without %s\n", MAILPARSER);
#endif
		edit = 0;
	}

	*rcbuf = '\0';
	*curberk = '\0';
	pathptr = replyname(hptr);;
	ptr = pathptr - 1;
	i = 0;
	for (ptr1 = address, ptr2 = pathptr; *ptr2; ptr1++, ptr2++) {
		if (index("\"\\$", *ptr2))
			*ptr1++ = '\\';
		*ptr1 = *ptr2;
	}
	*ptr1 = '\0';

	folbuf[0] = 0;				/* References */
	if (hptr->followid[0]) {
		strcpy(folbuf, hptr->followid);
		strcat(folbuf, ", ");
	}
	strcat(folbuf, hptr->ident);

	strcpy(subj, hptr->title);	/* Subject */
	while (isspace(*bptr))
		bptr++;
	if (*bptr != '\0')
		strcpy(subj, bptr);
	if (!prefix(subj, "Re:") && !prefix(subj, "re:")) {
		strcpy(bfr, subj);
		sprintf(subj, "Re: %s", bfr);
	}
	if (!edit) {
		fprintf(ofp, "To: %s\n", pathptr);
		ed = index(MAILER, '%');
		if (ed && ed[1] == 's')
			fprintf(ofp, "Subject: %s\n", subj);
		fflush(ofp);
	}

	/* Put the user in the editor to create the body of the followup. */
	if (edit) {
		strcpy(tf, tft);
		mktemp(tf);

		ed = getenv("EDITOR");
		if (ed == NULL)
			ed = DFTEDITOR;

		tfp = fopen(tf, "w");
		fprintf(tfp, "To: %s\n", pathptr);
		fprintf(tfp, "Subject: %s\n", subj);
		fprintf(tfp, "References: %s\n\n", folbuf);
		fclose(tfp);

		sprintf(edcmdbuf, "%s %s", ed, tf);
		system(edcmdbuf);
		strcpy(rcbuf, MAILPARSER);
		strcat(rcbuf, " -t");
		strcat(rcbuf, " < ");
		strcat(rcbuf, tf);
		if (access(tf, 4)) {
			fprintf(stderr, "Reply not sent: no input file.\n");
			return;
		}
		printf("Sending reply.\n");
		fflush(stdout);
		if (fork() == 0) {
			system(rcbuf);
			unlink(tf);
			_exit(0);
		}
	} else {
		sprintf(rcbuf, MAILER, hptr->title);
		sprintf(bfr, "%s %s", rcbuf, address);
		system(bfr);
	}
	hdr();
}

xmit_command()
{
	tfilename = filename;
	if (*bptr == '-') {
		if (*ofilename1)
			tfilename = ofilename1;
		bptr++;
	}
	if (*bptr != '\0' && *bptr != ' ') {
		fprintf(ofp, "Bad system name.\n");
		return;
	}
	while (*bptr == ' ')
		bptr++;
	if (*bptr == '\0') {
		fprintf(ofp, "Missing system name.\n");
		return;
	}
	if (s_find(&srec, bptr) == NULL) {
		fprintf(ofp, "%s not in SYSFILE\n", bptr);
		return;
	}
	transmit(&srec, tfilename);
}

next_ng_command()
{
	if (!*bptr || *bptr == '-') {
		obit = -1;
		if (cflag)
			clear(bit);
		else
			putc('\n', ofp);
		if (*bptr)
			actdirect = BACKWARD;
		rfq = 0;
		saveart;
		if (nextng()) {
			if (actdirect == BACKWARD)
				fprintf(ofp, "Can't back up.\n");
			else
				return TRUE;
		}
		return FALSE;
	}
	while (isspace(*bptr))
		bptr++;
	if (!validng(bptr)) {
		fprintf(ofp, "No such group.\n");
		return FALSE;
	}
	obit = -1;
	if (cflag)
		clear(bit);
	else
		putc('\n', ofp);
	readmode = SPEC;
	rfq = 0;
	saveart;
	back();
	selectng(bptr);
	return FALSE;
}

followup_command()
{
	int edit = 1;
	char subj[100];
	char folbuf[100];
	char *ng;
	FILE *tfp;

	hptr = &h;

	while (*bptr && index("d-", *bptr)) {
		switch (*bptr) {
		/* Followup the previous article. */
		case '-':
			hptr = &hbuf1;
			break;

		/* Don't edit the headers */
		case 'd':
			edit = 0;
			break;
		}
		bptr++;
	}

	/* Figure out the subject, newsgroups, and references for the followup. */
	ng = hptr->nbuf;			/* Newsgroups */
	if (hptr->followto[0])
		ng = hptr->followto;
	launder(ng);

	folbuf[0] = 0;				/* References */
	if (hptr->followid[0]) {
		strcpy(folbuf, hptr->followid);
		strcat(folbuf, ", ");
	}
	strcat(folbuf, hptr->ident);

	strcpy(subj, hptr->title);	/* Subject */
	while (isspace(*bptr))
		bptr++;
	if (*bptr != '\0')
		strcpy(subj, bptr);
	if (!prefix(subj, "Re:") && !prefix(subj, "re:")) {
		strcpy(bfr, subj);
		sprintf(subj, "Re: %s", bfr);
	}

	/* Determine the command line for the shell. */
	if (edit) {
		sprintf(bfr, "%s -h -D", FOLLOWUP);
	} else {
		sprintf(bfr, "%s -D -F '%s' -n %s -t \'", FOLLOWUP, folbuf, ng);
		strqcat(bfr, subj);
		strcat(bfr, "\'");
	}

	/* backslash special characters */
	for (ptr1 = rcbuf, ptr2 = bfr; *ptr2; ptr1++, ptr2++) {
		if (index("\\", *ptr2))
			*ptr1++ = '\\';
		*ptr1 = *ptr2;
	}
	*ptr1 = '\0';

	/* Let the user know what's going on. */
	fprintf(ofp, "Posting followup article to network.  Please use\n");
	fprintf(ofp, "reply ('r') instead unless your article is of general\n");
	fprintf(ofp, "interest.  (To abort press BREAK.)\n");
	fprintf(ofp, "Subject: %s\n", subj);
	fprintf(ofp, "Newsgroups: %s\n", ng);
	fprintf(ofp, "Hit <return> to continue, BREAK to abort: ");
	fflush(ofp);

	/* Give the user a chance to hit BREAK and back out. */
	hascaught = 0;
	oldsig = (char *) signal(SIGINT, catchintr);
	gets(edcmdbuf);
	signal(SIGINT, oldsig);
	if (hascaught)
		return;

	/* Play obnoxious warnings, if necessary. */
	if (recording(hptr->nbuf, 0))
		return;

	/* Put the user in the editor to create the body of the followup. */
	ed = getenv("EDITOR");
	if (ed == NULL || *ed == '\0')
		ed = DFTEDITOR;
	if (ed) {
		strcpy(tf, tft);
		mktemp(tf);

		tfp = fopen(tf, "w");
		if (edit) {
			fprintf(tfp, "Newsgroups: %s\n", ng);
			fprintf(tfp, "Subject: %s\n", subj);
			fprintf(tfp, "References: %s\n", folbuf);
			if (hptr->keywords[0])
				fprintf(tfp, "Keywords: %s\n", hptr->keywords);
			fprintf(tfp, "\n");
		}
		fclose(tfp);

		sprintf(edcmdbuf, "%s %s", ed, tf);
		system(edcmdbuf);
		strcat(rcbuf, "< ");
		strcat(rcbuf, tf);
		if (access(tf, 4)) {
			fprintf(stderr, "Article not posted: no input file.\n");
			return;
		}
		printf("Posting article.\n");
		fflush(stdout);
		if (fork() == 0) {
			system(rcbuf);
			unlink(tf);
			_exit(0);
		}
	} else {
		printf("%s\n", rcbuf);
		system(rcbuf);
	}
	hdr();
}

caesar_command()
{
	char	temp[100];
	char	*pp = bptr;
	FILE	*pfp, *popen();

	fprintf(stderr, "Caesar decoding:\n");
	strcpy(temp, CAESAR);
	if (*bptr) {
		strcat(temp, " ");
		strcat(temp, bptr);
	}
	if (NLINES(h, fp) > LNCNT && *PAGER) {
		strcat(temp, " | ");
		strcat(temp, PAGER);
	}
	pfp = popen(temp, "w");
	tprint(fp, pfp, FALSE);
	itsbeenseen(h.ident);
	fclose(fp);
	fp = NULL;
	pclose(pfp);
}

/*
 * Show the user the tail, if any, of the message on file
 * descriptor fd, and close fd.  The digester is considered,
 * and the pager is used if appropriate.
 */
showtail(fd)
FILE *fd;
{
	if (fd == NULL)
		return;

	if (dgest) {
		digest(fd, ofp, &h);
	} else if (!lflag && !pflag && !eflag) {
#ifdef PAGE
		/* Filter the tail of long messages through PAGER. */
		if (NLINES(h, fd) > LNCNT && *PAGER) {
			if (!index(PAGER, FMETA)) {
				FILE *pfp, *popen();
				int	cnt;

				pfp = popen(PAGER, "w");
				if (pfp == NULL)
					pfp = ofp;
				/*
				 * What follows is an attempt to prevent the
				 * next message from scrolling part of this
				 * message off the top of the screen before
				 * the poor luser can read it.
				 */
				tprint(fd, pfp, FALSE);
				itsbeenseen(h.ident);
				pclose(pfp);
			} 
			else
				pout(ofp);
# ifndef NOCOLON
			holdup = TRUE;
# endif NOCOLON
		}
		else
#endif
			tprint(fd, ofp, FALSE), itsbeenseen(h.ident);
	}
	fclose(fd);
}

/*
 * Find the next article we want to consider, if we're done with
 * the last one, and show the header.
 */
getnextart(minus)
int minus;
{
	if (minus)
		goto nextart2;	/* Kludge for "-" command. */

	if (bit == obit)	/* Return if still on same article as last time */
		return 0;

	sigtrap = FALSE;

nextart:
	dgest = 0;
	if (bit < 1 && !rflag)
		bit = 1;

	/* If done with this newsgroup, find the next one. */
	while (((long) bit > ngsize) || (rflag && bit < 1)) {
		int i;
		if (i=nextng()) {
			if (actdirect == BACKWARD) {
				fprintf(ofp, "Can't back up.\n");
				actdirect = FORWARD;
				continue;
			} 
			else if (rfq++ || pflag || cflag)
				return 1;
		}
		if (rflag)
			bit = ngsize + 1L;
		else
			bit = -1;
		if (uflag) {
			long now;
			time(&now);
			if (now - timelastsaved > 5*60 /* 5 minutes */) {
				printf("[Saving .newsrc]\n");
				fflush(stdout);
				writeoutrc();
				timelastsaved = now;
			}
		}
	}

nextart2:
#ifdef DEBUG
	fprintf(stderr, "article: %s/%d\n", groupdir, bit);
#endif
	if (rcreadok)
		rcreadok = 2;	/* have seen >= 1 article */
	sprintf(filename, "%s/%d", dirname(groupdir), bit);
	if (rfq && goodone[0])
		strcpy(filename, goodone);
	if (sigtrap) {
		if (sigtrap == SIGHUP)
			return 1;
		if (!rcreadok)
			xxit(0);
		fprintf(ofp, "Abort (n)?  ");
		fflush(ofp);
		gets(bfr);
		if (*bfr == 'y' || *bfr == 'Y')
			xxit(0);
		sigtrap = FALSE;
	}
#ifdef DEBUG
	fprintf(stderr, "filename = '%s'\n", filename);
#endif
	/* Decide if we want to show this article. */
	if (ignorenews
	|| access(filename, 4)
	|| ((fp = fopen(filename, "r")) == NULL)
	|| (hread(&h, fp, TRUE) == NULL)
	|| (!rfq && !select(&h, abs))) {
#ifdef DEBUG
		fprintf(stderr, "Bad article '%s'\n", filename);
#endif
		if (fp != NULL) {
			fclose(fp);
			fp = NULL;
		}
		clear(bit);
		obit = -1;
		nextbit();
		abs = FALSE;
		goto nextart;
	}
	abs = FALSE;
	actdirect = FORWARD;
	news = TRUE;
	hdr();
	if ((cflag && !lflag && !eflag) || pflag)
		tprint(fp, ofp, FALSE);
	if (cflag && lflag && eflag || pflag) {
		itsbeenseen(h.ident);
		sigtrap = FALSE;
		fclose(fp);
		fp = NULL;
	}
	obit = bit;
	return 0;
}

/*
 * Print out whatever the appropriate header is
 */
hdr()
{
	if (rfq)
		return;

#ifndef NOCOLON
	/* Wait for user to read previous article. */
	if (holdup) {
		fprintf(ofp, ":");
		fflush(ofp);
		holdup = FALSE;
		bfr[0] = '\0';
		gets(bfr);
		if (bfr[0])
			explaincolon();
	}
#endif NOCOLON

	if (lflag || eflag) {
		hprint(&h, ofp, 0);
		return;
	}

	/* Print out a header */
	if (ngrp) {
		pngsize = ngsize;
		ngrp--;
		nghprint(groupdir);
	}
	if (!hflag)
		fprintf(ofp, "Article %d of %ld, %s.\n",
			bit, pngsize, briefdate(h.subdate));
	hprint(&h, ofp, pflag ? 1 : 0);
}

explaincolon()
{
	static int calledbefore = 0;

	fprintf(ofp, "\n'%s' ignored.\n", bfr);
	if (calledbefore++ == 0) {
		fprintf(ofp, "The colon is to give you a chance to finish reading the\n");
		fprintf(ofp, "previous article before the next header scrolls it off\n");
		fprintf(ofp, "the top of the screen.  You should hit `return' or `newline'\n");
		fprintf(ofp, "when you are ready to go on to the next article.\n\n");
	}
	fflush(ofp);
}

nghprint(title)
char *title;
{
	char *tstr = "Newsgroup ";
	int l = strlen(title) + strlen(tstr);

	fprintf(ofp, "\n");
	if (!hflag) {
		dash(l, ofp);
		fprintf(ofp, "%s%s\n", tstr, title);
		dash(l, ofp);
	} else {
		fprintf(ofp, "%s%s, ", tstr, title);
		if (bit == pngsize)
			fprintf(ofp, "%ld\n", pngsize);
		else
			fprintf(ofp, "%d-%ld\n", bit, pngsize);
	}
	fprintf(ofp, "\n");
}

/*
 * Routine to catch a continue signal.
 */
catchcont()
{
#ifdef SIGCONT
	signal(SIGCONT, catchcont);
	sigtrap = SIGCONT;
#endif
	hdr();
}
