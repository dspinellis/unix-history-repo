/*
 * visual - visual news interface.
 * Kenneth Almquist
 */

#ifdef SCCSID
static char	*SccsId = "@(#)visual.c	1.38	10/15/87";
#endif /* SCCSID */

#include "rparams.h"
#ifdef USG
#include <sys/ioctl.h>
#include <termio.h>
#include <fcntl.h>
#else /* !USG */
#include <sgtty.h>
#endif /* !USG */

#include <errno.h>
#ifdef BSD4_2
#ifndef sigmask
#define sigmask(m) (1<<((m)-1))
#endif /* !sigmask */
#endif /* BSD4_2 */

#ifdef MYDB
#include "db.h"
#endif /* MYDB */

extern int errno;

#ifdef SIGTSTP
#include <setjmp.h>
#endif /* SIGTSTP */

#define ARTWLEN	(ROWS-2)/* number of lines used to display article */
#define even(cols) ((cols&1) ? cols + 1 : cols)
#ifdef STATTOP
#define PRLINE	0	/* prompter line */
#define SPLINE	1	/* secondary prompt line */
#define ARTWIN	2	/* first line of article window */
#define SECPRLEN 81	/* length of secondary prompter */
#else
#define PRLINE	(ROWS-1)/* prompter line */
#define SPLINE	(ROWS-2)/* secondary prompt line */
#define ARTWIN	0	/* first line of article window */
#define SECPRLEN 100	/* length of secondary prompter */
#endif

#define PIPECHAR '|'	/* indicate save command should pipe to program */
#define	CAGAIN	('e'&0x1F)	/* Save-to-same-place indicator */
#define META	0200	/* meta character bit (as in emacs) */
/* print (display) flags */
#define HDRONLY	0001	/* print header only */
#define NOPRT	0002	/* don't print at all */
#define NEWART	0004	/* force article display to be regenerated */
#define HELPMSG	0010	/* display currently contains help message */
/* prun flags */
#define CWAIT	0001	/* type "continue?" and wait for return */
#define BKGRND	0002	/* run process in the background */
/* values of curflag */
#define CURP1	1	/* cursor after prompt */
#define CURP2	2	/* cursor after secondary prompt */
#define CURHOME	3	/* cursor at home position */
/* flags for vsave routine */
#define SVHEAD	01	/* write out article header */
#define OVWRITE	02	/* overwrite the file if it already exists */
/* other files */

#define	saveart	oobit = bit;strcpy(ofilename1, filename);strcpy(ogroupdir, groupdir);hptr = h;h = hold;hold = hptr;ongsize = pngsize
#define NLINES(h, fp) (h->numlines[0] ? h->intnumlines : (h->intnumlines=linecnt(fp),sprintf(h->numlines, "%d", h->intnumlines), h->intnumlines))

/* terminal handler stuff */
extern int _junked;
#define okclear() (_junked = 1)
extern int COLS;
extern int ROWS;
extern int hasscroll;

FILE *tmpfile();
char *getmailname();
#ifdef MYDB
char *findparent();
#endif /* MYDB */
int onint();
int onstop();
int xxit();

char *Progname = "vnews";		/* for xerror */

/* variables shared between vnews routines */
static char linebuf[LBUFLEN];		/* temporary workspace */
static FILE *tfp;			/* temporary file */
static char tfname[] = "/tmp/vnXXXXXX";	/* name of temp file */
static long artbody;			/* offset of body into article */
static int quitflg;			/* if set, then quit */
static int erased;			/* current article has been erased */
static int artlines;			/* # lines in article body */
static int artread;			/* entire article has been read */
static int hdrstart;			/* beginning of header */
static int hdrend;			/* end of header */
static int lastlin;			/* number of lines in tempfile */
static int tflinno = 0;			/* next line in tempfile */
static int maxlinno;			/* number of lines in file + folded */
static char secpr[SECPRLEN];		/* secondary prompt */
static char prompt[30];			/* prompter */
static short prflags;			/* print flags (controls updscr) */
static short curflag;			/* where to locate cursor */
static int dlinno;			/* top line on screen */
static char timestr[20];		/* current time */
static int ismail;			/* true if user has mail */
static char *mailf;			/* user's mail file */
static int alflag;			/* set if unprocessed alarm signal */
static int atend;			/* set if at end of article */
static char cerase;			/* erase character */
static char ckill;			/* kill character */
static char cintr;			/* interrupt character */
#ifdef TIOCGLTC
static char cwerase;			/* word erase character */
#endif /* TIOCGLTC */
short ospeed;				/* terminal speed NOT STATIC */
static int intflag;			/* set if interrupt received */

#ifdef SIGTSTP
static int reading;			/* to keep stupid BSD from restarting reads */
jmp_buf intjmp, alrmjmp;
#endif /* SIGTSTP */

#ifdef MYDB
static int hasdb;			/* true if article data base exists */
#endif /* MYDB */

#ifdef DIGPAGE
static int endsuba;			/* end of sub-article in digest */
#endif

#ifdef MYDEBUG
FILE *debugf;				/* file to write debugging info on */
#endif

char *tft = "/tmp/folXXXXXX";

/*
 * These were made static for u370 with its buggy cc.
 * I judged it better to have one copy with no ifdefs than
 * to conditionally compile them as automatic variables
 * in readr (which they originally were).  Performance
 * considerations might warrant moving some of the simple
 * things into register variables, but I don't know what
 * breaks the u370 cc.
 */
#ifndef SERVER
static char goodone[BUFLEN];		/* last decent article		*/
#endif	/* !SERVER */
static char ogroupdir[BUFLEN];		/* last groupdir		*/
static char edcmdbuf[128];
static int rfq = 0;			/* for last article		*/
static long ongsize;			/* Previous ngsize		*/
static long pngsize;			/* Printing ngsize		*/
static char *bptr;			/* temp pointer.		*/
static char *tfilename;			/* temporary file name 		*/
static char ofilename1[BUFLEN];		/* previous file name		*/
static struct hbuf hbuf1, hbuf2; 	/* for minusing			*/
static struct hbuf *h = &hbuf1,		/* current header		*/
		*hold = &hbuf2,		/* previous header		*/
		*hptr;			/* temporary			*/
static char *ptr1, *ptr2, *ptr3;	/* for reply manipulation	*/
static int  aabs = FALSE;		/* TRUE if we asked absolutely	*/
static char *ed, tf[100];
static long oobit;			/* last bit, really		*/
static int dgest = 0;
static FILE *fp;			/* current article to be printed*/

readr()
{

#ifdef MYDEBUG
	debugf = fopen("DEBUG", "w");
	setbuf(debugf, (char *)NULL);
#endif
	if (aflag) {
		if (*datebuf) {
			if ((atime = cgtdate(datebuf)) == -1)
				xerror("Cannot parse date string");
		} else
			atime = 0;
	}

	if (SigTrap)
		xxit(1);
	(void) mktemp(tfname);
	(void) close(creat(tfname,0666));
	if ((tfp = fopen(tfname, "w+")) == NULL)
		xerror("Can't create temp file");
	(void) unlink(tfname);
	mailf = getmailname();
#ifdef MYDB
	if (opendb() >= 0) {
		hasdb = 1;
		fputs("Using article data base\n", stderr);	/*DEBUG*/
		getng();
	}
#endif
	ttysave();
	(void) signal(SIGINT, onint);
	(void) signal(SIGQUIT, xxit);
	if (SigTrap)
		xxit(1);
	ttyraw();
	timer();

	/* loop reading articles. */
	fp = NULL;
	obit = -1;
	nextng();
	quitflg = 0;
	while (quitflg == 0) {
		if (getnextart(FALSE))
			break;
#ifndef SERVER
		(void) strcpy(goodone, filename);
#endif	/* !SERVER */
		if (SigTrap)
			return;
		vcmd();
	}

	if (!news) {
		ttycooked();
		ospeed = 0;	/* to convince xxit() not to clear screen */
		fprintf(stderr, "No news.\n");
	}
}

/*
 * Read and execute a command.
 */
vcmd() {
	register c;
	char *p;
	long count;
	int countset;

	if (prflags & HDRONLY)
		appfile(fp, lastlin + 1);
	else
		appfile(fp, dlinno + ARTWLEN + 1);

#ifdef DIGPAGE
	endsuba = findend(dlinno);
	if (artlines > dlinno + ARTWLEN
	 || endsuba > 0 && endsuba < artlines
#else
	if (artlines > dlinno + ARTWLEN
#endif
	 || (prflags & HDRONLY) && artlines > hdrend) {
		atend = 0;
		if (prflags&HDRONLY || maxlinno == 0)
			(void) strcpy(prompt, "more? ");
		else
#ifdef DIGPAGE
			(void) sprintf(prompt, "more(%d%%)? ",
				((((endsuba > 0) ?
				endsuba : (dlinno + ARTWLEN)) -
				hdrend) * 100) / maxlinno);
#else /* !DIGPAGE */
			(void) sprintf(prompt, "more(%d%%)? ",
				((dlinno + ARTWLEN - hdrend) * 100) / maxlinno);
#endif /* !DIGPAGE */
	} else {
		atend = 1;
		(void) strcpy(prompt, "next? ");
		if (!erased)
			clear(bit);		/* article read */
	}
	curflag = CURP1;
	p = prompt + strlen(prompt);
	countset = 0;
	count = 0;
	/*
	 * Loop while accumulating a count, until an action character
	 * is entered. Also handle "meta" here.
	 *
	 * Count is the current count. Countset=0 means no count
	 * currently exists. Countset=1, count=0 is valid and means 
	 * a count of 0 has been entered 
	 */
	for (;;) {
		c = vgetc();
		if (c == cerase || c == '\b' || c == '\177') {
			if (countset == 0)
				break;		/* Use as action char */
			if (count < 10)
				countset = 0;	/* Erase only char of count */
			else
				count /= 10L;	/* Erase 1 char of count */
		} else {
#ifdef TIOCGLTC
			if (c == ckill || c == cwerase) {
#else
			if (c == ckill) {
#endif
				if (countset == 0)
					break;
				countset = 0;
			} else if (c < '0' || c > '9')
					break;
				else {
					countset = 1;
					count = (count * 10) + (c - '0');
				}
		}
		if (countset) {
			(void) sprintf(p, "%ld", count);
		} else {
			*p = '\0';
			count = 0;
		}
	}

	if (c == '\033') {			/* escape */
		(void) strcat(prompt, "M-");
		c = vgetc();
		if (c != cintr)
			c |= META;
	}
	secpr[0] = '\0';
	if (countset == 0)
		count = 1;
	docmd(c, count, countset);
	if (c != '?' && c != 'H')		/* UGGH */
		prflags &=~ HELPMSG;
	if (dlinno > hdrstart)
		prflags &=~ HDRONLY;
}


/*
 * Process one command, which has already been typed in.
 */
docmd(c, count, countset)
int c;
long count;
int countset;
{
	int i;
	long nart, Hoffset;
	char *findhist();

	switch (c) {

	/* display list of articles in current group */
	case 'l':
	case 'L':
		botscreen();
		ttycooked();
		list_group(groupdir, countset ? (int) count : 0,
			(c == 'l') ? FALSE : TRUE, pngsize);
		ttyraw();
		okclear();
		updscr();
		break;

	/* Show more of current article, or advance to next article */
	case '\n':
	case ' ':
#ifdef DIGPAGE
	case 'm':
#endif /* DIGPAGE */
	case '\06':	/* Control-F for vi compat */
		prflags &=~ NOPRT;
		if (atend)
			goto next;
		else if (prflags & HDRONLY) {
			prflags &=~ HDRONLY;
			if (hasscroll)
				dlinno = hdrstart;}
#ifdef DIGPAGE
		else if (endsuba > 0)
			dlinno = endsuba;
		else if (c == 'm') {
			do {
				if (lastlin >= maxlinno)
					goto next;
				else
					appfile(fp, lastlin + 1);
			} while(STRNCMP(linebuf, "------------------------", 24)
				!= 0);
			dlinno = endsuba = lastlin;
		}
#endif
		else if ((appfile(fp, dlinno + 2 * ARTWLEN), artread)
		 && hasscroll && artlines - dlinno <= ARTWLEN + 2)
			dlinno = artlines - ARTWLEN;
		else
			dlinno += ARTWLEN * count;
		break;

	/* No.  Go on to next article. */
	case '.':	/* useful if you have a keypad */
next:	case 'n':
		readmode = NEXT;
		FCLOSE(fp);
		clear(bit);
		saveart;
		nextbit();
		break;


	/* Back up count pages */
	case '\b':	
	case '\177':	
		if (dlinno == 0)
			goto backupone;
		/* NO BREAK */
	case META|'v':
	case '\002':	/* Control-B */
		dlinno -= ARTWLEN * count;
		if (dlinno < 0)
			dlinno = 0;
		break;

	/* forward half a page */
	case '\004':	/* Control-D, as in vi */
		if (!atend)
			dlinno += ARTWLEN/2 * count;
		break;

	/* backward half a page */
	case '\025':	/* Control-U */
		dlinno -= ARTWLEN/2 * count;
		if (dlinno < 0)
			dlinno = 0;
		break;

	/* forward count lines */
	case '\016':	/* Control-N */
	case '\005':	/* Control-E */
		dlinno += count;
		break;

	/* backwards count lines */
	case '\020':	/* Control-P */
	case '\031':	/* Control-Y */
		dlinno -= count;
		if (dlinno < 0)
			dlinno = 0;
		break;

	/* Turn displaying of article back on */
	case 'd':
		prflags &=~ NOPRT;
		break;

	/* display header */
	case 'h':
		dlinno = hdrstart;
		prflags |= HDRONLY;
		prflags &=~ NOPRT;
		break;

	/*
	 * Unsubscribe to the newsgroup and go on to next group
	 */

	case 'U':
	case 'u':
		strcat(prompt, "u");
		c = vgetc();
		if (c == 'g') {
			obit = -1;
			FCLOSE(fp);
			zapng = TRUE;
			saveart;
			if (nextng()) {
				if (actdirect == BACKWARD)
					msg("Can't back up.");
				else
					quitflg = 1;	/* probably unnecessary */
			}
		} else {
			if (c != cintr && c != ckill)
				beep();
				msg("Illegal command");
		}
		break;

		/* Print the current version of news */
	case 'v':
		msg("News version: %s", news_version);
		break;


	/* Decrypt joke.  Always does rot 13 */
	case 'D':
		appfile(fp, 32767);
		for (i = hdrend ; i < artlines ; i++) {
			register char ch, *p;
			tfget(linebuf, i);
			for (p = linebuf ; (ch = *p) != '\0' ; p++) {
				if (ch >= 'a' && ch <= 'z')
					*p = (ch - 'a' + 13) % 26 + 'a';
				else if (ch >= 'A' && ch <= 'Z')
					*p = (ch - 'A' + 13) % 26 + 'A';
			}
			tfput(linebuf, i);
		}
		prflags |= NEWART;
		prflags &=~ (HDRONLY|NOPRT);
		break;

		/* write out the article someplace */
		/* w writes out without the header */
		/* | defaults to pipeing */
	{
		static char savebuf[BUFLEN];
		int wflags;

	case PIPECHAR:
	case 's':
	case 'w':
		/* We loop back to here each time user types ^U to prompt */
		do {
			/* Prompt based on command char */
			msg( (c==PIPECHAR)? "|": "file: ");
			curflag = CURP2;
			while ((wflags = vgetc()) == ' ');
			if (wflags == cintr) {
				secpr[0] = '\0';
				break;
			}
			if (wflags != CAGAIN) {
				if ((wflags & 0x1F) == wflags) {	/* control char */
					pushback(wflags);
					savebuf[0] = 0;
				} else {
					if (c == PIPECHAR) {
						savebuf[0] = PIPECHAR;
						savebuf[1] = wflags;
						savebuf[2] = 0;
					} else {
						savebuf[0] = wflags;
						savebuf[1] = 0;
					}
				}
			} else {
				/* don't let them pipe to a saved filename */
				if (c == PIPECHAR && savebuf[0] != PIPECHAR) {
					savebuf[0] = PIPECHAR;
					savebuf[1] = 0;
				}
			}
					
			wflags = prget( (savebuf[0] == PIPECHAR) ? "" : "file: ",
					savebuf);
		} while (wflags == 2);
		if (wflags) break;	/* Interrupted out */
		wflags = 0;
		if (c == PIPECHAR) c = 's';
		if (c == 's')
			wflags |= SVHEAD;
		if (count != 1)
			wflags |= OVWRITE;
		bptr = savebuf;
		while( *bptr == ' ')
			bptr++;	/* strip leading spaces */

		if (*bptr != PIPECHAR && *bptr != '/') {
			char	hetyped[BUFLEN];
			char	*boxptr;
			(void) strcpy(hetyped, bptr);
			if (hetyped[0] == '~' && hetyped[1] == '/') {
  				strcpy(hetyped, bptr+2);
  				strcpy(bptr, userhome);
			} else if (boxptr = getenv("NEWSBOX")) {
 				if (index(boxptr, '%')) {
					struct stat stbf;
 					sprintf(bptr, boxptr, groupdir);
 					if (stat(bptr,&stbf) < 0) {
 						if (mkdir(bptr, 0777) < 0) {
							msg("Cannot create directory %s", bptr);
							break;
						}
					} else if ((stbf.st_mode&S_IFMT) !=  S_IFDIR) {
						msg("%s not a directory", bptr);
						break;
					}
				} else
					strcpy(bptr, boxptr);
  			 } else
  				bptr[0] = '\0';

			if (bptr[0])
				(void) strcat(bptr, "/");
			if (hetyped[0] != '\0')
				(void) strcat(bptr, hetyped);
			else
				(void) strcat(bptr, "Articles");
		}

		/* handle ~/ for pipes */
		if (*bptr == PIPECHAR) {
			char	fullname[BUFLEN];
			bptr++;		/* skip PIPECHAR */
			while( *bptr == ' ')
				bptr++;	/* strip leading spaces */
			if (bptr[0] == '~' && bptr[1] == '/') {
				strcpy(fullname,userhome);
				strcat(fullname,bptr+2);
			} else
				strcpy(fullname,bptr);
			/* we know PIPECHAR is in *savebuf */
			strcpy(savebuf+1,fullname);
			bptr = savebuf;
		}
				
		vsave(bptr, wflags);
		break;
	}

		/* back up  */
	case '-':
caseminus:
		aabs = TRUE;
		if (!*ofilename1) {
			msg("Can't back up.");
			break;
		}
		FCLOSE(fp);
		hptr = h;
		h = hold;
		hold = hptr;
		(void) strcpy(bfr, filename);
		(void) strcpy(filename, ofilename1);
		(void) strcpy(ofilename1, bfr);
		obit = bit;
		if (STRCMP(groupdir, ogroupdir)) {
			(void) strcpy(bfr, groupdir);
			selectng(ogroupdir, FALSE, FALSE);
			(void) strcpy(groupdir, ogroupdir);
			(void) strcpy(ogroupdir, bfr);
			ngrp = 1;
			back();
		}
		bit = oobit;
		oobit = obit;
		obit = -1;
		getnextart(TRUE);
		break;

		/* skip forwards */
	case '+':
	case '=':
caseplus:	if (count == 0)
			break;
		saveart;
		last = bit;
		for (i = 0; i < count; i++) {
			nextbit();
			if ((bit > pngsize) || (rflag && bit < 1))
				break;
		}
		FCLOSE(fp);
		obit = -1;
		break;

	/* exit - time updated to that of most recently read article */
	case 'q':
		quitflg = 1;
		break;

	case 'x':
		xxit(0);
		break;

	/* cancel the article. */
	case 'c':
		strcpy(prompt, "cancel [n]? ");
		if (vgetc() != 'y') {
			msg("Article not cancelled");
			break;
		}
		cancel_command();
		break;

	/* escape to shell */
	case '!': {
		register char *p;
		int flags;

		p = linebuf;
		*p = 0;
		if (prget("!", p))
			break;
		flags = CWAIT;
		if (*p == '\0') {
			(void) strcpy(linebuf, SHELL);
			flags = 0;
		}
		while (*p) p++;
		while (p > linebuf && p[-1] == ' ')
			p--;
		if (*--p == '&') {
			*p = '\0';
			flags = BKGRND;
		} else if (*p == PIPECHAR) {
			*p = '\0';
			(void) sprintf(bfr, "(%s)%cmail '%s'", linebuf, PIPECHAR, username);
			(void) strcpy(linebuf, bfr);
			flags |= BKGRND;
		} else {
			prflags |= NOPRT;
		}
		shcmd(linebuf, flags);
		break;
	}

	/* mail reply */
	case 'r':
		reply(FALSE);
		break;

	case 'R':
		reply(TRUE);
		break;

	case META|'r':
		direct_reply();
		break;

	/* next newsgroup */
	case 'N':
		FCLOSE(fp);
		if (next_ng_command())
			quitflg = 1;
		break;

	/*  mark the rest of the articles in this group as read */
	case 'K':
		saveart;
		while (bit <= ngsize && bit >= minartno) {
			clear(bit);
			nextbit();
		}
		FCLOSE(fp);
		break;

	/* Print the full header */
	case 'H':
		if (fp == NULL) {
			msg("No current article");
			break;
		}
		move(ARTWIN, 0);
		Hoffset = ftell(fp);
		(void) fseek(fp, 0L, 0);
		for (i = 0; i < ARTWLEN; i++) {
			if (fgets(linebuf, COLS, fp) == NULL)
				break;
			if (linebuf[0] == '\n')
				break;
			linebuf[COLS] = '\0';
			addstr(linebuf);
		}
		(void) fseek(fp, Hoffset, 0);
		for(; i < ARTWLEN; i++)
			addstr(linebuf);
		prflags |= HELPMSG|NEWART;
		break;
	case 'b':	/* backup 1 article */
backupone:
		count = bit - 1;
		/* NO BREAK */

	case 'A':	/* specific number */
		if (count > pngsize) {
			msg("not that many articles");
			break;
		}
		readmode = SPEC;
		aabs = TRUE;
		bit = count;
		obit = -1;
		FCLOSE(fp);
		break;

	/* display parent article */
	case 'p':
#ifdef MYDB
		if (hasdb && (ptr3 = findparent(h->ident, &nart)) != NULL) {
			msg("parent: %s/%ld", ptr3, nart);	/*DEBUG*/
			updscr();				/*DEBUG*/
			goto selectart;
		}
#endif
		if (h->followid[0] == '\0') {
			msg("no references line");
			break;
		}
		ptr1 = h->followid + strlen(h->followid);
		do {
			ptr2 = ptr1;
			if (*ptr2 == '\0')
				ptr1 = rindex(h->followid, ' ');
			else {
				*ptr2 = '\0';
				ptr1 = rindex(h->followid, ' ');
				*ptr2 = ' ';
			}
		} while (ptr1 != NULL && --count > 0);
		if (ptr1 == NULL)
			ptr1 = h->followid;
		else	++ptr1;
		(void) strncpy(linebuf, ptr1, ptr2 - ptr1);
		linebuf[ptr2 - ptr1] = '\0';
		msg("%s", linebuf);
		curflag = CURP2;
		updscr();		/* may take this out later */
		goto searchid;
	/* specific message ID. */
	case '<':
		/* could improve this */
		linebuf[0] = '<'; linebuf[1] = 0;
		if (prget("", linebuf)) {
			secpr[0] = 0;
			break;
		}
searchid:	secpr[0] = '\0';
		if (index(linebuf, '@') == NULL && index(linebuf, '>') == NULL) {
			ptr1 = linebuf;
			if (*ptr1 == '<')
				ptr1++;
			ptr2 = index(ptr1, '.');
			if (ptr2 != NULL) {
				*ptr2++ = '\0';
				(void) sprintf(bfr, "<%s@%s.UUCP>", ptr2, ptr1);
				(void) strcpy(linebuf, bfr);
			}
		}
		if (index(linebuf, '>') == NULL)
			(void) strcat(linebuf, ">");

		ptr1 = findhist(linebuf);
		if (ptr1 == NULL) {
			msg("%s not found", linebuf);
			break;
		}
		ptr2 = index(ptr1, '\t');
		ptr3 = index(++ptr2, '\t');
		ptr2 = index(++ptr3, ' ');
		if (ptr2)
			*ptr2 = '\0';
		ptr2 = index(ptr3, '/');
		if (!ptr2) {
			if (STRCMP(ptr3, "cancelled") == 0)
				msg("%s has been cancelled", linebuf);
			else
				msg("%s has expired", linebuf);
			break;
		}
		*ptr2++ = '\0';
		(void) sscanf(ptr2, "%ld", &nart);

		/*
		 * Go to a given article.  Ptr3 specifies the newsgroup
		 * and nart specifies the article number.
		 */
#ifdef MYDB
selectart:
#endif /* MYDB */
		aabs = TRUE;
		FCLOSE(fp);
		saveart;
		(void) strcpy(ogroupdir, ptr3);
		if (STRCMP(groupdir, ogroupdir)) {
			(void) strcpy(bfr, groupdir);
			selectng(ogroupdir, TRUE, PERHAPS);
			(void) strcpy(groupdir, ogroupdir);
			(void) strcpy(ogroupdir, bfr);
			ngrp = 1;
			back();
		}
		bit = nart;
		oobit = obit;
		obit = -1;
		getnextart(TRUE);
		if (bit != nart || STRCMP(groupdir, ptr3) != 0) {
			msg("can't read %s/%ld", ptr3, nart);
			goto caseminus;
		}
		rfq = 0;
		break;

	/* follow-up article */
	case 'f':
		if (STRCMP(h->followto, "poster") == 0) {
			reply(FALSE);
			break;
		}
#ifdef SERVER
		(void) sprintf(bfr, "%s/%s", BIN, "postnews");
#else	/* !SERVER */
		(void) sprintf(bfr, "%s/%s %s", BIN, "postnews", goodone);
#endif	/* !SERVER */
		shcmd(bfr, CWAIT);
		break;

	/* erase - pretend we haven't seen this article. */
	case 'e':
		erased = 1;
		set(bit);
		goto caseplus;	/* skip this article for now */

	case '#':
		msg("Article %ld of %ld", rfq ? oobit : bit, pngsize);
		break;

		/* error */
	case '?':
		{
			FILE *helpf;
			(void) sprintf(linebuf, "%s/vnews.help", LIB);
			if ((helpf = fopen(linebuf, "r")) == NULL) {
				msg("Can't open help file");
				break;
			}
			move(ARTWIN, 0);
			while (fgets(linebuf, LBUFLEN, helpf) != NULL)
				addstr(linebuf);
			(void) fclose(helpf);
			prflags |= HELPMSG|NEWART;
		}
		break;

	default:
		if (c != ckill && c != cintr && c != cerase) 
#ifdef TIOCGLTC
			if (c != cwerase)
#endif
			{
				beep();
				msg("Illegal command");
			}
		break;
	}
}

cancel_command()
{
	register char *poster, *r;
	int notauthor;
	char *senderof();

	poster = senderof(h);
	/* only compare up to '.' or ' ' */
	r = index(poster,'.');
	if (r == NULL)
		r = index(poster,' ');
	if (r != NULL)
		*r = '\0';
	tfilename = filename;
	notauthor = STRCMP(username, poster);
	if (uid != ROOTID && uid && notauthor) {
		msg("Can't cancel what you didn't write.");
		return;
	}
	if (!cancel(stderr, h, notauthor)) {
		clear(bit);
		saveart;
		nextbit();
		obit = -1;
		fp = NULL;
	}
	FCLOSE(fp);
}
/*
 * Generate replies
 */

reply(include)
	int include;
{
	char *arg[4];
	register FILE *rfp;
	char subj[132];
	register char *p;
	char *replyname();
	struct stat statb;
	time_t creatm;

	/* Put the user in the editor to create the body of the reply. */
	ed = getenv("EDITOR");
	if (ed == NULL || *ed == '\0')
		ed = DFTEDITOR;
	if (ed == NULL) {
		msg("You don't have an editor");
		return;
	}

	arg[0] = "/bin/sh";
	arg[1] = "-c";

	(void) strcpy(tf, tft);
	(void) mktemp(tf);
	(void) close(creat(tf,0600));
	if ((rfp = fopen(tf, "w")) == NULL) {
		msg("Can't create %s", tf) ;
		return;
	}
	(void) strcpy(subj, h->title);
	if (!PREFIX(subj, "Re:")){
		(void) strcpy(bfr, subj);
		(void) sprintf(subj, "Re: %s", bfr);
	}

	p = replyname(h);
	fprintf(rfp, "To: %s\n", p);
	fprintf(rfp, "Subject: %s\n", subj);
	fprintf(rfp, "In-reply-to: your article %s\n", h->ident);
#ifdef INTERNET
	fprintf(rfp, "News-Path: %s\n", h->path);
#endif /* INTERNET */
	(void) sprintf(rcbuf, "%s -t < %s; rm -f %s", MAILPARSER, tf, tf);
	putc('\n', rfp);
	if (include) {
		FILE *of;
		char buf[BUFSIZ];

#ifndef SERVER
		of = xart_open(goodone, "r");
		while (fgets(buf, sizeof buf, of) != NULL)
			if (buf[0] == '\n')
				break;
		while (fgets(buf, sizeof buf, of) != NULL)
			fprintf(rfp, "> %s", buf);
		fclose(of);
		putc('\n', rfp);
#endif		/* !SERVER */
	}

	fflush(rfp);
	(void) fstat(fileno(rfp), &statb);
	creatm = statb.st_mtime;
	(void) fclose(rfp);

	(void) sprintf(edcmdbuf, "exec %s %s", ed, tf);
	arg[2] = edcmdbuf;
	arg[3] = NULL;
	if (prun(arg, 0) != 0) {
		msg("Couldn't run editor");
		(void) unlink(tf);
		return;
	}

	if (access(tf, 4) || stat(tf, &statb)) {
		msg("No input file - mail not sent");
		(void) unlink(tf);
		return;
	}
	if (statb.st_mtime == creatm || statb.st_size < 5) {
		msg("File unchanged - no message posted");
		(void) unlink(tf);
		return;
	}

	arg[2] = rcbuf;
	arg[3] = NULL;
	prun(arg, BKGRND);
	prflags |= NOPRT;
}

direct_reply()
{
	register char *p;
	register char *q;
	char *arg[4];
	char address[PATHLEN];
	extern char *replyname();
	extern char *getenv();

	arg[0] = "/bin/sh";
	arg[1] = "-c";
	p = replyname(h);
	q = address;
	while (*p != '\0') {
		if (index("\"\\$", *p) != 0)
			*q++ = '\\';
		*q++ = *p++;
	}
	*q++ = '\0';
	if ((MAILER = getenv("MAILER")) == NULL)
		MAILER = "mail";
	sprintf(rcbuf, MAILER, hptr->title);
	sprintf(bfr, "%s %s", rcbuf, address);
	arg[2] = bfr;
	arg[3] = NULL;
	if (prun(arg, 0) != 0) {
		msg("Couldn't run mailer");
		return;
	}
	prflags |= NOPRT;
}

next_ng_command()
{
	set(bit);
	obit = -1;
	linebuf[0] = 0;
	if (prget("group? ", linebuf))
		return FALSE;
	bptr = linebuf;
	if (!*bptr || *bptr == '-') {
		if (*bptr)
			actdirect = BACKWARD;
		saveart;
		if (nextng()) {
			if (actdirect == BACKWARD)
				msg("Can't back up.");
			else
				return TRUE;
		}
		return FALSE;
	}
	while (isspace(*bptr))
		bptr++;
	if (!validng(bptr)) {
		msg("No such group.");
		return FALSE;
	}
	saveart;
	back();
	selectng(bptr, TRUE, TRUE);
	return FALSE;
}

/*
 * Find the next article we want to consider, if we're done with
 * the last one, and show the header.
 */
getnextart(minus)
int minus;
{
	int noaccess;
#ifdef SERVER
	char workspace[256];
#else	/* !SERVER */
	register DIR *dirp;
	register struct direct *dir;
#endif	/* !SERVER */
	long nextnum, tnum;
	long atol();
	noaccess = 0;
	if (minus)
		goto nextart2;	/* Kludge for "-" command. */

	if (bit == obit)	/* Return if still on same article as last time */
		return 0;

nextart:
	if (news) {
		curflag = CURHOME;
		_amove(0, 0);
		vflush();
	}
	dgest = 0;

	/* If done with this newsgroup, find the next one. */
	while (ngsize <= 0 || (!rflag && ((long) bit > ngsize)) || (rflag && bit < minartno)) {
		if (nextng()) {
			if (actdirect == BACKWARD) {
				msg("Can't back up.");
				actdirect = FORWARD;
				continue;
			}
			else /* if (rfq++ || pflag || cflag) */
				return 1;
		}
		if (rflag)
			bit = ngsize + 1;
		else
			bit = -1;
		noaccess = 2;
	}

	/* speed things up by not searching for article -1 */
	if (bit < 0) {
		bit = minartno - 1;
		nextbit();
		aabs = FALSE;
		goto nextart;
	}

nextart2:
	if (rcreadok)
		rcreadok = 2;	/* have seen >= 1 article */
#ifdef SERVER
	if ((fp = getarticle(groupdir, bit, "ARTICLE")) == NULL)
		goto badart;
	strcpy(filename, article_name());
	(void) fclose(fp);
	fp = NULL;
#else	/* !SERVER */
	(void) sprintf(filename, "%s/%ld", dirname(groupdir), bit);
	if (rfq && goodone[0])	/* ??? */
		strcpy(filename, goodone);
#endif	/* !SERVER */
	if (SigTrap == SIGHUP)
		return 1;
	/* Decide if we want to show this article. */
	if ((fp = art_open(filename, "r")) == NULL) {
		/* since there can be holes in legal article numbers, */
		/* we wait till we hit 5 consecutive bad articles */
		/* before we haul off and scan the directory */
		if (++noaccess < 5)
			goto badart;
		noaccess = 0;
#ifdef SERVER
		if (*groupdir == ' ' || *groupdir == '\0' || 
			set_group(groupdir) == NULL)
			goto nextart;
#else	/* !SERVER */
		dirp = opendir(dirname(groupdir));
		if (dirp == NULL) {
			if (errno != EACCES)
				msg("Can't open %s", dirname(groupdir));
			goto nextart;
		}
#endif	/* !SERVER */
		nextnum = rflag ? minartno - 1 : ngsize + 1;
#ifdef SERVER 
		tnum = nextnum;
		for(;;){
			(void) sprintf(bfr,"STAT %ld",tnum);
			put_server(bfr);
			(void) get_server(workspace,sizeof(workspace));
			if (*workspace != CHAR_OK) {
				if (rflag)
					tnum++;
				else
					tnum--;
				continue;
			}
#else	/* !SERVER */
		while ((dir = readdir(dirp)) != NULL) {
			if (!dir->d_ino)
				continue;
			tnum = atol(dir->d_name);
			if (tnum <= 0)
				continue;
#endif	/* !SERVER */
			if (rflag ? (tnum > nextnum && tnum < bit)
				  : (tnum < nextnum && tnum > bit))
				nextnum = tnum;
#ifdef SERVER
			break;		/* not exactly right */
#endif	/* SERVER */
		}
#ifndef SERVER
		closedir(dirp);
#endif	/* !SERVER */
		if (rflag ? (nextnum >= bit) : (nextnum <= bit))
			goto badart;
		do {
			clear(bit);
			nextbit();
		} while (rflag ? (nextnum < bit) : (nextnum > bit));
		obit = -1;
		aabs = FALSE;
		goto nextart;
	} else
		noaccess = 0;

	if (hread(h, fp, TRUE) == NULL || (!rfq && !aselect(h, aabs))) {
badart:
		FCLOSE(fp);
		clear(bit);
		obit = -1;
		nextbit();
		aabs = FALSE;
		goto nextart;
	}
	aabs = FALSE;
	actdirect = FORWARD;
	news = TRUE;
	artbody = ftell(fp);
	fmthdr();
	artlines = lastlin;
	artread = 0;
	prflags |= NEWART;
	prflags &=~ NOPRT;
	if (! cflag && hdrend < ARTWLEN && !cflag)
		prflags |= HDRONLY;
	dlinno = 0;
	maxlinno = NLINES(h, fp);
	erased = 0;

	obit = bit;
#ifdef SERVER
	(void) unlink(filename);
#endif	/* SERVER */
	return 0;
}

/*
 * Print out whatever the appropriate header is
 */
fmthdr() {
	char *briefdate();
	static FILE *ngfd = NULL;
	static int triedopen = 0;
	char pbuf[BUFLEN], *printbuffer = groupdir;

	lastlin = 0;
	if (ngrp) {
		pngsize = ngsize;
		ngrp--;
		if (!hflag) {
			if (!triedopen) {
				(void) sprintf(pbuf,"%s/newsgroups", LIB);
				ngfd = fopen(pbuf, "r");
				triedopen++;
			}
			if (ngfd != NULL) {
				register char *p;
				char ibuf[BUFLEN];
				rewind(ngfd);
				while (fgets(ibuf, BUFLEN, ngfd) != NULL) {
					p = index(ibuf, '\t');
					if (p)
						*p++ = '\0';
					if (STRCMP(ibuf, groupdir) == 0) {
						register char *q;
						q = rindex(p, '\t');
						if (q) {
							p = q;
							*p++ = '\0';
						}
						if (p) {
							q = index(p, '\n');
							if (q)
								*q = '\0';
							if (*--q == '.')
								*q = '\0';
						(void) sprintf(pbuf,"%s (%s)",
							groupdir, p);
							printbuffer = pbuf;
						}
						break;
					}
				}
			}
			(void) sprintf(linebuf, "Newsgroup %s", printbuffer);
			tfappend(linebuf);
		}
	}
	hdrstart = lastlin;
	if (!hflag) {
		(void) sprintf(linebuf, "Article %s %s",
			h->ident, briefdate(h->subdate));
		tfappend(linebuf);
	}
	xtabs(h);
	vhprint(h, pflag ? 1 : 0);
	(void) sprintf(linebuf, "(%d lines)", NLINES(h, fp)); tfappend(linebuf);
	tfappend("");
	hdrend = lastlin;
}

/*
 * Grow tabs into spaces in header fields, 'cause the rest of this
 * lax program drops turds all over tabs (so it does with \b's, but ..)
 */
xtabs(p)
register struct hbuf *p;
{
	xtabf(p->from, sizeof p->from);
	xtabf(p->path, sizeof p->path);
	xtabf(p->nbuf, sizeof p->nbuf);
	xtabf(p->title, sizeof p->title);
	xtabf(p->ident, sizeof p->ident);
	xtabf(p->replyto, sizeof p->replyto);
	xtabf(p->followid, sizeof p->followid);
	xtabf(p->subdate, sizeof p->subdate);
	xtabf(p->expdate, sizeof p->expdate);
	xtabf(p->ctlmsg, sizeof p->ctlmsg);
	xtabf(p->sender, sizeof p->sender);
	xtabf(p->followto, sizeof p->followto);
	xtabf(p->distribution, sizeof p->distribution);
	xtabf(p->organization, sizeof p->organization);
	xtabf(p->numlines, sizeof p->numlines);
	xtabf(p->keywords, sizeof p->keywords);
	xtabf(p->summary, sizeof p->summary);
	xtabf(p->approved, sizeof p->approved);
	xtabf(p->nf_id, sizeof p->nf_id);
	xtabf(p->nf_from, sizeof p->nf_from);
#ifdef DOXREFS
	xtabf(p->xref, sizeof p->xref);
#endif /* DOXREFS */
}

xtabf(s, size)
char *s;
int size;
{
	register char *p, *str;
	register c, i;
	char buf[LBUFLEN];

	str = s;
	if (index(str, '\t') == NULL)
		return;
	i = 0;
	for (p = buf; c = *str++; i++) {
		if (c == '\t') {
			*p++ = ' ';
			if ((i & 7) != 7)
				str--;
		} else if (c == '\n') {
			i = -1;
			*p++ = c;
		} else
			*p++ = c;
	}
	*p = '\0';
	strncpy(s, buf, size - 1);
}

/*
 * Print the file header to the temp file.
 */
vhprint(hp, verbose)
register struct hbuf *hp;
int	verbose;
{
	register char	*p1, *p2;
	char	fname[BUFLEN];
	char *tailpath();

	fname[0] = '\0';		/* init name holder */

	p1 = index(hp->from, '(');	/* Find the sender's full name. */
	if (p1 == NULL && hp->path[0])
		p1 = index(hp->path, '(');
	if (p1 != NULL) {
		(void) strcpy(fname, p1+1);
		p2 = index(fname, ')');
		if (p2 != NULL)
			*p2 = '\0';
	}

	(void) sprintf(linebuf, "Subject: %s", hp->title);
	tfappend(linebuf);
	if (!hflag && hp->summary[0])
		(void) sprintf(linebuf, "Summary: %s", hp->summary), tfappend(linebuf);
	if (!hflag && hp->keywords[0])
		(void) sprintf(linebuf, "Keywords: %s", hp->keywords), tfappend(linebuf);
	if (verbose) {
		(void) sprintf(linebuf, "From: %s", hp->from); tfappend(linebuf);
		(void) sprintf(linebuf, "Path: %s", hp->path); tfappend(linebuf);
		if (hp->organization[0]) {
			(void) sprintf(linebuf, "Organization: %s", hp->organization);
			tfappend(linebuf);
		}
	}
	else {
		if (p1 != NULL)
			*--p1 = '\0';		/* bump over the '(' */
#ifdef INTERNET
		/*
		 * Prefer Path line if it's in internet format, or if we don't
		 * understand internet format here, or if there is no reply-to.
		 */
		(void) sprintf(linebuf, "From: %s", hp->from);
#else
		(void) sprintf(linebuf, "Path: %s", tailpath(hp));
#endif
		if (fname[0] || (hp->organization[0] && !hflag)) {
			(void) strcat(linebuf, " (");
			if (fname[0] == '\0') {
				(void) strcpy(fname, hp->from);
				p2 = index(fname,'@');
				if (p2)
					*p2 = '\0';
			}
			(void) strcat(linebuf, fname);
			if (hp->organization[0] && !hflag) {
				(void) strcat(linebuf, " @ ");
				(void) strcat(linebuf, hp->organization);
			}
			(void) strcat(linebuf, ")");
		}
		tfappend(linebuf);
		if (p1 != NULL)
			*p1 = ' ';
		if (hp->ctlmsg[0]) {
			(void) sprintf(linebuf, "Control: %s", hp->ctlmsg);
			tfappend(linebuf);
		}
	}

	if (verbose) {
		(void) sprintf(linebuf, "Newsgroups: %s", hp->nbuf); tfappend(linebuf);
		(void) sprintf(linebuf, "Date: %s", hp->subdate); tfappend(linebuf);
		if (hp->sender[0]) {
			(void) sprintf(linebuf, "Sender: %s", hp->sender);
			tfappend(linebuf);
		}
		if (hp->replyto[0]) {
			(void) sprintf(linebuf, "Reply-To: %s", hp->replyto);
			tfappend(linebuf);
		}
		if (hp->followto[0]) {
			(void) sprintf(linebuf, "Followup-To: %s", hp->followto);
			tfappend(linebuf);
		}
	}
	else if (STRCMP(hp->nbuf, groupdir) != 0) {
		(void) sprintf(linebuf, "Newsgroups: %s", hp->nbuf);
		tfappend(linebuf);
		timer();
	}
}

#ifdef MYDB

char *
findparent(id, num)
char *id;
long *num;
{
	struct artrec a;
	char idbuf[BUFSIZE];
	char *ngname();

	strcpy(idbuf, id);
	lcase(idbuf);

	if (lookart(id, &a) == DNULL)
		return NULL;
	if (a.parent == DNULL)
		return NULL;
	readrec(a.parent, &a);
	*num = a.groups[0].artno;
	return ngname(a.groups[0].newsgroup);
}

#endif


/*
 * Append file to temp file, handling control characters, folding lines, etc.
 * We don't grow the temp file to more than nlines so that a user won't have
 * to wait for 20 seconds to read in a monster file from net.sources.
 * What we really want is coroutines--any year now.
 */

#define ULINE 0200
static char *maxcol;

appfile(iop, nlines)
register FILE *iop;
{
	register int c;
	register char *icol;	/* &linebuf[0] <= icol <= maxcol */

	if (artread || artlines >= nlines || iop == NULL)
		return;
	maxcol = linebuf;
	icol = linebuf;
	while ((c = getc(iop)) != EOF) {
		switch (c) {
		case ' ':
			if (icol == maxcol && icol < linebuf + LBUFLEN - 1) {
				*icol++ = ' ';
				maxcol = icol;
			} else {
				if (*icol == '_')
					*icol++ = ULINE | ' ';
				else
					icol++;
			}
			break;
		case '\t':
			icol = (icol - linebuf &~ 07) + 8 + linebuf;
			growline(icol);
			break;
		case '\b':
			if (icol > linebuf) --icol;
			break;
		case '\n':
			outline();
			if (artlines >= nlines)
				return;
			icol = linebuf;
			break;
		case '\r':
			icol = linebuf;
			break;
		case '\f':
			outline(); outline(); outline();
			if (artlines >= nlines)
				return;
			icol = linebuf;
			break;
		default:
			if (c < ' ' || c > '~')
				break;
			else if (icol >= linebuf + LBUFLEN - 1)
				icol++;
			else if (icol == maxcol) {
				*icol++ = c;
				maxcol = icol; }
			else if (c == '_')
				*icol++ |= ULINE;
			else if (*icol == '_')
				*icol++ = (c | ULINE);
			else	*icol++ = c;
			break;
		}
	}
	if (maxcol != linebuf)		/* file not terminated with newline */
		outline();
	artread++;
}

growline(col)
char *col;
{
	while (maxcol < col && maxcol < linebuf + LBUFLEN - 1)
		*maxcol++ = ' ';
}

outline()
{
	*maxcol = '\0';
	if (STRNCMP(linebuf, ">From ", 6) == 0) {
		register char *p;
		for (p = linebuf ; (*p = p[1]) != '\0' ; p++);
	}
	tfappend(linebuf);
	if (maxcol > linebuf)
		artlines = lastlin;
	maxcol = linebuf;
}


/*
 * Prompt the user and get a line.
 * "prompter" is the prompt.  "buf" contains a string which
 * will be used as the initial user response (which may be edited
 * by the user with backspace, ^U, etc).  The resulting line is
 * returned in "buf".  The result of prget() is:
 *	 0 if the line was terminated by NL or CR
 *	 1 if it was terminated by the interrupt character.
 *	 2 if it was terminated by erasing all the characters, including
 *	   one or more that were prompted initially in "buf".  (If "buf"
 * 	   was empty, this will never occur.)
 */
int
prget(prompter, buf)
char *prompter, *buf;
{
	register char *p, *q, *r;
	register char c;
	char lastc;
	char hadprompt = buf[0];

	curflag = CURP2;
	r = buf + strlen(buf);
	lastc = '\0';
	for (;;) {
		p = secpr;
		for (q = prompter ; *q ; q++)
			*p++ = *q;
		for (q = buf ; *q ; q++) {
			if (p < &secpr[SECPRLEN-1] && *q >= ' ' && *q <= '~')
				*p++ = *q;
		}
		*p = '\0';
		c = vgetc();
		if (c == '\n' || c == '\r' || c == cintr) {
			break;
		}
		if (c == cerase || c == '\b' || c == '\177') {
			if (lastc == '\\')
				r[-1] = c;
			else if (r > buf)
				r--;
		} else if (c == ckill) {
			if (lastc == '\\')
				r[-1] = c;
			else
				r = buf;
#ifdef TIOCGLTC
		} else if (c == cwerase) {
			if (lastc == '\\')
				r[-1] = c;
			else {
				while (r > buf && (r[-1] == ' ' || r[-1] == '\t'))
					r--;
				while (r > buf && r[-1] != ' ' && r[-1] != '\t')
					r--;
			}
#endif
		} else {
			*r++ = c;
		}
		lastc = c;
		*r = '\0';
		if ((r == buf) && hadprompt)
			return 2;
	}
	curflag = CURHOME;
	secpr[0] = '\0';
	return (c == cintr);
}



/*
 * Execute a shell command.
 */

shcmd(cmd, flags)
char *cmd;
{
	char *arg[4];

	arg[0] = SHELL, arg[1] = "-c", arg[2] = cmd, arg[3] = NULL;
	return prun(arg, flags);
}


prun(args, flags)
char **args;
{
	int pid;
	int i;
	int (*savequit)();
	char *env[100], **envp, **oenvp;
	char a[BUFLEN + 2];
	extern char **environ;
	int pstatus, retval;

	if (!(flags & BKGRND)) {
		botscreen();
		ttycooked();
#ifdef SIGTSTP
		(void) signal(SIGTSTP, SIG_DFL);
		(void) signal(SIGTTIN, SIG_DFL);
		(void) signal(SIGTTOU, SIG_DFL);
#endif
	}
#if defined(BSD4_2) && !defined(sun)
	while ((pid = vfork()) == -1)
#else /* !BSD4_2 */
	/* 4.1 BSD (at least) can't handle this vfork with -ljobs */
	while ((pid = fork()) == -1)
#endif /* !BSD4_2 */
		sleep(1);		/* must not clear alarm */
	if (pid == 0) {
		for (i = 3 ; i < 20 ; i++)
			close(i);
		if (flags & BKGRND) {
			(void) signal(SIGINT, SIG_IGN);
			(void) signal(SIGQUIT, SIG_IGN);
#ifdef SIGTSTP
			(void) signal(SIGTSTP, SIG_IGN);
			(void) signal(SIGTTIN, SIG_IGN);
			(void) signal(SIGTTOU, SIG_IGN);
#endif
			(void) close(0);
			(void) close(1);
			(void) open("/dev/null", 2);
			(void) dup(0);
		}
		/* set $A */
		(void) sprintf(a, "A=%s", filename);
		oenvp = environ;
		env[0] = a;
		for (envp = env + 1 ; *oenvp != NULL && envp < env + 98 ; oenvp++)
			if ((*oenvp)[0] != 'A' || (*oenvp)[1] != '=')
				*envp++ = *oenvp;
		*envp = NULL;

		(void) umask(savmask);
		execve(args[0], args, env);
		perror(args[0]);
		exit(20);
	}
	if (!(flags & BKGRND)) {
		savequit = signal(SIGQUIT, SIG_IGN);
		while ((i = wait(&pstatus)) != pid && (i != -1 || errno == EINTR))
			;
		if (i == -1)
			retval = 1;
		else
			retval = pstatus;
		if (flags & CWAIT) {
			fprintf(stderr, "[Hit return to continue]");
			while ((errno = 0, i = getchar()) != '\n'
				&& (i != EOF || errno == EINTR));
		}
		(void) signal(SIGQUIT, savequit);
		ttyraw();
		okclear();
#ifdef SIGTSTP
		(void) signal(SIGTSTP, onstop);
		(void) signal(SIGTTIN, onstop);
		(void) signal(SIGTTOU, onstop);
#endif
		return retval;
	} else
		return 0;
}

#ifdef DIGPAGE


/*
 * Find end of current subarticle in digest.
 */

findend(l)
{
	register int i, n;
	register char *p;

	for (i = l ; i < l + ARTWLEN && i < lastlin ; i++) {
		tfget(linebuf, i);
		for (p = linebuf ; *p == '-' ; p++)
			;
		n = (int) (p - linebuf);
		if ( (n > 23 && n < 33) || (n > 65 && n < 79)) {
			tfget(linebuf, ++i);
			if (linebuf[0] == '\0')
				return i + 1;
		}
	}
	return 0;
}

#endif


/*** Routines for handling temporary file ***/

/*
 * Append to temp file.
 * Long lines are folded.
 */

tfappend(tline)
register char *tline;
{
	register char *nxtlin;

	do {
		nxtlin = index(tline, '\n');
		if (nxtlin)
			*nxtlin++ = '\0';

		while (strlen(tline) > COLS) {
			tfput(tline, lastlin++);
			tline += COLS;
			maxlinno++;
		}
		tfput(tline, lastlin++);
	} while ((tline = nxtlin) != NULL);
}


tfput(tline, linno)
char *tline;
{
	register char *p;
	register FILE *rtfp;		/* try to make it a little faster */
	register int i;

	p = tline, i = even(COLS);
	tfseek(linno, 1);
	rtfp = tfp;
	while (--i >= 0) {
		if (*p)
			putc(*p++, rtfp);
		else
			putc('\0', rtfp);
	}
	tflinno++;
}


tfget(tline, linno)
char *tline;
{
	tfseek(linno, 0);
	fread(tline, even(COLS), 1, tfp);
	tline[COLS] = '\0';
	tflinno++;
}


tfseek(linno, wrflag)
{
	static int lastwrflag = 1;

	if (linno != tflinno || wrflag != lastwrflag) {
		(void) fseek(tfp, (long)linno * even(COLS), 0);
		tflinno = linno;
		lastwrflag = wrflag;
	}
}

/* VARARGS1 */
msg(s, a1, a2, a3, a4)
char *s;
long a1, a2, a3, a4;
{
	(void) sprintf(secpr, s, a1, a2, a3, a4);
}


/*
 * Update the display.
 * The display is entirely controlled by this routine,
 * which means that this routine may get pretty snarled.
 */

static int savelinno = -1;		/* dlinno on last call to updscr */
static int savepr;			/* prflags on last call */
#ifdef TIOCGWINSZ
static int UPDATING = 0, WINCH = 0;

/*
 * called by winch() from virtterm.c -- resets state information back
 * to start-up state and forces a full redraw of the screen.  The
 * current article is rewound to the beginning because it's would
 * be very difficult to get the screen to return to the exact point
 * in the file that the user left off (I know, I tried).
 */
winch_upd()
{
	if(UPDATING)	/* concurrency.  wow! */
		WINCH++;
	else if((WINCH == 0) && (savelinno >= 0)) {
		int  saveflag = curflag;

		/* reread the article */
		FCLOSE(fp);
		obit = -1;
		getnextart(FALSE);
		appfile(fp, dlinno + ARTWLEN + 1);

		/* fix up the screen */
		curflag = saveflag;
		strcpy(prompt,"more? ");
		okclear();
		updscr();
	}
}
#endif /* TIOCGWINSZ */


updscr()
{
	int count;
	int i;

#ifdef TIOCGWINSZ
	UPDATING++;
#endif /* TIOCGWINSZ */
	if (checkin())
		return;
	if ((prflags & HELPMSG) == 0
	 && (dlinno != savelinno || savepr != prflags)
	 && quitflg == 0) {
		if (dlinno != savelinno)
			prflags &=~ NOPRT;
		count = ARTWLEN;
		if (prflags & NOPRT)
			count = 0;
		if ((prflags & HDRONLY) && count > hdrend)
			count = hdrend - dlinno;
#ifdef DIGPAGE
		if (endsuba > 0 && count > endsuba - dlinno)
			count = endsuba - dlinno;
#endif
		if ((prflags & NEWART) == 0)
			ushift(ARTWIN, ARTWIN+ARTWLEN-1, dlinno - savelinno);
		if (count > lastlin - dlinno)
			count = lastlin - dlinno;
		for (i = ARTWIN ; i < ARTWIN + ARTWLEN ; i++)
			clrline(i);
		for (i = 0 ; i < count ; i++) {
			tfget(linebuf, dlinno + i);
			mvaddstr(ARTWIN + i, 0, linebuf);
		}
		prflags &=~ NEWART;
		savepr = prflags;
		savelinno = dlinno;
	}
	clrline(SPLINE), clrline(PRLINE);
#ifdef STATTOP
	mvaddstr(PRLINE, 0, prompt);
#else
	if (strlen(secpr) <= COLS)
		mvaddstr(PRLINE, 0, prompt);
#endif
	mvaddstr(PRLINE, 59, timestr);
	mvaddstr(PRLINE, 17, groupdir);
	addch(' '); addnum(bit); addch('/'); addnum(pngsize); addch(' ');
	if (ismail)
		mvaddstr(PRLINE, 75, ismail > 1? "MAIL" : "mail");
	mvaddstr(SPLINE, 0, secpr);
	if (curflag == CURP1)
		move(PRLINE, strlen(prompt));
	else if (curflag == CURHOME)
		move(0, 0);
	refresh();
#ifdef TIOCGWINSZ
	UPDATING=0;
	if (WINCH) { /* window changed while updating screen */
		WINCH = 0;
		winch_upd();
	}
#endif /* TIOCGWINSZ */
}

addnum(n)
register long n;
{
	if (n >= 10)
		addnum(n / 10);
	addch((char)(n % 10 + '0'));
}

/*
 * Called on alarm signal.
 * Simply sets flag, signal processed later.
 */

onalarm()
{
#ifdef SIGTSTP
	int dojump = reading;

	reading = FALSE;
	alflag++;
	if (dojump)
		longjmp(alrmjmp, 1);
#else /* !SIGTSTP */
	alflag++;
#endif
}

/*
 * Process alarm signal (or start clock)
 */
timer()
{
	time_t tod;
	int hour;
	int i;
	struct tm *t;
	struct stat statb;
	struct tm *localtime();
	static char months[] = "JanFebMarAprMayJunJulAugSepOctNovDec";
	static long oldmsize = 1000000L;
	static int rccount = 10;
	static time_t lastismail = 0;

	alflag = 0;
	(void) signal(SIGALRM, onalarm);
	(void) time(&tod);
	t = localtime(&tod);
	i = 60 - t->tm_sec;
	(void) alarm(i > 30? 30 : i);			/* reset alarm */
	hour = t->tm_hour % 12;
	if (hour == 0)  hour = 12;
	(void) sprintf(timestr, "%.3s %d %d:%02d",
		months + 3 * t->tm_mon, t->tm_mday, hour, t->tm_min);
	if (mailf == NULL || stat(mailf, &statb) < 0) {
		statb.st_size = 0;
	}
	if (statb.st_size > oldmsize) {
		ismail = 2;
		beep();
	} else {
		if (statb.st_size == 0)
			ismail = 0;
					/* force MAIL for at least 30 seconds */
		else if (ismail > 1 && (lastismail+30) < tod)
			ismail = 1;
	}
	oldmsize = statb.st_size;
	lastismail = tod;
	if (uflag && !xflag && --rccount < 0) {
		writeoutrc();
		if (secpr[0] == '\0')
			(void) strcpy(secpr, ".newsrc updated");
		rccount = 10;
	}
}

char *
getmailname()
{
	static char mailname[32];
	register char *p;

	if( (p = getenv("MAIL")) != NULL)
		return p;
#ifndef MMDF
	if (username[0] == '\0' || strlen(username) > 15)
		return NULL;
#ifdef USG
	(void) sprintf(mailname, "/usr/mail/%s", username);
#else /* !USG */
	(void) sprintf(mailname, "/usr/spool/mail/%s", username);
#endif /* !USG */
#else /* MMDF */
	(void) sprintf(mailname, "%s/mailbox", userhome);
#endif /* MMDF */
	return mailname;
}



/*** Terminal I/O ***/

#define INBUFSIZ 8

char inbuf[INBUFSIZ];			/* input buffer */
char outbuf[BUFSIZ];			/* output buffer */
int innleft = 0;			/* # of chars in input buffer */
int outnleft = BUFSIZ;			/* room left in output buffer */
char *innext;				/* next input character */
char *outnext = outbuf;			/* next space in output buffer */
#ifdef USG
int oflags;				/* fcntl flags (for nodelay read) */
#endif

/*
 * Input a character
 */

vgetc()
{
	register c;
#if defined(BSD4_2) || defined(BSD4_1C)
	int readfds, exceptfds;
#endif

recurse:
	if (--innleft >= 0) {
		c = *innext++;
	} else {
		if (alflag)
			timer();
		updscr();	/* update the display */
		for (;;) {
			if (innleft > 0 || alflag)
				goto recurse;
			intflag = 0;
#ifdef USG
			if (oflags & O_NDELAY) {
				oflags &=~ O_NDELAY;
				fcntl(0, F_SETFL, oflags);
			}
#endif
#ifdef SIGTSTP
			if (setjmp(alrmjmp))
				continue;
			if (setjmp(intjmp))
				return cintr;
			reading = TRUE;
#endif /* SIGTSTP */
#if defined(BSD4_2) || defined(BSD4_1C)
			/* Use a select because it can be interrupted. */
			readfds = 1; exceptfds = 1;
			select(1, &readfds, (int *)0, &exceptfds, (int *)0);
			if (!(readfds & 1))
				break;
#endif
			innleft = read(0, inbuf, INBUFSIZ);
#ifdef SIGTSTP
			reading = FALSE;
#endif /* SIGTSTP */
			if (innleft > 0)
				break;
			if (innleft == 0) {
				quitflg++;
				return cintr;
			}
			if (errno != EINTR)
				abort();	/* "Can't happen" */
			if (intflag) {
				intflag--;
				return cintr;
			}
		}
		innext = inbuf + 1;
		innleft--;
		c = inbuf[0];
	}
#ifndef USG
#ifndef CBREAK
	c &= 0177;
	if (c == '\034')	/* FS character */
		xxit(0);
#endif
#endif
	if (c == '\f') {
		okclear();
		prflags &=~ NOPRT;
		goto recurse;
	}
	if (c == '\r')
		c = '\n';
	return c;
}


/*
 * Push a character back onto the input stream.
 */

pushback(c)
{
	if (innext <= inbuf)
		abort();
	*--innext = c;
	innleft++;
}

/*
 * Check for terminal input
 */

checkin()
{
#ifdef FIONREAD
	int count;
#endif
#ifdef STATTOP
	if (innleft > 0)
#else
	if (innleft > 0 || alflag)
#endif
		return 1;
#if defined(USG) || defined(FIONREAD)
	if (ospeed >= B9600)
		return 0;
	vflush();
	if (ospeed <= B300)
		ttyowait();
#ifdef USG
	if ((oflags & O_NDELAY) == 0) {
		oflags |= O_NDELAY;
		(void) fcntl(0, F_SETFL, oflags);
	}
	if ((innleft = read(0, inbuf, INBUFSIZ)) > 0) {
		innext = inbuf;
		return 1;
	}
#endif
#ifdef FIONREAD
	count = 0;			/* in case FIONREAD fails */
	(void) ioctl(0, FIONREAD, (char *)&count);
	if (count)
		return 1;
#endif
#endif
	return 0;
}



/*
 * flush terminal input queue.
 */

clearin()
{
#ifdef USG
	(void) ioctl(0, TCFLSH, (char *)0);
#else
#ifdef TIOCFLUSH
	(void) ioctl(0, TIOCFLUSH, (char *)0);
#else
	struct sgttyb tty;
	(void) ioctl(0, TIOCGETP, &tty);
	(void) ioctl(0, TIOCSETP, &tty);
#endif
#endif
	innleft = 0;
}

vputc(c)
{
	if (--outnleft < 0) {
		vflush();
		outnleft--;
	}
	*outnext++ = c;
}

/*
 * Flush the output buffer
 */

vflush()
{
	register char *p;
	register int i;
#ifdef BSD4_2
	int mask;
#else
	unsigned oalarm;
#endif

#ifdef BSD4_2
	mask = sigblock(1 << (SIGALRM-1));
#else
	oalarm = alarm(0);
#endif
	for (p = outbuf ; p < outnext ; p += i) {
		if ((i = write(1, p, outnext - p)) < 0) {
			if (errno != EINTR)
				abort();	/* "Can't happen" */
			i = 0;
		}
	}
	outnleft = BUFSIZ;
	outnext = outbuf;
#ifdef BSD4_2
	sigsetmask(mask);
#else
	(void) alarm(oalarm);
#endif
}

/*** terminal modes ***/

#ifdef USG
static struct termio oldtty, newtty;

/*
 * Save tty modes
 */

ttysave()
{
	if (ioctl(1, TCGETA, &oldtty) < 0)
		xerror("Can't get tty modes");
	newtty = oldtty;
	newtty.c_iflag &=~ (INLCR|IGNCR|ICRNL);
	newtty.c_oflag &=~ (OPOST);
	newtty.c_lflag &=~ (ICANON|ECHO|ECHOE|ECHOK|ECHONL);
	newtty.c_lflag |=  (NOFLSH);
	newtty.c_cc[VMIN] = 1;
	newtty.c_cc[VTIME] = 0;
	cerase = oldtty.c_cc[VERASE];
	ckill = oldtty.c_cc[VKILL];
	cintr = oldtty.c_cc[VINTR];
	ospeed = oldtty.c_cflag & CBAUD;
	initterm();
}


/*
 * Set tty modes for visual processing
 */

ttyraw()
{
	while (ioctl(1, TCSETAF, &newtty) < 0 && errno == EINTR)
		;
	rawterm();
}

ttyowait()
{	/* wait for output queue to drain */
	while (ioctl(1, TCSETAW, &newtty) < 0 && errno == EINTR)
		;
}

/*
 * Restore tty modes
 */

ttycooked()
{
	cookedterm();
	vflush();
	while (ioctl(1, TCSETAF, &oldtty) < 0 && errno == EINTR)
		;
	oflags &=~ O_NDELAY;
	(void) fcntl(0, F_SETFL, oflags) ;
}

#else

static struct sgttyb oldtty, newtty;
#ifdef TIOCGLTC
static struct ltchars oldltchars, newltchars;
#endif

/*
 * Save tty modes
 */

ttysave()
{
#ifdef CBREAK
	struct tchars tchars;	/* special characters, including interrupt */
#endif
#ifdef SIGTSTP
	int getpgrp();
#if defined(BSD4_2) || defined(BSD4_1C)
	int tpgrp;
#else /* BSD4_1 */
	short tpgrp;
#endif /* BSD4_1 */

retry:
#ifdef BSD4_2
	(void) sigblock(sigmask(SIGTSTP)|sigmask(SIGTTIN)|sigmask(SIGTTOU));
#else /* !BSD4_2 */
	(void) signal(SIGTSTP, SIG_HOLD);
	(void) signal(SIGTTIN, SIG_HOLD);
	(void) signal(SIGTTOU, SIG_HOLD);
#endif /* !BSD4_2 */
	if (ioctl(2, TIOCGPGRP, (char *)&tpgrp) < 0)
		goto nottty;
	if (tpgrp != getpgrp(0)) { /* not in foreground */
		(void) signal(SIGTTOU, SIG_DFL);
#ifdef BSD4_2
		(void) sigsetmask(sigblock(0) & ~sigmask(SIGTTOU));
#endif /* BSD4_2 */
		(void) kill(0, SIGTTOU);
		/* job stops here waiting for SIGCONT */
		goto retry;
	}
	(void) signal(SIGTTIN, SIG_DFL);
	(void) signal(SIGTTOU, SIG_DFL);
	(void) signal(SIGTSTP, SIG_DFL);
#ifdef BSD4_2
	(void) sigsetmask(sigblock(0) & ~(sigmask(SIGTSTP)|sigmask(SIGTTIN)|sigmask(SIGTTOU)));
#endif /* BSD4_2 */
#endif /* SIGTSTP */
	if (ioctl(1, TIOCGETP, (char *)&oldtty) < 0)
nottty:		xerror("Can't get tty modes");
	newtty = oldtty;
	newtty.sg_flags &=~ (CRMOD|ECHO|XTABS);
#ifdef CBREAK
	newtty.sg_flags |= CBREAK;
	ioctl(1, TIOCGETC, (char *)&tchars);
	cintr = tchars.t_intrc;
#else /* !CBREAK */
	newtty.sg_flags |= RAW;
	cintr = '\0177';	/* forcibly this on V6 systems */
#endif /* !CBREAK */
	cerase = oldtty.sg_erase;
	ckill = oldtty.sg_kill;
	ospeed = oldtty.sg_ospeed;
#ifdef	TIOCGLTC
	if (ioctl(1, TIOCGLTC, (char *)&oldltchars) >= 0) {
		newltchars = oldltchars;
		newltchars.t_dsuspc = -1;
		cwerase = oldltchars.t_werasc;
	}
#endif
	initterm();
#ifdef SIGTSTP
	(void) signal(SIGTTIN, onstop);
	(void) signal(SIGTTOU, onstop);
	(void) signal(SIGTSTP, onstop);
#endif /* SIGTSTP */
}


/*
 * Set tty modes for visual processing
 */

ttyraw()
{
	while (ioctl(1, TIOCSETN, (char *)&newtty) < 0 && errno == EINTR)
		;
#ifdef TIOCGLTC
	if (newltchars.t_dsuspc == '\377')
	  while (ioctl(1, TIOCSLTC, (char *)&newltchars) < 0 && errno == EINTR)
		;
#endif
	rawterm();
}

ttyowait()
{	/* wait for output queue to drain */
#ifdef TIOCDRAIN	/* This ioctl is a local mod on linus */
	(void) ioctl(1, TIOCDRAIN, (char *)0);
#endif
}


/*
 * Restore tty modes
 */

ttycooked()
{
	cookedterm();
	vflush();
	while (ioctl(1, TIOCSETN, (char *)&oldtty) < 0 && errno == EINTR)
		;
#ifdef TIOCGLTC
	if (newltchars.t_dsuspc == '\377')
	  while (ioctl(1, TIOCSLTC, (char *)&oldltchars) < 0 && errno == EINTR)
		;
#endif
}

#endif



/*** signal handlers ***/

onint() {
#ifdef SIGTSTP
	int dojump = reading;

	reading = FALSE;
#endif /* SIGTSTP */
	if (!news) {
		ttycooked();
		xxit(1);
	}
	(void) signal(SIGINT, onint);
	clearin();			/* flush input queue */
#ifdef SIGTSTP
	if (dojump)
		longjmp(intjmp, 1);
#endif /* SIGTSTP */
	intflag++;
}

#ifdef SIGTSTP
onstop(signo)
int signo;
{
	/* restore old terminal state */
	botscreen();
	vflush();
	ttycooked();
	(void) signal(signo, SIG_DFL);
#ifdef BSD4_2
	(void) sigblock(sigmask(SIGALRM)|sigmask(SIGINT));
	(void) sigsetmask(sigblock(0) & ~sigmask(signo));
#else /* BSD4_1 */
	(void) alarm(0);
#endif /* BSD4_1 */
	(void) kill(0, signo);	/* stop here until continued */

	(void) signal(signo, onstop);
	/* restore our special terminal state */
	ttyraw();
#ifdef TIOCGWINSZ
	winch();	/* get current window size and redraw screen */
#else 	/* !TIOCGWINSZ */
	okclear();
	updscr();
#endif 	/* !TIOCGWINSZ */
#ifdef BSD4_2
	(void) sigsetmask(sigblock(0) & ~(sigmask(SIGALRM)|sigmask(SIGINT)));
#else /* BSD4_1 */
	timer();
#endif /* BSD4_1 */
}
#endif

/*** stolen from rfuncs2.c and modified ***/

vsave(to, flags)
register char *to;
{
	register FILE *ufp;
	int	isprogram = 0;
	int	isnew = 1;
	long	saveoff;
	char	temp[20];
	char	*fname;
	char	prog[BUFLEN + 24];
	int	err;

	saveoff = ftell(fp);
	(void) fseek(fp, artbody, 0);
	fname = to;
	if (*to == PIPECHAR) {
		if (strlen(to) > BUFLEN) {
			msg("Command name too long");
			goto out;
		}
		flags |= OVWRITE;
		(void) strcpy(temp, "/tmp/vnXXXXXX");
		(void) mktemp(temp);
		fname = temp;
		_amove(ROWS - 1, 0);
		vflush();
	}
	if ((flags & OVWRITE) == 0) {
		ufp = fopen(fname, "r");
		if (ufp != NULL) {
			(void) fclose(ufp);
			isnew = 0;
		}
	}
	(void) umask(savmask);

	if (*to == PIPECHAR)
		isprogram++;
	if ((ufp = fopen(fname, (flags & OVWRITE) == 0? "a" : "w")) == NULL) {
		msg("Cannot open %s", fname);
		goto out;
	}
	/*
	 * V7MAIL code is here to conform to V7 mail format.
	 * If you need a different format to be able to
	 * use your local mail command (such as four ^A's
	 * on the end of articles) substitute it here.
	 */
	if (flags & SVHEAD) {
#ifdef MMDF
		if (!isprogram)
			fprintf(ufp, "\001\001\001\001\n");
#endif /* MMDF */
#ifdef V7MAIL
		h->subtime = cgtdate(h->subdate);
		fprintf(ufp, "From %s %s", replyname(h), ctime(&h->subtime));
#endif
		hprint(h, ufp, 2);
#ifdef V7MAIL
		tprint(fp, ufp, TRUE);
		putc('\n', ufp);	/* force blank line at end (ugh) */
#else
		tprint(fp, ufp, FALSE);
#endif
	} else {
		tprint(fp, ufp, FALSE);
	}

	err = ferror(ufp);

	fclose(ufp);
	if (isprogram) {
		if (err)
			msg("error in writing temp file, maybe disk full?");
		else {
			(void) sprintf(prog, "(%s)<%s", to + 1, fname);
			shcmd(prog, CWAIT);
			prflags |= NOPRT;
		}
	} else {
		msg("%sfile: %s %s",
			err? "ERROR WHILE WRITING ": "",
			to,
			(flags&OVWRITE)? "written":
				isnew ? "created" : "appended");
	}

	/* If we got an error, screen may be messed.  E.g. 4.2BSD
	 * writes "disk full" messages to the user's tty.
	 */
	if (err) {
		okclear();
		updscr();
	}

out:
	if (isprogram) {
		(void) unlink(fname);
	}
	(void) umask(N_UMASK);
	(void) fseek(fp, saveoff, 0);
}

xxit(status)
int	status;
{
	(void) unlink(infile);
	(void) unlink(outfile);
#ifdef SORTACTIVE
	if (STRNCMP(ACTIVE,"/tmp/", 5) == 0)
		(void) unlink(ACTIVE);
#endif /* SORTACTIVE */
#ifdef SERVER
	(void) unlink(active_name());
	close_server();	
#endif	/* SERVER */
	if (ospeed) {	/* is == 0, we haven't been in raw mode yet */
		botscreen();
		vflush();
		ttycooked();
	}
	exit(status);
}
