#ifndef lint
static char sccsid[] = "@(#)msgs.c	4.1 %G%";
#endif lint
/*
 * msgs - a user bulletin board program
 *
 * usage:
 *	msgs [fhlopq] [[-]number]	to read messages
 *	msgs -s				to place messages
 *	msgs -c [-days]			to clean up the bulletin board
 *
 * prompt commands are:
 *	y	print message
 *	n	flush message, go to next message
 *	q	flush message, quit
 *	p	print message, turn on 'pipe thru more' mode
 *	P	print message, turn off 'pipe thru more' mode
 *	-	reprint last message
 *	s[-][<num>] [<filename>]	save message
 *	m[-][<num>]	mail with message in temp mbox
 *	x	exit without flushing this message
 */

#define V7		/* will look for TERM in the environment */
#define OBJECT		/* will object to messages without Subjects */
/* #define REJECT	/* will reject messages without Subjects
			   (OBJECT must be defined also) */
/* #define UNBUFFERED	/* use unbuffered output */

#include <stdio.h>
#include <sys/types.h>
#include <signal.h>
#include <sys/dir.h>
#include <sys/stat.h>
#include <ctype.h>
#include <pwd.h>
#include <sgtty.h>
#include "msgs.h"

#define CMODE	0666		/* bounds file creation mode */
#define NO	0
#define YES	1
#define SUPERUSER	0	/* superuser uid */
#define DAEMON		1	/* daemon uid */
#define NLINES	24		/* default number of lines/crt screen */
#define NDAYS	21		/* default keep time for messages */
#define DAYS	*24*60*60	/* seconds/day */
#define TEMP	"/tmp/msgXXXXXX"
#define MSGSRC	".msgsrc"	/* user's rc file */
#define BOUNDS	"bounds"	/* message bounds file */
#define NEXT	"Next message? [yq]"
#define MORE	"More? [ynq]"
#define NOMORE	"(No more) [q] ?"

typedef	char	bool;

FILE	*newmsg;
char	*sep = "-";
char	inbuf[BUFSIZ];
char	fname[128];
char	cmdbuf[128];
char	subj[128];
char	from[128];
char	date[128];
char	*ptr;
char	*in;
bool	local;
bool	ruptible;
bool	totty;
bool	seenfrom;
bool	seensubj;
bool	blankline;
bool	printing = NO;
bool	mailing = NO;
bool	quitit = NO;
bool	sending = NO;
bool	intrpflg = NO;
int	uid;
int	msg;
int	prevmsg;
int	lct;
int	nlines;
int	Lpp = NLINES;
time_t	t;
time_t	keep;
struct	sgttyb	otty;

char	*ctime();
char	*nxtfld();
int	onintr();
off_t	ftell();
FILE	*popen();
struct	passwd	*getpwuid();

extern	int	errno;

/* option initialization */
bool	hdrs = NO;
bool	qopt = NO;
bool	hush = NO;
bool	send = NO;
bool	locomode = NO;
bool	pause = NO;
bool	clean = NO;
bool	lastcmd = NO;

main(argc, argv)
int argc; char *argv[];
{
	bool newrc, already;
	int rcfirst = 0;		/* first message to print (from .rc) */
	int rcback = 0;			/* amount to back off of rcfirst*/
	int firstmsg, nextmsg, lastmsg = 0;
	int blast = 0;
	FILE *bounds, *msgsrc;

#ifndef UNBUFFERED
	char obuf[BUFSIZ];
	setbuf(stdout, obuf);
#else
	setbuf(stdout, NULL);
#endif

	gtty(fileno(stdout), &otty);
	time(&t);
	setuid(uid = getuid());
	ruptible = (signal(SIGINT, SIG_IGN) == SIG_DFL);
	if (ruptible)
		signal(SIGINT, SIG_DFL);

	argc--, argv++;
	while (argc > 0) {
		if (isdigit(argv[0][0])) {	/* starting message # */
			rcfirst = atoi(argv[0]);
		}
		else if (isdigit(argv[0][1])) {	/* backward offset */
			rcback = atoi( &( argv[0][1] ) );
		}
		else {
			ptr = *argv;
			while (*ptr) switch (*ptr++) {

			case '-':
				break;

			case 'c':
				if (uid != SUPERUSER && uid != DAEMON) {
					fprintf(stderr, "Sorry\n");
					exit(1);
				}
				clean = YES;
				break;

			case 'f':		/* silently */
				hush = YES;
				break;

			case 'h':		/* headers only */
				hdrs = YES;
				break;

			case 'l':		/* local msgs only */
				locomode = YES;
				break;

			case 'o':		/* option to save last message */
				lastcmd = YES;
				break;

			case 'p':		/* pipe thru 'more' during long msgs */
				pause = YES;
				break;

			case 'q':		/* query only */
				qopt = YES;
				break;

			case 's':		/* sending TO msgs */
				send = YES;
				break;

			default:
				fprintf(stderr,
					"usage: msgs [fhlopq] [[-]number]\n");
				exit(1);
			}
		}
		argc--, argv++;
	}

	/*
	 * determine current message bounds
	 */
	sprintf(fname, "%s/%s", USRMSGS, BOUNDS);
	bounds = fopen(fname, "r");

	if (bounds != NULL) {
		fscanf(bounds, "%d %d\n", &firstmsg, &lastmsg);
		fclose(bounds);
		blast = lastmsg;	/* save upper bound */
	}

	if (clean)
		keep = t - (rcback? rcback : NDAYS) DAYS;

	if (clean || bounds == NULL) {	/* relocate message bounds */
		struct direct dirent;
		struct stat stbuf;
		bool seenany = NO;

		FILE *d = fopen(USRMSGS, "r");
		if (d == NULL) {
			perror(USRMSGS);
			exit(errno);
		}

		firstmsg = 32767;
		lastmsg = 0;

		while (fread(&dirent, sizeof dirent, 1, d) == 1) {
			register char *cp = dirent.d_name;
			register int i = 0;

			if (dirent.d_ino == 0)
				continue;

			if (clean)
				sprintf(inbuf, "%s/%s", USRMSGS, cp);

			while (isdigit(*cp))
				i = i * 10 + *cp++ - '0';
			if (*cp)
				continue;	/* not a message! */

			if (clean) {
				if (stat(inbuf, &stbuf) != 0)
					continue;
				if (stbuf.st_mtime < keep
				    && stbuf.st_mode&S_IWRITE) {
					unlink(inbuf);
					continue;
				}
			}

			if (i > lastmsg)
				lastmsg = i;
			if (i < firstmsg)
				firstmsg = i;
			seenany = YES;
		}
		fclose(d);

		if (!seenany) {
			if (blast != 0)	/* never lower the upper bound! */
				lastmsg = blast;
			firstmsg = lastmsg + 1;
		}
		else if (blast > lastmsg)
			lastmsg = blast;

		if (!send) {
			bounds = fopen(fname, "w");
			if (bounds == NULL) {
				perror(fname);
				exit(errno);
			}
			chmod(fname, CMODE);
			fprintf(bounds, "%d %d\n", firstmsg, lastmsg);
			fclose(bounds);
		}
	}

	if (send) {
		/*
		 * Send mode - place msgs in USRMSGS
		 */
		bounds = fopen(fname, "w");
		if (bounds == NULL) {
			perror(fname);
			exit(errno);
		}

		nextmsg = lastmsg + 1;
		sprintf(fname, "%s/%d", USRMSGS, nextmsg);
		newmsg = fopen(fname, "w");
		if (newmsg == NULL) {
			perror(fname);
			exit(errno);
		}
		chmod(fname, 0644);

		fprintf(bounds, "%d %d\n", firstmsg, nextmsg);
		fclose(bounds);

		sending = YES;
		if (ruptible)
			signal(SIGINT, onintr);

		if (isatty(fileno(stdin))) {
			ptr = getpwuid(uid)->pw_name;
			printf("Message %d:\nFrom %s %sSubject: ",
				nextmsg, ptr, ctime(&t));
			fflush(stdout);
			fgets(inbuf, sizeof inbuf, stdin);
			putchar('\n');
			fflush(stdout);
			fprintf(newmsg, "From %s %sSubject: %s\n",
				ptr, ctime(&t), inbuf);
			blankline = seensubj = YES;
		}
		else
			blankline = seensubj = NO;
		for (;;) {
			fgets(inbuf, sizeof inbuf, stdin);
			if (feof(stdin) || ferror(stdin))
				break;
			blankline = (blankline || (inbuf[0] == '\n'));
			seensubj = (seensubj || (!blankline && (strncmp(inbuf, "Subj", 4) == 0)));
			fputs(inbuf, newmsg);
		}
#ifdef OBJECT
		if (!seensubj) {
			printf("NOTICE: Messages should have a Subject field!\n");
#ifdef REJECT
			unlink(fname);
#endif
			exit(1);
		}
#endif
		exit(ferror(stdin));
	}
	if (clean)
		exit(0);

	/*
	 * prepare to display messages
	 */
	totty = (isatty(fileno(stdout)) != 0);
	pause = pause && totty;

	sprintf(fname, "%s/%s", getenv("HOME"), MSGSRC);
	msgsrc = fopen(fname, "r");
	if (msgsrc) {
		newrc = NO;
		fscanf(msgsrc, "%d\n", &nextmsg);
		fclose(msgsrc);
		if (!rcfirst)
			rcfirst = nextmsg - rcback;
	}
	else {
		newrc = YES;
		nextmsg = 0;
	}
	msgsrc = fopen(fname, "a");
	if (msgsrc == NULL) {
		perror(fname);
		exit(errno);
	}
	if (rcfirst)
		firstmsg = rcfirst;
	if (newrc) {
		nextmsg = firstmsg;
		fseek(msgsrc, 0L, 0);
		fprintf(msgsrc, "%d\n", nextmsg);
		fflush(msgsrc);
	}

#ifdef V7
	if (totty) {
		if (tgetent(inbuf, getenv("TERM")) <= 0
		    || (Lpp = tgetnum("li")) <= 0) {
			Lpp = NLINES;
		}
	}
#endif
	Lpp -= 6;	/* for headers, etc. */

	already = NO;
	prevmsg = firstmsg;
	printing = YES;
	if (ruptible)
		signal(SIGINT, onintr);

	/*
	 * Main program loop
	 */
	for (msg = firstmsg; msg <= lastmsg; msg++) {

		sprintf(fname, "%s/%d", USRMSGS, msg);
		newmsg = fopen(fname, "r");
		if (newmsg == NULL)
			continue;

		gfrsub(newmsg);		/* get From and Subject fields */
		if (locomode && !local) {
			fclose(newmsg);
			continue;
		}

		if (qopt) {	/* This has to be located here */
			printf("There are new messages.\n");
			exit(0);
		}

		if (already && !hdrs)
			putchar('\n');
		already = YES;

		/*
		 * Print header
		 */
		nlines = 2;
		if (seenfrom) {
			printf("Message %d:\nFrom %s %s", msg, from, date);
			nlines++;
		}
		if (seensubj) {
			printf("Subject: %s", subj);
			nlines++;
		}
		else {
			if (seenfrom) {
				putchar('\n');
				nlines++;
			}
			while (nlines < 6
			    && fgets(inbuf, sizeof inbuf, newmsg)
			    && inbuf[0] != '\n') {
				fputs(inbuf, stdout);
				nlines++;
			}
		}

		lct = linecnt(newmsg);
		if (lct)
			printf("(%d%slines) ", lct, seensubj? " " : " more ");

		if (hdrs) {
			printf("\n-----\n");
			fclose(newmsg);
			continue;
		}

		/*
		 * Ask user for command
		 */
		if (totty)
			ask(lct? MORE : (msg==lastmsg? NOMORE : NEXT));
		else
			inbuf[0] = 'y';
cmnd:
		in = inbuf;
		switch (*in) {
			case 'x':
			case 'X':
				exit(0);

			case 'q':
			case 'Q':
				quitit = YES;
				printf("--Postponed--\n");
				exit(0);
				/* intentional fall-thru */
			case 'n':
			case 'N':
				if (msg >= nextmsg) sep = "Flushed";
				break;

			case 'p':
			case 'P':
				pause = (*in++ == 'p');
				/* intentional fallthru */
			case '\n':
			case 'y':
			default:
				if (*in == '-') {
					msg = prevmsg-1;
					sep = "replay";
					break;
				}
				if (isdigit(*in)) {
					msg = next(in);
					sep = in;
					break;
				}

				prmesg(nlines + lct + (seensubj? 1 : 0));
				prevmsg = msg;

		}

		printf("--%s--\n", sep);
		sep = "-";
		if (msg >= nextmsg) {
			nextmsg = msg + 1;
			fseek(msgsrc, 0L, 0);
			fprintf(msgsrc, "%d\n", nextmsg);
			fflush(msgsrc);
		}
		if (newmsg)
			fclose(newmsg);
		if (quitit)
			break;
	}

	if (already && !quitit && lastcmd && totty) {
		/*
		 * save or reply to last message?
		 */
		msg = prevmsg;
		ask(NOMORE);
		if (inbuf[0] == '-' || isdigit(inbuf[0]))
			goto cmnd;
	}
	if (!(already || hush || qopt))
		printf("No new messages.\n");
	exit(0);
}

prmesg(length)
int length;
{
	FILE *outf, *inf;
	int c;

	if (pause && length > Lpp) {
		sprintf(cmdbuf, PAGE, Lpp);
		outf = popen(cmdbuf, "w");
		if (!outf)
			outf = stdout;
		else
			setbuf(outf, NULL);
	}
	else
		outf = stdout;

	if (seensubj)
		putc('\n', outf);

	while (fgets(inbuf, sizeof inbuf, newmsg))
		fputs(inbuf, outf);

	if (outf != stdout) {
		pclose(outf);
	}
	else {
		fflush(stdout);
	}

	/* trick to force wait on output */
	stty(fileno(stdout), &otty);
}

onintr()
{
	signal(SIGINT, onintr);
	if (mailing)
		unlink(fname);
	if (sending) {
		unlink(fname);
		puts("--Killed--");
		exit(1);
	}
	if (printing) {
		putchar('\n');
		if (hdrs)
			exit(0);
		sep = "Interrupt";
		if (newmsg)
			fseek(newmsg, 0L, 2);
		intrpflg = YES;
	}
}

linecnt(f)
FILE *f;
{
	off_t oldpos = ftell(f);
	int l = 0;
	char lbuf[BUFSIZ];

	while (fgets(lbuf, sizeof lbuf, f))
		l++;
	clearerr(f);
	fseek(f, oldpos, 0);
	return (l);
}

next(buf)
char *buf;
{
	int i;
	sscanf(buf, "%d", &i);
	sprintf(buf, "Goto %d", i);
	return(--i);
}

ask(prompt)
char *prompt;
{
	char	inch;
	int	n, cmsg;
	off_t	oldpos;
	FILE	*cpfrom, *cpto;

	printf("%s ", prompt);
	fflush(stdout);
	intrpflg = NO;
	gets(inbuf);
	if (intrpflg)
		inbuf[0] = 'x';

	/*
	 * Handle 'mail' and 'save' here.
	 */
	if ((inch = inbuf[0]) == 's' || inch == 'm') {
		if (inbuf[1] == '-')
			cmsg = prevmsg;
		else if (isdigit(inbuf[1]))
			cmsg = atoi(&inbuf[1]);
		else
			cmsg = msg;
		sprintf(fname, "%s/%d", USRMSGS, cmsg);

		oldpos = ftell(newmsg);

		cpfrom = fopen(fname, "r");
		if (!cpfrom) {
			printf("Message %d not found\n", cmsg);
			ask (prompt);
			return;
		}

		if (inch == 's') {
			in = nxtfld(inbuf);
			if (*in) {
				for (n=0; in[n] > ' '; n++) { /* sizeof fname? */
					fname[n] = in[n];
				}
				fname[n] = NULL;
			}
			else
				strcpy(fname, "Messages");
		}
		else {
			strcpy(fname, TEMP);
			mktemp(fname);
			sprintf(cmdbuf, MAIL, fname);
			mailing = YES;
		}
		cpto = fopen(fname, "a");
		if (!cpto) {
			perror(fname);
			mailing = NO;
			fseek(newmsg, oldpos, 0);
			ask(prompt);
			return;
		}

		while (n = fread(inbuf, 1, sizeof inbuf, cpfrom))
			fwrite(inbuf, 1, n, cpto);

		fclose(cpfrom);
		fclose(cpto);
		fseek(newmsg, oldpos, 0);	/* reposition current message */
		if (inch == 's')
			printf("Message %d saved in \"%s\"\n", cmsg, fname);
		else {
			system(cmdbuf);
			unlink(fname);
			mailing = NO;
		}
		ask(prompt);
	}
}

gfrsub(infile)
FILE *infile;
{
	off_t frompos;

	seensubj = seenfrom = NO;
	local = YES;
	subj[0] = from[0] = date[0] = NULL;

	/*
	 * Is this a normal message?
	 */
	if (fgets(inbuf, sizeof inbuf, infile)) {
		if (strncmp(inbuf, "From", 4)==0) {
			/*
			 * expected form starts with From
			 */
			seenfrom = YES;
			frompos = ftell(infile);
			ptr = from;
			in = nxtfld(inbuf);
			if (*in) while (*in && *in > ' ') {
				if (*in == ':')
					local = NO;
				*ptr++ = *in++;
				/* what about sizeof from ? */
			}
			*ptr = NULL;
			if (*(in = nxtfld(in)))
				strncpy(date, in, sizeof date);
			else {
				date[0] = '\n';
				date[1] = NULL;
			}
		}
		else {
			/*
			 * not the expected form
			 */
			fseek(infile, 0L, 0);
			return;
		}
	}
	else
		/*
		 * empty file ?
		 */
		return;

	/*
	 * look for Subject line until EOF or a blank line
	 */
	while (fgets(inbuf, sizeof inbuf, infile)
	    && !(blankline = (inbuf[0] == '\n'))) {
		/*
		 * extract Subject line
		 */
		if (!seensubj && strncmp(inbuf, "Subj", 4)==0) {
			seensubj = YES;
			frompos = ftell(infile);
			strncpy(subj, nxtfld(inbuf), sizeof subj);
		}
	}
	if (!blankline)
		/*
		 * ran into EOF
		 */
		fseek(infile, frompos, 0);

	if (!seensubj)
		/*
		 * for possible use with Mail
		 */
		strncpy(subj, "(No Subject)\n", sizeof subj);
}

char *
nxtfld(s)
char *s;
{
	if (*s) while (*s && *s > ' ') s++;	/* skip over this field */
	if (*s) while (*s && *s <= ' ') s++;	/* find start of next field */
	return (s);
}
