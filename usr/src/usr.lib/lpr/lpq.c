/*	lpq.c	4.11	83/03/01	*/
/*
 * Spool Queue examination program
 *
 * lpq [+[n]] [-Pprinter] [user...] [job...]
 *
 * + means continually scan queue until empty
 * -P used to identify printer as per lpr/lpd
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <pwd.h>
#ifdef BSD41C
#include <netdb.h>
#else
#include <net/in.h>
#endif
#include <sgtty.h>
#include <ctype.h>
#include <errno.h>
#include "lp.local.h"
#include "getq.h"

#define MAXUSERS	50
#define MAXREQUESTS	50

char	*user[MAXUSERS];		/* users to process */
int	users;				/* # of users in user array */
int	requ[MAXREQUESTS];		/* job number of spool entries */
int	requests;			/* # of spool requests */

#define TERMCAP

#define DEFTIME		30	/* default sleep interval */
#define	JOBCOL		40	/* column for job # in -l format */
#define OWNCOL		7	/* start of Owner column in normal */

char	current[40];		/* current file being printed */
int	garbage;		/* # of garbage cf files */
int	rank = -1;		/* order to be printed (-1=none, 0=active) */
int	repeat;			/* + flag indicator */
int	slptime = DEFTIME;	/* pause between screen refereshes */
int	lflag = 0;		/* long output option */
int	first;			/* first file in ``files'' column? */
int	col;			/* column on screen */
int	SIZCOL = 62;		/* start of Size column in normal */
int	sendtorem;		/* are we sending to a remote? */

char	line[BUFSIZ];		/* input line from control file */
char	file[132];		/* print file name */
char	host[32];		/* host machine name */
char	*from;			/* from machine name */
char	*head0 = "Rank   Owner      Job  Files";
char	*head1 = "Total Size\n";

long	totsize = 0;		/* total print job size in bytes */

/*
 * Termcap stuff for fancy display
 */
#ifdef TERMCAP
struct sgttyb sbuf;
unsigned ospeed;
int	dumb;			/* whether to use capabilities */
char	PC;			/* pad character for output */
char	*UP;			/* up one line */
char	*BC;			/* backspace character, other than \b */
char	*CM;			/* cursor motion */
char	*CL;			/* clear display */
char	*TI;			/* terminal init for CM */
char	*TE;			/* terminal clear for CM */
char	*SO;			/* stand out start */
char	*SE;			/* stand out end */

char	*tgetstr();
int	putch();		/* for tputs' */
#endif

/*
 * Printcap (a la termcap) stuff for mutiple printers
 */
char	*SD;			/* spool directory */
char	*LO;			/* name of lock file */
char	*ST;			/* name of status file */
char	*LP;			/* line printer device name */
char	*RM;			/* remote machine name */
char	*RP;			/* remote printer name */
char	*BD;			/* bin directory name */
int	DU;			/* daemon user-id */
char	*pgetstr();

struct passwd *getpwnam(), *getpwuid();
char	*index();
char	*getenv();

main(argc, argv)
	char *argv[];
{
	register char *arg;
	register int n;
	char *printer = NULL;

	while (--argc) {
		if ((arg = *++argv)[0] == '+') {
			if (arg[1] != '\0')
				if ((slptime = atoi(&arg[1])) < 0)
					slptime = DEFTIME;
			repeat++;
		} else if (arg[0] == '-')
			switch(arg[1]) {

			case 'P':		/* printer name */
				printer = &arg[2];
				break;

			case 'l':		/* long output */
				lflag++;
				break;

			case 'R':		/* executing remotely */
				from = &arg[2];
				break;

			default:
				usage();
		} else {
			if (isdigit(arg[0])) {
				if (requests >= MAXREQUESTS)
					fatal("too many requests");
				requ[requests++] = atoi(arg);
			} else {
				if (users >= MAXUSERS)
					fatal("too many users");
				user[users++] = arg;
			}
		}
	}
	if (printer == NULL && (printer = getenv("PRINTER")) == NULL)
		printer = DEFLP;
	if (!chkprinter(printer))
		fatal("%s: unknown printer", printer);
	gethostname(host, sizeof(host));
	if (chdir(SD) < 0)
		fatal("can't chdir to spool directory");
#ifdef TERMCAP
	dumb = termcap();
#endif

	if (repeat) {
#ifdef TERMCAP
		if (TI)
			tputs(TI, 0, putch);
#endif
		do {
#ifdef TERMCAP
			if (!dumb) {
				tputs(CL, 0, putch);
				tputs(tgoto(CM, 0, 0), 0, putch);
			}
#endif
			if ((n = display()) > 0) {	
				sleep(slptime);
				rank = -1;
			}
		} while (n > 0);
#ifdef TERMCAP
		if (!dumb) {
			if (from == NULL) {
				standout(stdout, "Hit return to continue");
				while (getchar() != '\n');
			}
			if (TE)
				tputs(TE, 0, putch);
		}
#endif
	} else
		display();
}

/*
 * Display the current state of the queue.
 */
display()
{
	register struct queue *q;
	register int i, nitems;
	struct queue **queue;
	struct stat statb;
	FILE *fp;

	/*
	 * If there is no local printer, then print the queue on
	 * the remote machine and then what's in the queue here.
	 * Note that a file in transit may not show up in either queue.
	 */
	if (*LP == '\0') {
		register char *cp;
#ifdef BSD41C
		struct servent *sp = 0;
#endif
		int rem;

		if (RM == NULL)
			fatal("no line printer device or remote");
		sendtorem++;
		(void) sprintf(line, "%s/lpq -R%s", BD, from==NULL?host:from);
		cp = line;
		if (RP != NULL) {
			cp += strlen(cp);
			(void) sprintf(cp, " -P%s", RP);
		}
		if (lflag) {
			cp += strlen(cp);
			(void) sprintf(cp, " -l");
		}
		for (i = 0; i < requests; i++) {
			cp += strlen(cp);
			(void) sprintf(cp, " %d", requ[i]);
		}
		for (i = 0; i < users; i++) {
			cp += strlen(cp);
			*cp++ = ' ';
			strcpy(cp, user[i]);
		}
#ifdef BSD41C
		sp = getservbyname("shell", "tcp");
		if (sp == NULL)
			fatal("shell/tcp: unknown service");
		rem = rcmd(&RM, sp->s_port, "root", "root", line, 0);
#else
		rem = rcmd(&RM, IPPORT_CMDSERVER, "root", "root", line, 0);
#endif
		if (rem < 0)
			status("%s is down", RM);
		else {
			if ((fp = fdopen(rem, "r")) == NULL)
				fatal("cannot reopen remote descriptor");
			while (fgets(line, sizeof(line), fp) != NULL)
				switch (line[0]) {
				case '\1':
					rank = atoi(line+1);
					continue;
				default:
					fputs(line, stdout);
				}
			fclose(fp);
		}
		if (!repeat)
			setuid(DU);
	} else
		setuid(DU);
	/*
	 * Find all the control files in the spooling directory
	 */
	if ((nitems = getq(&queue)) < 0)
		fatal("cannot examine spooling area\n");
	if (nitems == 0) {
		if (from == NULL && rank < 0)
			printf("no entries\n");
		return(0);
	}
	fp = fopen(LO, "r");
#ifdef BSD41C
	if (fp == NULL || flock(fileno(fp), FEXLOCK|FNBLOCK) == 0) {
		if (fp != NULL)
			fclose(fp);
#else
	if (fp == NULL) {
#endif
		garbage = nitems;
		if (*LP && stat(LP, &statb) >= 0 && (statb.st_mode & 0777) == 0)
			status("Warning: printer is down");
		else
			status("Warning: no daemon present");
	} else {
		register char *cp = current;

		/* skip daemon pid */
		while ((*cp = getc(fp)) != EOF && *cp != '\n');
		/* read current file name */
		while ((*cp = getc(fp)) != EOF && *cp != '\n')
			cp++;
		*cp = '\0';
		fclose(fp);
	}
	/*
	 * Now, examine the control files and print out the jobs to
	 * be done for each user
	 */
	if (sendtorem)
		printf("\n%s: ", host);
	if ((fp = fopen(ST, "r")) != NULL) {
		if (fgets(line, sizeof(line), fp) != NULL)
			fputs(line, stdout);
		fclose(fp);
	}
	if (!lflag)
		header();
	for (i = 0; i < nitems; i++) {
		q = queue[i];
		inform(q->q_name);
		free(q);
	}
	free(queue);
	if (from != NULL)
		printf("\1%d\n", rank);
	return(nitems-garbage);
}

/*
 * Print the header for the short listing format
 */
header()
{
	printf(head0);
	col = strlen(head0)+1;
	blankfill(SIZCOL);
	printf(head1);
}

/*
 * If we have the capability, print this in standout mode
 */
standout(f, s, a1, a2)
	FILE *f;
	char *s;
{
#ifdef TERMCAP
	if (SO)
		tputs(SO, 0, putch);
	fprintf(f, s, a1, a2);
	if (SO && SE)
		tputs(SE, 0, putch);
#else
	fprintf(f, s, a1, a2);
#endif
}

inform(cf)
	char *cf;
{
	register int j, k;
	register char *cp;
	FILE *cfp;

	/*
	 * There's a chance the control file has gone away
	 * in the meantime; if this is the case just keep going
	 */
	if ((cfp = fopen(cf, "r")) == NULL)
		return;

	if (rank < 0)
		rank = 0;
	if (sendtorem || garbage || strcmp(cf, current))
		rank++;
	j = 0;
	while (fgets(line, sizeof(line), cfp) != NULL) {
		if ((cp = index(line, '\n')) != NULL)
			*cp = '\0';
		switch (line[0]) {
		case 'P': /* Was this file specified in the user's list? */
			if (!inlist(line+1, cf)) {
				fclose(cfp);
				return;
			}
			if (lflag) {
				printf("\n%s: ", line+1);
				col = strlen(line+1) + 2;
				prank(rank);
				blankfill(JOBCOL);
				printf(" [job %s]\n", cf+3);
			} else {
				col = 0;
				prank(rank);
				blankfill(OWNCOL);
				printf("%-10s %-3d  ", line+1, atoi(cf+3));
				col += 16;
				first = 1;
			}
			continue;
		default: /* some format specifer and file name? */
			if (line[0] < 'a' || line[0] > 'z')
				continue;
			if (j == 0 || strcmp(file, line+1) != 0)
				strcpy(file, line+1);
			j++;
			continue;
		case 'N':
			show(line+1, file, j);
			file[0] = '\0';
			j = 0;
		}
	}
	fclose(cfp);
	if (!lflag) {
		blankfill(SIZCOL);
		printf("%D bytes\n", totsize);
		totsize = 0;
	}
}

inlist(name, file)
	char *name, *file;
{
	register int *r, n;
	register char **u, *cp;

	if (users == 0 && requests == 0)
		return(1);
	/*
	 * Check to see if it's in the user list
	 */
	for (u = user; u < &user[users]; u++)
		if (!strcmp(*u, name))
			return(1);
	/*
	 * Check the request list
	 */
	for (n = 0, cp = file+3; isdigit(*cp); )
		n = n * 10 + (*cp++ - '0');
	for (r = requ; r < &requ[requests]; r++)
		if (*r == n && !strcmp(cp, from == NULL ? host : from))
			return(1);
	return(0);
}

show(nfile, file, copies)
	register char *nfile, *file;
{
	if (strcmp(nfile, " ") == 0)
		nfile = "(standard input)";
	if (lflag)
		ldump(nfile, file, copies);
	else
		dump(nfile, file, copies);
}

/*
 * Fill the line with blanks to the specified column
 */
blankfill(n)
	register int n;
{
	while (col++ < n)
		putchar(' ');
}

/*
 * Give the abbreviated dump of the file names
 */
dump(nfile, file, copies)
	char *nfile, *file;
{
	register short n, fill;
	struct stat lbuf;

	/*
	 * Print as many files as will fit
	 *  (leaving room for the total size)
	 */
	 fill = first ? 0 : 2;	/* fill space for ``, '' */
	 if (((n = strlen(nfile)) + col + fill) >= SIZCOL-4) {
		if (col < SIZCOL) {
			printf(" ..."), col += 4;
			blankfill(SIZCOL);
		}
	} else {
		if (first)
			first = 0;
		else
			printf(", ");
		printf("%s", nfile);
		col += n+fill;
	}
	if (*file && !stat(file, &lbuf))
		totsize += copies * lbuf.st_size;
}

/*
 * Print the long info about the file
 */
ldump(nfile, file, copies)
	char *nfile, *file;
{
	struct stat lbuf;

	putchar('\t');
	if (copies > 1)
		printf("%-2d copies of %-19s", copies, nfile);
	else
		printf("%-32s", nfile);
	if (*file && !stat(file, &lbuf))
		printf(" %D bytes", lbuf.st_size);
	else
		printf(" ??? bytes");
	putchar('\n');
}

/*
 * Print the job's rank in the queue,
 *   update col for screen management
 */
prank(n)
{
	char line[100];
	static char *r[] = {
		"th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th"
	};

	if (n == 0) {
		printf("active");
		col += 6;
		return;
	}
	if ((n/10) == 1)
		(void) sprintf(line, "%dth", n);
	else
		(void) sprintf(line, "%d%s", n, r[n%10]);
	col += strlen(line);
	printf("%s", line);
}

/*VARARGS1*/
status(message, a1, a2, a3)
	char *message;
{
	register int fd;
	char buf[BUFSIZ];

	umask(0);
	if ((fd = creat(ST, FILMOD)) < 0) {
		fatal("cannot create status file");
		exit(1);
	}
	sprintf(buf, message, a1, a2, a3);
	strcat(buf, "\n");
	(void) write(fd, buf, strlen(buf));
	(void) close(fd);
}

usage()
{
	printf("usage: lpq [-l] [+[n]] [-Pprinter] [user...] [job...]\n");
	exit(1);
}

fatal(s, a)
	char *s;
{
#ifdef TERMCAP
	if (TE)
		tputs(TE, 0, putch);
#endif
	if (from != NULL)
		standout(stderr, "%s: ", host);
	standout(stderr, "lpq: ");
	standout(stderr, s, a);
	putc('\n', stderr);
	exit(2);
}

#ifdef TERMCAP
char *
capstrings[] = {
	"bc", "cl", "cm", "so", "se", "ti", "te", "up",
	0
};

char **
caps[] = {
	&BC, &CL, &CM, &SO, &SE, &TI, &TE, &UP,
};

/*
 * All we need from termcap is to clear screen and
 *   position cursor at the top; if these aren't available
 *   we say the terminal is dumb and let things scroll
 */
termcap()
{
	char *term, tbuf[BUFSIZ];
	static char buf[BUFSIZ/2];
	register short columns;
	char *bp = buf;
	register char **p, ***q;

	ioctl(0, TIOCGETP, (char *)&sbuf);
	ospeed = sbuf.sg_ospeed;
	if ((term = getenv("TERM")) != NULL && tgetent(tbuf, term) > 0) {
		for (p = capstrings, q = caps; *p != NULL; p++, q++)
			**q = tgetstr(*p, &bp);
		PC = *tgetstr("pc", &bp);
		if ((columns = tgetnum("co")) >= 0)
			SIZCOL = columns-18;
	}
	return(CL == NULL || CM == NULL);
}
#endif

/*
 * Interrogate the printer data base
 */
chkprinter(s)
	char *s;
{
	static char buf[BUFSIZ/2];
	char b[BUFSIZ];
	int stat;
	char *bp = buf;

	if ((stat = pgetent(b, s)) < 0)
		fatal("can't open description file");
	else if (stat == 0)
		return(0);
	if ((LP = pgetstr("lp", &bp)) == NULL)
		LP = DEFDEVLP;
	if ((SD = pgetstr("sd", &bp)) == NULL)
		SD = DEFSPOOL;
	if ((LO = pgetstr("lo", &bp)) == NULL)
		LO = DEFLOCK;
	if ((ST = pgetstr("st", &bp)) == NULL)
		ST = DEFSTAT;
	if ((BD = pgetstr("bd", &bp)) == NULL)
		BD = DEFBINDIR;
	if ((DU = pgetnum("du")) < 0)
		DU = DEFUID;
	RM = pgetstr("rm", &bp);
	RP = pgetstr("rp", &bp);
	return(1);
}

#ifdef TERMCAP
/*
 * Putchar writearound for tputs
 */
putch(c)
	char c;
{
	putchar(c);
}
#endif
