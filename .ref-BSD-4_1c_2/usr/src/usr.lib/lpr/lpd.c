/*	lpd.c	4.17	83/03/09	*/
/*
 * lpd -- remote spooling line-printer daemon
 *
 *      Recorded into c from assembly. Part of the code is stolen
 *	from the dat-phone daemon.
 *	Runs set user-id root in order to connect to remote machines
 *	without passwords.
 *	NOTE: the lock file is used to pass information to lpq and lprm.
 *	it does not need to be removed because file locks are dynamic.
 */

#include <signal.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sgtty.h>
#ifdef BSD41C
#include <netdb.h>
#else
#include <net/in.h>
#endif
#include <errno.h>
#include <wait.h>
#include "lp.local.h"
#include "getq.h"

/*
 * Some utilities used by lpd
 */
#define PR		"/bin/pr"
#define MAIL		"/usr/lib/sendmail"

#define DORETURN	0	/* absorb fork error */
#define DOABORT		1	/* abort lpd if dofork fails */
#define DOUNLOCK	2	/* remove lock file before aborting */

char	line[132];		/* line from control file */
char	title[80];		/* ``pr'' title */
FILE	*cfp;			/* control file */
int	pfd;			/* printer file descriptor */
int	ofd;			/* output filter file descriptor */
int	lfd;			/* lock file descriptor */
int	pid;			/* id of lpd process */
int	prchild;		/* id of pr process */
int	child;			/* id of any filters */
int	ofilter;		/* id of output filter, if any */
int	tof = 1;		/* top of form; init true if open does ff */
int	remote;			/* non zero if sending files to remote */

int	DU;			/* daeomon user-id */
char	*DN;			/* daemon path name */
char	*LP;			/* line printer device name */
char	*RM;			/* remote machine name */
char	*RP;			/* remote printer name */
char	*RL;			/* path name of rlpr on remote machine */
char	*LO;			/* lock file name */
char	*ST;			/* status file name */
char	*SD;			/* spool directory */
char	*AF;			/* accounting file */
char	*LF;			/* log file for error messages */
char	*OF;			/* name of ouput filter (one only) */
char	*IF;			/* name of input filter (per job) */
char	*TF;			/* name of troff filter */
char	*VF;			/* name of vplot filter */
char	*CF;			/* name of cifplot filter */
char	*PF;			/* name of vrast filter */
char	*FF;			/* form feed string */
char	*TR;			/* trailer string to be output when Q empties */
short	SF;			/* suppress FF on each print job */
short	SH;			/* suppress header page */
short	SB;			/* short banner instead of normal header */
short	PW;			/* page width */
short	PL;			/* page length */
short	BR;			/* baud rate if lp is a tty */
short	FC;			/* flags to clear if lp is a tty */
short	FS;			/* flags to set if lp is a tty */

extern	banner();		/* big character printer */
char	logname[32];		/* user's login name */
char	hostname[32];		/* user's host machine */
char	jobname[32];		/* job or file name */
char	class[32];		/* classification field */
char	width[10] = "-w";	/* page width for `pr' */
char	length[10] = "-l";	/* page length for `pr' */

char	*name;			/* name of program */
char	*printer;		/* name of printer */
extern	errno;

char	*rindex();
char	*pgetstr();

/*ARGSUSED*/
main(argc, argv)
	char *argv[];
{
	struct stat stb;
	register struct queue *q, **qp;
	struct queue **queue;
	register int i, nitems;
	long pidoff;
	int cleanup();

	/*
	 * set-up controlled environment; trap bad signals
	 */
	signal(SIGHUP, SIG_IGN);
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGTERM, cleanup);		/* for use with lprm */

	name = argv[0];
	printer = argv[1];
	init();					/* set up capabilities */
	for (i = 0; i < NOFILE; i++)
		(void) close(i);
	(void) open("/dev/null", 0);	/* standard input */
	(void) open(LF, 1);		/* standard output */
	(void) dup2(1, 2);		/* standard error */
	(void) lseek(1, 0L, 2);		/* append if into a file */

	/*
	 * uses short form file names
	 */
	if (chdir(SD) < 0) {
		log("cannot chdir to %s", SD);
		exit(1);
	}
#ifdef BSD41C
	umask(0);
	if ((lfd = open(LO, FWRONLY|FCREATE|FTRUNCATE|FEXLOCK|FNBLOCK, FILMOD)) < 0) {
		if (errno == EWOULDBLOCK)	/* active deamon present */
			exit(0);
		log("cannot create %s", LO);
		exit(1);
	}
#else
	if (stat(LO, &stb) >= 0 || (lfd = creat(LO, 0444)) < 0)
		exit(0);
#endif
	/*
	 * kill the parent so the user's shell can function
	 */
#ifdef BSD41C
	if (dofork(DOABORT))
#else
	if (dofork(DOUNLOCK))
#endif
		exit(0);
	/*
	 * write process id for others to know
	 */
	pid = getpid();
	sprintf(line, "%u\n", pid);
	pidoff = i = strlen(line);
	if (write(lfd, line, i) != i)
		log("cannot write daemon pid");
	/*
	 * acquire line printer or remote connection
 	 */
	if (*LP) {
		for (i = 1; ; i = i < 256 ? i << 1 : i) {
			pfd = open(LP, 1);
			if (pfd >= 0)
				break;
			if (errno == ENOENT) {
				log("cannot open %s", LP);
#ifndef BSD41C
				unlink(LO);
#endif
				exit(1);
			}
			if (i == 1)
				status("waiting for %s to become ready (offline ?)", printer);
			sleep(i);
		}
		if (isatty(pfd))
			setty();
	}
	else if (RM != NULL) {
#ifdef BSD41C
		struct servent *sp;

		sp = getservbyname("shell", "tcp");
		if (sp == NULL) {
			log("shell/tcp: unknown service");
			exit(1);
		}
#endif
		strcpy(line, RL);
		if (RP != NULL)
			(void) sprintf(line+strlen(line), " -P%s", RP);
		for (i = 1; ; i = i < 512 ? i << 1 : i) {
#ifdef BSD41C
			pfd = rcmd(&RM, sp->s_port, "root", "root", line, 0);
#else
			pfd = rcmd(&RM, IPPORT_CMDSERVER, "root", "root", line, 0);
#endif
			if (pfd >= 0)
				break;
			if (i == 1)
				status("waiting for %s to come up", RM);
			sleep(i);
		}
		response();
		remote = 1;
	}
	else {
		log("no line printer device or remote machine name");
#ifndef BSD41C
		(void) unlink(LO);
#endif
		exit(1);
	}
	/*
	 * Start running as daemon instead of root
	 */
	setuid(DU);
	/*
	 * Start up an output filter, if needed.
	 */
	if (OF) {
		int p[2];
		char *cp;

		pipe(p);
#ifdef BSD41C
		if ((ofilter = dofork(DOABORT)) == 0) {	/* child */
#else
		if ((ofilter = dofork(DOUNLOCK)) == 0) {	/* child */
#endif
			dup2(p[0], 0);		/* pipe is std in */
			dup2(pfd, 1);		/* printer is std out */
			for (i = 3; i < NOFILE; i++)
				(void) close(i);
			if ((cp = rindex(OF, '/')) == NULL)
				cp = OF;
			else
				cp++;
			execl(OF, cp, 0);
			log("can't execl output filter %s", OF);
			exit(1);
		}
		(void) close(p[0]);		/* close input side */
		ofd = p[1];			/* use pipe for output */
	} else {
		ofd = pfd;
		ofilter = 0;
	}

	/*
	 * search the spool directory for work and sort by queue order.
	 */
again:
	if ((nitems = getq(&queue)) < 0) {
		log("can't scan spool directory %s", SD);
#ifndef BSD41C
		(void) unlink(LO);
#endif
		exit(1);
	}
	if (nitems == 0) {		/* EOF => no work to do */
		if (!SF && !tof)
			(void) write(ofd, FF, strlen(FF));
		if (TR != NULL)		/* output trailer */
			(void) write(ofd, TR, strlen(TR));
#ifndef BSD41C
		(void) unlink(LO);
#endif
		exit(0);
	}

	/*
	 * we found something to do now do it --
	 *    write the name of the current control file into the lock file
	 *    so the spool queue program can tell what we're working on
	 */
	if (!remote)
		status("%s is ready and printing", printer);
	else
		status("sending to %s", RM);
	for (qp = queue; nitems--; ) {
		q = *qp++;
		if (stat(q->q_name, &stb) < 0)
			continue;
		lseek(lfd, pidoff, 0);
		sprintf(line, "%s\n", q->q_name);
		i = strlen(line);
		if (write(lfd, line, i) != i)
			log("can't write (%d) control file name", errno);
		if (!remote)
			i = doit(q->q_name);
		else
			i = sendit(q->q_name);
		if (i > 0) {	/* restart daemon to reprint job */
#ifndef BSD41C
			unlink(LO);
#endif
			execl(DN, DN, printer);
			log("cannot execl %d", DN);
			exit(1);
		}
		free(q);
	}
	free((char *)queue);
	goto again;
}

char	fonts[4][50];	/* fonts for troff */

static char ifonts[4][50] = {
	"/usr/lib/vfont/R",
	"/usr/lib/vfont/I",
	"/usr/lib/vfont/B",
	"/usr/lib/vfont/S"
};

/*
 * The remaining part is the reading of the control file (cf)
 * and performing the various actions.
 * Returns 0 if everthing was OK and 1 if we should try to reprint the job.
 */
doit(file)
	char *file;
{
	register int i;
	int bombed = 0;

	/*
	 * open control file
	 */
	if ((cfp = fopen(file, "r")) == NULL) {
		log("control file (%s) open failure <errno = %d>", file, errno);
		return(0);
	}
	/*
	 * Reset troff fonts.
	 */
	for (i = 0; i < 4; i++)
		strcpy(fonts[i], ifonts[i]);

	/*
	 *      read the control file for work to do
	 *
	 *      file format -- first character in the line is a command
	 *      rest of the line is the argument.
	 *      valid commands are:
	 *
	 *		J -- "job name" on banner page
	 *		C -- "class name" on banner page
	 *              L -- "literal" user's name to print on banner
	 *		T -- "title" for pr
	 *		H -- "host name" of machine where lpr was done
	 *              P -- "person" user's login name
	 *              I -- "indent" changes default indents driver
	 *                   must have stty/gtty avaialble
	 *              f -- "file name" name of file to print
	 *		1 -- "R font file" for troff
	 *		2 -- "I font file" for troff
	 *		3 -- "B font file" for troff
	 *		4 -- "S font file" for troff
	 *		N -- "name" of file (used by lpq)
	 *              U -- "unlink" name of file to remove
	 *                    (after we print it. (Pass 2 only)).
	 *		M -- "mail" to user when done printing
	 *
	 *      getline reads a line and expands tabs to blanks
	 */

	/* pass 1 */

	while (getline())
		switch (line[0]) {
		case 'H':
			strcpy(hostname, line+1);
			if (class[0] == '\0')
				strcpy(class, line+1);
			continue;

		case 'P':
			strcpy(logname, line+1);
			continue;

		case 'J':
			if (line[1] != '\0' )
				strcpy(jobname, line+1);
			else
				strcpy(jobname, " ");
			continue;

		case 'C':
			if (line[1] != '\0' )
				strcpy(class, line+1);
			else if (class[0] == '\0')
				gethostname(class, sizeof (class));
			continue;

		case 'T':	/* header title for pr */
			strcpy(title, line+1);
			continue;

		case 'L':	/* identification line */
			if (!SH)
				banner(line+1, jobname);
			continue;

		case '1':	/* troff fonts */
		case '2':
		case '3':
		case '4':
			strcpy(fonts[line[0]-'1'], line+1);
			continue;

		case 'W':	/* page width */
			strcpy(width+2, line+1);
			continue;

		default:
			if ((i = dump(line[0], line+1)) > 0)
				return(1);
			else if (i < 0)
				bombed = 1;
			title[0] = '\0';
			continue;

		case 'I':
		case 'N':
		case 'U':
		case 'M':
			continue;
		}

	/* pass 2 */

	fseek(cfp, 0L, 0);
	while (getline())
		switch (line[0]) {
		case 'M':
			sendmail(bombed);
			continue;

		case 'U':
			(void) unlink(line+1);
		}
	/*
	 * clean-up incase another control file exists
	 */
	(void) fclose(cfp);
	(void) unlink(file);
	return(0);
}

/*
 * Print a file.
 * Set up the chain [ PR [ | IF ] ] or [ {IF | TF | CF | VF} ].
 * Return -1 if a non-recoverable error occured, 1 if a recoverable error and
 * 0 if all is well.
 */
dump(format, file)
	int format;
	char *file;
{
	register int n, fi, fo;
	register char *prog;
	char *av[15], buf[BUFSIZ];
	int pid, p[2];
	union wait status;

	if ((fi = open(file, 0)) < 0) {
		log("%s: open failure <errno = %d>", file, errno);
		return(-1);
	}
	if (!SF && !tof)		/* start on a fresh page */
		(void) write(ofd, FF, strlen(FF));
	if (IF == NULL && (format == 'f' || format == 'l')) {
		while ((n = read(fi, buf, BUFSIZ)) > 0)
			if (write(ofd, buf, n) != n) {
				(void) close(fi);
				return(1);
			}
		tof = 0;
		(void) close(fi);
		return(0);
	}
	switch (format) {
	case 'p':	/* print file using 'pr' */
		if (IF == NULL) {	/* use output filter */
			prog = PR;
			av[0] = "pr";
			av[1] = width;
			av[2] = length;
			av[3] = "-h";
			av[4] = *title ? title : " ";
			av[5] = 0;
			goto start;
		}
		pipe(p);
		if ((prchild = dofork(DORETURN)) == 0) {	/* child */
			dup2(fi, 0);		/* file is stdin */
			dup2(p[1], 1);		/* pipe is stdout */
			for (n = 3; n < NOFILE; n++)
				(void) close(n);
			execl(PR, "pr", width, length, "-h", *title ? title : " ", 0);
			log("cannot execl %s", PR);
			exit(2);
		}
		(void) close(p[1]);		/* close output side */
		(void) close(fi);
		fi = p[0];			/* use pipe for input */
	case 'f':	/* print plain text file */
		prog = IF;
		av[1] = width;
		av[2] = length;
		n = 3;
		fo = pfd;
		break;
	case 'l':	/* like 'f' but pass control characters */
		prog = IF;
		av[1] = "-l";
		av[2] = width;
		av[3] = length;
		n = 4;
		fo = pfd;
		break;
	case 't':	/* print troff output */
		(void) unlink(".railmag");
		fo = creat(".railmag", 0666);
		for (n = 0; n < 4; n++) {
			if (fonts[n][0] != '/')
				(void) write(fo, "/usr/lib/vfont/", 15);
			(void) write(fo, fonts[n], strlen (fonts[n]));
			(void) write(fo, "\n", 1);
		}
		(void) close(fo);
		prog = TF;
		n = 1;
		fo = pfd;
		break;
	case 'c':	/* print cifplot output */
		prog = CF;
		n = 1;
		fo = pfd;
		break;
	case 'v':	/* print vplot output (plain raster file) */
		prog = VF;
		n = 1;
		fo = pfd;
		break;
	default:
		log("illegal format character '%c'", format);
		return(-1);
	}
	if ((av[0] = rindex(prog, '/')) != NULL)
		av[0]++;
	else
		av[0] = prog;
	av[n++] = "-n";
	av[n++] = logname;
	av[n++] = "-h";
	av[n++] = hostname;
	av[n++] = AF;
	av[n++] = 0;
start:
	if (ofilter > 0) {		/* stop output filter */
		write(ofd, "\031\1", 2);
		while ((pid = wait3(&status, WUNTRACED, 0)) > 0 && pid != ofilter)
			;
		if (status.w_stopval != WSTOPPED) {
			log("output filter died (%d)", status.w_retcode);
			return(1);
		}
	}
	if ((child = dofork(DORETURN)) == 0) {	/* child */
		dup2(fi, 0);
		dup2(fo, 1);
		for (n = 3; n < NOFILE; n++)
			(void) close(n);
		execv(prog, av);
		log("cannot execl %s", prog);
		exit(2);	/* execl failed or not one of above cases. */
	}
	(void) close(fi);
	if (child < 0) {
		child = 0;
		return(-1);
	}
	while ((pid = wait(&status)) > 0 && pid != child)
		;
	tof = 1;
	child = 0;
	prchild = 0;
	if (ofilter > 0) {		/* restart output filter */
		if (kill(ofilter, SIGCONT) < 0) {
			log("cannot continue output filter");
#ifndef BSD41C
			unlink(LO);
#endif
			exit(1);
		}
	}
	if (!WIFEXITED(status) || status.w_retcode > 1) {
		log("Daemon Filter Malfunction");
		return(-1);
	} else if (status.w_retcode == 1)
		return(1);
	return(0);
}

/*
 * Getline reads a line from the control file, removes tabs, converts
 * new-line to null and leaves it in line.
 * returns 0 at EOF or the number of characters read.
 */
getline()
{
	register int linel = 0;
	register char *lp = line;
	register c;

	while ((c = getc(cfp)) != '\n') {
		if (c == EOF)
			return(0);
		if (c == '\t') {
			do {
				*lp++ = ' ';
				linel++;
			} while ((linel & 07) != 0);
			continue;
		}
		*lp++ = c;
		linel++;
	}
	*lp++ = '\0';
	return(linel);
}

/*
 * Send the daemon control file (cf) and any data files.
 */
sendit(file)
	char *file;
{
	register int linelen, i;
	char last[132];

	/*
	 * open control file
	 */
	if ((cfp = fopen(file, "r")) == NULL) {
		log("control file (%s) open failure <errno = %d>", file, errno);
		return(0);
	}
	/*
	 *      read the control file for work to do
	 *
	 *      file format -- first character in the line is a command
	 *      rest of the line is the argument.
	 *      commands of interest are:
	 *
	 *              f -- "file name" name of file to print
	 *              U -- "unlink" name of file to remove
	 *                    (after we print it. (Pass 2 only)).
	 */

	/*
	 * pass 1
	 */
	while (getline()) {
	again:
		if (line[0] >= 'a' && line[0] <= 'z') {
			strcpy(last, line);
			while (linelen = getline())
				if (strcmp(last, line))
					break;
			if (send('\3', last+1) > 0)
				return(1);
			if (linelen)
				goto again;
			break;
		}
	}
	if (send('\2', file) > 0)
		return(1);
	/*
	 * pass 2
	 */
	fseek(cfp, 0L, 0);
	while (getline())
		if (line[0] == 'U')
			(void) unlink(line+1);
	/*
	 * clean-up incase another control file exists
	 */
	(void) fclose(cfp);
	(void) unlink(file);
	return(0);
}

/*
 * Send a data file to the remote machine and spool it.
 * Return positive if we should try resending.
 */
send(type, file)
	char type, *file;
{
	register int f, i, amt;
	struct stat stb;
	char buf[BUFSIZ];
	int sizerr;

	if ((f = open(file, 0)) < 0 || fstat(f, &stb) < 0) {
		log("file (%s) open failure <errno = %d>", file, errno);
		return(-1);
	}
	(void) sprintf(buf, "%c%d %s\n", type, stb.st_size, file);
	if (write(pfd, buf, strlen(buf)) < 0)
		return(1);
	if (response())
		return(1);
	sizerr = 0;
	for (i = 0; i < stb.st_size; i += BUFSIZ) {
		amt = BUFSIZ;
		if (i + amt > stb.st_size)
			amt = stb.st_size - i;
		if (sizerr == 0 && read(f, buf, amt) != amt)
			sizerr = 1;
		if (write(pfd, buf, amt) < 0)
			return(1);
	}
	(void) close(f);
	if (sizerr) {
		log("%s: changed size", file);
		if(write(pfd, "\1", 1) < 0)	/* signal an error occured */
			return(1);
	} else {
		if (write(pfd, "", 1) < 0)
			return(1);
		if (response())
			return(1);
	}
	return(0);
}

/*
 * Check to make sure there have been no errors and that both programs
 * are in sync with eachother.
 * Return non-zero if the connection was lost.
 */
response()
{
	char resp;

	if (read(pfd, &resp, 1) != 1)
		return(1);
	if (resp) {
		log("fatal error in rlpr");
#ifndef BSD41C
		(void) unlink(LO);
#endif
		exit(1);
	}
	return(0);
}

/*
 * Banner printing stuff
 */
banner(name1, name2)
	char *name1, *name2;
{
	time_t tvec;
	extern char *ctime();

	time(&tvec);
	if (!SF && !tof)
		(void) write(ofd, FF, strlen(FF));
	if (SB) {	/* short banner only */
		if (class[0]) {
			(void) write(ofd, class, strlen(class));
			(void) write(ofd, ":", 1);
		}
		(void) write(ofd, name1, strlen(name1));
		(void) write(ofd, "  Job: ", 7);
		(void) write(ofd, name2, strlen(name2));
		(void) write(ofd, "  Date: ", 8);
		(void) write(ofd, ctime(&tvec), 24);
		(void) write(ofd, "\n", 1);
	} else {	/* normal banner */
		(void) write(ofd, "\n\n\n", 3);
		scan_out(ofd, name1, '\0');
		(void) write(ofd, "\n\n", 2);
		scan_out(ofd, name2, '\0');
		if (class[0]) {
			(void) write(ofd,"\n\n\n",3);
			scan_out(ofd, class, '\0');
		}
		(void) write(ofd, "\n\n\n\n\t\t\t\t\tJob:  ", 15);
		(void) write(ofd, name2, strlen(name2));
		(void) write(ofd, "\n\t\t\t\t\tDate: ", 12);
		(void) write(ofd, ctime(&tvec), 24);
		(void) write(ofd, "\n", 1);
	}
	if (!SF)
		(void) write(ofd, FF, strlen(FF));
	tof = 1;
}

char *
scnline(key, p, c)
	register char key, *p;
	char c;
{
	register scnwidth;

	for (scnwidth = WIDTH; --scnwidth;) {
		key <<= 1;
		*p++ = key & 0200 ? c : BACKGND;
	}
	return (p);
}

#define TRC(q)	(((q)-' ')&0177)

scan_out(scfd, scsp, dlm)
	int scfd;
	char *scsp, dlm;
{
	register char *strp;
	register nchrs, j;
	char outbuf[LINELEN+1], *sp, c, cc;
	int d, scnhgt;
	extern char scnkey[][HEIGHT];	/* in lpdchar.c */

	for (scnhgt = 0; scnhgt++ < HEIGHT+DROP;) {
		strp = &outbuf[0];
		sp = scsp;
		for (nchrs = 0;;) {
			d = dropit(c = TRC(cc = *sp++));
			if ((!d && scnhgt>HEIGHT ) || (scnhgt<=DROP && d))
				for (j=WIDTH; --j;)
					*strp++ = BACKGND;
			else
				strp = scnline(scnkey[c][scnhgt-1-d], strp, cc);
			if (*sp==dlm || *sp=='\0' || nchrs++>=LINELEN/(WIDTH+1)-1)
				break;
			*strp++ = BACKGND;
			*strp++ = BACKGND;
		}
		while (*--strp== BACKGND && strp >= outbuf)
			;
		strp++;
		*strp++ = '\n';	
		(void) write(scfd, outbuf, strp-outbuf);
	}
}

dropit(c)
	char c;
{
	switch(c) {

	case TRC('_'):
	case TRC(';'):
	case TRC(','):
	case TRC('g'):
	case TRC('j'):
	case TRC('p'):
	case TRC('q'):
	case TRC('y'):
		return (DROP);

	default:
		return (0);
	}
}

/*
 * sendmail ---
 *   tell people about job completion
 */
sendmail(bombed)
	int bombed;
{
	static int p[2];
	register int i;
	int stat;
	register char *cp;
	char buf[100];

	pipe(p);
	if ((stat = dofork(DORETURN)) == 0) {
		(void) close(0);
		dup(p[0]);
		for (i = 3; i <= NOFILE; i++)
			(void) close(i);
		if ((cp = rindex(MAIL, '/')) != NULL)
			cp++;
		else
			cp = MAIL;
		sprintf(buf, "%s.%s", hostname, line+1);
		execl(MAIL, cp, buf, 0);
		exit(0);
	} else if (stat > 0) {
		(void) close(1);
		dup(p[1]);
		printf("To: %s\n", line+1);
		printf("Subject: printer job\n\n");
		printf("Your printer job ");
		if (*jobname)
			printf("(%s) ", jobname);
		if (bombed)
			printf("bombed\n");
		else
			printf("is done\n");
		fflush(stdout);
		(void) close(1);
	}
	(void) close(p[0]);
	(void) close(p[1]);
	open(LF, 1);
	wait(&stat);
}

/*
 * dofork - fork with retries on failure
 */
dofork(action)
	int action;
{
	register int i, pid;

	for (i = 0; i < 20; i++) {
		if ((pid = fork()) < 0)
			sleep((unsigned)(i*i));
		else
			return (pid);
	}
	log("can't fork");

	switch(action) {
	case DORETURN:
		return (-1);
	default:
		log("bad action (%d) to dofork", action);
		/*FALL THRU*/
#ifndef BSD41C
	case DOUNLOCK:
		(void) unlink(LO);
		/*FALL THRU*/
#endif
	case DOABORT:
		exit(1);
	}
	/*NOTREACHED*/
}

cleanup()
{
	signal(SIGTERM, SIG_IGN);
	if (prchild > 0)
		kill(child, SIGKILL);	/* get rid of pr's */
	if (child > 0)
		kill(child, SIGKILL);	/* get rid of filters */
	if (ofilter > 0)
		kill(ofilter, SIGKILL);	/* get rid of output filter */
	while (wait(0) > 0)
		;
	exit(0);			/* lprm removes the lock file */
}

/*VARARGS1*/
log(message, a1, a2, a3)
	char *message;
{
	short console = isatty(fileno(stderr));

	fprintf(stderr, console ? "\r\n%s: " : "%s: ", name);
	fprintf(stderr, message, a1, a2, a3);
	if (console)
		putc('\r', stderr);
	putc('\n', stderr);
	fflush(stderr);
}

/*VARARGS1*/
status(message, a1, a2, a3)
	char *message;
{
	register int fd;
	char buf[BUFSIZ];

	if ((fd = creat(ST, FILMOD)) < 0) {
		log("cannot create %s", ST);
#ifndef BSD41C
		unlink(LO);
#endif
		exit(1);
	}
	sprintf(buf, message, a1, a2, a3);
	strcat(buf, "\n");
	(void) write(fd, buf, strlen(buf));
	(void) close(fd);
}

init()
{
	char b[BUFSIZ];
	static char buf[BUFSIZ/2];
	static char *bp = buf;
	int status;

	if ((status = pgetent(b, printer)) < 0) {
		printf("%s: can't open printer description file\n", name);
		exit(3);
	} else if (status == 0) {
		printf("%s: unknown printer\n", printer);
		exit(4);
	}
	if ((DN = pgetstr("dn", &bp)) == NULL)
		DN = DEFDAEMON;
	if ((LP = pgetstr("lp", &bp)) == NULL)
		LP = DEFDEVLP;
	if ((LO = pgetstr("lo", &bp)) == NULL)
		LO = DEFLOCK;
	if ((ST = pgetstr("st", &bp)) == NULL)
		ST = DEFSTAT;
	if ((LF = pgetstr("lf", &bp)) == NULL)
		LF = DEFLOGF;
	if ((SD = pgetstr("sd", &bp)) == NULL)
		SD = DEFSPOOL;
	if ((FF = pgetstr("ff", &bp)) == NULL)
		FF = DEFFF;
	if ((RL = pgetstr("rl", &bp)) == NULL)
		RL = DEFRLPR;
	if ((DU = pgetnum("du")) < 0)
		DU = DEFUID;
	if ((PW = pgetnum("pw")) < 0)
		PW = DEFWIDTH;
	sprintf(&width[2], "%d", PW);
	if ((PL = pgetnum("pl")) < 0)
		PL = DEFLENGTH;
	sprintf(&length[2], "%d", PL);
	RM = pgetstr("rm", &bp);
	RP = pgetstr("rp", &bp);
	AF = pgetstr("af", &bp);
	OF = pgetstr("of", &bp);
	IF = pgetstr("if", &bp);
	TF = pgetstr("tf", &bp);
	VF = pgetstr("vf", &bp);
	CF = pgetstr("cf", &bp);
	PF = pgetstr("pf", &bp);
	TR = pgetstr("tr", &bp);
	SF = pgetflag("sf");
	SH = pgetflag("sh");
	SB = pgetflag("sb");
	BR = pgetnum("br");
	if ((FC = pgetnum("fc")) < 0)
		FC = 0;
	if ((FS = pgetnum("fs")) < 0)
		FS = 0;
}

struct bauds {
	int	baud;
	int	speed;
} bauds[] = {
	50,	B50,
	75,	B75,
	110,	B110,
	134,	B134,
	150,	B150,
	200,	B200,
	300,	B300,
	600,	B600,
	1200,	B1200,
	1800,	B1800,
	2400,	B2400,
	4800,	B4800,
	9600,	B9600,
	19200,	EXTA,
	38400,	EXTB,
	0,	0
};

/*
 * setup tty lines.
 */
setty()
{
	struct sgttyb ttybuf;
	register struct bauds *bp;

	if (ioctl(pfd, TIOCGETP, (char *)&ttybuf) < 0) {
		log("cannot get tty parameters");
#ifndef BSD41C
		(void) unlink(LO);
#endif
		exit(1);
	}

	if (BR > 0) {
		for (bp = bauds; bp->baud; bp++)
			if (BR == bp->baud)
				break;
		if (!bp->baud) {
			log("illegal baud rate %d", BR);
#ifndef BSD41C
			(void) unlink(LO);
#endif
			exit(1);
		}
		ttybuf.sg_ispeed = ttybuf.sg_ospeed = bp->speed;
	}
	if (FC)
		ttybuf.sg_flags &= ~FC;
	if (FS)
		ttybuf.sg_flags |= FS;

	if (ioctl(pfd, TIOCSETN, (char *)&ttybuf) < 0) {
		log("cannot set tty parameters");
#ifndef BSD41C
		(void) unlink(LO);
#endif
		exit(1);
	}
}
