/*	lprm.c	4.11	83/03/01	*/
/*
 * lprm - remove the current user's spool entry
 *
 * lprm [-] [[job #] [user] ...]
 *
 * Using information in the lock file, lprm will kill the
 * currently active daemon (if necessary), remove the associated files,
 * and startup a new daemon.  Priviledged users may remove anyone's spool
 * entries, otherwise one can only remove their own.
 */

#include <stdio.h>
#include <signal.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/file.h>
#include <dir.h>
#include <pwd.h>
#ifdef BSD41C
#include <netdb.h>
#else
#include <net/in.h>
#endif
#include "lp.local.h"

#define MAXUSERS	50
#define MAXREQUESTS	50

/*
 * Stuff for handling lprm specifications
 */
char	*user[MAXUSERS];		/* users to process */
int	users;				/* # of users in user array */
int	requ[MAXREQUESTS];		/* job number of spool entries */
int	requests;			/* # of spool requests */
int	cur_daemon;			/* daemon's pid */
char	current[40];			/* active control file name */
int	all = 0;			/* eliminate all files (root only) */

/*
 * Stuff for printcap (a la termcap) description
 */
char	*SD;				/* spool directory */
char	*LO;				/* lock file name */
char	*DN;				/* daemon path name */
char	*LP;				/* line printer device name */
char	*RM;				/* remote machine name */
char	*RP;				/* remote printer name */
char	*BD;				/* bin directory of lprm on remote */
char	*AF;				/* accounting file name */
char	*pgetstr();

char	line[132];			/* line buffer */
char	person[32];			/* name of person doing lprm */
char	root[32];			/* name of root */
char	host[32];			/* machine we're running on */
char	*from;				/* host machine name of person */

struct passwd *getpwuid();
char	*getenv();
char	*rindex();

main(argc, argv)
	char *argv[];
{
	register char *arg;
	register int i;
	struct passwd *p;
	char *printer = NULL;
	struct direct **files;
	int nitems, assasinated = 0;
	int select();

	i = getuid();
	if ((p = getpwuid(i)) == NULL)
		fatal("who are you?");
	strcpy(person, p->pw_name);
	if ((p = getpwuid(0)) == NULL)
		fatal("who is UID 0?");
	strcpy(root, p->pw_name);
	gethostname(host, sizeof(host));
	while (--argc) {
		if ((arg = *++argv)[0] == '-') {
			switch (arg[1]) {
			case 'P':
				printer = &arg[2];
				break;
			case 'R': /* running remotely */
				if (i != 0)
					usage();
				from = &arg[2];
				break;
			case '\0':
				if (!users) {
					users = -1;
					break;
				}
			default:
				usage();
			}
		} else {
			if (users < 0)
				usage();
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
	/*
	 * If the format was `lprm -' and the user isn't super-user,
	 *  then fake things to look like he said `lprm user'.
	 */
	if (users < 0) {
		if (i != 0) {
			user[0] = person;
			users = 1;
		} else
			all = 1;
	}
	/*
	 * Get the printer information.
	 */
	if (printer == NULL && (printer = getenv("PRINTER")) == NULL)
		printer = DEFLP;
	if (!chkprinter(printer))
		fatal("%s: unknown printer", printer);
	if (chdir(SD) < 0)
		fatal("can't chdir to spool directory");
	if ((nitems = scandir(".", &files, select, NULL)) < 0)
		fatal("can't access spool directory");
	if (nitems == 0) {
		chkremote();
		exit(0);
	}
	/*
	 * Check for an active daemon (in which case we kill it
	 *  if it is reading our file) then remove stuff
	 *  (after which we have to restart the daemon).
	 */
	if (lockchk(LO) && chk(current)) {
		assasinated = (kill(cur_daemon, SIGTERM) == 0);
		if (!assasinated)
			fatal("can't kill daemon");
	}
	/*
	 * process the files
	 */
	for (i = 0; i < nitems; i++)
		process(files[i]->d_name);
	chkremote();
	/*
	 * Restart the daemon if it was killed
	 */
	if (assasinated) {
#ifndef BSD41C
		unlink(LO);
#endif
		execl(DN, arg = rindex(DN, '/') ? arg+1 : DN, printer, 0);
		fatal("can't restart daemon");
	}
}

/*
 * Process a lock file: collect the pid of the active
 *  daemon and the file name of the active spool entry.
 * Return boolean indicating existence of a lock file.
 */
lockchk(s)
	char *s;
{
	register FILE *fp;
	register int i, n;
	extern int errno;

	if ((fp = fopen(s, "r")) == NULL)
		if (errno == EACCES)
			fatal("can't access lock file");
		else
			return(0);
#ifdef BSD41C
	if (flock(fileno(fp), FEXLOCK|FNBLOCK) == 0 || getline(fp) == 0) {
#else
	if (getline(fp) == 0) {
#endif
		(void) fclose(fp);
		return(0);
	}
	cur_daemon = atoi(line);
	for (i = 1; (n = fread(current, sizeof(char), sizeof(current), fp)) <= 0; i++) {
		if (i > 20) {
			n = 1;
			break;
		}
		sleep(i);
	}
	current[n-1] = '\0';
	(void) fclose(fp);
	return(1);
}

/*
 * Process a control file.
 */
process(file)
	char *file;
{
	FILE *cfp;

	if (!chk(file))
		return;
	if ((cfp = fopen(file, "r")) == NULL)
		fatal("cannot open %s", file);
	while (getline()) {
		switch (line[0]) {
		case 'U':  /* unlink associated files */
			if (from != NULL)
				printf("%s:", host);
			printf(unlink(line+1) ? "cannot dequeue %s\n" :
				"%s dequeued\n", line+1);
		}
	}
	(void) fclose(cfp);
	if (from != NULL)
		printf("%s:", host);
	printf(unlink(file) ? "cannot dequeue %s\n" : "%s dequeued\n", file);
}

/*
 * Do the dirty work in checking
 */
chk(file)
	char *file;
{
	register int *r, n;
	register char **u, *cp;
	FILE *cfp;

	if (all)
		return(1);
	/*
	 * get the owner's name from the control file.
	 */
	if ((cfp = fopen(file, "r")) == NULL)
		return(0);
	while (getline(cfp)) {
		if (line[0] == 'P')
			break;
	}
	(void) fclose(cfp);
	if (line[0] != 'P')
		return(0);

	if (users == 0 && requests == 0)
		return(!strcmp(file, current) && isowner(person, file));
	/*
	 * Check the request list
	 */
	for (n = 0, cp = file+3; isdigit(*cp); )
		n = n * 10 + (*cp++ - '0');
	for (r = requ; r < &requ[requests]; r++)
		if (*r == n && isowner(person, file))
			return(1);
	/*
	 * Check to see if it's in the user list
	 */
	for (u = user; u < &user[users]; u++)
		if (isowner(*u, file))
			return(1);
	return(0);
}

/*
 * If root is removing a file on the local machine, allow it.
 * If root is removing a file on a remote machine, only allow
 * files sent from the local machine to be removed.
 * Normal users can only remove the file from where it was sent.
 */
isowner(name, file)
	char *name, *file;
{
	if (from == NULL)
		return(access(file, 2) == 0);
	return((!strcmp(name, root) || !strcmp(line+1, name)) &&
		!strcmp(from, file+6));
}

/*
 * Read a line from the control file.
 * Convert new-line to null and leave it in line.
 * returns 0 at EOF or the number of characters read.
 */
getline(fp)
	FILE *fp;
{
	register int linel = 0;
	register char *lp = line;
	register c;

	while ((c = getc(fp)) != '\n' && c != EOF) {
		*lp++ = c;
		linel++;
	}
	*lp++ = '\0';
	return (linel);
}

/*
 * Check to see if we are sending files to a remote machine. If we are,
 * then try removing files on the remote machine.
 */
chkremote()
{
	register char *cp;
	char buf[BUFSIZ];
	register int i, rem;
#ifdef BSD41C
	struct servent *sp;
#endif
	extern int errno;

	if (*LP || RM == NULL)
		return;	/* not sending to a remote machine */
	cp = buf;
	(void) sprintf(cp, "%s/lprm -R%s", BD, from == NULL ? host : from);
	if (RP != NULL) {
		cp += strlen(cp);
		(void) sprintf(cp, " -P%s", RP);
	}
	for (i = 0; i < users; i++) {
		cp += strlen(cp);
		*cp++ = ' ';
		strcpy(cp, user[i]);
	}
	for (i = 0; i < requests; i++) {
		cp += strlen(cp);
		(void) sprintf(cp, " %d", requ[i]);
	}
#ifdef BSD41C
	if ((sp = getservbyname("shell", "tcp")) == NULL)
		fatal("cannot find sever");
	rem = rcmd(&RM, sp->s_port, "root", "root", buf, 0);
#else
	rem = rcmd(&RM, IPPORT_CMDSERVER, "root", "root", buf, 0);
#endif
	if (rem < 0) {
		if (from != NULL)
			printf("%s: ", host);
		printf("connection to %s is down\n", RM);
		exit(0);
	}
	for (;;) {
		i = read(rem, buf, sizeof(buf));
		if (i <= 0)
			break;
		(void) write(1, buf, i);
	}
}

/*
 * Return 1 if the filename begins with 'cf'
 */
select(d)
	struct direct *d;
{
	return(d->d_name[0] == 'c' && d->d_name[1] == 'f');
}

fatal(s, a)
	char *s, *a;
{
	if (from != NULL)
		printf("%s: ", host);
	printf("lprm: fatal error: ");
	printf(s, a);
	putchar('\n');
	exit(1);
}

usage()
{
	printf("usage: lprm [-] [-Pprinter] [[job #] [user] ...]\n");
	exit(2);
}

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
	if ((DN = pgetstr("dn", &bp)) == NULL)
		DN = DEFDAEMON;
	if ((SD = pgetstr("sd", &bp)) == NULL)
		SD = DEFSPOOL;
	if ((LO = pgetstr("lo", &bp)) == NULL)
		LO = DEFLOCK;
	if ((BD = pgetstr("bd", &bp)) == NULL)
		BD = DEFBINDIR;
	if ((LP = pgetstr("lp", &bp)) == NULL)
		LP = DEFDEVLP;
	RM = pgetstr("rm", &bp);
	RP = pgetstr("rp", &bp);
	AF = pgetstr("af", &bp);
	return(1);
}
