/*
 * v6mail
 */
#include <sysexits.h>

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <sys/times.h>
#include <ctype.h>
#include <errno.h>
#include <pwd.h>
#include <signal.h>

char	*ctime(), *index(), *rindex(), *ctime(), *strcpy(), *getlogin();
char	*mktemp();
struct	passwd *getpwnam(), *getpwuid();
time_t	time();
struct	utmp *getutmp();
char	*getdate();
int	errno;

#include <stdio.h>

#define MAILMODE 0644
#define MSGSCMD "/usr/ucb/msgs"
#define MAILDIR "/usr/spool/mail"

char	lettmp[]  = "/tmp/MaXXXXX";	/* keep letter before sending it */
char	preptmp[] = "/tmp/mbXXXXX";	/* if prepending msg, use this file */
int	chew;				/* if true, strip extra from lines */
int	dflag;				/* if true, don't call sendmail */
char	shopcnt[30] = "0";		/* hop count parameter for rmt mail */
int	errs;				/* no of errs in sending */
char	deleteonly;			/* if true, just delete mailbox */
char 	remname[50];			/* if non-empty, from line extra */

main(argc, argv)
	int argc;
	char **argv;
{
	register int myuid;
	int delexit();
	char namebuf[128], *sn = NULL, logindir[60];
	struct passwd *pwd;

	(void) mktemp(lettmp);
	(void) mktemp(preptmp);
	(void) unlink(lettmp);
	(void) unlink(preptmp);
	myuid = getuid();
	logindir[0] = 0;
	sn = getlogin();
	if (sn == NULL || *sn == 0 || *sn == ' ') {
		pwd = getpwuid(myuid);		/* will read passwd file */
		if (pwd != NULL){
			sn = pwd->pw_name;
			(void) strcpy(logindir, pwd->pw_dir);
		}
		if (sn == NULL) {
			fprintf(stderr, "Who are you?\n");
			delexit(EX_OSFILE);
		}
	}
	(void) strcpy(namebuf, sn);
	if (argc < 2)
		goto hitit;
	for (argc--, argv++; argc > 0 && argv[0][0] == '-'; argc--, argv++)
	switch (argv[0][1]) {

	case 'y':
	case 'n':
		argc++, argv--;
hitit:
		printmail(argc, argv, namebuf, logindir);
		delexit(EX_OK);

	case 'r':	/* one-arg -r--   -r addr */
		if (argc < 2)
			continue;
		/* ignore -r if not network or root */
		if (strcmp("network", namebuf) == 0 || myuid == 0 ||
/*###86 [lint] index arg. 1 used inconsistently v6mail.c(86) :: v6mail.c(244)%%%*/
/*###86 [lint] index arg. 2 used inconsistently v6mail.c(86) :: v6mail.c(244)%%%*/
		    strcmp("uucp", namebuf) == 0 || index('!', argv[1])) {
			(void) strcpy(namebuf, argv[1]);
			chew++;		/* eat From lines */
		}
		else
			(void) strcpy(remname, argv[1]);
		argc--, argv++;
		continue;

	case 'h':	/* hop count - used by network */
		if (argc < 2)
			continue;
		(void) strcpy(shopcnt, argv[1]);
		argc--, argv++;
		continue;

	case 'd':	/* really deliver this message */
		dflag++;
		continue;

	case 'D':	/* only delete the invokers mailbox */
		deleteonly++;
		goto hitit;		/* delete mail box, thats all */
	}
	/* if we are already ignoring signals, catch sigint */
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		(void) signal(SIGINT, delexit);
	argc++, argv--;
	bulkmail(argc, argv, namebuf);
	delexit(EX_OK);
}

printmail(argc, argv, name, logindir)
	int argc;
	char **argv;
	char *name, *logindir;
{
	register int c;
	FILE *fdin;
	char sfnmail[60], mbox[120];
	struct stat stb;

	(void) sprintf(sfnmail, "%s/%s", MAILDIR, name);
	if (deleteonly) {
		remove(sfnmail);
		return;
	}
	fdin = fopen(sfnmail, "r");
	if (fdin < 0 || fstat(fileno(fdin), &stb) < 0 || stb.st_size == 0) {
		printf("No mail.\n");
		return;
	}
	if (stb.st_nlink > 1) {
		printf("%s: Too many links.\n", sfnmail);
		return;
	}
	(void) getput(fdin, stdout);
	(void) fclose(fdin);
	(void) fflush(stdout);
	c = 'y';
	if (argc < 2) {
		if (isatty(0)) {
			printf("Save (y or n) ?"); (void) fflush(stdout);
			c = getchar();
		}
	} else
		c = argv[1][1];
	switch (c) {

	default:
		delexit(EX_OK);
		/*NOTREACHED*/

	case 'x':
		return;

	case 'y':
		(void) sprintf(mbox, "%s/mbox", logindir);
		if (writeable(mbox)) {
			perror(mbox);
			return;
		}
		printf("Saving mail in %s.\n", mbox);
		if (append(sfnmail, mbox, getuid(), getgid()) == 0)
			return;
		/* fall into... */

	case 'n':
		remove(sfnmail);
		return;
	}
}

bulkmail(argc, argv, from)
	char **argv, *from;
{
	char linebuf[BUFSIZ];
	FILE *fdout;

	if (dflag == 0) {
		argv[0] = "sendmail";
		argv[argc] = 0;
		execv("/usr/lib/sendmail", argv);
		perror("/usr/lib/sendmail");
		_exit(1);
	}
	fdout = fopen(lettmp, "w");
	if (fdout == NULL) {
		perror(lettmp);
		delexit(EX_OSFILE);
	}

	/*
	 * If delivering mail from the network via mail -r,
	 * Strip the leading line and throw it away, as long
	 * as it begins with "From ..." (and preserve the date if poss.)
	 */
	if (chew) {
		if (fgets(linebuf, BUFSIZ, stdin) == 0)
			goto skip;
		if (!strncmp(linebuf, "From ", 5) != 0)
			printfromline(fdout, getdate(linebuf), from);
		else {
			printfromline(fdout, (char *)0, from);
			fprintf(fdout, "%s", linebuf);
		}
	} else
		printfromline(fdout, (char *)0, from);
skip:
	if (remname[0])
		fprintf(fdout, "(from %s)\n", remname);
	if (getput(stdin, fdout) == 0)
		delexit(EX_OSERR);
	putc('\n', fdout);
	(void) fclose(fdout);
	while (--argc > 0)
		sendto(*++argv);
	delexit(errs);
}

printfromline(fdout, date, from)
	FILE *fdout;
	char *date, *from;
{
	time_t t;

	if (date == NULL) {
		t = time((time_t *)0);
		date = ctime(&t);
	}
	fprintf(fdout, "From %s  %s", from, date);
}

/* look over linebuf and return ptr to date, NULL if error */
char *
getdate(linebuf)
	char *linebuf;
{
	register char *s = linebuf;

/*###244 [lint] index arg. 2 used inconsistently v6mail.c(86) :: v6mail.c(244)%%%*/
/*###244 [lint] index arg. 1 used inconsistently v6mail.c(86) :: v6mail.c(244)%%%*/
	while (s = index(' ', s))
		if (!strncmp(s, " Sun ", 5) ||
		    !strncmp(s, " Mon ", 5) ||
		    !strncmp(s, " Tue ", 5) ||
		    !strncmp(s, " Wed ", 5) ||
		    !strncmp(s, " Thu ", 5) ||
		    !strncmp(s, " Fri ", 5) ||
		    !strncmp(s, " Sat ", 5))
			return (s + 1);
	return (0);
}

int	saved = 0;

sendto(person)
	char *person;
{
	char mailboxname[BUFSIZ];
	struct passwd *pwd;

	if (index('/', person)) {
		if (!writeable(person)) {
			perror(person);
			return;
		}
		lock(person);
		(void) append(lettmp, person, -1, -1);
		unlock();
		return;
	}
	pwd = getpwnam(person);
	if (pwd) {
		(void) sprintf(mailboxname, "%s/%s", MAILDIR, person);
		lock(mailboxname);
		(void) append(lettmp, mailboxname, pwd->pw_uid, pwd->pw_gid);
		unlock();
		return;
	}
	fprintf(stderr, "Can't send to %s.\n", person);
	errs++;
	if (!isatty(0) || saved)
		return;
	saved++;
	if (!writeable("dead.letter")) {
		perror("dead.letter");
		return;
	}
	printf("Letter saved in 'dead.letter'\n");
	(void) append(lettmp, "dead.letter", getuid(), getgid());
}

#include <sys/socket.h>
#include <net/in.h>

struct sockaddr_in biffaddr = { AF_INET, IPPORT_BIFFUDP };

append(from, to, uid, gid)
	char *from, *to;
	int uid, gid;
{
	register FILE *fdin, *fdout;
	int ret;
	struct stat stb;
	char *cp, buf[100]; int f;

	if (stat(to, &stb) >= 0 && (stb.st_mode&S_IFMT) != S_IFREG) {
		fprintf(stderr, "Not a plain file: %s\n", to);
		goto fail;
	}
	fdout = fopen(to, "a");
	if (fdout == NULL) {
		perror(to);
		goto fail;
	}
	if (uid != -1) {
		(void) chown(to, uid, gid);
		(void) chmod(to, MAILMODE);
	}
	if ((fdin = fopen(from, "r")) == NULL) {
		perror(from);
		return (0);
	}
	cp = rindex(to, '/');
	if (cp) {
		char *host = "localhost";
		biffaddr.sin_addr.s_addr = rhost(&host);
#if vax || pdp11
		biffaddr.sin_port =
		    (biffaddr.sin_port<<8) | ((biffaddr.sin_port>>8) & 0xff);
#endif
		f = socket(SOCK_DGRAM, 0, 0, 0);
		(void) sprintf(buf, "%s@%d\n", cp+1, ftell(fdout)); 
	}
	ret = getput(fdin, fdout);
	(void) fclose(fdin);
	(void) fclose(fdout);
	if (cp && f >= 0) {
		send(f, &biffaddr, buf, strlen(buf)+1);
		(void) close(f);
	}
	return (ret);
fail:
	errs++;
	return (0);
}

delexit(status)
	int status;
{

	(void) unlink(lettmp);
	(void) unlink(preptmp);
	exit(status);
}

getput(fdin, fdout)
	register FILE *fdin, *fdout;
{
	register int c;

	while ((c = getc(fdin)) != EOF) {
		errno = 0;
		putc(c, fdout);
		if (errno) {
			perror("mail");
			return (0);
		}
	}
	return (1);
}

writeable(name)
	char *name;
{
	struct stat stb;
	char *cp;
	int ok;

	if (stat(name, &stb) < 0) {
		cp = rindex(name, '/');
		if (cp)
			*cp = 0;
		ok = access(cp ? "." : name, 2) == 0;
		if (cp)
			*cp = '/';
		return (ok);
	}
	return (access(name, 2) == 0);
}

char	locktmp[30];				/* Usable lock temporary */
char	curlock[50];				/* Last used name of lock */
int	locked;					/* To note that we locked it */

/*
 * Lock the specified mail file by setting the file mailfile.lock.
 * We must, of course, be careful to unlink the lock file by a call
 * to unlock before we stop.  The algorithm used here is to see if
 * the lock exists, and if it does, to check its modify time.  If it
 * is older than 30 seconds, we assume error and set our own file.
 * Otherwise, we wait for 5 seconds and try again.
 */
lock(file)
	char *file;
{
	register int f;
	struct stat statbuf;
	time_t curtime;

	if (locked)
		return;
	(void) sprintf(curlock, "%s%s", file, ".lock");
	(void) sprintf(locktmp, "%s/tmXXXXXX", MAILDIR);
	(void) mktemp(locktmp);
	(void) unlink(locktmp);
	for (;;) {
		f = lock1(locktmp, curlock);
		if (f == 0) {
			locked = 1;
			return;
		}
		if (stat(curlock, &statbuf) < 0)
			return;
		(void) time(&curtime);
		if (curtime < statbuf.st_mtime + 30) {
			sleep(5);
			continue;
		}
		(void) unlink(curlock);
	}
}

unlock()
{

	if (locked)
		(void) unlink(curlock);
	locked = 0;
}

/*
 * Attempt to set the lock by creating the temporary file,
 * then doing a link/unlink.  If it fails, return -1 else 0
 */
lock1(tempfile, name)
	char tempfile[], name[];
{
	int fno;

	fno = creat(tempfile, 0400);
	if (fno < 0)
		return (-1);
	(void) close(fno);
	if (link(tempfile, name) < 0) {
		(void) unlink(tempfile);
		return (-1);
	}
	(void) unlink(tempfile);
	return (0);
}

remove(sfn)
	char *sfn;
{
	int i;

	if (unlink(sfn) < 0) {
		i = creat(sfn, MAILMODE);
		if (i >= 0)
			(void) close(i);
	}
}
