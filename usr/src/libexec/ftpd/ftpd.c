/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)ftpd.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * FTP server.
 */
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <sys/wait.h>

#include <netinet/in.h>

#include <arpa/ftp.h>
#include <arpa/inet.h>

#include <stdio.h>
#include <signal.h>
#include <pwd.h>
#include <setjmp.h>
#include <netdb.h>
#include <errno.h>

/*
 * File containing login names
 * NOT to be used on this machine.
 * Commonly used to disallow uucp.
 */
#define	FTPUSERS	"/etc/ftpusers"

extern	int errno;
extern	char *sys_errlist[];
extern	char *crypt();
extern	char version[];
extern	char *home;		/* pointer to home directory for glob */
extern	FILE *popen(), *fopen();
extern	int pclose(), fclose();

struct	sockaddr_in ctrl_addr;
struct	sockaddr_in data_source;
struct	sockaddr_in data_dest;
struct	sockaddr_in his_addr;

struct	hostent *hp;

int	data;
jmp_buf	errcatch;
int	logged_in;
struct	passwd *pw;
int	debug;
int	timeout;
int	logging;
int	guest;
int	wtmp;
int	type;
int	form;
int	stru;			/* avoid C keyword */
int	mode;
int	usedefault = 1;		/* for data transfers */
char	hostname[32];
char	remotehost[32];

/*
 * Timeout intervals for retrying connections
 * to hosts that don't accept PORT cmds.  This
 * is a kludge, but given the problems with TCP...
 */
#define	SWAITMAX	90	/* wait at most 90 seconds */
#define	SWAITINT	5	/* interval between retries */

int	swaitmax = SWAITMAX;
int	swaitint = SWAITINT;

int	lostconn();
int	reapchild();
FILE	*getdatasock(), *dataconn();

main(argc, argv)
	int argc;
	char *argv[];
{
	int options = 0, addrlen;
	char *cp;

	addrlen = sizeof (his_addr);
	if (getpeername(0, &his_addr, &addrlen) < 0) {
		fprintf(stderr, "%s: ", argv[0]);
		perror("getpeername");
		exit(1);
	}
	addrlen = sizeof (ctrl_addr);
	if (getsockname(0, &ctrl_addr, &addrlen) < 0) {
		fprintf(stderr, "%s: ", argv[0]);
		perror("getsockname");
		exit(1);
	}
	data_source.sin_port = htons(ntohs(ctrl_addr.sin_port) - 1);
	debug = 0;
	argc--, argv++;
	while (argc > 0 && *argv[0] == '-') {
		for (cp = &argv[0][1]; *cp; cp++) switch (*cp) {

		case 'v':
			debug = 1;
			break;

		case 'd':
			debug = 1;
			options |= SO_DEBUG;
			break;

		case 'l':
			logging = 1;
			break;

		case 't':
			timeout = atoi(++cp);
			goto nextopt;
			break;

		default:
			fprintf(stderr, "ftpd: Unknown flag -%c ignored.\n",
			     *cp);
			break;
		}
nextopt:
		argc--, argv++;
	}
	signal(SIGPIPE, lostconn);
	signal(SIGCHLD, SIG_IGN);
	dolog(&his_addr);
	/* do telnet option negotiation here */
	/*
	 * Set up default state
	 */
	logged_in = 0;
	data = -1;
	type = TYPE_A;
	form = FORM_N;
	stru = STRU_F;
	mode = MODE_S;
	gethostname(hostname, sizeof (hostname));
	reply(220, "%s FTP server (%s) ready.",
		hostname, version);
	for (;;) {
		setjmp(errcatch);
		yyparse();
	}
}

reapchild()
{
	union wait status;

	while (wait3(&status, WNOHANG, 0) > 0)
		;
}

lostconn()
{

	if (debug)
		fprintf(stderr, "Lost connection.\n");
	dologout(-1);
}

pass(passwd)
	char *passwd;
{
	char *xpasswd, *savestr();
	static struct passwd save;

	if (logged_in || pw == NULL) {
		reply(503, "Login with USER first.");
		return;
	}
	if (!guest) {		/* "ftp" is only account allowed no password */
		xpasswd = crypt(passwd, pw->pw_passwd);
		/* The strcmp does not catch null passwords! */
		if (*pw->pw_passwd == '\0' || strcmp(xpasswd, pw->pw_passwd)) {
			reply(530, "Login incorrect.");
			pw = NULL;
			return;
		}
	}
	setegid(pw->pw_gid);
	initgroups(pw->pw_name, pw->pw_gid);
	if (chdir(pw->pw_dir)) {
		reply(550, "User %s: can't change directory to %s.",
			pw->pw_name, pw->pw_dir);
		goto bad;
	}

	/* grab wtmp before chroot */
	wtmp = open("/usr/adm/wtmp", O_WRONLY|O_APPEND);
	if (guest && chroot(pw->pw_dir) < 0) {
		reply(550, "Can't set guest privileges.");
		if (wtmp >= 0) {
			(void) close(wtmp);
			wtmp = -1;
		}
		goto bad;
	}
	if (!guest)
		reply(230, "User %s logged in.", pw->pw_name);
	else
		reply(230, "Guest login ok, access restrictions apply.");
	logged_in = 1;
	dologin(pw);
	seteuid(pw->pw_uid);
	/*
	 * Save everything so globbing doesn't
	 * clobber the fields.
	 */
	save = *pw;
	save.pw_name = savestr(pw->pw_name);
	save.pw_passwd = savestr(pw->pw_passwd);
	save.pw_comment = savestr(pw->pw_comment);
	save.pw_gecos = savestr(pw->pw_gecos, &save.pw_gecos);
	save.pw_dir = savestr(pw->pw_dir);
	save.pw_shell = savestr(pw->pw_shell);
	pw = &save;
	home = pw->pw_dir;		/* home dir for globbing */
	return;
bad:
	seteuid(0);
	pw = NULL;
}

char *
savestr(s)
	char *s;
{
	char *malloc();
	char *new = malloc(strlen(s) + 1);
	
	if (new != NULL)
		strcpy(new, s);
	return (new);
}

retrieve(cmd, name)
	char *cmd, *name;
{
	FILE *fin, *dout;
	struct stat st;
	int (*closefunc)();

	if (cmd == 0) {
#ifdef notdef
		/* no remote command execution -- it's a security hole */
		if (*name == '|')
			fin = popen(name + 1, "r"), closefunc = pclose;
		else
#endif
			fin = fopen(name, "r"), closefunc = fclose;
	} else {
		char line[BUFSIZ];

		sprintf(line, cmd, name), name = line;
		fin = popen(line, "r"), closefunc = pclose;
	}
	if (fin == NULL) {
		if (errno != 0)
			reply(550, "%s: %s.", name, sys_errlist[errno]);
		return;
	}
	st.st_size = 0;
	if (cmd == 0 &&
	    (stat(name, &st) < 0 || (st.st_mode&S_IFMT) != S_IFREG)) {
		reply(550, "%s: not a plain file.", name);
		goto done;
	}
	dout = dataconn(name, st.st_size, "w");
	if (dout == NULL)
		goto done;
	if (send_data(fin, dout) || ferror(dout))
		reply(550, "%s: %s.", name, sys_errlist[errno]);
	else
		reply(226, "Transfer complete.");
	fclose(dout), data = -1;
done:
	(*closefunc)(fin);
}

store(name, mode)
	char *name, *mode;
{
	FILE *fout, *din;
	int (*closefunc)(), dochown = 0;

#ifdef notdef
	/* no remote command execution -- it's a security hole */
	if (name[0] == '|')
		fout = popen(&name[1], "w"), closefunc = pclose;
	else
#endif
	{
		struct stat st;

		if (stat(name, &st) < 0)
			dochown++;
		fout = fopen(name, mode), closefunc = fclose;
	}
	if (fout == NULL) {
		reply(550, "%s: %s.", name, sys_errlist[errno]);
		return;
	}
	din = dataconn(name, (off_t)-1, "r");
	if (din == NULL)
		goto done;
	if (receive_data(din, fout) || ferror(fout))
		reply(550, "%s: %s.", name, sys_errlist[errno]);
	else
		reply(226, "Transfer complete.");
	fclose(din), data = -1;
done:
	if (dochown)
		(void) chown(name, pw->pw_uid, -1);
	(*closefunc)(fout);
}

FILE *
getdatasock(mode)
	char *mode;
{
	int s, on = 1;

	if (data >= 0)
		return (fdopen(data, mode));
	s = socket(AF_INET, SOCK_STREAM, 0);
	if (s < 0)
		return (NULL);
	seteuid(0);
	if (setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &on, sizeof (on)) < 0)
		goto bad;
	/* anchor socket to avoid multi-homing problems */
	data_source.sin_family = AF_INET;
	data_source.sin_addr = ctrl_addr.sin_addr;
	if (bind(s, &data_source, sizeof (data_source), 0) < 0)
		goto bad;
	seteuid(pw->pw_uid);
	return (fdopen(s, mode));
bad:
	seteuid(pw->pw_uid);
	close(s);
	return (NULL);
}

FILE *
dataconn(name, size, mode)
	char *name;
	off_t size;
	char *mode;
{
	char sizebuf[32];
	FILE *file;
	int retry = 0;

	if (size >= 0)
		sprintf (sizebuf, " (%ld bytes)", size);
	else
		(void) strcpy(sizebuf, "");
	if (data >= 0) {
		reply(125, "Using existing data connection for %s%s.",
		    name, sizebuf);
		usedefault = 1;
		return (fdopen(data, mode));
	}
	if (usedefault)
		data_dest = his_addr;
	usedefault = 1;
	file = getdatasock(mode);
	if (file == NULL) {
		reply(425, "Can't create data socket (%s,%d): %s.",
		    inet_ntoa(data_source.sin_addr),
		    ntohs(data_source.sin_port),
		    sys_errlist[errno]);
		return (NULL);
	}
	reply(150, "Opening data connection for %s (%s,%d)%s.",
	    name, inet_ntoa(data_dest.sin_addr.s_addr),
	    ntohs(data_dest.sin_port), sizebuf);
	data = fileno(file);
	while (connect(data, &data_dest, sizeof (data_dest), 0) < 0) {
		if (errno == EADDRINUSE && retry < swaitmax) {
			sleep(swaitint);
			retry += swaitint;
			continue;
		}
		reply(425, "Can't build data connection: %s.",
		    sys_errlist[errno]);
		(void) fclose(file);
		data = -1;
		return (NULL);
	}
	return (file);
}

/*
 * Tranfer the contents of "instr" to
 * "outstr" peer using the appropriate
 * encapulation of the date subject
 * to Mode, Structure, and Type.
 *
 * NB: Form isn't handled.
 */
send_data(instr, outstr)
	FILE *instr, *outstr;
{
	register int c;
	int netfd, filefd, cnt;
	char buf[BUFSIZ];

	switch (type) {

	case TYPE_A:
		while ((c = getc(instr)) != EOF) {
			if (c == '\n') {
				if (ferror (outstr))
					return (1);
				putc('\r', outstr);
			}
			putc(c, outstr);
			if (c == '\r')
				putc ('\0', outstr);
		}
		if (ferror (instr) || ferror (outstr))
			return (1);
		return (0);
		
	case TYPE_I:
	case TYPE_L:
		netfd = fileno(outstr);
		filefd = fileno(instr);

		while ((cnt = read(filefd, buf, sizeof (buf))) > 0)
			if (write(netfd, buf, cnt) < 0)
				return (1);
		return (cnt < 0);
	}
	reply(504,"Unimplemented TYPE %d in send_data", type);
	return (1);
}

/*
 * Transfer data from peer to
 * "outstr" using the appropriate
 * encapulation of the data subject
 * to Mode, Structure, and Type.
 *
 * N.B.: Form isn't handled.
 */
receive_data(instr, outstr)
	FILE *instr, *outstr;
{
	register int c;
	int cnt;
	char buf[BUFSIZ];


	switch (type) {

	case TYPE_I:
	case TYPE_L:
		while ((cnt = read(fileno(instr), buf, sizeof buf)) > 0)
			if (write(fileno(outstr), buf, cnt) < 0)
				return (1);
		return (cnt < 0);

	case TYPE_E:
		reply(504, "TYPE E not implemented.");
		return (1);

	case TYPE_A:
		while ((c = getc(instr)) != EOF) {
			if (c == '\r') {
				if (ferror (outstr))
					return (1);
				if ((c = getc(instr)) != '\n')
					putc ('\r', outstr);
				if (c == '\0')
					continue;
			}
			putc (c, outstr);
		}
		if (ferror (instr) || ferror (outstr))
			return (1);
		return (0);
	}
	fatal("Unknown type in receive_data.");
	/*NOTREACHED*/
}

fatal(s)
	char *s;
{
	reply(451, "Error in server: %s\n", s);
	reply(221, "Closing connection due to server error.");
	dologout(0);
}

reply(n, s, args)
	int n;
	char *s;
{

	printf("%d ", n);
	_doprnt(s, &args, stdout);
	printf("\r\n");
	fflush(stdout);
	if (debug) {
		fprintf(stderr, "<--- %d ", n);
		_doprnt(s, &args, stderr);
		fprintf(stderr, "\n");
		fflush(stderr);
	}
}

lreply(n, s, args)
	int n;
	char *s;
{
	printf("%d-", n);
	_doprnt(s, &args, stdout);
	printf("\r\n");
	fflush(stdout);
	if (debug) {
		fprintf(stderr, "<--- %d-", n);
		_doprnt(s, &args, stderr);
		fprintf(stderr, "\n");
	}
}

replystr(s)
	char *s;
{
	printf("%s\r\n", s);
	fflush(stdout);
	if (debug)
		fprintf(stderr, "<--- %s\n", s);
}

ack(s)
	char *s;
{
	reply(200, "%s command okay.", s);
}

nack(s)
	char *s;
{
	reply(502, "%s command not implemented.", s);
}

yyerror()
{
	reply(500, "Command not understood.");
}

delete(name)
	char *name;
{
	struct stat st;

	if (stat(name, &st) < 0) {
		reply(550, "%s: %s.", name, sys_errlist[errno]);
		return;
	}
	if ((st.st_mode&S_IFMT) == S_IFDIR) {
		if (rmdir(name) < 0) {
			reply(550, "%s: %s.", name, sys_errlist[errno]);
			return;
		}
		goto done;
	}
	if (unlink(name) < 0) {
		reply(550, "%s: %s.", name, sys_errlist[errno]);
		return;
	}
done:
	ack("DELE");
}

cwd(path)
	char *path;
{

	if (chdir(path) < 0) {
		reply(550, "%s: %s.", path, sys_errlist[errno]);
		return;
	}
	ack("CWD");
}

makedir(name)
	char *name;
{
	struct stat st;
	int dochown = stat(name, &st) < 0;
	
	if (mkdir(name, 0777) < 0) {
		reply(550, "%s: %s.", name, sys_errlist[errno]);
		return;
	}
	if (dochown)
		(void) chown(name, pw->pw_uid, -1);
	ack("MKDIR");
}

removedir(name)
	char *name;
{

	if (rmdir(name) < 0) {
		reply(550, "%s: %s.", name, sys_errlist[errno]);
		return;
	}
	ack("RMDIR");
}

pwd()
{
	char path[MAXPATHLEN + 1];

	if (getwd(path) == NULL) {
		reply(451, "%s.", path);
		return;
	}
	reply(251, "\"%s\" is current directory.", path);
}

char *
renamefrom(name)
	char *name;
{
	struct stat st;

	if (stat(name, &st) < 0) {
		reply(550, "%s: %s.", name, sys_errlist[errno]);
		return ((char *)0);
	}
	reply(350, "File exists, ready for destination name");
	return (name);
}

renamecmd(from, to)
	char *from, *to;
{

	if (rename(from, to) < 0) {
		reply(550, "rename: %s.", sys_errlist[errno]);
		return;
	}
	ack("RNTO");
}

dolog(sin)
	struct sockaddr_in *sin;
{
	struct hostent *hp = gethostbyaddr(&sin->sin_addr,
		sizeof (struct in_addr), AF_INET);
	time_t t;

	if (hp) {
		strncpy(remotehost, hp->h_name, sizeof (remotehost));
		endhostent();
	} else
		strncpy(remotehost, inet_ntoa(sin->sin_addr),
		    sizeof (remotehost));
	if (!logging)
		return;
	t = time(0);
	fprintf(stderr,"FTPD: connection from %s at %s", remotehost, ctime(&t));
	fflush(stderr);
}

#include <utmp.h>

#define	SCPYN(a, b)	strncpy(a, b, sizeof (a))
struct	utmp utmp;

/*
 * Record login in wtmp file.
 */
dologin(pw)
	struct passwd *pw;
{
	char line[32];

	if (wtmp >= 0) {
		/* hack, but must be unique and no tty line */
		sprintf(line, "ftp%d", getpid());
		SCPYN(utmp.ut_line, line);
		SCPYN(utmp.ut_name, pw->pw_name);
		SCPYN(utmp.ut_host, remotehost);
		utmp.ut_time = time(0);
		(void) write(wtmp, (char *)&utmp, sizeof (utmp));
		if (!guest) {		/* anon must hang on */
			(void) close(wtmp);
			wtmp = -1;
		}
	}
}

/*
 * Record logout in wtmp file
 * and exit with supplied status.
 */
dologout(status)
	int status;
{

	if (logged_in) {
		(void) seteuid(0);
		if (wtmp < 0)
			wtmp = open("/usr/adm/wtmp", O_WRONLY|O_APPEND);
		if (wtmp >= 0) {
			SCPYN(utmp.ut_name, "");
			SCPYN(utmp.ut_host, "");
			utmp.ut_time = time(0);
			(void) write(wtmp, (char *)&utmp, sizeof (utmp));
			(void) close(wtmp);
		}
	}
	/* beware of flushing buffers after a SIGPIPE */
	_exit(status);
}

/*
 * Special version of popen which avoids
 * call to shell.  This insures noone may 
 * create a pipe to a hidden program as a side
 * effect of a list or dir command.
 */
#define	tst(a,b)	(*mode == 'r'? (b) : (a))
#define	RDR	0
#define	WTR	1
static	int popen_pid[5];

static char *
nextarg(cpp)
	char *cpp;
{
	register char *cp = cpp;

	if (cp == 0)
		return (cp);
	while (*cp && *cp != ' ' && *cp != '\t')
		cp++;
	if (*cp == ' ' || *cp == '\t') {
		*cp++ = '\0';
		while (*cp == ' ' || *cp == '\t')
			cp++;
	}
	if (cp == cpp)
		return ((char *)0);
	return (cp);
}

FILE *
popen(cmd, mode)
	char *cmd, *mode;
{
	int p[2], ac, gac;
	register myside, hisside, pid;
	char *av[20], *gav[512];
	register char *cp;

	if (pipe(p) < 0)
		return (NULL);
	cp = cmd, ac = 0;
	/* break up string into pieces */
	do {
		av[ac++] = cp;
		cp = nextarg(cp);
	} while (cp && *cp && ac < 20);
	av[ac] = (char *)0;
	gav[0] = av[0];
	/* glob each piece */
	for (gac = ac = 1; av[ac] != NULL; ac++) {
		char **pop;
		extern char **glob(), **copyblk();

		pop = glob(av[ac]);
		if (pop == (char **)NULL) {	/* globbing failed */
			char *vv[2];

			vv[0] = av[ac];
			vv[1] = 0;
			pop = copyblk(vv);
		}
		av[ac] = (char *)pop;		/* save to free later */
		while (*pop && gac < 512)
			gav[gac++] = *pop++;
	}
	gav[gac] = (char *)0;
	myside = tst(p[WTR], p[RDR]);
	hisside = tst(p[RDR], p[WTR]);
	if ((pid = fork()) == 0) {
		/* myside and hisside reverse roles in child */
		close(myside);
		dup2(hisside, tst(0, 1));
		close(hisside);
		execv(gav[0], gav);
		_exit(1);
	}
	for (ac = 1; av[ac] != NULL; ac++)
		blkfree((char **)av[ac]);
	if (pid == -1)
		return (NULL);
	popen_pid[myside] = pid;
	close(hisside);
	return (fdopen(myside, mode));
}

pclose(ptr)
	FILE *ptr;
{
	register f, r, (*hstat)(), (*istat)(), (*qstat)();
	int status;

	f = fileno(ptr);
	fclose(ptr);
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);
	hstat = signal(SIGHUP, SIG_IGN);
	while ((r = wait(&status)) != popen_pid[f] && r != -1)
		;
	if (r == -1)
		status = -1;
	signal(SIGINT, istat);
	signal(SIGQUIT, qstat);
	signal(SIGHUP, hstat);
	return (status);
}

/*
 * Check user requesting login priviledges.
 * Disallow anyone mentioned in the file FTPUSERS
 * to allow people such as uucp to be avoided.
 */
checkuser(name)
	register char *name;
{
	char line[BUFSIZ], *index();
	FILE *fd;
	int found = 0;

	fd = fopen(FTPUSERS, "r");
	if (fd == NULL)
		return (1);
	while (fgets(line, sizeof (line), fd) != NULL) {
		register char *cp = index(line, '\n');

		if (cp)
			*cp = '\0';
		if (strcmp(line, name) == 0) {
			found++;
			break;
		}
	}
	fclose(fd);
	return (!found);
}
