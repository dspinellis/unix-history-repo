/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1985 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)ftpd.c	5.17 (Berkeley) %G%";
#endif /* not lint */

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
#include <arpa/telnet.h>

#include <stdio.h>
#include <signal.h>
#include <pwd.h>
#include <setjmp.h>
#include <netdb.h>
#include <errno.h>
#include <strings.h>
#include <syslog.h>

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
extern	FILE *popen(), *fopen(), *freopen();
extern	int  pclose(), fclose();
extern	char *getline();
extern	char cbuf[];

struct	sockaddr_in ctrl_addr;
struct	sockaddr_in data_source;
struct	sockaddr_in data_dest;
struct	sockaddr_in his_addr;

int	data;
jmp_buf	errcatch, urgcatch;
int	logged_in;
struct	passwd *pw;
int	debug;
int	timeout = 900;    /* timeout after 15 minutes of inactivity */
int	logging;
int	guest;
int	type;
int	form;
int	stru;			/* avoid C keyword */
int	mode;
int	usedefault = 1;		/* for data transfers */
int	pdata;			/* for passive mode */
int	unique;
int	transflag;
char	tmpline[7];
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
int	myoob();
FILE	*getdatasock(), *dataconn();

main(argc, argv)
	int argc;
	char *argv[];
{
	int addrlen, on = 1;
	long pgid;
	char *cp;

	addrlen = sizeof (his_addr);
	if (getpeername(0, &his_addr, &addrlen) < 0) {
		syslog(LOG_ERR, "getpeername (%s): %m",argv[0]);
		exit(1);
	}
	addrlen = sizeof (ctrl_addr);
	if (getsockname(0, (char *) &ctrl_addr, &addrlen) < 0) {
		syslog(LOG_ERR, "getsockname (%s): %m",argv[0]);
		exit(1);
	}
	data_source.sin_port = htons(ntohs(ctrl_addr.sin_port) - 1);
	debug = 0;
	openlog("ftpd", LOG_PID, LOG_DAEMON);
	argc--, argv++;
	while (argc > 0 && *argv[0] == '-') {
		for (cp = &argv[0][1]; *cp; cp++) switch (*cp) {

		case 'v':
			debug = 1;
			break;

		case 'd':
			debug = 1;
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
	(void) freopen("/dev/null", "w", stderr);
	(void) signal(SIGPIPE, lostconn);
	(void) signal(SIGCHLD, SIG_IGN);
	if ((int)signal(SIGURG, myoob) < 0)
		syslog(LOG_ERR, "signal: %m");

	/* handle urgent data inline */
#ifdef SO_OOBINLINE
	if (setsockopt(0, SOL_SOCKET, SO_OOBINLINE, (char *)&on, sizeof(on)) < 0) {
		syslog(LOG_ERR, "setsockopt: %m");
	}
#endif SO_OOBINLINE
	pgid = getpid();
	if (ioctl(fileno(stdin), SIOCSPGRP, (char *) &pgid) < 0) {
		syslog(LOG_ERR, "ioctl: %m");
	}
	dolog(&his_addr);
	/* do telnet option negotiation here */
	/*
	 * Set up default state
	 */
	data = -1;
	type = TYPE_A;
	form = FORM_N;
	stru = STRU_F;
	mode = MODE_S;
	tmpline[0] = '\0';
	(void) gethostname(hostname, sizeof (hostname));
	reply(220, "%s FTP server (%s) ready.",
		hostname, version);
	for (;;) {
		(void) setjmp(errcatch);
		(void) yyparse();
	}
}

lostconn()
{

	if (debug)
		syslog(LOG_DEBUG, "lost connection");
	dologout(-1);
}

static char ttyline[20];

/*
 * Helper function for sgetpwnam().
 */
char *
sgetsave(s)
	char *s;
{
#ifdef notdef
	char *new = strdup(s);
#else
	char *malloc();
	char *new = malloc((unsigned) strlen(s) + 1);
#endif
	
	if (new == NULL) {
		reply(553, "Local resource failure");
		dologout(1);
	}
#ifndef notdef
	(void) strcpy(new, s);
#endif
	return (new);
}

/*
 * Save the result of a getpwnam.  Used for USER command, since
 * the data returned must not be clobbered by any other command
 * (e.g., globbing).
 */
struct passwd *
sgetpwnam(name)
	char *name;
{
	static struct passwd save;
	register struct passwd *p;
	char *sgetsave();

	if ((p = getpwnam(name)) == NULL)
		return (p);
	if (save.pw_name) {
		free(save.pw_name);
		free(save.pw_passwd);
		free(save.pw_comment);
		free(save.pw_gecos);
		free(save.pw_dir);
		free(save.pw_shell);
	}
	save = *p;
	save.pw_name = sgetsave(p->pw_name);
	save.pw_passwd = sgetsave(p->pw_passwd);
	save.pw_comment = sgetsave(p->pw_comment);
	save.pw_gecos = sgetsave(p->pw_gecos);
	save.pw_dir = sgetsave(p->pw_dir);
	save.pw_shell = sgetsave(p->pw_shell);
	return (&save);
}

pass(passwd)
	char *passwd;
{
	char *xpasswd;

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
		reply(530, "User %s: can't change directory to %s.",
			pw->pw_name, pw->pw_dir);
		goto bad;
	}

	/* open wtmp before chroot */
	(void)sprintf(ttyline, "ftp%d", getpid());
	logwtmp(ttyline, pw->pw_name, remotehost);
	logged_in = 1;

	if (guest) {
		if (chroot(pw->pw_dir) < 0) {
			reply(550, "Can't set guest privileges.");
			goto bad;
		}
		reply(230, "Guest login ok, access restrictions apply.");
	} else
		reply(230, "User %s logged in.", pw->pw_name);
	seteuid(pw->pw_uid);
	home = pw->pw_dir;		/* home dir for globbing */
	return;
bad:
	seteuid(0);
	pw = NULL;
}

retrieve(cmd, name)
	char *cmd, *name;
{
	FILE *fin, *dout;
	struct stat st;
	int (*closefunc)(), tmp;

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

		(void) sprintf(line, cmd, name), name = line;
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
	if ((tmp = send_data(fin, dout)) > 0 || ferror(dout) > 0) {
		reply(550, "%s: %s.", name, sys_errlist[errno]);
	}
	else if (tmp == 0) {
		reply(226, "Transfer complete.");
	}
	(void) fclose(dout);
	data = -1;
	pdata = -1;
done:
	(*closefunc)(fin);
}

store(name, mode)
	char *name, *mode;
{
	FILE *fout, *din;
	int (*closefunc)(), dochown = 0, tmp;
	char *gunique(), *local;

#ifdef notdef
	/* no remote command execution -- it's a security hole */
	if (name[0] == '|')
		fout = popen(&name[1], "w"), closefunc = pclose;
	else
#endif
	{
		struct stat st;

		local = name;
		if (stat(name, &st) < 0) {
			dochown++;
		}
		else if (unique) {
			if ((local = gunique(name)) == NULL) {
				return;
			}
			dochown++;
		}
		fout = fopen(local, mode), closefunc = fclose;
	}
	if (fout == NULL) {
		reply(553, "%s: %s.", local, sys_errlist[errno]);
		return;
	}
	din = dataconn(local, (off_t)-1, "r");
	if (din == NULL)
		goto done;
	if ((tmp = receive_data(din, fout)) > 0 || ferror(fout) > 0) {
		reply(552, "%s: %s.", local, sys_errlist[errno]);
	}
	else if (tmp == 0 && !unique) {
		reply(226, "Transfer complete.");
	}
	else if (tmp == 0 && unique) {
		reply(226, "Transfer complete (unique file name:%s).", local);
	}
	(void) fclose(din);
	data = -1;
	pdata = -1;
done:
	if (dochown)
		(void) chown(local, pw->pw_uid, -1);
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
	if (setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *) &on, sizeof (on)) < 0)
		goto bad;
	/* anchor socket to avoid multi-homing problems */
	data_source.sin_family = AF_INET;
	data_source.sin_addr = ctrl_addr.sin_addr;
	if (bind(s, &data_source, sizeof (data_source)) < 0)
		goto bad;
	seteuid(pw->pw_uid);
	return (fdopen(s, mode));
bad:
	seteuid(pw->pw_uid);
	(void) close(s);
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
		(void) sprintf (sizebuf, " (%ld bytes)", size);
	else
		(void) strcpy(sizebuf, "");
	if (pdata > 0) {
		struct sockaddr_in from;
		int s, fromlen = sizeof(from);

		s = accept(pdata, &from, &fromlen);
		if (s < 0) {
			reply(425, "Can't open data connection.");
			(void) close(pdata);
			pdata = -1;
			return(NULL);
		}
		(void) close(pdata);
		pdata = s;
		reply(150, "Opening data connection for %s (%s mode)%s.",
		     name, type == TYPE_A ? "ascii" : "binary", sizebuf);
		return(fdopen(pdata, mode));
	}
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
	data = fileno(file);
	while (connect(data, &data_dest, sizeof (data_dest)) < 0) {
		if (errno == EADDRINUSE && retry < swaitmax) {
			sleep((unsigned) swaitint);
			retry += swaitint;
			continue;
		}
		reply(425, "Can't build data connection: %s.",
		    sys_errlist[errno]);
		(void) fclose(file);
		data = -1;
		return (NULL);
	}
	reply(150, "Opening data connection for %s (%s mode)%s.",
	    name, type == TYPE_A ? "ascii" : "binary", sizebuf);
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

	transflag++;
	if (setjmp(urgcatch)) {
		transflag = 0;
		return(-1);
	}
	switch (type) {

	case TYPE_A:
		while ((c = getc(instr)) != EOF) {
			if (c == '\n') {
				if (ferror (outstr)) {
					transflag = 0;
					return (1);
				}
				(void) putc('\r', outstr);
			}
			(void) putc(c, outstr);
		/*	if (c == '\r')			*/
		/*		putc ('\0', outstr);	*/
		}
		transflag = 0;
		if (ferror (instr) || ferror (outstr)) {
			return (1);
		}
		return (0);
		
	case TYPE_I:
	case TYPE_L:
		netfd = fileno(outstr);
		filefd = fileno(instr);

		while ((cnt = read(filefd, buf, sizeof (buf))) > 0) {
			if (write(netfd, buf, cnt) < 0) {
				transflag = 0;
				return (1);
			}
		}
		transflag = 0;
		return (cnt < 0);
	}
	reply(550, "Unimplemented TYPE %d in send_data", type);
	transflag = 0;
	return (-1);
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


	transflag++;
	if (setjmp(urgcatch)) {
		transflag = 0;
		return(-1);
	}
	switch (type) {

	case TYPE_I:
	case TYPE_L:
		while ((cnt = read(fileno(instr), buf, sizeof buf)) > 0) {
			if (write(fileno(outstr), buf, cnt) < 0) {
				transflag = 0;
				return (1);
			}
		}
		transflag = 0;
		return (cnt < 0);

	case TYPE_E:
		reply(553, "TYPE E not implemented.");
		transflag = 0;
		return (-1);

	case TYPE_A:
		while ((c = getc(instr)) != EOF) {
			while (c == '\r') {
				if (ferror (outstr)) {
					transflag = 0;
					return (1);
				}
				if ((c = getc(instr)) != '\n')
					(void) putc ('\r', outstr);
			/*	if (c == '\0')			*/
			/*		continue;		*/
			}
			(void) putc (c, outstr);
		}
		transflag = 0;
		if (ferror (instr) || ferror (outstr))
			return (1);
		return (0);
	}
	transflag = 0;
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

reply(n, s, p0, p1, p2, p3, p4)
	int n;
	char *s;
{

	printf("%d ", n);
	printf(s, p0, p1, p2, p3, p4);
	printf("\r\n");
	(void) fflush(stdout);
	if (debug) {
		syslog(LOG_DEBUG, "<--- %d ", n);
		syslog(LOG_DEBUG, s, p0, p1, p2, p3, p4);
	}
}

lreply(n, s, p0, p1, p2, p3, p4)
	int n;
	char *s;
{
	printf("%d-", n);
	printf(s, p0, p1, p2, p3, p4);
	printf("\r\n");
	(void) fflush(stdout);
	if (debug) {
		syslog(LOG_DEBUG, "<--- %d- ", n);
		syslog(LOG_DEBUG, s, p0, p1, p2, p3, p4);
	}
}

ack(s)
	char *s;
{
	reply(250, "%s command successful.", s);
}

nack(s)
	char *s;
{
	reply(502, "%s command not implemented.", s);
}

yyerror(s)
	char *s;
{
	char *cp;

	cp = index(cbuf,'\n');
	*cp = '\0';
	reply(500, "'%s': command not understood.",cbuf);
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
	reply(257, "MKD command successful.");
}

removedir(name)
	char *name;
{

	if (rmdir(name) < 0) {
		reply(550, "%s: %s.", name, sys_errlist[errno]);
		return;
	}
	ack("RMD");
}

pwd()
{
	char path[MAXPATHLEN + 1];

	if (getwd(path) == NULL) {
		reply(550, "%s.", path);
		return;
	}
	reply(257, "\"%s\" is current directory.", path);
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
	extern char *ctime();

	if (hp) {
		(void) strncpy(remotehost, hp->h_name, sizeof (remotehost));
		endhostent();
	} else
		(void) strncpy(remotehost, inet_ntoa(sin->sin_addr),
		    sizeof (remotehost));
	if (!logging)
		return;
	t = time((time_t *) 0);
	syslog(LOG_INFO,"FTPD: connection from %s at %s", remotehost, ctime(&t));
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
		logwtmp(ttyline, "", "");
	}
	/* beware of flushing buffers after a SIGPIPE */
	_exit(status);
}

/*
 * Check user requesting login priviledges.
 * Disallow anyone who does not have a standard
 * shell returned by getusershell() (/etc/shells).
 * Disallow anyone mentioned in the file FTPUSERS
 * to allow people such as uucp to be avoided.
 */
checkuser(name)
	register char *name;
{
	register char *cp;
	FILE *fd;
	struct passwd *p;
	char *shell;
	int found = 0;
	char line[BUFSIZ], *index(), *getusershell();

	if ((p = getpwnam(name)) == NULL)
		return (0);
	if ((shell = p->pw_shell) == NULL || *shell == 0)
		shell = "/bin/sh";
	while ((cp = getusershell()) != NULL)
		if (strcmp(cp, shell) == 0)
			break;
	endusershell();
	if (cp == NULL)
		return (0);
	if ((fd = fopen(FTPUSERS, "r")) == NULL)
		return (1);
	while (fgets(line, sizeof (line), fd) != NULL) {
		if ((cp = index(line, '\n')) != NULL)
			*cp = '\0';
		if (strcmp(line, name) == 0) {
			found++;
			break;
		}
	}
	(void) fclose(fd);
	return (!found);
}

myoob()
{
	char *cp;

	/* only process if transfer occurring */
	if (!transflag) {
		return;
	}
	cp = tmpline;
	if (getline(cp, 7, stdin) == NULL) {
		reply(221, "You could at least say goodby.");
		dologout(0);
	}
	upper(cp);
	if (strcmp(cp, "ABOR\r\n"))
		return;
	tmpline[0] = '\0';
	reply(426,"Transfer aborted. Data connection closed.");
	reply(226,"Abort successful");
	longjmp(urgcatch, 1);
}

/*
 * Note: The 530 reply codes could be 4xx codes, except nothing is
 * given in the state tables except 421 which implies an exit.  (RFC959)
 */
passive()
{
	int len;
	struct sockaddr_in tmp;
	register char *p, *a;

	pdata = socket(AF_INET, SOCK_STREAM, 0);
	if (pdata < 0) {
		reply(530, "Can't open passive connection");
		return;
	}
	tmp = ctrl_addr;
	tmp.sin_port = 0;
	seteuid(0);
	if (bind(pdata, (struct sockaddr *) &tmp, sizeof(tmp)) < 0) {
		seteuid(pw->pw_uid);
		(void) close(pdata);
		pdata = -1;
		reply(530, "Can't open passive connection");
		return;
	}
	seteuid(pw->pw_uid);
	len = sizeof(tmp);
	if (getsockname(pdata, (char *) &tmp, &len) < 0) {
		(void) close(pdata);
		pdata = -1;
		reply(530, "Can't open passive connection");
		return;
	}
	if (listen(pdata, 1) < 0) {
		(void) close(pdata);
		pdata = -1;
		reply(530, "Can't open passive connection");
		return;
	}
	a = (char *) &tmp.sin_addr;
	p = (char *) &tmp.sin_port;

#define UC(b) (((int) b) & 0xff)

	reply(227, "Entering Passive Mode (%d,%d,%d,%d,%d,%d)", UC(a[0]),
		UC(a[1]), UC(a[2]), UC(a[3]), UC(p[0]), UC(p[1]));
}

char *
gunique(local)
	char *local;
{
	static char new[MAXPATHLEN];
	char *cp = rindex(local, '/');
	int d, count=0;
	char ext = '1';

	if (cp) {
		*cp = '\0';
	}
	d = access(cp ? local : ".", 2);
	if (cp) {
		*cp = '/';
	}
	if (d < 0) {
		syslog(LOG_ERR, "%s: %m", local);
		return((char *) 0);
	}
	(void) strcpy(new, local);
	cp = new + strlen(new);
	*cp++ = '.';
	while (!d) {
		if (++count == 100) {
			reply(452, "Unique file name not cannot be created.");
			return((char *) 0);
		}
		*cp++ = ext;
		*cp = '\0';
		if (ext == '9') {
			ext = '0';
		}
		else {
			ext++;
		}
		if ((d = access(new, 0)) < 0) {
			break;
		}
		if (ext != '0') {
			cp--;
		}
		else if (*(cp - 2) == '.') {
			*(cp - 1) = '1';
		}
		else {
			*(cp - 2) = *(cp - 2) + 1;
			cp--;
		}
	}
	return(new);
}
