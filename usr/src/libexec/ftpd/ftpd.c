#ifndef lint
static char sccsid[] = "@(#)ftpd.c	4.13 (Berkeley) %G%";
#endif

/*
 * FTP server.
 */
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/socket.h>

#include <netinet/in.h>

#include <stdio.h>
#include <signal.h>
#include <wait.h>
#include <pwd.h>
#include <setjmp.h>
#include <netdb.h>
#include <errno.h>

#include "ftp.h"

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
int	logging = 1;
int	guest;
int	type;
int	form;
int	stru;			/* avoid C keyword */
int	mode;
int	usedefault = 1;		/* for data transfers */
char	hostname[32];
char	*remotehost;
struct	servent *sp;

int	lostconn();
int	reapchild();
FILE	*getdatasock(), *dataconn();
char	*ntoa();

main(argc, argv)
	int argc;
	char *argv[];
{
	int ctrl, s, options = 0;
	char *cp;

	sp = getservbyname("ftp", "tcp");
	if (sp == 0) {
		fprintf(stderr, "ftpd: fpt/tcp: unknown service\n");
		exit(1);
	}
	ctrl_addr.sin_port = sp->s_port;
	data_source.sin_port = htons(ntohs(sp->s_port) - 1);
	signal(SIGPIPE, lostconn);
	debug = 0;
	argc--, argv++;
	while (argc > 0 && *argv[0] == '-') {
		for (cp = &argv[0][1]; *cp; cp++) switch (*cp) {

		case 'd':
			debug = 1;
			options |= SO_DEBUG;
			break;

		default:
			fprintf(stderr, "Unknown flag -%c ignored.\n", cp);
			break;
		}
		argc--, argv++;
	}
#ifndef DEBUG
	if (fork())
		exit(0);
	for (s = 0; s < 10; s++)
		(void) close(s);
	(void) open("/dev/null", 0);
	(void) dup2(0, 1);
	{ int tt = open("/dev/tty", 2);
	  if (tt > 0) {
		ioctl(tt, TIOCNOTTY, 0);
		close(tt);
	  }
	}
#endif
	while ((s = socket(AF_INET, SOCK_STREAM, 0, 0)) < 0) {
		perror("ftpd: socket");
		sleep(5);
	}
	if (options & SO_DEBUG)
		if (setsockopt(s, SOL_SOCKET, SO_DEBUG, 0, 0) < 0)
			perror("ftpd: setsockopt (SO_DEBUG)");
#ifdef notdef
	if (setsockopt(s, SOL_SOCKET, SO_KEEPALIVE, 0, 0) < 0)
		perror("ftpd: setsockopt (SO_KEEPALIVE)");
#endif
	while (bind(s, &ctrl_addr, sizeof (ctrl_addr), 0) < 0) {
		perror("ftpd: bind");
		sleep(5);
	}
	sigset(SIGCHLD, reapchild);
	listen(s, 10);
	for (;;) {
		int hisaddrlen = sizeof (his_addr);

		ctrl = accept(s, &his_addr, &hisaddrlen, 0);
		if (ctrl < 0) {
			if (errno == EINTR)
				continue;
			perror("ftpd: accept");
			continue;
		}
		if (fork() == 0) {
			if (logging)
				dolog(&his_addr);
			close(s);
			dup2(ctrl, 0), close(ctrl), dup2(0, 1);
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
		close(ctrl);
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

	fatal("Connection closed.");
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
		if (strcmp(xpasswd, pw->pw_passwd) != 0) {
			reply(530, "Login incorrect.");
			pw = NULL;
			return;
		}
	}
	setegid(pw->pw_gid);
	initgroups(pw->pw_name, pw->pw_gid);
	if (chdir(pw->pw_dir)) {
		reply(550, "User %s: can't change directory to $s.",
			pw->pw_name, pw->pw_dir);
		goto bad;
	}
	if (guest && chroot(pw->pw_dir) < 0) {
		reply(550, "Can't set guest privileges.");
		goto bad;
	}
	if (!guest)
		reply(230, "User %s logged in.", pw->pw_name);
	else
		reply(230, "Guest login ok, access restrictions apply.");
	logged_in = 1;
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
	return(new);
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
		if (*name == '!')
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
	if (name[0] == '!')
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
	din = dataconn(name, -1, "r");
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
	int s;

	if (data >= 0)
		return (fdopen(data, mode));
	s = socket(AF_INET, SOCK_STREAM, 0, 0);
	if (s < 0)
		return (NULL);
	seteuid(0);
	if (setsockopt(s, SOL_SOCKET, SO_REUSEADDR, 0, 0) < 0)
		goto bad;
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
	int size;
	char *mode;
{
	char sizebuf[32];
	FILE *file;

	if (size >= 0)
		sprintf(sizebuf, " (%d bytes)", size);
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
		    ntoa(data_source.sin_addr),
		    ntohs(data_source.sin_port),
		    sys_errlist[errno]);
		return (NULL);
	}
	reply(150, "Opening data connection for %s (%s,%d)%s.",
	    name, ntoa(data_dest.sin_addr.s_addr),
	    ntohs(data_dest.sin_port), sizebuf);
	data = fileno(file);
	if (connect(data, &data_dest, sizeof (data_dest), 0) < 0) {
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
			if (c == '\n')
				putc('\r', outstr);
			if (putc(c, outstr) == EOF)
				return (1);
		}
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
	int cr, escape, eof;
	int netfd, filefd, cnt;
	char buf[BUFSIZ];


	switch (type) {

	case TYPE_I:
	case TYPE_L:
		netfd = fileno(instr);
		netfd = fileno(outstr);
		while ((cnt = read(netfd, buf, sizeof buf)) > 0)
			if (write(filefd, buf, cnt) < 0)
				return (1);
		return (cnt < 0);

	case TYPE_E:
		reply(504, "TYPE E not implemented.");
		return (1);

	case TYPE_A:
		cr = 0;
		while ((c = getc(instr)) != EOF) {
			if (cr) {
				if (c != '\r' && c != '\n')
					putc('\r', outstr);
				putc(c, outstr);
				cr = c == '\r';
				continue;
			}
			if (c == '\r') {
				cr = 1;
				continue;
			}
			putc(c, outstr);
		}
		if (cr)
			putc('\r', outstr);
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
	exit(0);
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
	char *p;

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

int guest;
/*
 * Test pathname for guest-user safety.
 */
inappropriate_request(name)
	char *name;
{
	int bogus = 0, depth = 0, length = strlen(name);
	char *p, *s;

	if (!guest)
		return (0);
	if (name[0] == '/' || name[0] == '|')
		bogus = 1;
	for (p = name; p < name+length;) {
		s = p;				/* start of token */
		while ( *p && *p!= '/')
			p++;
		*p = 0;
		if (strcmp(s, "..") == 0)
			depth -= 1;		/* backing up */
		else if (strcmp(s, ".") == 0)
			depth += 0;		/* no change */
		else
			depth += 1;		/* descending */
		if (depth < 0) {
			bogus = 1;
			break;
		}
	}
	if (bogus)
		reply(553, "%s: pathname disallowed guest users", name);
	return (bogus);
}

/*
 * Convert network-format internet address
 * to base 256 d.d.d.d representation.
 */
char *
ntoa(in)
	struct in_addr in;
{
	static char b[18];
	register char *p;

	in.s_addr = ntohl(in.s_addr);
	p = (char *)&in;
#define	UC(b)	(((int)b)&0xff)
	sprintf(b, "%d.%d.%d.%d", UC(p[0]), UC(p[1]), UC(p[2]), UC(p[3]));
	return (b);
}

dolog(sin)
	struct sockaddr_in *sin;
{
	struct hostent *hp = gethostbyaddr(&sin->sin_addr,
		sizeof (struct in_addr), AF_INET);
	char *remotehost;
	time_t t;

	if (hp)
		remotehost = hp->h_name;
	else
		remotehost = "UNKNOWNHOST";
	t = time(0);
	fprintf(stderr,"FTP: connection from %s at %s", remotehost, ctime(&t));
	fflush(stderr);
}
