/*
 * fancied up comsat program.....
 */

#ifndef lint
static char *rcsid_comsat_c = "$Header: comsat.c,v 10.3 86/02/01 15:18:47 tony Rel $";
#endif	lint

#define XWIND
#define SHORTSWEET

#ifndef lint
static	char sccsid[] = "@(#)comsat.c	4.12 (Berkeley) 8/1/84";
#endif

#ifndef XWIND
#include <sys/types.h>
#include <sys/socket.h>
#else
#include <X/Xlib.h>
#include <ttyent.h>
#include <pwd.h>
char *indexs();
extern char **environ;
#endif
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/file.h>

#include <netinet/in.h>

#include <stdio.h>
#include <sgtty.h>
#include <utmp.h>
#include <signal.h>
#include <errno.h>
#include <netdb.h>
#include <syslog.h>

char *index(), *rindex();

/*
 * comsat
 */
int	debug = 0;
#define	dprintf	if (debug) printf

#define MAXUTMP 100		/* down from init */

struct	sockaddr_in sin = { AF_INET };
extern	errno;

char	hostname[32];
struct	utmp utmp[100];
int	nutmp;
int	uf;
unsigned utmpmtime;			/* last modification time for utmp */
int	onalrm();
int	reapchildren();
long	lastmsgtime;

#define	MAXIDLE	120
#define NAMLEN (sizeof (uts[0].ut_name) + 1)

main(argc, argv)
	int argc;
	char *argv[];
{
	register int cc;
	char buf[BUFSIZ];
	char msgbuf[100];
	struct sockaddr_in from;
	int fromlen;

	/* verify proper invocation */
	fromlen = sizeof (from);
	if (getsockname(0, &from, &fromlen) < 0) {
		fprintf(stderr, "%s: ", argv[0]);
		perror("getsockname");
		_exit(1);
	}
	chdir("/usr/spool/mail");
	if ((uf = open("/etc/utmp",0)) < 0) {
		openlog("comsat", 0, 0);
		syslog(LOG_ERR, "/etc/utmp: %m");
		(void) recv(0, msgbuf, sizeof (msgbuf) - 1, 0);
		exit(1);
	}
	lastmsgtime = time(0);
	gethostname(hostname, sizeof (hostname));
	onalrm();
	signal(SIGALRM, onalrm);
	signal(SIGTTOU, SIG_IGN);
	signal(SIGCHLD, reapchildren);
	for (;;) {
		cc = recv(0, msgbuf, sizeof (msgbuf) - 1, 0);
		if (cc <= 0) {
			if (errno != EINTR)
				sleep(1);
			errno = 0;
			continue;
		}
		sigblock(1<<SIGALRM);
		msgbuf[cc] = 0;
		lastmsgtime = time(0);
		mailfor(msgbuf);
		sigsetmask(0);
	}
}

reapchildren()
{

	while (wait3((struct wait *)0, WNOHANG, (struct rusage *)0) > 0)
		;
}

onalrm()
{
	struct stat statbf;
	struct utmp *utp;

	if (time(0) - lastmsgtime >= MAXIDLE)
		exit(0);
	dprintf("alarm\n");
	alarm(15);
	fstat(uf, &statbf);
	if (statbf.st_mtime > utmpmtime) {
		dprintf(" changed\n");
		utmpmtime = statbf.st_mtime;
		lseek(uf, 0, 0);
		nutmp = read(uf,utmp,sizeof(utmp))/sizeof(struct utmp);
	} else
		dprintf(" ok\n");
}

mailfor(name)
	char *name;
{
	register struct utmp *utp = &utmp[nutmp];
	register char *cp;
	int offset;

	dprintf("mailfor %s\n", name);
	cp = name;
	while (*cp && *cp != '@')
		cp++;
	if (*cp == 0) {
		dprintf("bad format\n");
		return;
	}
	*cp = 0;
	offset = atoi(cp+1);
	while (--utp >= utmp)
		if (!strncmp(utp->ut_name, name, sizeof(utmp[0].ut_name)))
			notify(utp, offset);
}

char	*cr;

notify(utp, offset)
	register struct utmp *utp;
{
	int fd, flags, n, err, msglen;
	struct sgttyb gttybuf;
	char tty[sizeof (utmp[0].ut_line) + 6], msgbuf[BUFSIZ];
	char name[sizeof (utmp[0].ut_name) + 1];
	struct stat stb;
#ifdef XWIND
	struct ttyent *te;
	char *s;
	char dname[260];
#endif

	strcpy(tty, "/dev/");
	strncat(tty, utp->ut_line, sizeof(utp->ut_line));
	dprintf("notify %s on %s\n", utp->ut_name, tty);
	if (stat(tty, &stb) == 0 && (stb.st_mode & 0100) == 0) {
		dprintf("wrong mode\n");
		return;
	}
	strncpy(name, utp->ut_name, sizeof (utp->ut_name));
	name[sizeof (name) - 1] = '\0';
#ifdef XWIND
	te = getttynam(tty+5);
	if ((s = indexs(te->ty_getty, "xterm")) &&
	    (s = indexs(s, " -L ")) &&
	    (s = index(s, ':'))) {
		while (*(--s) != ' ') ;
		s++;
		if (*s == ':') {
			gethostname(dname, sizeof (dname));
			strcat(dname, s);
		} else
			strcpy(dname, s);
		Xprintf(msgbuf, name, offset);
		if (fork())
			return;
		Xnotify(name, dname, msgbuf);
	}
#endif
	if ((fd = open(tty, O_WRONLY|O_NDELAY)) < 0) {
		dprintf("%s: open failed\n", tty);
		return;
	}
	if ((flags = fcntl(fd, F_GETFL, 0)) == -1) {
		dprintf("fcntl(F_GETFL) failed %d\n", errno);
		return;
	}
	ioctl(fd, TIOCGETP, &gttybuf);
	cr = (gttybuf.sg_flags & CRMOD) ? "" : "\r";
#ifdef SHORTSWEET
	sprintf(msgbuf, "%s\n\007New mail from ", cr);
#else
	sprintf(msgbuf, "%s\n\007New mail for %s@%s\007 has arrived:%s\n",
	    cr, name, hostname, cr);
#endif
	jkfprintf(msgbuf+strlen(msgbuf), name, offset);
	if (fcntl(fd, F_SETFL, flags | FNDELAY) == -1)
		goto oldway;
	msglen = strlen(msgbuf);
	n = write(fd, msgbuf, msglen);
	err = errno;
	(void) fcntl(fd, F_SETFL, flags);
	(void) close(fd);
	if (n == msglen)
		return;
	if (err != EWOULDBLOCK) {
		dprintf("write failed %d\n", errno);
		return;
	}
oldway:
	if (fork()) {
		(void) close(fd);
		return;
	}
	signal(SIGALRM, SIG_DFL);
	alarm(30);
	(void) write(fd, msgbuf, msglen);
	exit(0);
}

#ifdef SHORTSWEET
jkfprintf(mp, name, offset)
	register char *mp;
{
	register FILE *fi;
	char line[BUFSIZ];

	dprintf("HERE %s's mail starting at %d\n", name, offset);

	if ((fi = fopen(name, "r")) == NULL) {
		dprintf("Cant read the mail\n");
		return;
	}
	fseek(fi, offset, L_SET);
	/* 
         * put the contents of the From: line into mp buffer
	 */
	while (fgets(line, sizeof (line), fi) != NULL) {
        	register char *cp;

                if (strncmp (line, "From:", 5) != 0)
		    continue;
		cp = index(line, '\n');
		if (cp)
			*cp = '\0';
                for (cp = line+5; (*cp == ' ' || *cp == '\t'); cp++);
		sprintf(mp, "%s%s\n", cp, cr);
                return;
	}
        /* didn't find the From: line */
        sprintf (mp, "???%s\n", cr);
}
#else
jkfprintf(mp, name, offset)
	register char *mp;
{
	register FILE *fi;
	register int linecnt, charcnt;
	char line[BUFSIZ];
	int inheader;

	dprintf("HERE %s's mail starting at %d\n", name, offset);

	if ((fi = fopen(name, "r")) == NULL) {
		dprintf("Cant read the mail\n");
		return;
	}
	fseek(fi, offset, L_SET);
	/* 
	 * Print the first 7 lines or 560 characters of the new mail
	 * (whichever comes first).  Skip header crap other than
	 * From, Subject, To, and Date.
	 */
	linecnt = 7;
	charcnt = 560;
	inheader = 1;
	while (fgets(line, sizeof (line), fi) != NULL) {
		register char *cp;
		int cnt;

		if (linecnt <= 0 || charcnt <= 0) {  
			sprintf(mp, "...more...%s\n", cr);
			mp += strlen(mp);
			return;
		}
		if (strncmp(line, "From ", 5) == 0)
			continue;
		if (inheader && (line[0] == ' ' || line[0] == '\t'))
			continue;
		cp = index(line, ':');
		if (cp == 0 || (index(line, ' ') && index(line, ' ') < cp))
			inheader = 0;
		else
			cnt = cp - line;
		if (inheader &&
		    strncmp(line, "Date", cnt) &&
		    strncmp(line, "From", cnt) &&
		    strncmp(line, "Subject", cnt) &&
		    strncmp(line, "To", cnt))
			continue;
		cp = index(line, '\n');
		if (cp)
			*cp = '\0';
		sprintf(mp, "%s%s\n", line, cr);
		mp += strlen(mp);
		linecnt--, charcnt -= strlen(line);
	}
	sprintf(mp, "----%s\n", cr);
	mp += strlen(mp);
}
#endif

#ifdef XWIND
char *indexs(s1, s2)
	register char *s1, *s2;
{
	register int z = strlen(s2);
	while (*s1 && strncmp(s1, s2, z))
		s1++;
	return(s1);
}

Xprintf(mp, name, offset)
	register char *mp;
{
	register FILE *fi;
	char line[BUFSIZ];

	dprintf("HERE %s's mail starting at %d\n", name, offset);

	sprintf(mp, "You have new mail.");
	if ((fi = fopen(name, "r")) == NULL) {
		dprintf("Cant read the mail\n");
		return;
	}
	fseek(fi, offset, L_SET);
	/* 
         * put the contents of the From: line into mp buffer
	 */
	while (fgets(line, sizeof (line), fi) != NULL) {
        	register char *cp;

                if (strncmp (line, "From:", 5) != 0)
		    continue;
		cp = index(line, '\n');
		if (cp)
			*cp = '\0';
                for (cp = line+5; (*cp == ' ' || *cp == '\t'); cp++);
		sprintf(mp, "Mail from %s", cp);
		break;
	}
	fclose(fi);
}

short cursor[] = {0x0000, 0x7ffe, 0x4fc2, 0x4ffe, 0x7ffe,
		  0x7ffe, 0x781e, 0x7ffe , 0x7ffe, 0x0000};

Xnotify (name, dname, notice)
	char *name;
	char *dname;
	char *notice;
{
	struct passwd *pwent;
	char *envbuf[2];
	char homebuf[280];
	Display *dpy;
	WindowInfo winfo;
	FontInfo finfo;
	Font font;
	int width, height;
	Window w;
	XEvent rep;
	int timeout = 0;
	int reverse = 0;
	int bwidth = 2;
	int inner = 2;
	int vertical = 2;
	int volume = 0;
	int forepix = BlackPixel;
	int backpix = WhitePixel;
	int brdrpix = BlackPixel;
	int mouspix = BlackPixel;
	char *option;
	char *font_name = "8x13";
	char *fore_color = NULL;
	char *back_color = NULL;
	char *brdr_color = NULL;
	char *mous_color = NULL;
	Color cdef;

	if (!XOpenDisplay(dname))
		exit(0);
	if (pwent = getpwnam(name)) {
		strcpy(homebuf, "HOME=");
		strcat(homebuf, pwent->pw_dir);
		envbuf[0] = homebuf;
		envbuf[1] = NULL;
		environ = envbuf;
		if (option = XGetDefault("biff", "BodyFont"))
			font_name = option;
		fore_color = XGetDefault("biff", "Foreground");
		back_color = XGetDefault("biff", "Background");
		brdr_color = XGetDefault("biff", "Border");
		mous_color = XGetDefault("biff", "Mouse");
		if (option = XGetDefault("biff", "BorderWidth"))
			bwidth = atoi(option);
		if (option = XGetDefault("biff", "InternalBorder"))
			inner = atoi(option);
		if (option = XGetDefault("biff", "Timeout"))
			timeout = atoi(option);
		if (option = XGetDefault("biff", "Volume"))
			volume = atoi(option);
		if (option = XGetDefault("biff", "Offset"))
			vertical = atoi(option);
		if ((option = XGetDefault("biff", "ReverseVideo")) &&
		    strcmp(option, "on") == 0)
			reverse = 1;
	}
	if (reverse) {
		brdrpix = backpix;
		backpix = forepix;
		forepix = brdrpix;
		mouspix = forepix;
	}

	if ((font = XGetFont(font_name)) == NULL)
		exit(0);
	if (DisplayCells() > 2) {
		if (back_color && XParseColor(back_color, &cdef) &&
		    XGetHardwareColor(&cdef))
			backpix = cdef.pixel;
		if (fore_color && XParseColor(fore_color, &cdef) &&
		    XGetHardwareColor(&cdef))
			forepix = cdef.pixel;
		if (brdr_color && XParseColor(brdr_color, &cdef) &&
		    XGetHardwareColor(&cdef))
			brdrpix = cdef.pixel;
		if (mous_color && XParseColor(mous_color, &cdef) &&
		    XGetHardwareColor(&cdef))
			mouspix = cdef.pixel;
	}
	XQueryFont(font, &finfo);
	XQueryWindow (RootWindow, &winfo);
	width = XQueryWidth (notice, font) + (inner << 1);
	height = finfo.height + (inner << 1);
	if (vertical < 0)
		vertical += winfo.height - height - (bwidth << 1);
	w = XCreateWindow(RootWindow, (winfo.width - width - (bwidth << 1)) / 2,
			  vertical, width, height, bwidth,
			  XMakeTile(brdrpix), XMakeTile(backpix));
	XStoreName(w, notice);
	XSelectInput(w, ButtonPressed|ButtonReleased|ExposeWindow);
	XDefineCursor(w, XCreateCursor(16, 10, cursor, NULL, 7, 5,
				       mouspix, backpix, GXcopy));
	XMapWindow(w);
	XFeep(volume);
	if (timeout > 0) {
		signal(SIGALRM, exit);
		alarm(timeout * 60);
	}
	if (inner) inner--;
	while (1) {
		XText(w, inner, inner, notice, strlen(notice),
			font, forepix, backpix);
		XNextEvent(&rep);
		if (rep.type == ButtonPressed)
			exit(0);
	}
}
#endif
