#ifndef lint
static	char *sccsid = "@(#)comsat.c	4.9 83/07/04";
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include <netinet/in.h>

#include <stdio.h>
#include <sgtty.h>
#include <utmp.h>
#include <signal.h>
#include <errno.h>
#include <netdb.h>

/*
 * comsat
 */
int	debug = 0;
#define	dprintf	if (debug) printf

#define MAXUTMP 100		/* down from init */

struct	sockaddr_in sin = { AF_INET };
extern	errno;

struct	utmp utmp[100];
int	nutmp;
int	uf;
unsigned utmpmtime;			/* last modification time for utmp */
int	onalrm();
struct	servent *sp;

#define NAMLEN (sizeof (uts[0].ut_name) + 1)

main(argc, argv)
char **argv;
{
	register cc;
	char buf[BUFSIZ];
	int s;

	sp = getservbyname("biff", "udp");
	if (sp == 0) {
		fprintf(stderr, "comsat: biff/udp: unknown service\n");
		exit(1);
	}
	if (!debug)
	if (fork())
		exit();
	chdir("/usr/spool/mail");
	if((uf = open("/etc/utmp",0)) < 0)
		perror("/etc/utmp"), exit(1);
	sleep(10);
	onalrm();
	signal(SIGALRM, onalrm);
	signal(SIGTTOU, SIG_IGN);
	s = socket(AF_INET, SOCK_DGRAM, 0, 0);
	if (s < 0) {
		perror("socket");
		exit(1);
	}
	sin.sin_port = sp->s_port;
	if (bind(s, &sin, sizeof (sin), 0) < 0) {
		perror("bind");
		exit(1);
	}
	for (;;) {
		char msgbuf[100];
		int cc;

		cc = recv(s, msgbuf, sizeof (msgbuf) - 1, 0);
		if (cc <= 0) {
			if (errno != EINTR)
				sleep(1);
			errno = 0;
			continue;
		}
		msgbuf[cc] = 0;
		mailfor(msgbuf);
	}
}

onalrm()
{
	struct stat statbf;
	struct utmp *utp;

	dprintf("alarm\n");
	alarm(15);
	fstat(uf,&statbf);
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
	char *rindex();
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
			if (fork() == 0) {
				signal(SIGALRM, SIG_DFL);
				alarm(30);
				notify(utp, offset), exit(0);
			} else
				while (wait3(0, WNOHANG, 0) > 0)
					continue;
}

char *cr;

notify(utp, offset)
	register struct utmp *utp;
{
	FILE *tp;
	struct sgttyb gttybuf;
	char tty[20], hostname[32];
	char name[sizeof (utmp[0].ut_name) + 1];
	struct stat stb;

	strcpy(tty, "/dev/");
	strncat(tty, utp->ut_line, sizeof(utp->ut_line));
	dprintf("notify %s on %s\n", utp->ut_name, tty);
	if (stat(tty, &stb) == 0 && (stb.st_mode & 0100) == 0) {
		dprintf("wrong mode\n");
		return;
	}
	if ((tp = fopen(tty,"w")) == 0) {
		dprintf("fopen failed\n");
		return;
	}
	ioctl(fileno(tp), TIOCGETP, &gttybuf);
	cr = (gttybuf.sg_flags & CRMOD) ? "" : "\r";
	gethostname(hostname, sizeof (hostname));
	strncpy(name, utp->ut_name, sizeof (utp->ut_name));
	name[sizeof (name) - 1] = 0;
	fprintf(tp,"%s\n\007New mail for %s@%s\007 has arrived:%s\n",
	    cr, name, hostname, cr);
	fprintf(tp,"----%s\n", cr);
	jkfprintf(tp, name, offset);
	fclose(tp);
}

jkfprintf(tp, name, offset)
	register FILE *tp;
{
	register FILE *fi;
	register int linecnt, charcnt;
	char line[BUFSIZ];
	int inheader;

	dprintf("HERE %s's mail starting at %d\n",
	    name, offset);
	if ((fi = fopen(name,"r")) == NULL) {
		dprintf("Cant read the mail\n");
		return;
	}
	fseek(fi, offset, 0);
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
		char *index();
		int cnt;

		if (linecnt <= 0 || charcnt <= 0) {  
			fprintf(tp,"...more...%s\n", cr);
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
		fprintf(tp,"%s%s\n", line, cr);
		linecnt--, charcnt -= strlen(line);
	}
	fprintf(tp,"----%s\n", cr);
}
