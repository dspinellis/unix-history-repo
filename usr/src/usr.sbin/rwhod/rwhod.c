#ifndef lint
static char sccsid[] = "@(#)rwhod.c	4.5 82/11/14";
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/ioctl.h>

#include <netinet/in.h>

#include <nlist.h>
#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <utmp.h>
#include <ctype.h>
#include <netdb.h>

#include "rwhod.h"

struct	sockaddr_in sin = { AF_INET };

extern	errno;

char	*localnet = "localnet";
char	*myname = "myname";

struct	nlist nl[] = {
#define	NL_AVENRUN	0
	{ "_avenrun" },
#define	NL_BOOTTIME	1
	{ "_boottime" },
	0
};

struct	whod mywd;
int	s, utmpf, kmemf = -1;

int	onalrm();
char	*strcpy(), *sprintf();
long	lseek();
int	getkmem();

main()
{
	struct sockaddr_in from;
	char path[64];
	int addr;
	struct servent *sp;

	sp = getservbyname("who", "udp");
	if (sp == 0) {
		fprintf(stderr, "rwhod: udp/who: unknown service\n");
		exit(1);
	}
	sp->s_port = htons(sp->s_port);
#ifndef DEBUG
	if (fork())
		exit(0);
	{ int s;
	  for (s = 0; s < 10; s++)
		(void) close(s);
	  (void) open("/", 0);
	  (void) dup2(0, 1);
	  (void) dup2(0, 2);
	  s = open("/dev/tty", 2);
	  if (s >= 0) {
		ioctl(s, TIOCNOTTY, 0);
		(void) close(s);
	  }
	}
#endif
	(void) chdir("/dev");
	(void) signal(SIGHUP, getkmem);
	if (getuid()) {
		fprintf(stderr, "rwhod: not super user\n");
		exit(1);
	}
	addr = rhost(&localnet);
	if (addr == -1) {
		fprintf(stderr, "rwhod: no localnet\n");
		exit(1);
	}
	sin.sin_addr.s_addr = addr;
	if (rhost(&myname) == -1) {
		fprintf(stderr, "rwhod: don't know \"myname\"\n");
		exit(1);
	}
	strncpy(mywd.wd_hostname, myname, sizeof (mywd.wd_hostname) - 1);
	utmpf = open("/etc/utmp", 0);
	if (utmpf < 0) {
		(void) close(creat("/etc/utmp", 0644));
		utmpf = open("/etc/utmp", 0);
	}
	if (utmpf < 0) {
		perror("rwhod: /etc/utmp");
		exit(1);
	}
	sin.sin_port = sp->s_port;
	getkmem();
	if ((s = socket(0, SOCK_DGRAM, 0, 0)) < 0) {
		perror("rwhod: socket");
		exit(1);
	}
	if (bind(s, &sin, sizeof (sin), 0) < 0) {
		perror("rwhod: bind");
		exit(1);
	}
	sigset(SIGALRM, onalrm);
	onalrm();
	for (;;) {
		struct whod wd;
		int cc, whod, len=sizeof (from);

		cc = recvfrom(s, (char *)&wd, sizeof (struct whod), 0, &from, &len);
		if (cc <= 0) {
			if (cc < 0 && errno != EINTR)
				perror("rwhod: recv");
			continue;
		}
		if (from.sin_port != sp->s_port) {
			fprintf(stderr, "rwhod: %d: bad from port\n",
				ntohs(from.sin_port));
			continue;
		}
#ifdef notdef
		if (gethostbyname(wd.wd_hostname) == 0) {
			fprintf(stderr, "rwhod: %s: unknown host\n",
				wd.wd_hostname);
			continue;
		}
#endif
		if (!verify(wd.wd_hostname)) {
			fprintf(stderr, "rwhod: malformed host name from %x\n",
				from.sin_addr);
			continue;
		}
		(void) sprintf(path, "/etc/whod.%s", wd.wd_hostname);
		whod = creat(path, 0666);
		if (whod < 0) {
			fprintf(stderr, "rwhod: ");
			perror(path);
			continue;
		}
		(void) time(&wd.wd_recvtime);
		(void) write(whod, (char *)&wd, cc);
		(void) close(whod);
	}
}

/*
 * Check out host name for unprintables
 * and other funnies before allowing a file
 * to be created.  Sorry, but blanks aren't allowed.
 */
verify(name)
	register char *name;
{
	register int size = 0;

	while (*name) {
		if (!isascii(*name) || !isalnum(*name))
			return (0);
		name++, size++;
	}
	return (size > 0);
}

int	utmptime;
int	utmpent;
struct	utmp utmp[100];
int	alarmcount;

onalrm()
{
	register int i;
	struct stat stb;
	register struct whoent *we = mywd.wd_we, *wlast;
	int cc;
	double avenrun[3];
	time_t now = time(0);

	if (alarmcount % 10 == 0)
		getkmem();
	alarmcount++;
	(void) fstat(utmpf, &stb);
	if (stb.st_mtime != utmptime) {
		(void) lseek(utmpf, (long)0, 0);
		cc = read(utmpf, (char *)utmp, sizeof (utmp));
		if (cc < 0) {
			perror("/etc/utmp");
			return;
		}
		wlast = &mywd.wd_we[(1024 / sizeof (struct whoent)) - 1];
		utmpent = cc / sizeof (struct utmp);
		for (i = 0; i < utmpent; i++)
			if (utmp[i].ut_name[0]) {
				we->we_utmp = utmp[i];
				if (we >= wlast)
					break;
				we++;
			}
		utmpent = we - mywd.wd_we;
	}
	we = mywd.wd_we;
	for (i = 0; i < utmpent; i++) {
		if (stat(we->we_utmp.ut_line, &stb) >= 0)
			we->we_idle = now - stb.st_atime;
		we++;
	}
	(void) lseek(kmemf, (long)nl[NL_AVENRUN].n_value, 0);
	(void) read(kmemf, (char *)avenrun, sizeof (avenrun));
	for (i = 0; i < 3; i++)
		mywd.wd_loadav[i] = avenrun[i] * 100;
	cc = (char *)we - (char *)&mywd;
	(void) time(&mywd.wd_sendtime);
	(void) sendto(s, (char *)&mywd, cc, 0, &sin, sizeof (sin));
	(void) alarm(60);
}

getkmem()
{
	struct nlist *nlp;

	signal(SIGHUP, getkmem);
	if (kmemf >= 0)
		(void) close(kmemf);
loop:
	for (nlp = &nl[sizeof (nl) / sizeof (nl[0])]; --nlp >= nl; ) {
		nlp->n_value = 0;
		nlp->n_type = 0;
	}
	nlist("/vmunix", nl);
	if (nl[0].n_value == 0) {
		fprintf(stderr, "/vmunix namelist botch\n");
		sleep(300);
		goto loop;
	}
	kmemf = open("/dev/kmem", 0);
	if (kmemf < 0) {
		perror("/dev/kmem");
		sleep(300);
		goto loop;
	}
	(void) lseek(kmemf, (long)nl[NL_BOOTTIME].n_value, 0);
	(void) read(kmemf, (char *)&mywd.wd_boottime, sizeof (mywd.wd_boottime));
}
