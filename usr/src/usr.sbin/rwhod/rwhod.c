#ifndef lint
static char sccsid[] = "@(#)rwhod.c	4.1 82/04/02";
#endif

#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <net/in.h>
#include <sys/socket.h>
#include <errno.h>
#include <utmp.h>
#include "rwhod.h"
#include <sys/stat.h>
#include <nlist.h>
#include <sys/ioctl.h>

struct	sockaddr_in sin = { AF_INET, IPPORT_WHOSERVER };

extern	errno;

char	*localnet = "localnet";
char	*myname = "myname";

struct	nlist nl[] = {
#define	NL_AVENRUN	0
	{ "_avenrun" },
#define	NL_BOOTIME	1
	{ "_bootime" },
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
		fprintf(stderr, "not super user\n");
		exit(1);
	}
	addr = rhost(&localnet);
	if (addr == -1) {
		fprintf(stderr, "no localnet for whod\n");
		exit(1);
	}
	sin.sin_addr.s_addr = addr;
	if (rhost(&myname) == -1) {
		fprintf(stderr, "don't know my name\n");
		exit(1);
	}
	strncpy(mywd.wd_hostname, myname, sizeof (mywd.wd_hostname) - 1);
	utmpf = open("/etc/utmp", 0);
	if (utmpf < 0) {
		(void) close(creat("/etc/utmp", 0644));
		utmpf = open("/etc/utmp", 0);
	}
	if (utmpf < 0) {
		perror("/etc/utmp");
		exit(1);
	}
	getkmem();
#ifdef vax
	sin.sin_port = htons(sin.sin_port);
#endif
again:
	if ((s = socket(SOCK_DGRAM, 0, &sin, 0)) < 0) {
		perror("socket");
		sleep(5);
		goto again;
	}
	sigset(SIGALRM, onalrm);
	onalrm();
	for (;;) {
		struct whod wd;
		int cc, whod;

		cc = receive(s, &from, (char *)&wd, sizeof (struct whod));
		if (cc <= 0) {
			if (cc < 0 && errno != EINTR)
				perror("receive");
			continue;
		}
#ifdef vax
		from.sin_port = ntohs(from.sin_port);
#endif
		if (from.sin_port != IPPORT_WHOSERVER) {
			printf("bad from port %d\n", from.sin_port);
			continue;
		}
/*
		if (rhost(&wd.wd_hostname) == -1) {
			printf("unknown host %s\n", wd.wd_hostname);
			continue;
		}
*/
		(void) sprintf(path, "/etc/whod.%s", wd.wd_hostname);
		whod = creat(path, 0666);
		if (whod < 0) {
			printf("can't create %s\n", path);
			continue;
		}
		(void) time(&wd.wd_recvtime);
		(void) write(whod, (char *)&wd, cc);
		(void) close(whod);
	}
}

int	utmptime;
int	utmpent;
struct	utmp utmp[100];
int	alarmcount;

onalrm()
{
	register int i;
	struct stat stb;
	register struct whoent *we = mywd.wd_we;
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
		utmpent = cc / sizeof (struct utmp);
		for (i = 0; i < utmpent; i++)
			if (utmp[i].ut_name[0]) {
				we->we_utmp = utmp[i];
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
	send(s, &sin, (char *)&mywd, cc);
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
	(void) lseek(kmemf, (long)nl[NL_BOOTIME].n_value, 0);
	(void) read(kmemf, (char *)&mywd.wd_bootime, sizeof (mywd.wd_bootime));
}
