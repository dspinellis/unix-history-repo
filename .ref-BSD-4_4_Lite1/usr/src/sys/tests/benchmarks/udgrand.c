/*
 * UNIX domain datagram socket random
 * send benchmark.
 */

#include <stdio.h>
#include <signal.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/un.h>
#include <sys/resource.h>

struct	sockaddr_un sun;
struct	sockaddr_un myname;

void	catchsig();
#define	MAXMSGS		10000
int	lens[MAXMSGS];
char	*malloc();

main(argc, argv)
	char *argv[];
{
	register char *buf;
	register int i, kb, msgs = 0;
	int msglen = 0, ms, lens[MAXMSGS];
	int pid, s, sunlen;
	struct timeval t1, t2;
	struct rusage r1, r2;

	if (argc < 3) {
		printf("usage: %s #msgs max-msg-length\n", argv[0]);
		exit(1);
	}
	msgs = atoi(argv[1]);
	msglen = atoi(argv[2]);
	buf = malloc(msglen);
	if (buf == 0) {
		printf("Couldn't allocate data buffer\n");
		exit(1);
	}
	for (i = 0; i < msgs; i++)
		lens[i] = random() % msglen;
	for (i = 0; i < msglen; i++)
		buf[i] = random() & 0xff;
	myname.sun_family = AF_UNIX;
	printf("%d messages, max message length %d bytes\n", msgs, msglen);
	signal(SIGINT, catchsig);
	pid = fork();
	if (pid == 0) {
		s = socket(AF_UNIX, SOCK_DGRAM, 0);
		if (s < 0) {
			perror("socket");
			exit(1);
		}
		sprintf(myname.sun_path, "unixdg%d", getpid());
		if (bind(s, (const struct sockaddr *)&myname,
		    strlen(myname.sun_path) + 2) < 0) {
			perror("bind (child)");
			exit(1);
		}
		for (i = 0; i < msgs; i++) {
			sunlen = sizeof (sun);
			if (recvfrom(s, buf, lens[i], 0,
			    (struct sockaddr *)&sun, &sunlen) < 0)
				perror("recvfrom");
		}
	} else {
		s = socket(AF_UNIX, SOCK_DGRAM, 0);
		if (s < 0) {
			perror("socket");
			exit(1);
		}
		sprintf(myname.sun_path, "unixdg%d", getpid());
		if (bind(s, (const struct sockaddr *)&myname,
		    strlen(myname.sun_path) + 2) < 0) {
			perror("bind (parent)");
			exit(1);
		}
		sun.sun_family = AF_UNIX;
		sprintf(sun.sun_path, "unixdg%d", pid);
		sunlen = strlen(sun.sun_path) + 2;
		getrusage(RUSAGE_SELF, &r1);
		gettimeofday(&t1, (struct timezone *)0);
		for (kb = 0, i = 0; i < msgs; kb += lens[i], i++)
			if (sendto(s, buf, lens[i], 0,
			    (const struct sockaddr *)&sun, sunlen) < 0)
				perror("sendto");
		gettimeofday(&t2, (struct timezone *)0);
		getrusage(RUSAGE_SELF, &r2);
		timevalsub(&t2, &t1);
		ms = t2.tv_usec / 1000;
		printf("%d msgs (%d bytes) in %d.%d secs", msgs,
		    kb, t2.tv_sec, ms / 100);
#define	nz(x)	(x == 0 ? 1 : x)
		printf(", %d bytes/msg, %6.2f kb/s, %4.1f ms/msg\n",
		    kb / nz(msgs), (8. * kb) / (nz(t2.tv_sec) * 1024.),
		    (1000. * t2.tv_sec + ms) / nz(msgs));
		timevalsub(&r2.ru_stime, &r1.ru_stime);
		timevalsub(&r2.ru_utime, &r1.ru_utime);
		r2.ru_nvcsw -= r1.ru_nvcsw;
		r2.ru_nivcsw -= r1.ru_nivcsw;
		printf("System %d.%d, user %d.%d, %d vcsw, %d ivcsw\n",
		    r2.ru_stime.tv_sec, r2.ru_stime.tv_usec / 100000,
		    r2.ru_utime.tv_sec, r2.ru_utime.tv_usec / 100000,
		    r2.ru_nvcsw, r2.ru_nivcsw);
		kill(pid, SIGINT);
	}
	close(s);
	unlink(myname.sun_path);
}

void
catchsig(s)
	int s;
{

	unlink(myname.sun_path);
	exit(1);
}

/*
 * Add and subtract routines for timevals.
 * N.B.: subtract routine doesn't deal with
 * results which are before the beginning,
 * it just gets very confused in this case.
 * Caveat emptor.
 */
timevaladd(t1, t2)
	struct timeval *t1, *t2;
{

	t1->tv_sec += t2->tv_sec;
	t1->tv_usec += t2->tv_usec;
	timevalfix(t1);
}

timevalsub(t1, t2)
	struct timeval *t1, *t2;
{

	t1->tv_sec -= t2->tv_sec;
	t1->tv_usec -= t2->tv_usec;
	timevalfix(t1);
}

timevalfix(t1)
	struct timeval *t1;
{

	if (t1->tv_usec < 0) {
		t1->tv_sec--;
		t1->tv_usec += 1000000;
	}
	if (t1->tv_usec >= 1000000) {
		t1->tv_sec++;
		t1->tv_usec -= 1000000;
	}
}
