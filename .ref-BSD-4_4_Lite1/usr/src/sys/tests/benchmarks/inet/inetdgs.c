#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <netinet/in.h>

#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include <errno.h>

struct	sockaddr_in sin;
jmp_buf	j;
int	catchsig();

main(argc, argv)
	char *argv[];
{
	int s, n;
	char buf[2 * BUFSIZ];
	struct rusage r1, r2;

	if (argc < 2) {
		printf("usage: %s port\n", argv[0]);
		exit(1);
	}
	s = socket(AF_INET, SOCK_DGRAM, 0);
	if (s < 0) {
		perror("inetdgs: socket");
		exit(1);
	}
	sin.sin_family = AF_INET;
	sin.sin_port = htons(atoi(argv[1]));
	if (bind(s, &sin, sizeof (sin)) < 0) {
		perror("inetdgs: bind");
		exit(1);
	}
	for (;;) {
		int count, ms, kb, fromlen;
		struct timeval t1, t2;
		struct sockaddr_in from;
		extern int errno;

		for (;;) {
			fromlen = sizeof (from);
			n = recvfrom(s, buf, sizeof (buf), 0, &from, &fromlen);
			if (n < 0) {
				if (errno != EINTR)
					perror("inetdgs: recvfrom");
				continue;
			}
			if (strncmp(buf, "begin", 5) == 0)
				break;
		}
		count = kb = 0;
		getrusage(RUSAGE_SELF, &r1);
		gettimeofday(&t1, (struct timezone *)0);
		for (;;) {
			fromlen = sizeof (from);
			n = recvfrom(s, buf, sizeof (buf), 0, &from, &fromlen);
			if (n < 0) {
				if (errno != EINTR)
					perror("inetdgs: recvfrom");
				continue;
			}
			count++, kb += n;
			if (strncmp(buf, "end", 3) == 0)
				break;
		}
		gettimeofday(&t2, (struct timezone *)0);
		getrusage(RUSAGE_SELF, &r2);
		sleep(2);
		timevalsub(&t2, &t1);
		ms = t2.tv_usec / 1000;
		printf("%d msgs (%d bytes) in %d.%d secs", count,
		    kb, t2.tv_sec, ms / 100);
		printf(", %d bytes/msg, %6.2f kb/s, %4.1f ms/msg\n",
		    kb / count, (8. * kb) / (t2.tv_sec * 1024.),
		   (1000. * t2.tv_sec + ms) / count);
		timevalsub(&r2.ru_stime, &r1.ru_stime);
		timevalsub(&r2.ru_utime, &r1.ru_utime);
		r2.ru_nvcsw -= r1.ru_nvcsw;
		r2.ru_nivcsw -= r1.ru_nivcsw;
		printf("System %d.%d, user %d.%d, %d vcsw, %d ivcsw\n",
		    r2.ru_stime.tv_sec, r2.ru_stime.tv_usec / 100000,
		    r2.ru_utime.tv_sec, r2.ru_utime.tv_usec / 100000,
		    r2.ru_nvcsw, r2.ru_nivcsw);
	}
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
