#include <stdio.h>
#include <strings.h>
#include <signal.h>
#include <netdb.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <sys/resource.h>

struct	sockaddr_in sin;
char	myname[256];

#define	MAXMSGS		(5000 - 1)
#define	MAXMSGLEN	BUFSIZ

char	*malloc();

main(argc, argv)
	char *argv[];
{
	int s, sinlen;
	struct hostent *hp;
	register char *buf;
	register int i, kb, msgs = 0;
	int msglen = 0, ms, lens[MAXMSGS];
	struct timeval t1, t2;
	struct rusage r1, r2;

	if (argc < 2) {
		printf("usage: %s port [ #msgs ] [ max-msg-length ]\n",
		   argv[0]);
		exit(1);
	}
	s = socket(AF_INET, SOCK_DGRAM, 0);
	if (s < 0) {
		perror("socket");
		exit(1);
	}
	sinlen = sizeof (myname);
	gethostname(myname, &sinlen);
	hp = gethostbyname(myname);
	if (hp == 0) {
		printf("%s: host unknown\n", myname);
		exit(1);
	}
	sin.sin_family = AF_INET;
	bcopy(hp->h_addr, &sin.sin_addr, hp->h_length);
	if (bind(s, &sin, sizeof (sin)) < 0) {
		perror("inetdg: bind");
		exit(1);
	}
	sin.sin_port = htons(atoi(argv[1]));
	if (argc > 2)
		msgs = atoi(argv[2]);
	if (msgs <= 0 || msgs > MAXMSGS)
		msgs = MAXMSGS;
	if (argc > 3)
		msglen = atoi(argv[3]);
	if (msglen <= 0 || msglen >= MAXMSGLEN)
		msglen = MAXMSGLEN;
	buf = malloc(msglen);
	if (buf == 0) {
		printf("couldn't allocate data buffer\n");
		exit(1);
	}
	for (i = 0; i < msgs; i++)
		lens[i] = random() % msglen;
	for (i = 0; i < msglen; i++)
		buf[i] = random() & 0xff;
	printf("%d messages, max message length %d bytes\n",
	   msgs + 1, msglen);
	(void) sendto(s, "begin", 5, 0, &sin, sizeof (sin));
	kb = 0;
	getrusage(RUSAGE_SELF, &r1);
	gettimeofday(&t1, (struct timezone *)0);
	for (i = 0; i < msgs; i++) {
		if (sendto(s, buf, lens[i], 0, &sin, sizeof (sin)) < 0)
			perror("inetdg: sendto");
		kb += lens[i];
	}
	(void) sendto(s, "end", 3, 0, &sin, sizeof (sin));
	gettimeofday(&t2, (struct timezone *)0);
	getrusage(RUSAGE_SELF, &r2);
	msgs++, kb += 3;
	timevalsub(&t2, &t1);
	ms = t2.tv_usec / 1000;
	printf("%d msgs (%d bytes) in %d.%d secs", msgs,
	    kb, t2.tv_sec, ms / 100);
	printf(", %d bytes/msg, %6.2f kb/s, %4.1f ms/msg\n",
	    kb / msgs, (8. * kb) / (t2.tv_sec * 1024.),
	    (1000. * t2.tv_sec + ms) / msgs);
	timevalsub(&r2.ru_stime, &r1.ru_stime);
	timevalsub(&r2.ru_utime, &r1.ru_utime);
	r2.ru_nvcsw -= r1.ru_nvcsw;
	r2.ru_nivcsw -= r1.ru_nivcsw;
	printf("System %d.%d, user %d.%d, %d vcsw, %d ivcsw\n",
	    r2.ru_stime.tv_sec, r2.ru_stime.tv_usec / 100000,
	    r2.ru_utime.tv_sec, r2.ru_utime.tv_usec / 100000,
	    r2.ru_nvcsw, r2.ru_nivcsw);
	pause();
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
