#include <sys/types.h>
#include <sys/socket.h>

#include <netinet/in.h>

#include <signal.h>
#include <stdio.h>
#include <netdb.h>

#define	MEGABYTES	4

main(argc, argv)
	char *argv[];
{
	register int i, j, mega = MEGABYTES;
	struct sockaddr_in sin;
	char *buf, *malloc();
	int s, debug = 0, on = 1, writesize = 1024, bufsize = 0;
	struct hostent *hp;

	if (argc > 3 && !strcmp(argv[1], "-d")) {
		debug++;
		argc--;
		argv++;
	}
	if (argc < 3) {
		printf(
"usage: dataout [ -d ] host port [# megabytes] [ writesize ] [ bufsize ]\n");
		exit(1);
	}
	hp = gethostbyname(argv[1]);
	if (hp == NULL) {
		printf("%s: unknown host\n", argv[1]);
		exit(1);
	}
	bzero(&sin, sizeof (sin));
	sin.sin_family = hp->h_addrtype;
	bcopy(hp->h_addr, &sin.sin_addr, hp->h_length);
	argv++, argc--;
	sin.sin_port = htons(atoi(argv[1]));
	argv++, argc--;
	if (argc > 1) {
		mega = atoi(argv[1]);
		if (mega < 0 || mega > 2*MEGABYTES)
			printf("%d: bad # megabytes\n", mega), exit(2);
	}
	if (argc > 2) {
		writesize = atoi(argv[2]);
		if (writesize <= 0) {
			printf("bad writesize %d\n", writesize);
			exit(2);
		}
	}
	if (argc > 3) {
		bufsize = atoi(argv[3]);
		if (bufsize <= 0) {
			printf("bad bufsize %d\n", bufsize);
			exit(2);
		}
	}
	buf = malloc(writesize);
	signal(SIGPIPE, SIG_IGN);
	printf("send %d megabytes\n", mega);
	s = socket(AF_INET, SOCK_STREAM, 0);
	if (s < 0) {
		perror("socket");
		exit(1);
	}
	if (debug)
		setsockopt(s, SOL_SOCKET, SO_DEBUG, &on, sizeof(on));
	if (bufsize)
		setsockopt(s, SOL_SOCKET, SO_SNDBUF, &bufsize, sizeof(bufsize));
	if (connect(s, &sin, sizeof (sin)) < 0) {
		perror("connect");
		close(s);
		exit(2);
	}
	mega = mega * 1024 * 1024 / writesize;
	for (i = 0; i < mega; i++)
		if (sendto(s, buf, 1, MSG_OOB) < 0) {
			perror("sendto OOB");
			break;
		}
	close(s);
}
