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
	struct sockaddr_in sin, myaddr;
	char buf[1024];
	int s;
	struct hostent *hp;

	if (argc < 3) {
		printf("usage: dataout host port [# megabytes]\n");
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
	signal(SIGPIPE, SIG_IGN);
	printf("send %d megabytes\n", mega);
	s = socket(AF_INET, SOCK_STREAM, 0);
	if (s < 0) {
		perror("socket");
		exit(1);
	}
	bzero(&myaddr, sizeof (myaddr));
	myaddr.sin_family = sin.sin_family;
	myaddr.sin_addr.s_addr = INADDR_ANY;
	if (bind(s, &myaddr, sizeof (myaddr)) < 0) {
		perror("bind");
		exit(1);
	}
	if (connect(s, &sin, sizeof (sin)) < 0) {
		perror("connect");
		close(s);
		exit(2);
	}
	mega *= 1024;
	for (i = 0; i < mega; i++)
		if (write(s, buf, sizeof (buf)) < 0) {
			perror("write");
			break;
		}
	close(s);
}
