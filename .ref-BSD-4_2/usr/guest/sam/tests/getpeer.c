#include <sys/types.h>
#include <sys/socket.h>

#include <netinet/in.h>

#include <stdio.h>
#include <netdb.h>

main(argc, argv)
	char *argv[];
{
	struct sockaddr_in sin, myaddr;
	int s, sinlen;
	struct hostent *hp;

	if (argc < 3) {
		printf("usage: getpeer host port\n");
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
	sinlen = sizeof (sin);
	if (getpeername(s, &sin, &sinlen) < 0)
		perror("getpeer");
	else
		printf("peer: len %d, %s.%d\n", sinlen, 
		    inet_ntoa(sin.sin_addr), ntohs(sin.sin_port));
	close(s);
}
