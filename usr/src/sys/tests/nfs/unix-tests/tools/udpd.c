/*	@(#)udpd.c	1.1 88/10/12 NFS Rev 2 Testsuite	*/
/*
 *  server for simple udp ping program.
 *  listens on socket for request, then echos back
 *  request from client.
 */
#define UDP_PORT        3457

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

main(argc, argv)
char **argv;
{
	int s;				/* socket */
	struct sockaddr_in addr;	/* socket address */
	int ret;
	int addrlen = sizeof(struct sockaddr_in);
	struct hostent *hp;
	char buf[BUFSIZ];

	if ((s = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0)
		xxit("socket");

	addr.sin_family = AF_INET;
	addr.sin_port = htons(UDP_PORT);
	addr.sin_addr.s_addr = INADDR_ANY;

	if (bind(s, &addr, addrlen) < 0)
		xxit("bind");

	while(1) {
		fprintf(stderr, "%s awaiting request\n", argv[0]);
		addrlen = sizeof(struct sockaddr_in);
		ret = recvfrom(s, buf, BUFSIZ, 0, &addr, &addrlen);
		if (ret < 0)
			xxit("recvfrom");
		if (hp = gethostbyaddr(&addr.sin_addr, sizeof(addr.sin_addr),
					AF_INET))
			fprintf(stderr, "\
%s: accepted request from host %s\n", argv[0], hp->h_name);
		else
			fprintf(stderr, "\
%s: accepted request from host %x\n", argv[0], addr.sin_addr.s_addr);

		fprintf(stderr, " recvfrom ret %d\n", ret);
		if (ret > 0) {
			*buf = *buf + 1;
			ret = sendto(s, buf, ret, 0, &addr, addrlen);
			fprintf(stderr, " sendto ret %d\n", ret);
		}
	}
}

xxit(s)
char *s;
{
	perror(s);
	exit(1);
}

