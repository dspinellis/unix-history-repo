/*	@(#)tcpd.c	1.1 88/10/12 NFS Rev 2 Testsuite	*/
/*
 *  server for simple tcp ping program.
 *  listens on socket for connection request, then echos back
 *  request from client.
 */
#define TCP_PORT        3456

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

main(argc, argv)
char **argv;
{
	int s, ns;			/* sockets */
	struct sockaddr_in addr;	/* socket address */
	int ret;
	int addrlen = sizeof(struct sockaddr_in);
	struct hostent *hp;
	char buf[BUFSIZ];

	if ((s = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0)
		xxit("socket");

	addr.sin_family = AF_INET;
	addr.sin_port = htons(TCP_PORT);
	addr.sin_addr.s_addr = INADDR_ANY;

	if (bind(s, &addr, addrlen) < 0)
		xxit("bind");

	if (listen(s, 5) < 0)
		xxit("listen");

	while(1) {
		fprintf(stderr, "%s awaiting accept\n", argv[0]);
		addrlen = sizeof(struct sockaddr_in);
		if ((ns = accept(s, &addr, &addrlen)) < 0)
			xxit("accept");
		if (hp = gethostbyaddr(&addr.sin_addr, sizeof(addr.sin_addr),
					AF_INET))
			fprintf(stderr, "\
%s: accepted connection from host %s\n", argv[0], hp->h_name);
		else
			fprintf(stderr, "\
%s: accepted connection from host %x\n", argv[0], addr.sin_addr.s_addr);

		ret = read(ns, buf, BUFSIZ);
		fprintf(stderr, " read ret %d\n", ret);
		if (ret > 0) {
			*buf = *buf + 1;
			ret = write(ns, buf, ret);
			fprintf(stderr, " write ret %d\n", ret);
		}
		sleep(5);
		close(ns);
	}
}

xxit(s)
char *s;
{
	perror(s);
	exit(1);
}

