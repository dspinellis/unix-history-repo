/*	@(#)udp.c	1.1 88/10/12 NFS Rev 2 Testsuite	*/
/*
 *  client for simple udp ping program.
 *  send request to server, who will echo request back.
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
	int s, len, ret;
        struct sockaddr_in addr;        /* socket address */
        int addrlen = sizeof(struct sockaddr_in);
	struct hostent *hp;
	char *peer;
	char buf[BUFSIZ];
	char *msg = "This is a test message!";

	if (argc != 2) {
		fprintf(stderr, "usage: %s hostname\n", argv[0]);
		exit(1);
	}
	peer = argv[1];

	if ((hp = gethostbyname(peer)) == NULL) {
		fprintf(stderr, "Can't find host %s\n", peer);
		exit(1);
	}

        if ((s = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0)
                xxit("socket");
	addr.sin_family = AF_INET;
	addr.sin_port = htons(UDP_PORT);
	addr.sin_addr = *(struct in_addr *)hp->h_addr;
	fprintf(stderr, "Sending request to %s (%x)\n", peer, 
			addr.sin_addr.s_addr);

	strcpy(buf, msg);
	len = strlen(buf);

	ret = sendto(s, buf, len, 0, &addr, addrlen);
	fprintf(stderr, " sendto ret %d (len %d)\n", ret, len);
	ret = recvfrom(s, buf, BUFSIZ, 0, &addr, &addrlen);
	fprintf(stderr, " recvfrom ret %d\n", ret);

	if (ret != len || *buf != *msg + 1 || strcmp(buf+1, msg+1))
		fprintf(stderr, "Message error: sent '%s' recv '%s'\n",
			msg, buf);
	else
		fprintf(stderr, "udp ping to %s ok\n", peer);
}

xxit(s)
char *s;
{
	perror(s);
	exit(1);
}
