#include <stdio.h>
#include <netdb.h>

#include <sys/types.h>
#include <sys/uio.h>
#include <sys/socket.h>

#include <netinet/in.h>
#include <netinet/in_systm.h>
#include <netinet/ip.h>

char	myname[32];
struct	ip ip;
char	buf[BUFSIZ];
struct	iovec iov[2];

main(argc, argv)
	char *argv[];
{
	struct sockaddr_in sin;
	struct hostent *hp;
	int s, n, i;

	if (argc < 2) {
		printf("nothing to send\n");
		exit(1);
	}
	s = socket(AF_INET, SOCK_RAW, 0);
	if (s < 0) {
		perror("socket");
		exit(1);
	}
	bzero(&sin, sizeof (sin));
	sin.sin_family = AF_INET;
	if (gethostname(myname, sizeof (myname)) < 0) {
		perror("gethostname");
		exit(1);
	}
	hp = gethostbyname(myname);
	if (hp == 0) {
		printf("%s: don't know my name\n", myname);
		exit(1);
	}
	bcopy(hp->h_addr, &sin.sin_addr, hp->h_length);
	if (connect(s, &sin, sizeof (sin)) < 0) {
		perror("connect");
		exit(1);
	}
	if (write(s, argv[1], strlen(argv[1])) < 0) {
		perror("writev");
		exit(1);
	}
	iov[0].iov_base = (caddr_t)&ip;
	iov[0].iov_len = sizeof (ip);
	iov[1].iov_base = buf;
	iov[1].iov_len = sizeof (buf);
	n = readv(s, iov, 2);
	if (n < 0) {
		perror("readv");
		exit(1);
	}
	printf("read returns %d bytes, %d data\n", n, n - sizeof (ip));
	printip(&ip);
	printf("data: %s\n", buf);
}

printip(ip)
	register struct ip *ip;
{

	printf("header length: %d version %d\n", ip->ip_hl, ip->ip_v);
	printf("type of service: %d total length %d\n", ip->ip_tos, ip->ip_len);
	printf("id: %x frag offset: %x ttl: %d protocol: %d\n",
	    ip->ip_id, ip->ip_off, ip->ip_ttl, ip->ip_p);
	printf("checksum: %d source: %s dst: %s\n", ip->ip_sum,
	    inet_ntoa(ip->ip_src), inet_ntoa(ip->ip_dst));
}
