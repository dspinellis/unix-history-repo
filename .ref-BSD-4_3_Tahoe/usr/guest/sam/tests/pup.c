#include <stdio.h>
#include <netdb.h>

#include <sys/types.h>
#include <sys/uio.h>
#include <sys/socket.h>

#include <netpup/pup.h>

char	myname[32];
struct	pup_header pup;
char	buf[BUFSIZ];
struct	iovec iov[2];

main(argc, argv)
	char *argv[];
{
	struct sockaddr_pup sup;
	struct hostent *hp;
	int s, n, i;

	if (argc < 2) {
		printf("nothing to send\n");
		exit(1);
	}
	s = socket(AF_PUP, SOCK_RAW, 0);
	if (s < 0) {
		perror("socket");
		exit(1);
	}
	bzero(&sup, sizeof (sup));
	sup.spup_family = AF_PUP;
	if (gethostname(myname, sizeof (myname)) < 0) {
		perror("gethostname");
		exit(1);
	}
	hp = gethostbyname(myname);
	if (hp == 0) {
		printf("%s: don't know my name\n", myname);
		exit(1);
	}
	sup.spup_host = inet_lnaof(*(int *)hp->h_addr);
	sup.spup_net = inet_netof(*(int *)hp->h_addr);
	if (connect(s, &sup, sizeof (sup)) < 0) {
		perror("connect");
		exit(1);
	}
	iov[0].iov_base = (caddr_t)&pup;
	iov[0].iov_len = sizeof (pup);
	iov[1].iov_base = buf;
	pup.pup_type = 99;
	strcpy(buf, argv[1]);
#define	even(x)	(((x) + 1) & ~1)
	iov[1].iov_len = even(strlen(iov[1].iov_base)) + 2;
	if (writev(s, iov, 2) < 0) {
		perror("writev");
		exit(1);
	}
	iov[1].iov_len = sizeof (buf);
	n = readv(s, iov, 2);
	if (n < 0) {
		perror("readv");
		exit(1);
	}
	printf("read returns %d bytes, %d data\n", n, n - sizeof (pup));
	printpup(&pup);
	n -= sizeof (pup);
	printf("checksum: %x\n", ntohs(*(u_short *)(buf + n - 2)));
	buf[n - 2] = '\0';
	printf("data: %s\n", buf);
}

printpup(p)
	register struct pup_header *p;
{

	printf("length: %d tcontrol %d\n", ntohs(p->pup_length),
	    p->pup_tcontrol);
	printf("type: %d id: %d\n", p->pup_type, ntohs(p->pup_id));
	printf("dnet: %d dhost: %d dsock: %x\n",
	    p->pup_dnet, p->pup_dhost, ntohl(*(int *)(p->pup_dsock)));
	printf("snet: %d shost: %d ssock: %x\n",
	    p->pup_snet, p->pup_shost, ntohl(*(int *)(p->pup_ssock)));
}
