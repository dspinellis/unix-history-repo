#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

main()
{
	int s;
	struct sockaddr_in address;

	s = socket(AF_INET, SOCK_DGRAM, 0);
	if (s == -1) {
		perror("socket");
		exit(1);
	}
	bzero(&address, sizeof(struct sockaddr_in));
	address.sin_family = AF_INET;
	if (bind(s, &address, sizeof(struct sockaddr_in)) == -1) {
		perror("bind");
		exit(1);
	}
	printhostname(s);
}

printhostname(s)
	int s;
{
	int n;
	char buf[100];
	struct hostent *hp;

	n = sizeof(buf);;
	if (getsockname(s, buf, &n) == -1) {
		perror("getsockname");
		exit(1);
	}
	printf("name is :%s:\n",buf);
	/*
	 * Now check that the address is valid.
	 */
	hp = gethostbyaddr(buf, n, AF_INET);
	if (hp == 0)
		printf("Address not found.\n");
	else
		printf("%s\n", hp->h_name);
}

