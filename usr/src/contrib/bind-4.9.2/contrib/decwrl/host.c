/* host - print information about a host
 * originally written by Paul Vixie @DEC WRL, January 1989
 */

#ifndef lint
static char RcsId[] = "$Header: host.c,v 1.1 89/04/05 15:41:12 vixie Locked $";
#endif

#include <stdio.h>

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <netdb.h>

main(argc, argv)
	char **argv;
{
	long l_addr;
	struct in_addr addr;
	struct hostent *host;
	long **cp;

	if (argc != 2) {
		printf("usage:  %s hostname\n", argv[0]);
		exit(1);
	}
	l_addr = inet_addr(argv[1]);
	if (l_addr != -1) {
		addr = * (struct in_addr *) &l_addr;
		printf("[%s]\n", inet_ntoa(addr));
		if (!(host = gethostbyaddr(&addr, sizeof addr, AF_INET))) {
			perror("gethostbyaddr");
			exit(1);
		}
	} else {
		printf("{%s}\n", argv[1]);
		if (!(host = gethostbyname(argv[1]))) {
			perror("gethostbyname");
			exit(1);
		}
	}
	printf("name: %s\n", host->h_name);
	if (host->h_aliases && *host->h_aliases) {
		printf("aliases:");
		for (cp = (long**) host->h_aliases;  *cp;  cp++)
			printf(" %s", *cp);
		printf("\n");
	}
	if (host->h_addr_list && *host->h_addr_list) {
		printf("addresses:");
		for (cp = (long**) host->h_addr_list;  *cp;  cp++)
			printf(" %s", inet_ntoa(**cp));
		printf("\n");
	}
}
