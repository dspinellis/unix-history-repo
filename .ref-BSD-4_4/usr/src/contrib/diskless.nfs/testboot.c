#include <stdio.h>
#include <sys/types.h>
#include "socket.h"
#include "in.h"

main()
{
	int i, j;
	struct sockaddr_in cl;
	char buf[512];

	cl.sin_len = sizeof (struct sockaddr_in);
	cl.sin_family = AF_INET;
	cl.sin_port = 0;
	cl.sin_addr.s_addr = inet_addr("131.104.48.117");
	i = open_boot(&cl);
	if (i < 0) {
		fprintf(stderr, "Open\n");
		exit(1);
	}
	while ((j = read_boot(i, buf, 512)) > 0)
		write(1, buf, j);
}
