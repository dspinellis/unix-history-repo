#ifndef lint
static char sccsid[] = "@(#)hostid.c	4.3 (Berkeley) %G%";
#endif

#include <stdio.h>

extern	char *index();
extern	unsigned long inet_addr();

main(argc, argv)
	int argc;
	char **argv;
{
	register char *id;
	int hostid;

	if (argc < 2) {
		printf("%#x\n", gethostid());
		exit(0);
	}
	id = argv[1];

	if (index(id, '.') != NULL)
		hostid = (int) inet_addr(id);
	else {
		if (*id == '0' && (id[1] == 'x' || id[1] == 'X'))
			id += 2;
		if (sscanf(id, "%x", &hostid) != 1) {
			fprintf(stderr, "usage: %s [hexnum or internet address]\n", argv[0]);
			exit(1);
		}
	}

	if (sethostid(hostid) < 0) {
		perror("sethostid");
		exit(1);
	}

	exit(0);
}
