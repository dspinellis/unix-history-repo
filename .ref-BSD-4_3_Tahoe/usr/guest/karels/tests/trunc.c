#include <stdio.h>

main(ac, av)
	char *av[];
{
	int len = 0;

	if (ac < 2) {
		fprintf(stderr, "usage: %s file [offset]\n", av[0]);
		exit(1);
	}
	if (ac > 2)
		len = atoi(av[2]);
	if (truncate(av[1], len) < 0)
		perror("truncate");
}
