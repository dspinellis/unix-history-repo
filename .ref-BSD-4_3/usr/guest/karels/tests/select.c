#include <sys/param.h>

#define tbit(p, n)	((p)[(n)/32] & (1 << ((n) % 32)))
#define sbit(p, n)	((p)[(n)/32] |= (1 << ((n) % 32)))
#define cbit(p, n)	((p)[(n)/32] &= ~(1 << ((n) % 32)))

main(ac, av)
	char *av[];
{
	int nofile = getdtablesize();
	register i, fd;
	int ret;
	int rdfds[1][(NOFILE+31)/32];
	char buf[80];

	if (ac > 1)
		fd = atoi(av[1]);
	else fd = nofile - 1;
	fd = dup2(0, fd);
	if (fd < 0)
		perror("dup2");

	for (;;) {
		bzero(rdfds, sizeof(rdfds));
		sbit(rdfds[0], fd);
		ret = select(nofile, rdfds[0], 0, 0, 0);
		if (ret < 0) {
			perror(select);
			exit(1);
		}
		printf("select: %d [%x, %x] (", ret, rdfds[0][0], rdfds[0][1]);
		for (i = 0; i < nofile; i++)
			if (tbit(rdfds[0], i))
				printf("%d,", i);
		printf(")\n");
		if (tbit(rdfds[0], fd)) {
			ret = read(fd, buf, sizeof(buf));
			if (ret == 0)
				exit(0);
			write(1, buf, ret);
		}
	}
}
