/*
 * Random page access with
 * a gaussian distribution.
 *
 * Allocate a large (zero fill on demand) address
 * space and fault the pages in a random gaussian
 * order.
 */

float	sqrt(), log(), rnd(), cos(), gauss();
char	*valloc();
int	rand();

main(argc, argv)
	char *argv[];
{
	register int pn, i, niter, delta;
	register char *pages;
	float sd = 10.0;
	int npages = 4096, pagesize, debug = 0;
	char *name;

	name = argv[0];
	argc--, argv++;
again:
	if (argc < 1) {
usage:
		printf(
"usage: %s [ -d ] [ -p #pages ] [ -s standard-deviation ] iterations\n", name);
		exit(1);
	}
	if (strcmp(*argv, "-s") == 0) {
		argc--, argv++;
		if (argc < 1)
			goto usage;
		sscanf(*argv, "%f", &sd);
		if (sd <= 0) {
			printf("%s: Bad standard deviation.\n", *argv);
			exit(2);
		}
		argc--, argv++;
		goto again;
	}
	if (strcmp(*argv, "-p") == 0) {
		argc--, argv++;
		if (argc < 1)
			goto usage;
		npages = atoi(*argv);
		if (npages <= 0) {
			printf("%s: Bad page count.\n", *argv);
			exit(2);
		}
		argc--, argv++;
		goto again;
	}
	if (strcmp(*argv, "-d") == 0) {
		argc--, argv++;
		debug++;
		goto again;
	}
	niter = atoi(*argv);
	pagesize = getpagesize();
	pages = valloc(npages*pagesize);
	if (pages == (char *)0) {
		printf("Can't allocate %d pages (%2.1f megabytes).\n",
		    npages, (npages*pagesize) / (1024. * 1024.));
		exit(3);
	}
	pn = 0;
	for (i = 0; i < niter; i++) {
		delta = gauss(sd, 0.0);
		while (pn + delta < 0 || pn + delta > npages)
			delta = gauss(sd, 0.0);
		pn += delta;
		if (debug)
			printf("touch page %d\n", pn);
		else
			pages[pn * pagesize] = 1;
	}
}

float
gauss(sd, mean)
	float sd, mean;
{
	register float qa, qb;

	qa = sqrt(log(rnd()) * -2.0);
	qb = 3.14159 * rnd();
	return (qa * cos(qb) * sd + mean);
}

float
rnd()
{
	static int seed = 1;
	static int biggest = 0x7fffffff;

	return ((float)rand(seed) / (float)biggest);
}
