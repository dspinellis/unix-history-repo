/*
 * Random page access benchmark.
 */
#include <sys/vadvise.h>

char	*valloc();
int	rand();

main(argc, argv)
	char *argv[];
{
	register int npages = 4096, pagesize, pn, i, niter;
	int vflag = 0, debug = 0;
	char *pages, *name;

	name = argv[0];
	argc--, argv++;
again:
	if (argc < 1) {
usage:
		printf("usage: %s [ -d ] [ -v ] [ -p #pages ] niter\n", name);
		exit(1);
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
	if (strcmp(*argv, "-v") == 0) {
		argc--, argv++;
		vflag++;
		goto again;
	}
	if (strcmp(*argv, "-d") == 0) {
		argc--, argv++;
		debug++;
		goto again;
	}
	niter = atoi(*argv);
	pagesize = getpagesize();
	pages = valloc(npages * pagesize);
	if (pages == (char *)0) {
		printf("Can't allocate %d pages (%2.1f megabytes).\n",
		    npages, (npages * pagesize) / (1024. * 1024.));
		exit(3);
	}
	if (vflag)
		vadvise(VA_ANOM);
	for (i = 0; i < niter; i++) {
		pn = random() % npages;
		if (debug)
			printf("touch page %d\n", pn);
		pages[pagesize * pn] = 1;
	}
}
