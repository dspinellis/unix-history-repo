/*
 * Convert from the dot files and one field active file to a
 * two field active file.
 */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

char *LIB, *SPOOL;
char oactive[100], nactive[100];
char dotfile[100];
char ng[100];
FILE *ofd, *nfd;

struct stat sbuf;

main(argc, argv)
char **argv;
{
	register char *p;
	
	if (argc != 3) {
		printf("Usage: cvtdot LIB SPOOL\n");
		exit(1);
	}

	LIB = argv[1];
	SPOOL = argv[2];

	sprintf(oactive, "%s/%s", LIB, "active");
	sprintf(nactive, "%s/%s", LIB, "nactive");
	ofd = fopen(oactive, "r");
	if (ofd == NULL) {
		fprintf(stderr, "Cannot open %s\n", oactive);
		exit(1);
	}
	nfd = fopen(nactive, "w");
	if (nfd == NULL) {
		fprintf(stderr, "Cannot create %s\n", nactive);
		exit(1);
	}

	while (fgets(ng, sizeof ng, ofd) != NULL) {
		for (p=ng; *p!='\n'; p++)
			;
		*p = 0;
		sprintf(dotfile, "%s/.%s", SPOOL, ng);
		stat(dotfile, &sbuf);
		fprintf(nfd, "%s %05ld\n", ng, sbuf.st_size);
	}
	fclose(ofd);
	fclose(nfd);
}
