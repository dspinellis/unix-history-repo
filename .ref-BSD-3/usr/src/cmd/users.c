/*
 * users
 */
char	*malloc();

#include <stdio.h>
#include <utmp.h>

struct utmp utmp;

main(argc, argv)
char **argv;
{
	register char *tp, *s;
	register FILE *fi;

	s = "/etc/utmp";
	if(argc == 2)
		s = argv[1];
	if ((fi = fopen(s, "r")) == NULL) {
		puts("who: cannot open utmp");
		exit(1);
	}
	while (fread((char *)&utmp, sizeof(utmp), 1, fi) == 1) {
		if(utmp.ut_name[0] == '\0')
			continue;
		putline();
	}
	summary();
}

char	*names[128];
char	**namp = names;
putline()
{
	char temp[9];
	strncpy(temp, utmp.ut_name, 8);
	*namp = malloc(strlen(temp) + 1);
	strcpy(*namp++, temp);
}

scmp(p, q)
char **p, **q;
{
	return(strcmp(*p, *q));
}
summary()
{
	register char **p;

	qsort(names, namp - names, sizeof names[0], scmp);
	for (p=names; p < namp; p++) {
		if (p != names)
			printf(" ");
		printf("%s", *p);
	}
	printf("\n");
}
