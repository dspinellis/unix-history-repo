#include <stdio.h>

#define ACULOG	"/usr/adm/aculog"

main(argc, argv)
char *argv[];
{
	register FILE *fd;
	register int i;
	register char *p;
	char line[256];
	extern char *index();

	if ((fd = fopen(ACULOG, "r")) == NULL) {
		fprintf(stderr, "can't open %s\n", ACULOG);
		exit(1);
	}
	while (fgets(line, sizeof(line), fd) != NULL)
		if (p = index(line, ' ')) {
			for (i = 1; i < argc; i++)
				if (strncmp(argv[i], line, p-line) == 0)
					break;
			if (i == argc && argc > 1)
				continue;
			printf("%s", line);
		}
}
