/* Copyright (c) 1979 Regents of the University of California */
#include <stdio.h>
char	ibuf[BUFSIZ];

main(argc, argv)
	int argc;
	char *argv[];
{
	register char *linp;
	FILE *f;
	register int c;
	char line[1000];
	
	setbuf(stdout, ibuf);
	argc--;
	argv++;
	f = stdin;
	do {
		if (argc > 0) {
			close(0);
			if ((f=fopen(argv[0], "r")) < 0) {
				perror(argv[0]);
				exit(1);
			}
			argc--, argv++;
		}
		for (;;) {
			linp = line;
			for (;;) {
				c = getc(f);
				if (c == '\n' || c == -1)
					break;
				*linp++ = c;
			}
			if (linp == line) {
				if (c == '\n')
					printf("\n");
			} else {
				*linp = 0;
				printf("%s\r%s\n", line, line);
			}
			if (c == -1)
				break;
		}
	} while (argc > 0);
	fflush(stdout);
	exit(0);
}
