#include <stdio.h>
/*
 * soelim - a filter to process n/troff input eliminating .so's
 *
 * Author: Bill Joy UCB July 8, 1977
 *
 * This program eliminates .so's from a n/troff input stream.
 * It can be used to prepare safe input for submission to the
 * phototypesetter since the software supporting the operator
 * doesn't let him do chdir.
 *
 * This is a kludge and the operator should be given the
 * ability to do chdir.
 *
 * This program is more generally useful, it turns out, because
 * the program tbl doesn't understand ".so" directives.
 */

main(argc, argv)
	int argc;
	char *argv[];
{

	argc--;
	argv++;
	if (argc == 0) {
		fprintf(stderr, "Usage: %s file [ file ... ]\n", argv[-1]);
		exit(1);
	}
	do {
		process(argv[0]);
		argv++;
		argc--;
	} while (argc > 0);
	exit(0);
}

process(file)
	char *file;
{
	register char *cp;
	register int c;
	char fname[BUFSIZ];
	FILE *soee;

	soee = fopen(file, "r");
	if (soee == NULL) {
		perror(file);
		return;
	}
	for (;;) {
		c = getc(soee);
		if (c < 0)
			break;
		if (c != '.')
			goto simple;
		c = getc(soee);
		if (c != 's') {
			putchar('.');
			goto simple;
		}
		c = getc(soee);
		if (c != 'o') {
			printf(".s");
			goto simple;
		}
		do
			c = getc(soee);
		while (c == ' ' || c == '\t');
		cp = fname;
		for (;;) {
			switch (c) {

			case ' ':
			case '\t':
			case '\n':
			case EOF:
				goto donename;

			default:
				*cp++ = c;
				c = getc(soee);
				continue;
			}
		}
donename:
		if (cp == fname) {
			printf(".so");
			goto simple;
		}
		*cp++ = 0;
		process(fname);
		continue;
simple:
		if (c == EOF)
			break;
		putchar(c);
	}
	fclose(soee);
}
