#
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

#define	EOF	-1

extern	fout;

main(argc, argv)
	int argc;
	char *argv[];
{

	argc--;
	argv++;
	fout = dup(1);
	if (argc == 0) {
		printf("Usage: %s file [ file ... ]\n", argv[-1]);
		exit(1);
	}
	do {
		process(argv[0]);
		argv++;
		argc--;
	} while (argc > 0);
	flush();
	exit(0);
}

process(file)
	char *file;
{
	register char *cp;
	register char c;
	char fname[100];
	int ibuf[259];

	if (fopen(file, ibuf) < 0) {
		flush();
		perror(file);
		flush();
		exit(1);
	}
	for (;;) {
		c = getc(ibuf);
		if (c == EOF)
			break;
		if (c != '.')
			goto simple;
		c = getc(ibuf);
		if (c != 's') {
			putchar('.');
			goto simple;
		}
		c = getc(ibuf);
		if (c != 'o') {
			printf(".s");
			goto simple;
		}
		do
			c = getc(ibuf);
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
					c = getc(ibuf);
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
	close(ibuf[0]);
}
