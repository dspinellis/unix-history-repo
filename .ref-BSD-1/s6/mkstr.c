#
/*
 * mkstr - create a string error message file by massaging C source
 *
 * Bill Joy UCB August 1977
 *
 * Based on an earlier program conceived by Bill Joy and Chuck Haley
 *
 * Program to create a string error message file
 * from a group of C programs.  Arguments are the name
 * of the file where the strings are to be placed, the
 * prefix of the new files where the processed source text
 * is to be placed, and the files to be processed.
 *
 * The program looks for 'error("' in the source stream.
 * Whenever it finds this, this and the following characters from the '"'
 * to a '"' are replaced by 'error(seekpt' where seekpt is a
 * pointer into the error message file.
 * If the '(' is not immediately followed by a '"' only the name change
 * occurs.
 *
 * The optional '-' causes strings to be added at the end of the
 * existing error message file for recompilation of single routines.
 */

extern	fout;
int	ibuf[259], sbuf[259];
int	stbuf[18];
char	addon;
char	*progname;
char	usagestr[]	"usage: %s [ - ] mesgfile prefix file ...\n";
char	name[100], *np;
int	seekpt;

main(argc, argv)
	int argc;
	char *argv[];
{

	argc--, progname = *argv++;
	if (argc > 1 && argv[0][0] == '-')
		addon++, argc--, argv++;
	if (argc < 3)
		printf(usagestr, progname), exit(1);
	if (addon == 0 || (sbuf[0] = open(argv[0], 1)) < 0)
		if (fcreat(argv[0], sbuf) < 0)
			perror(argv[0]), exit(1);
	fstat(sbuf[0], stbuf);
	seekpt = stbuf[5];
	seek(sbuf[0], 0, 2);
	argc--, argv++;
	fout = dup(1);
	strcpy(name, argv[0]);
	np = name + strlen(name);
	argc--, argv++;
	do {
		strcpy(np, argv[0]);
		close(1);
		if (creat(name, 0644) < 0)
			perror(name), exit(1);
		fout = dup(1);
		close(0);
		if (fopen(argv[0], ibuf) < 0)
			perror(argv[0]), exit(1);
		process();
		flush();
		close(fout);
		argc--, argv++;
	} while (argc > 0);
	fflush(sbuf);
}

process()
{
	register char *cp;
	register c;

	for (;;) {
		c = getc(ibuf);
again:
		if (c == -1)
			return;
		if (c != 'e') {
			putchar(c);
			continue;
		}
		c = nomatch("error(");
		if (c != -1)
			goto again;
		printf("error(");
		c = getc(ibuf);
		if (c != '"')
			putchar(c);
		else
			copystr();
	}
}

nomatch(ocp)
	char *ocp;
{
	register char *cp;
	register c;

	for (cp = ocp + 1; *cp; cp++) {
		c = getc(ibuf);
		if (c != *cp) {
			while (ocp < cp)
				putchar(*ocp++);
			return (c);
		}
	}
	return (-1);
}

copystr()
{
	register c, ch;
	
	printf("%d", seekpt);
	ch = -1;
	for (;;) {
		c = ch;
		ch = -1;
		if (c == -1)
			c = getc(ibuf);
		switch (c) {
			case '"':
				putc(0, sbuf);
				putc('\n', sbuf);
				seekpt =+ 2;
				return;
			case '\\':
				c = getc(ibuf);
				switch (c) {
					case 'b':
						c = '\b';
						break;
					case 't':
						c = '\t';
						break;
					case 'r':
						c = '\r';
						break;
					case 'n':
						c = '\n';
						break;
					case '\n':
						continue;
					case 'f':
						c = '\f';
						break;
					case '0':
						c = 0;
						break;
					case '\\':
						break;
					default:
						if (!octdigit(c))
							break;
						c =- '0';
						ch = getc(ibuf);
						if (!octdigit(ch))
							break;
						c =<< 7, c =+ ch - '0';
						ch = getc(ibuf);
						if (!octdigit(ch))
							break;
						c =<< 3, c=+ ch - '0', ch = -1;
						break;
				}
		}
		putc(c, sbuf), seekpt++;
	}
}

octdigit(c)
	char c;
{

	return (c >= '0' && c <= '7');
}

strcpy(to, from)
	register char *to, *from;
{

	while (*to++ = *from++)
		continue;
}
