extern	int fout;
int	ibuf[259];

main(argc, argv)
	int argc;
	char *argv[];
{
	register char *linp;
	register int c;
	char line[1000];
	
	fout = dup(1);
	argc--;
	argv++;
	do {
		if (argc > 0) {
			close(0);
			if (fopen(argv[0], ibuf) < 0) {
				perror(argv[0]);
				exit(1);
			}
		}
		for (;;) {
			linp = line;
			for (;;) {
				c = getc(ibuf);
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
	flush();
	exit(0);
}
