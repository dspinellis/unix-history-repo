#include <stdio.h>

main(argc, argv)
char **argv;
{
	register bflg, c, n;
	int undc=8;

	bflg = 1;
	if (argc>1 && argv[1][0]=='-') {
		undc = atoi(&argv[1][1]);
		if (undc>16)
			undc = 0;
		argc--;
		argv++;
	}
	if (argc>1) {
		if ((freopen(argv[1], "r", stdin))==NULL) {
			fprintf(stderr, "Cannot open %s\n", argv[1]);
			exit(1);
		}
	}
	while((c = getchar()) != EOF) {
		if(c == '\n' || c == 014) {
			bflg = 1;
			putchar(c);
			continue;
		}
		if(bflg) {
			for (n=0; n<undc; n++)
				putchar('\b');
			bflg = 0;
		}
		putchar(c);
	}
	return(0);
}
