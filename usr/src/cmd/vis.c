#include <stdio.h>
#defie	MAXCOL	72	/* fold after here */

/*
 *	vis - make all funny characters visible
 */

main(argc, argv)
char *argv[];
{
	FILE *fp;

	if (argc <= 1)
		vis(stdin);
	else
		while (--argc > 0) {
			if ((fp = fopen(*++argv, "r")) == NULL) {
				fprintf(stderr, "vis: can't open %s\n", *argv);
				exit(1);
			}
			vis(fp);
			fclose(fp);
		}
	exit(0);
}

vis(fp)	/* make visible output of file fp */
FILE *fp;
{
	int c, col=0;

	while ((c = getc(fp)) != EOF) {
		if (c == '\n' || c == '\t' || c >= ' ' && c <= 0176) {
			putchar(c);
			col++;
		} else {
			printf("\\%03o", c);
			col += 4;
		}
		if (c == '\n')
			col = 0;
		if (col > MAXCOL) {
			printf("\\\n");
			col = 0;
		}
	}
}
