/* A simple but fairly efficient C version of the Unix "wc" tool */

#include <stdio.h>
#include <ctype.h>

main()
{
	register int c, cc = 0, wc = 0, lc = 0;
	FILE *f = stdin;

	while ((c = getc(f)) != EOF) {
		++cc;
		if (isgraph(c)) {
			++wc;
			do {
				c = getc(f);
				if (c == EOF)
					goto done;
				++cc;
			} while (isgraph(c));
		}
		if (c == '\n')
			++lc;
	}
done:	printf( "%8d%8d%8d\n", lc, wc, cc );
}
