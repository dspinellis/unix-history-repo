/* @(#)testsys.c	4.1 (Berkeley) 1/1/83 */
/*
 * This dinky program repeatedly reads two filenames from standard input
 * and prints out the remapped filenames.
 * At EOF, it effectively chdir(II)s to /usr/spool/uucp
 * and repeats the above.
 */

#include <stdio.h>

char b1[100], b2[100];

extern char *_fixf(), *_fixf2();

main()
{
	register char *s1, *s2;

top:
	while (gets(b1)) {
		if (gets(b2) == NULL)
			exit(1);
		s1 = _fixf(b1);
		s2 = _fixf2(b2);
		printf("%s, %s\n", s1, s2);
	}
	_savfile("/usr/spool/uucp");
	goto top;
}
