/*
 * Globbing for MS-DOS.  Relies on the expansion done by the library
 * startup code. (dds)
 */

#include <stdio.h>
#include <string.h>

main(int argc, char *argv[])
{
	register i;

	for (i = 1; i < argc; i++) {
		fputs(strlwr(argv[i]), stdout);
		putchar(0);
	}
}
