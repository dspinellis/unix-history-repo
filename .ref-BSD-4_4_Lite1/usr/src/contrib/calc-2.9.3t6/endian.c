/*
 * endian - Determine the byte order of a long on your machine.
 *
 * Big Endian:	    Amdahl, 68k, Pyramid, Mips, Sparc, ...
 * Little Endian:   Vax, 32k, Spim (Dec Mips), i386, i486, ...
 */
/*
 * Copyright (c) 1993 by Landon Curt Noll.  All Rights Reserved.
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright, this permission notice and text
 * this comment, and the disclaimer below appear in all of the following:
 *
 *	supporting documentation
 *	source copies
 *	source works derived from this source
 *	binaries derived from this source or from derived source
 *
 * LANDON CURT NOLL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO
 * EVENT SHALL LANDON CURT NOLL BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * chongo was here	/\../\
 */

#include <stdio.h>

/* byte order array */
char byte[8] = { (char)0x12, (char)0x36, (char)0x48, (char)0x59,
		 (char)0x01, (char)0x23, (char)0x45, (char)0x67 };

main()
{
    /* pointers into the byte order array */
    int *intp = (int *)byte;
#if defined(DEBUG)
    short *shortp = (short *)byte;
    long *longp = (long *)byte;

    printf("byte: %02x %02x %02x %02x %02x %02x %02x %02x\n",
	byte[0], byte[1], byte[2], byte[3],
	byte[4], byte[5], byte[6], byte[7]);
    printf("short: %04x %04x %04x %04x\n",
	shortp[0], shortp[1], shortp[2], shortp[3]);
    printf("int: %08x %08x\n",
	intp[0], intp[1]);
    printf("long: %08x %08x\n",
	longp[0], longp[1]);
#endif

    /* Print the standard <machine/endian.h> defines */
    printf("#define BIG_ENDIAN\t4321\n");
    printf("#define LITTLE_ENDIAN\t1234\n");

    /* Determine byte order */
    if (intp[0] == 0x12364859) {
	/* Most Significant Byte first */
	printf("#define BYTE_ORDER\tBIG_ENDIAN\n");
    } else if (intp[0] == 0x59483612) {
	/* Least Significant Byte first */
	printf("#define BYTE_ORDER\tLITTLE_ENDIAN\n");
    } else {
	fprintf(stderr, "Unknown int Byte Order, set BYTE_ORDER in Makefile\n");
	exit(1);
    }
    exit(0);
}
