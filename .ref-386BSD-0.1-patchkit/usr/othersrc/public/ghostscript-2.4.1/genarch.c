/* genarch.c */
/* Generate a header file (arch.h) with parameters */
/* reflecting the machine architecture and compiler characteristics. */

#include <stdio.h>

/* We should write the result on stdout, but the Turbo C 'make' */
/* can't handle output redirection (sigh). */

main(argc, argv)
    int argc;
    char *argv[];
{	char *fname = argv[1];
	long one = 1;
	long lm1 = -1;
	long lr1 = lm1 >> 1, lr2 = lm1 >> 2;
	unsigned long um1 = ~(unsigned long)0;
	int im1 = -1;
	int ir1 = im1 >> 1, ir2 = im1 >> 2;
	int ars;
	int lwidth = sizeof(long) * 8;
	float f0 = 0.0, f1 = 1.0, fm1 = -1.0;
	FILE *f = fopen(fname, "w");
	if ( f == NULL )
	   {	fprintf(stderr, "genarch.c: can't open %s for writing\n", fname);
		exit(1);
	   }
	fprintf(f, "/* Parameters derived from machine and compiler architecture */\n\n");
	fprintf(f, "#define arch_is_big_endian %d\n", 1 - *(char *)&one);
	fprintf(f, "#define arch_sizeof_short %d\n", sizeof(short));
	fprintf(f, "#define arch_sizeof_int %d\n", sizeof(int));
	fprintf(f, "#define arch_sizeof_long %d\n", sizeof(long));
	fprintf(f, "#define arch_floats_are_IEEE %d\n",
		(*(long *)&f0 == 0 && *(long *)&f1 == 0x3f800000L &&
		 *(long *)&fm1 == 0xbf800000L ? 1 : 0));
	/* There are three cases for arithmetic right shift: */
	/* always correct, correct except for right-shifting a long by 1 */
	/* (a bug in some versions of the Turbo C compiler), and */
	/* never correct. */
	ars = (lr2 != -1 || ir1 != -1 || ir2 != -1 ? 0 :
		lr1 != -1 ? 1 :		/* Turbo C problem */
		2);
	fprintf(f, "#define arch_arith_rshift %d\n", ars);
	/* Some machines can't handle a variable shift by */
	/* the full width of a long. */
	fprintf(f, "#define arch_can_shift_full_long %d\n",
		um1 >> lwidth == 0);
	fclose(f);
	return 0;
}
