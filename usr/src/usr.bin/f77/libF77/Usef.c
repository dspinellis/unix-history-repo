/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)Usef.c	1.5 (Berkeley) %G%";
#endif /* not lint */

/*		returns '-f' if need to use -f to bypass C bug		*/

static char *needs_f[] = {
	"besj0_", "besj1_", "besjn_", "besy0_", "besy1_", "besyn_",
	"c_abs", "erf_", "erfc_", "r_abs", "r_acos", "r_asin",
	"r_atan", "r_atn2", "r_cos", "r_cosh", "r_exp", "r_imag",
	"r_int", "r_lg10", "r_log", "r_sign", "r_sin",
	"r_sinh", "r_sqrt", "r_tan", "r_tanh", "rand_", "random_",
	0,
	};

main(argc, argv)
int argc;
char **argv;
{
	char **ptr;
	float f;

	if (sizeof (f + f) != sizeof f)
	{
		argv++;
		ptr = needs_f;
		while( *ptr != 0 ) {
			if( strcmp( *ptr++, *argv ) == 0 )
			{
				printf("-f");
				exit(0);
			}
		}
	}
	printf(" ");
	exit(0);
}
