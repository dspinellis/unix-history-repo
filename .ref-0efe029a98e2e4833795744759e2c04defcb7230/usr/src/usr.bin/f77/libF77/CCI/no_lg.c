/*
 *	@(#)no_lg.c	5.1 (Berkeley) %G%
 */

/*
 *	@(#)no_lg.c	1.2
 *
 *	ld -lg ...
 * then /usr/lib/libg.a is loaded and defines _dbsubc as non-zero.
 * Otherwise, this is loaded defining it as zero.
 * in main.c, f77_abort() uses this to decide whether or not to call
 * abort to terminate.
 */

char id_no_lg[] = "@(#)no_lg.c	5.2";

int _dbsubc = 0;
