/*
 * set flag to initialize fortran-66 mods
 *	@(#)libI66.c	1.2
 * usage: f77 ... -lI66 ...
 */

short ccntrl_ = 1;	/* carriage control on all units */
short blzero_ = 1;	/* blanks are zero input, 0 => 0.0 on output */
short opnbof_ = 1;	/* open files at beginning */
