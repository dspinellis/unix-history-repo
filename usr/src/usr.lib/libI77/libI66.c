/*
 * set flag to initialize fortran-66 mods
 *	@(#)libI66.c	1.4
 * usage: f77 ... -lI66 ...
 */

#include	"fiodefs.h"

struct ioiflg	ioiflg_ = {
	 0,		/* open files at beginning */
	 1,		/* carriage control on all units */
	 1,		/* blanks are zero on input; 0 => 0.0 on output */
};
