/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)escseq.c	5.1 (Berkeley) 4/30/85";
#endif not lint

/*
 * escseq: get us out of any escape sequence we are in the middle of
 * and put us into the requested kind of escape sequence.
 */

#include "2648.h"

escseq(mode)
int mode;
{
	if (mode == _escmode)
		return;
	/* Get out of previous mode */
	switch (_escmode) {
	case NONE:
		break;
	case ESCD:
		if (mode == TEXT) {
			outchar('s');
			_escmode = mode;
			return;
		}
	case ESCP:
	case ESCM:
		outchar('Z');	/* no-op */
		break;
	case TEXT:
		outstr("\33*dT");
		break;
	}
	/* Get into new mode */
	switch (_escmode = mode) {
	case NONE:
		break;
	case ESCD:
		outstr("\33*d");
		break;
	case ESCP:
		outstr("\33*p");
		break;
	case ESCM:
		outstr("\33*m");
		break;
	case TEXT:
		outstr("\33*dS");
		break;
	}
}
