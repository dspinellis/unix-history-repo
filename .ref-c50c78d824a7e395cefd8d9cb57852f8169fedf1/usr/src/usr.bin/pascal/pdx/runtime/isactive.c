/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)isactive.c	5.1 (Berkeley) %G%";
#endif not lint
/*
 * Decide a the given function is currently active.
 */

#include "defs.h"
#include "runtime.h"
#include "frame.rep"
#include "sym.h"
#include "machine.h"
#include "process.h"

BOOLEAN isactive(f)
SYM *f;
{
	if (isfinished(process)) {
		return(FALSE);
	} else {
		if (f == program) {
			return(TRUE);
		}
		return(findframe(f) != NIL);
	}
}
