/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)isactive.c 1.1 %G%";

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
