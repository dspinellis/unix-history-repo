/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)printnews.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * Print out news during single step tracing.
 *
 * We have to handle all the single stepping possibilities,
 * including combinations.  A combination of single stepping
 * by line and by instruction causes "curline" to be 0 but
 * "ss_lines" to be TRUE.  We avoid trying to print lines in this case.
 */

#include "defs.h"
#include "breakpoint.h"
#include "sym.h"
#include "source.h"
#include "object.h"
#include "mappings.h"
#include "machine.h"

printnews()
{
    if (ss_variables) {
	prvarnews();
    }
    if (trcond()) {
	if (ss_lines && curline > 0) {
	    skimsource(srcfilename(pc));
	    printf("trace:  ");
	    printlines(curline, curline);
	}
	if (ss_instructions) {
	    printf("inst trace: ");
	    printinst(pc, pc);
	}
    }
    bpact();
    if (stopcond()) {
	isstopped = TRUE;
	curline = srcline(pc);
	printstatus();
    }
}
