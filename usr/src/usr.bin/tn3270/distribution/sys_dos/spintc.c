/*
 *	Copyright (c) 1984-1987 by the Regents of the
 *	University of California and by Gregory Glenn Minshall.
 *
 *	Permission to use, copy, modify, and distribute these
 *	programs and their documentation for any purpose and
 *	without fee is hereby granted, provided that this
 *	copyright and permission appear on all copies and
 *	supporting documentation, the name of the Regents of
 *	the University of California not be used in advertising
 *	or publicity pertaining to distribution of the programs
 *	without specific prior permission, and notice be given in
 *	supporting documentation that copying and distribution is
 *	by permission of the Regents of the University of California
 *	and by Gregory Glenn Minshall.  Neither the Regents of the
 *	University of California nor Gregory Glenn Minshall make
 *	representations about the suitability of this software
 *	for any purpose.  It is provided "as is" without
 *	express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)spintc.c	3.1 (Berkeley) %G%";
#endif	/* not lint */

#include <stdio.h>
#include <dos.h>
#include <stdlib.h>

#include "../general/general.h"
#include "spint.h"

#define	PSP_ENVIRONMENT		0x2c
#define	PSP_FCB1		0x5c
#define	PSP_FCB2		0x6c

typedef struct {
    int
	environment,		/* Segment address of environment */
	cmd_ptr_offset,		/* Offset of command to execute */
	cmd_ptr_segment,	/* Segment where command lives */
	fcb1_ptr_offset,	/* Offset of FCB 1 */
	fcb1_ptr_segment,	/* Segment of FCB 1 */
	fcb2_ptr_offset,	/* Offset of FCB 2 */
	fcb2_ptr_segment;	/* Segment of FCB 2 */
} ExecList;


static int int_offset, int_segment;


void
spint_finish(spint)
Spint *spint;
{
    union REGS regs;
    struct SREGS sregs;

    if (spint->done == 0) {
	return;				/* Not done yet */
    }

    /*
     * Restore old interrupt handler.
     */

    regs.h.ah = 0x25;
    regs.h.al = spint->int_no;
    regs.x.dx = int_offset;
    sregs.ds = int_segment;
    intdosx(&regs, &regs, &sregs);

    if (spint->regs.x.cflag) {
	fprintf(stderr, "0x%x return code from EXEC.\n", spint->regs.x.ax);
	spint->done = 1;
	spint->rc = 99;
	return;
    }

    regs.h.ah = 0x4d;			/* Get return code */

    intdos(&regs, &regs);

    spint->rc = regs.x.ax;
}

void
spint_continue(spint)
Spint *spint;
{
    _spint_continue(spint);		/* Return to caller */
    spint_finish(spint);
}


void
spint_start(command, spint)
char *command;
Spint *spint;
{
    ExecList mylist;
    char *comspec;
    void _spint_int();
    union REGS regs;
    struct SREGS sregs;

    /*
     * Get comspec.
     */
    comspec = getenv("COMSPEC");
    if (comspec == 0) {			/* Can't find where command.com is */
	fprintf(stderr, "Unable to find COMSPEC in the environment.");
	spint->done = 1;
	spint->rc = 99;	/* XXX */
	return;
    }

    /*
     * Now, hook up our interrupt routine.
     */

    regs.h.ah = 0x35;
    regs.h.al = spint->int_no;
    intdosx(&regs, &regs, &sregs);

    /* Save old routine */
    int_offset = regs.x.bx;
    int_segment = sregs.es;

    regs.h.ah = 0x25;
    regs.h.al = spint->int_no;
    regs.x.dx = (int) _spint_int;
    segread(&sregs);
    sregs.ds = sregs.cs;
    intdosx(&regs, &regs, &sregs);

    /*
     * Read in segment registers.
     */

    segread(&spint->sregs);

    /*
     * Set up registers for the EXEC call.
     */

    spint->regs.h.ah = 0x4b;
    spint->regs.h.al = 0;
    spint->regs.x.dx = (int) comspec;
    spint->sregs.es = spint->sregs.ds;		/* Superfluous, probably */
    spint->regs.x.bx = (int) &mylist;

    /*
     * Set up EXEC parameter list.
     */

    ClearElement(mylist);
    mylist.cmd_ptr_offset = (int) command;
    mylist.cmd_ptr_segment = spint->sregs.ds;
    mylist.fcb1_ptr_offset = PSP_FCB1;
    mylist.fcb1_ptr_segment = _psp;
    mylist.fcb2_ptr_offset = PSP_FCB2;
    mylist.fcb2_ptr_segment = _psp;
    mylist.environment = *((int far *)(((long)_psp<<16)|PSP_ENVIRONMENT));

    /*
     * Call to assembly language routine to actually set up for
     * the spint.
     */

    _spint_start(spint);

    spint_finish(spint);
}
