/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)machine.h	5.4 (Berkeley) %G%
 */

/*
 * Definitions for the machine module.
 *
 * This module has the low level machine interface.  This consists
 * mostly of dealing with machine instructions and also setting
 * and unsetting breakpoints.
 */

ADDRESS pc;			/* current program counter */
LINENO curline;			/* line number associated with pc */
SYM *curfunc;			/* pointer to active function symbol */

int setbp();			/* set a breakpoint */
int unsetbp();			/* unset a breakpoint */
BOOLEAN isbperr();		/* test if a breakpoint has occurred */
int printerror();		/* print out an execution error message */
ADDRESS nextaddr();		/* address of next line to be executed */
BOOLEAN isendofproc();		/* test if address is at end of procedure */
int printinst(), printninst();	/* print the instruction at a given address */
int printdata(), printndata();	/* print the contents of a given data address */
