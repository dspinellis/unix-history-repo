/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)process.h	5.1 (Berkeley) %G%
 */

/*
 * Definitions for process module.
 *
 * This module contains the routines to manage the execution and
 * tracing of the debuggee process.
 */

typedef struct process PROCESS;

PROCESS *process;

initstart();		/* initial process start up */
run();			/* start program running */
arginit();		/* initialize program arguments */
setargs();		/* special argument handling */
newarg();		/* add a new argument to list for program */
inarg();		/* set standard input for program */
outarg();		/* set standard output for program */
cont();			/* continue execution where last left off */
step();			/* single step */
stepc();		/* single step command */
stepto();		/* execute up to a given address */
next();			/* single step, skip over calls */
endprogram();		/* note the termination of the program */
printstatus();		/* print current error */
printwhere();		/* print current source line and file */
BOOLEAN isfinished();	/* TRUE if process has terminated */
iread(), dread();	/* read from the process' address space */
iwrite(), dwrite();	/* write to the process' address space */
