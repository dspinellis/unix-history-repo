/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)process.h	5.2 (Berkeley) 1/3/88
 */

/*
 * Definitions for process module.
 *
 * This module contains the routines to manage the execution and
 * tracing of the debuggee process.
 */

typedef struct process PROCESS;

PROCESS *process;

int initstart();	/* initial process start up */
int run();		/* start program running */
int arginit();		/* initialize program arguments */
int setargs();		/* special argument handling */
int newarg();		/* add a new argument to list for program */
int inarg();		/* set standard input for program */
int outarg();		/* set standard output for program */
int cont();		/* continue execution where last left off */
int step();		/* single step */
int stepc();		/* single step command */
int stepto();		/* execute up to a given address */
int next();		/* single step, skip over calls */
int endprogram();	/* note the termination of the program */
int printstatus();	/* print current error */
int printwhere();	/* print current source line and file */
BOOLEAN isfinished();	/* TRUE if process has terminated */
int iread(), dread();	/* read from the process' address space */
int iwrite(), dwrite();	/* write to the process' address space */
