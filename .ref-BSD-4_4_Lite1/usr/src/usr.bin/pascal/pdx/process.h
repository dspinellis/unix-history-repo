/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)process.h	8.1 (Berkeley) 6/6/93
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
