/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)runtime.h	5.1 (Berkeley) %G%
 */

/*
 * Definitions for the runtime environment.
 *
 * In general, runtime organization is language, machine, and
 * even compiler dependent.
 */

BOOLEAN isactive();		/* tell if a symbol is currently active */
ADDRESS entry_addr();		/* entry address of current proc/func */
ADDRESS entry_point();		/* point where environment is setup */
ADDRESS return_addr();		/* return address of current proc/func */
ADDRESS caller_addr();		/* entry of caller of current proc/func */
where();			/* print out currently active procedures */
dump();				/* dump the world */
callproc();			/* call a procedure */
procreturn();			/* return from a "call"-ed procedure */
ADDRESS address();		/* address of a variable */
ADDRESS firstline();		/* address of first line in a procedure */
findbeginning();		/* find address of beginning of a procedure */
runtofirst();			/* step to first line in current procedure */
ADDRESS lastaddr();		/* address of last line in program */
ADDRESS fparamaddr();		/* entry address of a function parameter */
