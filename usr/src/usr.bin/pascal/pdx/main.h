/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)main.h	5.1 (Berkeley) %G%
 */

*
 * Definitions for main program.
 *
 * The main program just handles the command arguments and then
 * gives control to the command module.  It's also the center of
 * error recovery, since non-fatal errors longjmp into the main routine.
 */

BOOLEAN opt[26];	/* true if command line option given */

#define option(c)	opt[(c)-'a']
#define isterm(file)	(option('i') || isatty(fileno(file)))

main();			/* debugger main routine */
init();			/* read in source and object data */
erecover();		/* does non-local goto for error recovery */
quit();			/* clean-up before exiting */
