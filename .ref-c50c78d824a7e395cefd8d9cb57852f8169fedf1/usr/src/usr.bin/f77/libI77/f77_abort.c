/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)f77_abort.c	5.2	%G%
 *
 *	all f77 aborts eventually call f77_abort.
 *	f77_abort cleans up open files and terminates with a dump if needed,
 *	with a message otherwise.	
 *
 */

#include <signal.h>
#include "fio.h"

char *getenv();
extern int errno;
int _lg_flag;	/* _lg_flag is non-zero if -lg was specified to ld */

f77_abort( err_val, act_core )
{
	char first_char, *env_var;
	int core_dump;

	env_var = getenv("f77_dump_flag");
	first_char = (env_var == NULL) ? 0 : *env_var;

	signal(SIGILL, SIG_DFL);
	sigsetmask(0);			/* don't block */

	/* see if we want a core dump:
		first line checks for signals like hangup - don't dump then.
		second line checks if -lg specified to ld (e.g. by saying
			-g to f77) and checks the f77_dump_flag var. */
	core_dump = ((nargs() != 2) || act_core) &&
	    ( (_lg_flag && (first_char != 'n')) || first_char == 'y');

	if( !core_dump )
		fprintf(units[STDERR].ufd,"*** Execution terminated\n");

	f_exit();
	_cleanup();
	if( nargs() ) errno = err_val;
	else errno = -2;   /* prior value will be meaningless,
				so set it to undefined value */

	if( core_dump ) abort();
	else  exit( errno );
}
