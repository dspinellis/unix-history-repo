/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)exect.s	5.1 (Berkeley) %G%";
#endif not lint

#include "SYS.h"
#include <machine/psl.h>

ENTRY(exect)
	bispsw	$PSL_T
	chmk	$SYS_execve
	jmp	cerror		# exect(file, argv, env)
