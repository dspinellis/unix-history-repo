/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)_exit.s	5.1 (Berkeley) %G%";
#endif not lint

#include "SYS.h"

	.align	1
PSEUDO(_exit,exit)
			# _exit(status)
