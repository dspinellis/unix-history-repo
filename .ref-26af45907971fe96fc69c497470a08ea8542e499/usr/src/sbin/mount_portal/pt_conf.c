/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 * All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pt_conf.c	8.1 (Berkeley) %G%
 *
 * $Id: pt_conf.c,v 1.2 1992/05/27 07:09:27 jsp Exp jsp $
 */

#include <sys/types.h>
#include <sys/param.h>
#include "portald.h"

provider providers[] = {
	{ "exec",	portal_exec },
	{ "file",	portal_file },
	{ "tcp",	portal_tcp },
	{ 0, 0 }
};
