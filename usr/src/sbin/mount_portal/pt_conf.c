/*
 * Copyright (c) 1992 The Regents of the University of California
 * Copyright (c) 1990, 1992 Jan-Simon Pendry
 * All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pt_conf.c	1.1 (Berkeley) %G%
 *
 * $Id: pt_conf.c,v 1.2 1992/05/27 07:09:27 jsp Exp jsp $
 */

#include <sys/types.h>
#include "portald.h"

provider providers[] = {
	{ "exec",	portal_exec },
	{ "file",	portal_file },
	{ "tcp",	portal_tcp },
	{ 0, 0 }
};
