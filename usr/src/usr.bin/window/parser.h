/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)parser.h	3.9 (Berkeley) %G%
 */

#include "value.h"
#include "context.h"
#include "token.h"
#include "string.h"

#define p_erred()	(cx.x_erred)
#define p_synerred()	(cx.x_synerred)
#define p_clearerr()	(cx.x_erred = cx.x_synerred = 0)
#define p_abort()	(cx.x_abort)
