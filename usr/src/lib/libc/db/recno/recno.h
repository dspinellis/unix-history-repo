/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)recno.h	8.1 (Berkeley) %G%
 */

enum SRCHOP { SDELETE, SINSERT, SEARCH};	/* Rec_search operation. */

#include "../btree/btree.h"
#include "extern.h"
