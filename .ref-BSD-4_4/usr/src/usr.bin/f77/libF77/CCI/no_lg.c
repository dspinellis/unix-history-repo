/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)no_lg.c	5.2 (Berkeley) 4/12/91";
#endif /* not lint */

/*
 *	@(#)no_lg.c	1.2
 *
 *	ld -lg ...
 * then /usr/lib/libg.a is loaded and defines _dbsubc as non-zero.
 * Otherwise, this is loaded defining it as zero.
 * in main.c, f77_abort() uses this to decide whether or not to call
 * abort to terminate.
 */

char id_no_lg[] = "@(#)no_lg.c	5.2";

int _dbsubc = 0;
