/*-
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)apilib.h	4.2 (Berkeley) %G%
 */

/*
 * What one needs to specify
 */

extern int
    api_sup_errno,			/* Supervisor error number */
    api_sup_fcn_id,			/* Supervisor function id (0x12) */
    api_fcn_errno,			/* Function error number */
    api_fcn_fcn_id;			/* Function ID (0x6b, etc.) */
