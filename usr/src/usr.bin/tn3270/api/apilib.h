/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)apilib.h	3.2 (Berkeley) %G%
 */

/*
 * What one needs to specify
 */

extern int
    api_sup_errno,			/* Supervisor error number */
    api_sup_fcn_id,			/* Supervisor function id (0x12) */
    api_fcn_errno,			/* Function error number */
    api_fcn_fcn_id;			/* Function ID (0x6b, etc.) */
