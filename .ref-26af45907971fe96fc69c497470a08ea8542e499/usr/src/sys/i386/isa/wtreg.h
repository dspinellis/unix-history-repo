/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)wtreg.h	7.1 (Berkeley) %G%
 */

/*
 *
 * Copyright (c) 1989 Carnegie-Mellon University.
 * All rights reserved.
 *
 * Authors: Robert Baron
 * 
 * Permission to use, copy, modify and distribute this software and
 * its documentation is hereby granted, provided that both the copyright
 * notice and this permission notice appear in all copies of the
 * software, derivative works or modified versions, and any portions
 * thereof, and that both notices appear in supporting documentation.
 * 
 * CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS "AS IS" 
 * CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND 
 * FOR ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 * 
 * Carnegie Mellon requests users of this software to return to
 *
 *  Software Distribution Coordinator  or  Software.Distribution@CS.CMU.EDU
 *  School of Computer Science
 *  Carnegie Mellon University
 *  Pittsburgh PA 15213-3890
 *
 * any improvements or extensions that they make and grant Carnegie the
 * rights to redistribute these changes.
 */

/* 
 * HISTORY
 * $Log:	wtreg.h,v $
 * Revision 2.2.1.1  90/01/08  13:29:25  rvb
 * 	Add Intel copyright.
 * 	[90/01/08            rvb]
 * 
 * Revision 2.2  89/09/25  12:33:09  rvb
 * 	Driver was provided by Intel 9/18/89.
 * 	[89/09/23            rvb]
 * 
 */

/*
 *
 *  Copyright 1988, 1989 by Intel Corporation
 *
 */

/*
 *  wtioctl.h
 *   defines ioctl parameters for direct QIC commands
 */

#define	WTIOC	('W'<<8)
#define	WTQICMD	(WTIOC|0)

/* QIC commands allowed */
#define	SELECT	0x01
#define	REWIND	0x21
#define	ERASE	0x22
#define	RETENS	0x24
