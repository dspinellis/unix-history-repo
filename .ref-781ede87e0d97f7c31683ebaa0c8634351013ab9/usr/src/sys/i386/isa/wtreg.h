/* 
 * Mach Operating System
 * Copyright (c) 1989 Carnegie-Mellon University
 * All rights reserved.  The CMU software License Agreement specifies
 * the terms and conditions for use and redistribution.
 *	@(#)wtreg.h	1.1 (Berkeley) %G%
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
