/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)externs.h	1.3 (Berkeley) %G%
 */

/*
 * External references from the controller.
 */

#if	!defined(MSDOS)
extern char *access_api();
extern void movetous(), movetothem(), unaccess_api();
#endif	/* !defined(MSDOS) */

extern unsigned char
	*memNSchr();	/* Search for a character ANDED, increment by stride */

extern int
	DataToNetwork(),
	OutputClock,
	suspend(),
	TransparentClock,
	UnLocked;		/* keyboard is UnLocked? */

extern void
	command(),
	ConnectScreen(),
	ExitString(),
	init_inbound(),
	LocalClearScreen(),
	RefreshScreen(),
	RingBell(),
	setconnmode(),
	StopScreen(),
	TransOut(),
	TransStop();
