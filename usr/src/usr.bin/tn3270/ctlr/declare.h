/*-
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)declare.h	4.2 (Berkeley) %G%
 */

/*
 * Declarations of routines from the controller.
 */

extern void
	AddHost(),
	DoReadModified(),
	DoReadBuffer(),
	OptInit(),
	SendToIBM(),
	SendTransparent();

extern int
	DataFrom3270(),
	DataFromNetwork(),
	OptOrder(),
	OutputClock,
	TransparentClock;
