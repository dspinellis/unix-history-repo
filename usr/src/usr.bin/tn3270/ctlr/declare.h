/*
 * Declarations of routines from the controller.
 *
 *	@(#)declare.h	1.2 (Berkeley) %G%
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
