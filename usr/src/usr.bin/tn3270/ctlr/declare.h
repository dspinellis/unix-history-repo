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
