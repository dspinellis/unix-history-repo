/*
 * External references from the controller.
 *
 *	@(#)externs.h	1.2 (Berkeley) %G%
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
