/*
 * External references from the controller.
 */

#if	!defined(MSDOS)
extern char *access_api();
extern void movetous(), movetothem(), unaccess_api();
#endif	/* !defined(MSDOS) */

extern int
	TransparentClock,
	OutputClock,
	UnLocked;		/* keyboard is UnLocked? */
extern void
	ConnectScreen(),
	init_inbound(),
	LocalClearScreen(),
	RefreshScreen(),
	RingBell(),
	StopScreen(),
	TransOut(),
	TransStop();
