/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)pxinfo.h	5.2 (Berkeley) 4/7/87
 */

/*
 * This is the information we get after the initial trap that px does.
 * By passing the "-d" flag, we cause px to call a procedure with the
 * the following information:
 *
 *	address of the display
 *	address of the display pointer
 *	address of the beginning of the object code
 *
 *	the address of a variable containing the address of the first local
 *		variable in the interpreter procedure
 *		(this is used to find the frame where the pc is)
 *	the address of the main interpreter loop
 */

#ifdef tahoe
typedef struct {
	short trp_savemask;
	short trp_removed;
	int trp_oldfp;
	ADDRESS *disp;
	ADDRESS *dp;
	ADDRESS objstart;
	ADDRESS pcaddrp;
	ADDRESS loopaddr;
} TRAPARGS;
#else
typedef struct {
	int nargs;
	ADDRESS *disp;
	ADDRESS *dp;
	ADDRESS objstart;
	ADDRESS pcaddrp;
	ADDRESS loopaddr;
} TRAPARGS;
#endif

ADDRESS *DISPLAY;
ADDRESS *DP;
ADDRESS ENDOFF;
ADDRESS PCADDRP;
ADDRESS LOOPADDR;
ADDRESS *pcframe;
#ifdef tahoe
ADDRESS RETLOC;
ADDRESS INTFP;
#endif
