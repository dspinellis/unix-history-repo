/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)pxinfo.h	5.3 (Berkeley) %G%
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
 *	the address of the program counter used in the interpreter procedure
 *		(px actually keeps the pc in a register if it can, but stores
 *		 it in this location each time around the interpreter loop.)
 *	the address of the main interpreter loop (past the store of pc)
 */

#ifdef tahoe
typedef struct {
	short trp_savemask;
	short trp_removed;
	int trp_oldfp;
	ADDRESS *disp;
	ADDRESS *dp;
	ADDRESS objstart;
	ADDRESS pcaddr;
	ADDRESS loopaddr;
} TRAPARGS;
#else
typedef struct {
	int nargs;
	ADDRESS *disp;
	ADDRESS *dp;
	ADDRESS objstart;
	ADDRESS pcaddr;
	ADDRESS loopaddr;
} TRAPARGS;
#endif

ADDRESS *DISPLAY;
ADDRESS *DP;
ADDRESS ENDOFF;
ADDRESS PCADDR;
ADDRESS LOOPADDR;
#ifdef tahoe
ADDRESS RETLOC;
ADDRESS INTFP;
#endif
