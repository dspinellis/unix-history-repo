/*-
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)telextrn.h	4.3 (Berkeley) %G%
 */

/*
 * Definitions of external routines and variables for tn3270
 */

/*
 * Pieces exported from the telnet susbsection.
 */

extern int
#if defined(unix)
	HaveInput,
#endif /* defined(unix) */
	tout,
	tin;

extern char	*transcom;

extern int
	netflush(),
	quit(),
	TtyChars(),
	DataToTerminal();

extern void
	outputPurge(),
	EmptyTerminal(),
	StringToTerminal(),
	ExitPerror(),
	setcommandmode();

/*
 * Pieces exported from other random locations.
 */

extern char
	*strsave();
