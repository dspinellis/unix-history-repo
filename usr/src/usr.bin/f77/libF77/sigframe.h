/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 *
 *	@(#)sigframe.h	5.2 (Berkeley) 4/12/91
 */

/*
 * Definition of signal handler frame.
 */

struct sigframe {
	int	sf_signum;
	int	sf_code;
	struct	sigcontext *sf_scp;
	int	(*sf_handler)();
	int	r1;
	int 	r0;
	struct	sigcontext *sf_scpcopy;
}; 

