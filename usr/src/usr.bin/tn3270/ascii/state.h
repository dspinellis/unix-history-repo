/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)state.h	3.2 (Berkeley) %G%
 */

#define	INCLUDED_STATE

/* this defines the state structure used by the key mapping routines */


#define	STATE_NULL	-1		/* Falls off edge */
#define	STATE_GOTO	-2		/* GOTO internal state */

#define state	struct State
struct State {
    int		match;		/* character to match */
    int		result;		/* 3270 control code */
    state	*next;		/* next entry in this same state */
    state	*address;	/* if goto, where is next state */
};
