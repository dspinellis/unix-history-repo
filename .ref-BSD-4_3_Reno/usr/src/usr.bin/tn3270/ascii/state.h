/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)state.h	4.1 (Berkeley) 12/4/88
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
