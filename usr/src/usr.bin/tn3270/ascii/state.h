/*
 *	@(#)state.h	3.1  10/29/86
 */

#define	INCLUDED_STATE

/* this defines the state structure used by the key mapping routines */

#define state	struct State
struct State {
    int		match;		/* character to match */
    int		result;		/* 3270 control code */
    state	*next;		/* next entry in this same state */
    state	*address;	/* if goto, where is next state */
};
