/*
 *	@(#)frame.h	7.1 (Berkeley) 5/21/88
 */

/*
 * Definition of the tahoe call frame.
 */
struct frame {
	int	fr_savpc;		/* saved program counter */
	u_int	fr_mask:16,		/* register save mask */
		fr_removed:16;		/* 4*number of arguments + 4 */
	int	fr_savfp;		/* saved frame pointer */
};
