/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* macros for getting at and setting the current argument count */

extern int	arg_supplied_p,	/* NO, YES, or YES_NODIGIT */
		arg_count;

#define YES_NODIGIT	2

#define arg_type()		arg_supplied_p
#define arg_value()		arg_count
#define set_is_an_arg(there_is)	{ arg_supplied_p = (there_is); }
#define set_arg_value(n)	{ arg_supplied_p = YES; arg_count = (n); }
#define negate_arg_value()	{ arg_count = -arg_count; }
#define clr_arg_value()		{ arg_supplied_p = NO; arg_count = 1; }
#define is_an_arg()		(arg_supplied_p != NO)
