/*
 *  Interpress utilities
 *
 * Copyright (c) 1984, 1985, 1986 Xerox Corp.
 *
 * HISTORY 
 * 17-Jun-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Added warning about Op() macro.
 *
 *
 *  Written for Xerox Corporation by William LeFebvre
 *  30-May-1984
 *
 */

/*
 *  Subroutines to help build interpress files:
 *
 *  literal interface level - these routines produce interpress output at
 *			      the token level.
 */

/*
 *  This file contains the macro definitions for some of the literal
 *  operations.  This is done for efficiency reasons.
 */

# define    append_short_number(number)	\
		append_n_byte_int((long)(number + INTEGER_ZERO), 2)

# define    AppendIdentifier(string)	\
		append_Sequence(sequenceIdentifier, strlen(string), (unsigned char *)string)

# define    AppendString(string)	\
		append_Sequence(sequenceString, strlen(string), (unsigned char *)string)

# define    AppendComment(string)	\
		append_Sequence(sequenceComment, strlen(string), (unsigned char *)string)

# define    AppendInsertFile(string)	\
		append_Sequence(sequenceInsertFile, strlen(string), (unsigned char *)string)

/*
 * An abbreviation for AppendOp: 
 *	using this guy is really a bad idea because it depends on a bug
 *	in the C compiler.  While it will work for PCC, the Tartan compiler
 *	will choke on it.  Don't use this feature if you want your code to
 *	be portable.
 */
#ifndef lint
# define    Op(string)		AppendOp((unsigned)OP_/**/string)
#else
# define    Op(string)		AppendOp(1)  /* is this the right thing? */
#endif
