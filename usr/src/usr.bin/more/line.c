/*
 * Copyright (c) 1988 Mark Nudleman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by Mark Nudleman and the University of California, Berkeley.  The
 * name of Mark Nudleman or the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)line.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * Routines to manipulate the "line buffer".
 * The line buffer holds a line of output as it is being built
 * in preparation for output to the screen.
 * We keep track of the PRINTABLE length of the line as it is being built.
 */

#include "less.h"

static char linebuf[1024];	/* Buffer which holds the current output line */
static char *curr;		/* Pointer into linebuf */
static int column;		/* Printable length, accounting for
				   backspaces, etc. */
/*
 * A ridiculously complex state machine takes care of backspaces 
 * when in BS_SPECIAL mode.  The complexity arises from the attempt
 * to deal with all cases, especially involving long lines with underlining,
 * boldfacing or whatever.  There are still some cases which will break it.
 *
 * There are four states:
 *	LN_NORMAL is the normal state (not in underline mode).
 *	LN_UNDERLINE means we are in underline mode.  We expect to get
 *		either a sequence like "_\bX" or "X\b_" to continue
 *		underline mode, or anything else to end underline mode.
 *	LN_BOLDFACE means we are in boldface mode.  We expect to get sequences
 *		like "X\bX\b...X\bX" to continue boldface mode, or anything
 *		else to end boldface mode.
 *	LN_UL_X means we are one character after LN_UNDERLINE
 *		(we have gotten the '_' in "_\bX" or the 'X' in "X\b_").
 *	LN_UL_XB means we are one character after LN_UL_X 
 *		(we have gotten the backspace in "_\bX" or "X\b_";
 *		we expect one more ordinary character, 
 *		which will put us back in state LN_UNDERLINE).
 *	LN_BO_X means we are one character after LN_BOLDFACE
 *		(we have gotten the 'X' in "X\bX").
 *	LN_BO_XB means we are one character after LN_BO_X
 *		(we have gotten the backspace in "X\bX";
 *		we expect one more 'X' which will put us back
 *		in LN_BOLDFACE).
 */
static int ln_state;		/* Currently in normal/underline/bold/etc mode? */
#define	LN_NORMAL	0	/* Not in underline, boldface or whatever mode */
#define	LN_UNDERLINE	1	/* In underline, need next char */
#define	LN_UL_X		2	/* In underline, got char, need \b */
#define	LN_UL_XB	3	/* In underline, got char & \b, need one more */
#define	LN_BOLDFACE	4	/* In boldface, need next char */
#define	LN_BO_X		5	/* In boldface, got char, need \b */
#define	LN_BO_XB	6	/* In boldface, got char & \b, need same char */

public char *line;		/* Pointer to the current line.
				   Usually points to linebuf. */

extern int bs_mode;
extern int tabstop;
extern int bo_width, be_width;
extern int ul_width, ue_width;
extern int sc_width, sc_height;

/*
 * Rewind the line buffer.
 */
	public void
prewind()
{
	line = curr = linebuf;
	ln_state = LN_NORMAL;
	column = 0;
}

/*
 * Append a character to the line buffer.
 * Expand tabs into spaces, handle underlining, boldfacing, etc.
 * Returns 0 if ok, 1 if couldn't fit in buffer.
 */

#define	NEW_COLUMN(newcol)	if ((newcol) + ((ln_state)?ue_width:0) > sc_width) \
					return (1); else column = (newcol)

	public int
pappend(c)
	int c;
{
	if (c == '\0')
	{
		/*
		 * Terminate any special modes, if necessary.
		 * Append a '\0' to the end of the line.
		 */
		switch (ln_state)
		{
		case LN_UL_X:
			curr[0] = curr[-1];
			curr[-1] = UE_CHAR;
			curr++;
			break;
		case LN_BO_X:
			curr[0] = curr[-1];
			curr[-1] = BE_CHAR;
			curr++;
			break;
		case LN_UL_XB:
		case LN_UNDERLINE:
			*curr++ = UE_CHAR;
			break;
		case LN_BO_XB:
		case LN_BOLDFACE:
			*curr++ = BE_CHAR;
			break;
		}
		ln_state = LN_NORMAL;
		*curr = '\0';
		return (0);
	}

	if (curr > linebuf + sizeof(linebuf) - 12)
		/*
		 * Almost out of room in the line buffer.
		 * Don't take any chances.
		 * {{ Linebuf is supposed to be big enough that this
		 *    will never happen, but may need to be made 
		 *    bigger for wide screens or lots of backspaces. }}
		 */
		return (1);

	if (bs_mode == BS_SPECIAL)
	{
		/*
		 * Advance the state machine.
		 */
		switch (ln_state)
		{
		case LN_NORMAL:
			if (curr <= linebuf + 1 || curr[-1] != '\b')
				break;

			if (c == curr[-2])
				goto enter_boldface;
			if (c == '_' || curr[-2] == '_')
				goto enter_underline;
			curr -= 2;
			break;

enter_boldface:
			/*
			 * We have "X\bX" (including the current char).
			 * Switch into boldface mode.
			 */
			if (column + bo_width + be_width + 1 >= sc_width)
				/*
				 * Not enough room left on the screen to 
				 * enter and exit boldface mode.
				 */
				return (1);

			if (bo_width > 0 && 
			    curr > linebuf + 2 && curr[-3] == ' ')
			{
				/*
				 * Special case for magic cookie terminals:
				 * if the previous char was a space, replace 
				 * it with the "enter boldface" sequence.
				 */
				curr[-3] = BO_CHAR;
				column += bo_width-1;
			} else
			{
				curr[-1] = curr[-2];
				curr[-2] = BO_CHAR;
				column += bo_width;
				curr++;
			}
			goto ln_bo_xb_case;

enter_underline:
			/*
			 * We have either "_\bX" or "X\b_" (including
			 * the current char).  Switch into underline mode.
			 */
			if (column + ul_width + ue_width + 1 >= sc_width)
				/*
				 * Not enough room left on the screen to 
				 * enter and exit underline mode.
				 */
				return (1);

			if (ul_width > 0 && 
			    curr > linebuf + 2 && curr[-3] == ' ')
			{
				/*
				 * Special case for magic cookie terminals:
				 * if the previous char was a space, replace 
				 * it with the "enter underline" sequence.
				 */
				curr[-3] = UL_CHAR;
				column += ul_width-1;
			} else
			{
				curr[-1] = curr[-2];
				curr[-2] = UL_CHAR;
				column += ul_width;
				curr++;
			}
			goto ln_ul_xb_case;
			/*NOTREACHED*/
		case LN_UL_XB:
			/*
			 * Termination of a sequence "_\bX" or "X\b_".
			 */
			if (c != '_' && curr[-2] != '_' && c == curr[-2])
			{
				/*
				 * We seem to have run on from underlining
				 * into boldfacing - this is a nasty fix, but
				 * until this whole routine is rewritten as a
				 * real DFA, ...  well ...
				 */
				curr[0] = curr[-2];
				curr[-2] = UE_CHAR;
				curr[-1] = BO_CHAR;
				curr += 2; /* char & non-existent backspace */
				ln_state = LN_BO_XB;
				goto ln_bo_xb_case;
			}
ln_ul_xb_case:
			if (c == '_')
				c = curr[-2];
			curr -= 2;
			ln_state = LN_UNDERLINE;
			break;
		case LN_BO_XB:
			/*
			 * Termination of a sequnce "X\bX".
			 */
			if (c != curr[-2] && (c == '_' || curr[-2] == '_'))
			{
				/*
				 * We seem to have run on from
				 * boldfacing into underlining.
				 */
				curr[0] = curr[-2];
				curr[-2] = BE_CHAR;
				curr[-1] = UL_CHAR;
				curr += 2; /* char & non-existent backspace */
				ln_state = LN_UL_XB;
				goto ln_ul_xb_case;
			}
ln_bo_xb_case:
			curr -= 2;
			ln_state = LN_BOLDFACE;
			break;
		case LN_UNDERLINE:
			if (column + ue_width + bo_width + 1 + be_width >= sc_width)
				/*
				 * We have just barely enough room to 
				 * exit underline mode and handle a possible
				 * underline/boldface run on mixup.
				 */
				return (1);
			ln_state = LN_UL_X;
			break;
		case LN_BOLDFACE:
			if (c == '\b')
			{
				ln_state = LN_BO_XB;
				break;
			}
			if (column + be_width + ul_width + 1 + ue_width >= sc_width)
				/*
				 * We have just barely enough room to 
				 * exit underline mode and handle a possible
				 * underline/boldface run on mixup.
				 */
				return (1);
			ln_state = LN_BO_X;
			break;
		case LN_UL_X:
			if (c == '\b')
				ln_state = LN_UL_XB;
			else
			{
				/*
				 * Exit underline mode.
				 * We have to shuffle the chars a bit
				 * to make this work.
				 */
				curr[0] = curr[-1];
				curr[-1] = UE_CHAR;
				column += ue_width;
				if (ue_width > 0 && curr[0] == ' ')
					/*
					 * Another special case for magic
					 * cookie terminals: if the next
					 * char is a space, replace it
					 * with the "exit underline" sequence.
					 */
					column--;
				else
					curr++;
				ln_state = LN_NORMAL;
			} 
			break;
		case LN_BO_X:
			if (c == '\b')
				ln_state = LN_BO_XB;
			else
			{
				/*
				 * Exit boldface mode.
				 * We have to shuffle the chars a bit
				 * to make this work.
				 */
				curr[0] = curr[-1];
				curr[-1] = BE_CHAR;
				column += be_width;
				if (be_width > 0 && curr[0] == ' ')
					/*
					 * Another special case for magic
					 * cookie terminals: if the next
					 * char is a space, replace it
					 * with the "exit boldface" sequence.
					 */
					column--;
				else
					curr++;
				ln_state = LN_NORMAL;
			} 
			break;
		}
	}
	
	if (c == '\t') 
	{
		/*
		 * Expand a tab into spaces.
		 */
		do
		{
			NEW_COLUMN(column+1);
		} while ((column % tabstop) != 0);
		*curr++ = '\t';
		return (0);
	}

	if (c == '\b')
	{
		if (bs_mode == BS_CONTROL)
		{
			/*
			 * Treat backspace as a control char: output "^H".
			 */
			NEW_COLUMN(column+2);
			*curr++ = ('H' | 0200);
		} else
		{
			/*
			 * Output a real backspace.
			 */
			column--;
			*curr++ = '\b';
		}
		return (0);
	} 

	if (control_char(c))
	{
		/*
		 * Put a "^X" into the buffer.
		 * The 0200 bit is used to tell put_line() to prefix
		 * the char with a ^.  We don't actually put the ^
		 * in the buffer because we sometimes need to move
		 * chars around, and such movement might separate 
		 * the ^ from its following character.
		 * {{ This should be redone so that we can use an
		 *    8 bit (e.g. international) character set. }}
		 */
		NEW_COLUMN(column+2);
		*curr++ = (carat_char(c) | 0200);
		return (0);
	}

	/*
	 * Ordinary character.  Just put it in the buffer.
	 */
	NEW_COLUMN(column+1);
	*curr++ = c;
	return (0);
}

/*
 * Analogous to forw_line(), but deals with "raw lines":
 * lines which are not split for screen width.
 * {{ This is supposed to be more efficient than forw_line(). }}
 */
	public POSITION
forw_raw_line(curr_pos)
	POSITION curr_pos;
{
	register char *p;
	register int c;
	POSITION new_pos;

	if (curr_pos == NULL_POSITION || ch_seek(curr_pos) ||
		(c = ch_forw_get()) == EOI)
		return (NULL_POSITION);

	p = linebuf;

	for (;;)
	{
		if (c == '\n' || c == EOI)
		{
			new_pos = ch_tell();
			break;
		}
		if (p >= &linebuf[sizeof(linebuf)-1])
		{
			/*
			 * Overflowed the input buffer.
			 * Pretend the line ended here.
			 * {{ The line buffer is supposed to be big
			 *    enough that this never happens. }}
			 */
			new_pos = ch_tell() - 1;
			break;
		}
		*p++ = c;
		c = ch_forw_get();
	}
	*p = '\0';
	line = linebuf;
	return (new_pos);
}

/*
 * Analogous to back_line(), but deals with "raw lines".
 * {{ This is supposed to be more efficient than back_line(). }}
 */
	public POSITION
back_raw_line(curr_pos)
	POSITION curr_pos;
{
	register char *p;
	register int c;
	POSITION new_pos;

	if (curr_pos == NULL_POSITION || curr_pos <= (POSITION)0 ||
		ch_seek(curr_pos-1))
		return (NULL_POSITION);

	p = &linebuf[sizeof(linebuf)];
	*--p = '\0';

	for (;;)
	{
		c = ch_back_get();
		if (c == '\n')
		{
			/*
			 * This is the newline ending the previous line.
			 * We have hit the beginning of the line.
			 */
			new_pos = ch_tell() + 1;
			break;
		}
		if (c == EOI)
		{
			/*
			 * We have hit the beginning of the file.
			 * This must be the first line in the file.
			 * This must, of course, be the beginning of the line.
			 */
			new_pos = (POSITION)0;
			break;
		}
		if (p <= linebuf)
		{
			/*
			 * Overflowed the input buffer.
			 * Pretend the line ended here.
			 */
			new_pos = ch_tell() + 1;
			break;
		}
		*--p = c;
	}
	line = p;
	return (new_pos);
}
