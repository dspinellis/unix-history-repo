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
 * when in BS_UNDERLINE mode.  The complexity arises from the attempt
 * to deal with all cases, especially involving long lines with underlining.
 * There are still some cases which will break it.
 *
 * There are four states:
 *	UL_NORMAL is the normal state (not in underline mode).
 *	UL_YES means we are in underline mode.  We expect to get
 *		either a sequence like "_\bX" or "X\b_" to continue
 *		underline mode, or just some ordinary characters
 *		(no backspaces) to end underline mode.
 *	UL_X means we are one character after UL_YES
 *		(we have gotten the '_' in "_\bX" or the 'X' in "X\b_").
 *	UL_XB means we are one character after UL_X 
 *		(we have gotten the backspace in "_\bX" or "X\b_";
 *		we expect one more ordinary character, 
 *		which will put us back in state UL_YES).
 */
static int ul_state;		/* Currently in underline mode? */
#define	UL_NORMAL	0	/* Not in underline mode */
#define	UL_YES		1	/* In underline, need next char */
#define	UL_X		2	/* In underline, got char, need \b */
#define	UL_XB		3	/* In underline, got char & \b, need one more */

public char *line;		/* Pointer to the current line.
				   Usually points to linebuf. */

extern int bs_mode;
extern int tabstop;
extern int ul_width, ue_width;
extern int sc_width, sc_height;

/*
 * Rewind the line buffer.
 */
	public void
prewind()
{
	line = curr = linebuf;
	ul_state = UL_NORMAL;
	column = 0;
}

/*
 * Append a character to the line buffer.
 * Expand tabs into spaces, handle underlining.
 * Returns 0 if ok, 1 if couldn't fit in buffer.
 */

#define	NEW_COLUMN(newcol)	if ((newcol) + ((ul_state)?ue_width:0) > sc_width) \
					return (1); else column = (newcol)

	public int
pappend(c)
	int c;
{
	if (c == '\0')
	{
		/*
		 * Terminate underline mode, if necessary.
		 * Append a '\0' to the end of the line.
		 */
		switch (ul_state)
		{
		case UL_X:
			curr[0] = curr[-1];
			curr[-1] = UE_CHAR;
			curr++;
			break;
		case UL_XB:
		case UL_YES:
			*curr++ = UE_CHAR;
			break;
		}
		ul_state = UL_NORMAL;
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

	if (bs_mode == BS_UNDERLINE)
	{
		/*
		 * Advance the state machine.
		 */
		switch (ul_state)
		{
		case UL_NORMAL:
			if (curr <= linebuf + 1 || curr[-1] != '\b')
				break;
			if (c != '_' && curr[-2] != '_')
			{
				curr -= 2;
				break;
			}

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
			/* Fall thru */
		case UL_XB:
			/*
			 * Termination of a sequnce "_\bX" or "X\b_".
			 */
			if (c == '_')
				c = curr[-2];
			curr -= 2;
			ul_state = UL_YES;
			break;
		case UL_YES:
			if (column + ue_width + 1 >= sc_width)
				/*
				 * We have just barely enough room to 
				 * exit underline mode.  
				 */
				return (1);
			ul_state = UL_X;
			break;
		case UL_X:
			if (c == '\b')
				ul_state = UL_XB;
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
				if (ul_width > 0 && curr[0] == ' ')
					/*
					 * Another special case for magic
					 * cookie terminals: if the next
					 * char is a space, replace it
					 * with the "exit underline" sequence.
					 */
					column--;
				else
					curr++;
				ul_state = UL_NORMAL;
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
		(c = ch_forw_get()) == EOF)
		return (NULL_POSITION);

	p = linebuf;

	for (;;)
	{
		if (c == '\n' || c == EOF)
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
		if (c == EOF)
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
