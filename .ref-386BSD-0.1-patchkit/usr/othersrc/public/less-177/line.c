/*
 * Routines to manipulate the "line buffer".
 * The line buffer holds a line of output as it is being built
 * in preparation for output to the screen.
 */

#include "less.h"

static char linebuf[1024];	/* Buffer which holds the current output line */
static char attr[1024];		/* Extension of linebuf to hold attributes */
static int curr;		/* Index into linebuf */
static int column;		/* Printable length, accounting for
				   backspaces, etc. */
static int overstrike;		/* Next char should overstrike previous char */
static int is_null_line;	/* There is no current line */
static char pendc;

static int do_append();

extern int bs_mode;
extern int tabstop;
extern int linenums;
extern int ctldisp;
extern int twiddle;
extern int binattr;
extern int auto_wrap, ignaw;
extern int bo_s_width, bo_e_width;
extern int ul_s_width, ul_e_width;
extern int bl_s_width, bl_e_width;
extern int sc_width, sc_height;

/*
 * Rewind the line buffer.
 */
	public void
prewind()
{
	curr = 0;
	column = 0;
	overstrike = 0;
	is_null_line = 0;
	pendc = '\0';
}

/*
 * Insert the line number (of the given position) into the line buffer.
 */
	public void
plinenum(pos)
	POSITION pos;
{
	register int lno;
	register int i;
	register int n;

	/*
	 * We display the line number at the start of each line
	 * only if the -N option is set.
	 */
	if (linenums != 2)
		return;

	/*
	 * Get the line number and put it in the current line.
	 * {{ Note: since find_linenum calls forw_raw_line,
	 *    it may seek in the input file, requiring the caller 
	 *    of plinenum to re-seek if necessary. }}
	 */
	lno = find_linenum(pos);

	sprintf(&linebuf[curr], "%6d", lno);
	n = strlen(&linebuf[curr]);
	column += n;
	for (i = 0;  i < n;  i++)
		attr[curr++] = 0;

	/*
	 * Append enough spaces to bring us to the next tab stop.
	 * {{ We could avoid this at the cost of adding some
	 *    complication to the tab stop logic in pappend(). }}
	 */
	do
	{
		linebuf[curr] = ' ';
		attr[curr++] = 0;
		column++;
	} while ((column % tabstop) != 0);
}

/*
 * Return the printing width of the start (enter) sequence
 * for a given character attribute.
 */
	int
attr_swidth(a)
	int a;
{
	switch (a)
	{
	case BOLD:	return (bo_s_width);
	case UNDERLINE:	return (ul_s_width);
	case BLINK:	return (bl_s_width);
	}
	return (0);
}

/*
 * Return the printing width of the end (exit) sequence
 * for a given character attribute.
 */
	int
attr_ewidth(a)
	int a;
{
	switch (a)
	{
	case BOLD:	return (bo_e_width);
	case UNDERLINE:	return (ul_e_width);
	case BLINK:	return (bl_e_width);
	}
	return (0);
}

/*
 * Return the printing width of a given character and attribute,
 * if the character were added to the current position in the line buffer.
 * Adding a character with a given attribute may cause an enter or exit
 * attribute sequence to be inserted, so this must be taken into account.
 */
	static int
pwidth(c, a)
	int c;
	int a;
{
	register int w;

	if (c == '\b')
		/*
		 * Backspace moves backwards one position.
		 */
		return (-1);

	if (control_char(c))
		/*
		 * Control characters do unpredicatable things,
		 * so we don't even try to guess; say it doesn't move.
		 * This can only happen if the -r flag is in effect.
		 */
		return (0);

	/*
	 * Other characters take one space,
	 * plus the width of any attribute enter/exit sequence.
	 */
	w = 1;
	if (curr > 0 && attr[curr-1] != a)
		w += attr_ewidth(attr[curr-1]);
	if (a && (curr == 0 || attr[curr-1] != a))
		w += attr_swidth(a);
	return (w);
}

/*
 * Delete the previous character in the line buffer.
 */
	static void
backc()
{
	curr--;
	column -= pwidth(linebuf[curr], attr[curr]);
}

/*
 * Append a character and attribute to the line buffer.
 */
	static int
storec(c, a)
	int c;
	int a;
{
	register int w;

	w = pwidth(c, a);
	if (ctldisp > 0 && column + w + attr_ewidth(a) > sc_width)
		/*
		 * Won't fit on screen.
		 */
		return (1);

	if (curr >= sizeof(linebuf)-2)
		/*
		 * Won't fit in line buffer.
		 */
		return (1);

	/*
	 * Special handling for "magic cookie" terminals.
	 * If an attribute enter/exit sequence has a printing width > 0,
	 * and the sequence is adjacent to a space, delete the space.
	 * We just mark the space as invisible, to avoid having too
	 * many spaces deleted.
	 * {{ Note that even if the attribute width is > 1, we
	 *    delete only one space.  It's not worth trying to do more.
	 *    It's hardly worth doing this much. }}
	 */
	if (curr > 0 && a != NORMAL && 
		linebuf[curr-1] == ' ' && attr[curr-1] == NORMAL &&
		attr_swidth(a) > 0)
	{
		/*
		 * We are about to append an enter-attribute sequence
		 * just after a space.  Delete the space.
		 */
		attr[curr-1] = INVIS;
		column--;
	} else if (curr > 0 && attr[curr-1] != NORMAL && 
		attr[curr-1] != INVIS && c == ' ' && a == NORMAL &&
		attr_ewidth(attr[curr-1]) > 0)
	{
		/*
		 * We are about to append a space just after an 
		 * exit-attribute sequence.  Delete the space.
		 */
		a = INVIS;
		column--;
	}
	/* End of magic cookie handling. */

	linebuf[curr] = c;
	attr[curr] = a;
	column += w;
	return (0);
}

/*
 * Append a character to the line buffer.
 * Expand tabs into spaces, handle underlining, boldfacing, etc.
 * Returns 0 if ok, 1 if couldn't fit in buffer.
 */
	public int
pappend(c)
	register int c;
{
	if (pendc)
	{
		if (do_append(pendc))
			/*
			 * Oops.  We've probably lost the char which
			 * was in pendc, since caller won't back up.
			 */
			return (1);
		pendc = '\0';
	}

	if (c == '\r' && bs_mode == BS_SPECIAL)
	{
		/*
		 * Don't put the CR into the buffer until we see 
		 * the next char.  If the next char is a newline,
		 * discard the CR.
		 */
		pendc = c;
		return (0);
	}

	return (do_append(c));
}

	static int
do_append(c)
	int c;
{
	register char *s;
	register int a;

#define	STOREC(c,a)	if (storec((c),(a))) return (1); else curr++

	if (overstrike)
	{
		/*
		 * Overstrike the character at the current position
		 * in the line buffer.  This will cause either 
		 * underline (if a "_" is overstruck), 
		 * bold (if an identical character is overstruck),
		 * or just deletion of the character in the buffer.
		 */
		overstrike = 0;
		if (c == linebuf[curr])
			STOREC(linebuf[curr], BOLD);
		else if (c == '_')
			STOREC(linebuf[curr], UNDERLINE);
		else if (linebuf[curr] == '_')
			STOREC(c, UNDERLINE);
		else if (control_char(c))
			goto do_control_char;
		else
			STOREC(c, NORMAL);
	} else if (c == '\b')
	{
		switch (bs_mode)
		{
		case BS_NORMAL:
			STOREC(c, NORMAL);
			break;
		case BS_CONTROL:
			goto do_control_char;
		case BS_SPECIAL:
			if (curr == 0)
				break;
			backc();
			overstrike = 1;
			break;
		}
	} else if (c == '\t') 
	{
		/*
		 * Expand a tab into spaces.
		 */
		do
		{
			STOREC(' ', NORMAL);
		} while ((column % tabstop) != 0);
	} else if (control_char(c))
	{
	do_control_char:
		if (ctldisp == 0)
		{
			/*
			 * Output as a normal character.
			 */
			STOREC(c, NORMAL);
		} else 
		{
			/*
			 * Output in the (blinking) ^X format.
			 */
			s = prchar(c);  
			a = binattr;

			/*
			 * Make sure we can get the entire representation
			 * the character on this line.
			 */
			if (column + strlen(s) + 
			    attr_swidth(a) + attr_ewidth(a) > sc_width)
				return (1);

			for ( ;  *s != 0;  s++)
				STOREC(*s, a);
		}
	} else
	{
		STOREC(c, NORMAL);
	}

	return (0);
}

/*
 * Terminate the line in the line buffer.
 */
	public void
pdone(endline)
	int endline;
{
	if (pendc && (pendc != '\r' || !endline))
		/*
		 * If we had a pending character, put it in the buffer.
		 * But discard a pending CR if we are at end of line
		 * (that is, discard the CR in a CR/LF sequence).
		 */
		(void) do_append(pendc);

	/*
	 * Add a newline if necessary,
	 * and append a '\0' to the end of the line.
	 */
	if (column < sc_width || !auto_wrap || ignaw || ctldisp == 0)
	{
		linebuf[curr] = '\n';
		attr[curr] = NORMAL;
		curr++;
	}
	linebuf[curr] = '\0';
	attr[curr] = NORMAL;
}

/*
 * Get a character from the current line.
 * Return the character as the function return value,
 * and the character attribute in *ap.
 */
	public int
gline(i, ap)
	register int i;
	register int *ap;
{
	if (is_null_line)
	{
		/*
		 * If there is no current line, we pretend the line is
		 * either "~" or "", depending on the "twiddle" flag.
		 */
		*ap = NORMAL;
		if (twiddle)
			return ("~\n"[i]);
		return ("\n"[i]);
	}

	*ap = attr[i];
	return (linebuf[i] & 0377);
}

/*
 * Indicate that there is no current line.
 */
	public void
null_line()
{
	is_null_line = 1;
}

/*
 * Analogous to forw_line(), but deals with "raw lines":
 * lines which are not split for screen width.
 * {{ This is supposed to be more efficient than forw_line(). }}
 */
	public POSITION
forw_raw_line(curr_pos, linep)
	POSITION curr_pos;
	char **linep;
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
	if (linep != NULL)
		*linep = linebuf;
	return (new_pos);
}

/*
 * Analogous to back_line(), but deals with "raw lines".
 * {{ This is supposed to be more efficient than back_line(). }}
 */
	public POSITION
back_raw_line(curr_pos, linep)
	POSITION curr_pos;
	char **linep;
{
	register char *p;
	register int c;
	POSITION new_pos;

	if (curr_pos == NULL_POSITION || curr_pos <= ch_zero() ||
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
			new_pos = ch_zero();
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
	if (linep != NULL)
		*linep = p;
	return (new_pos);
}
