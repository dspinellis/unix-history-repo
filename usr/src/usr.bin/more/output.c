/*
 * Copyright (c) 1988 Mark Nudleman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mark Nudleman.
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
 */

#ifndef lint
static char sccsid[] = "@(#)output.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*
 * High level routines dealing with the output to the screen.
 */

#include "less.h"

public int errmsgs;	/* Count of messages displayed by error() */

extern int sigs;
extern int sc_width, sc_height;
extern int ul_width, ue_width;
extern int so_width, se_width;
extern int bo_width, be_width;
extern int tabstop;
extern int twiddle;
extern int screen_trashed;
extern int any_display;
extern char *line;
extern char *first_cmd;

/*
 * Display the line which is in the line buffer.
 */
	public void
put_line()
{
	register char *p;
	register int c;
	register int column;
	extern int auto_wrap, ignaw;

	if (sigs)
	{
		/*
		 * Don't output if a signal is pending.
		 */
		screen_trashed = 1;
		return;
	}

	if (line == NULL)
		line = (twiddle) ? "~" : "";

	column = 0;
	for (p = line;  *p != '\0';  p++)
	{
		switch (c = *p)
		{
		case UL_CHAR:
			ul_enter();
			column += ul_width;
			break;
		case UE_CHAR:
			ul_exit();
			column += ue_width;
			break;
		case BO_CHAR:
			bo_enter();
			column += bo_width;
			break;
		case BE_CHAR:
			bo_exit();
			column += be_width;
			break;
		case '\t':
			do
			{
				putchr(' ');
				column++;
			} while ((column % tabstop) != 0);
			break;
		case '\b':
			putbs();
			column--;
			break;
		default:
			if (c & 0200)
			{
				/*
				 * Control characters arrive here as the
				 * normal character [carat_char(c)] with
				 * the 0200 bit set.  See pappend().
				 */
				putchr('^');
				putchr(c & 0177);
				column += 2;
			} else
			{
				putchr(c);
				column++;
			}
		}
	}
	if (column < sc_width || !auto_wrap || ignaw)
		putchr('\n');
}

/*
 * Is a given character a "control" character?
 * {{ ASCII DEPENDENT }}
 */
	public int
control_char(c)
	int c;
{
	return (c < ' ' || c == '\177');
}

/*
 * Return the printable character used to identify a control character
 * (printed after a carat; e.g. '\3' => "^C").
 * {{ ASCII DEPENDENT }}
 */
	public int
carat_char(c)
	int c;
{
	return ((c == '\177') ? '?' : (c | 0100));
}


static char obuf[1024];
static char *ob = obuf;

/*
 * Flush buffered output.
 */
	public void
flush()
{
	register int n;

	n = ob - obuf;
	if (n == 0)
		return;
	if (write(1, obuf, n) != n)
		screen_trashed = 1;
	ob = obuf;
}

/*
 * Output a character.
 */
	public void
putchr(c)
	int c;
{
	if (ob >= &obuf[sizeof(obuf)])
		flush();
	*ob++ = c;
}

/*
 * Output a string.
 */
	public void
putstr(s)
	register char *s;
{
	while (*s != '\0')
		putchr(*s++);
}

/*
 * Output a message in the lower left corner of the screen
 * and wait for carriage return.
 */

static char return_to_continue[] = "  (press RETURN)";

	public void
error(s)
	char *s;
{
	register int c;
	static char buf[2];

	errmsgs++;
	if (!any_display)
	{
		/*
		 * Nothing has been displayed yet.
		 * Output this message on error output (file
		 * descriptor 2) and don't wait for a keystroke
		 * to continue.
		 *
		 * This has the desirable effect of producing all
		 * error messages on error output if standard output
		 * is directed to a file.  It also does the same if
		 * we never produce any real output; for example, if
		 * the input file(s) cannot be opened.  If we do
		 * eventually produce output, code in edit() makes
		 * sure these messages can be seen before they are
		 * overwritten or scrolled away.
		 */
		write(2, s, strlen(s));
		write(2, "\n", 1);
		return;
	}

	lower_left();
	clear_eol();
	so_enter();
	putstr(s);
	putstr(return_to_continue);
	so_exit();

	c = getchr();
	if (c != '\n' && c != '\r' && c != ' ' && c != READ_INTR)
	{
		buf[0] = c;
		first_cmd = buf;
	}
	lower_left();

	if (strlen(s) + sizeof(return_to_continue) + 
		so_width + se_width + 1 > sc_width)
		/*
		 * Printing the message has probably scrolled the screen.
		 * {{ Unless the terminal doesn't have auto margins,
		 *    in which case we just hammered on the right margin. }}
		 */
		repaint();

	flush();
}

static char intr_to_abort[] = "... (interrupt to abort)";

	public void
ierror(s)
	char *s;
{

	lower_left();
	clear_eol();
	so_enter();
	putstr(s);
	putstr(intr_to_abort);
	so_exit();
	flush();
}
