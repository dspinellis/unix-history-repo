/*
 * Functions which manipulate the command buffer.
 * Used only by command() and related functions.
 */

#include "less.h"

extern int erase_char, kill_char;
extern int sc_width;

static char cmdbuf[120];	/* Buffer for holding a multi-char command */
static int cmd_col;		/* Current column of the multi-char command */
static char *cp;		/* Pointer into cmdbuf */

/*
 * Reset command buffer (to empty).
 */
	public void
cmd_reset()
{
	cp = cmdbuf;
	*cp = '\0';
	cmd_col = 0;
}

/*
 * How many characters are in the command buffer?
 */
	public int
len_cmdbuf()
{
	return (cp - cmdbuf);
}

/*
 * Backspace in the command buffer.
 */
	public int
cmd_erase()
{
	register char *s;

	if (cp == cmdbuf)
		/*
		 * Backspace past beginning of the string:
		 * this usually means abort the command.
		 */
		return (1);

	--cp;
	if (*cp == ESC)
		s = "ESC";
	else
		s = prchar(*cp);
	while (*s++ != '\0')
	{
		backspace();
		cmd_col--;
	}
	*cp = '\0';
	return (0);
}

/*
 * Process a single character of a multi-character command, such as
 * a number, or the pattern of a search command.
 */
	public int
cmd_char(c)
	int c;
{
	char *s;

	if (c == erase_char)
	{
		if (cmd_erase())
			return (1);
	} else if (c == kill_char)
	{
		/* {{ Could do this faster, but who cares? }} */
		while (cmd_erase() == 0)
			;
	} else if (cp >= &cmdbuf[sizeof(cmdbuf)-1])
	{
		/*
		 * No room in the command buffer.
		 */
		bell();
	} else if (cmd_col >= sc_width-4)
	{
		/*
		 * No room on the screen.
		 * {{ Could get fancy here; maybe shift the displayed
		 *    line and make room for more chars, like ksh. }}
		 */
		bell();
	} else
	{
		/*
		 * Append the character to the string.
		 */
		*cp++ = c;
		*cp = '\0';
		if (c == ESC)
			s = "ESC";
		else
			s = prchar(c);
		putstr(s);
		cmd_col += strlen(s);
	}
	return (0);
}

/*
 * Return the number currently in the command buffer.
 */
	public int
cmd_int()
{
	return (atoi(cmdbuf));
}

/*
 * Display a string, usually as a prompt for input into the command buffer.
 */
	public void
cmd_putstr(s)
	char *s;
{
	putstr(s);
	cmd_col += strlen(s);
}

/*
 * Return a pointer to the command buffer.
 */
	public char *
get_cmdbuf()
{
	return (cmdbuf);
}
