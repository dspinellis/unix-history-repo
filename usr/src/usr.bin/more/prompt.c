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
static char sccsid[] = "@(#)prompt.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * Prompting and other messages.
 * There are three flavors of prompts, SHORT, MEDIUM and LONG,
 * selected by the -m/-M options.
 * There is also the "equals message", printed by the = command.
 * A prompt is a message composed of various pieces, such as the 
 * name of the file being viewed, the percentage into the file, etc.
 */

#include "less.h"
#include "position.h"

extern int pr_type;
extern int ispipe;
extern int hit_eof;
extern int new_file;
extern int sc_width;
extern int so_width, se_width;
extern char *current_file;
extern int ac;
extern char **av;
extern int curr_ac;
extern int linenums;

/*
 * Prototypes for the three flavors of prompts.
 * These strings are expanded by pr_expand().
 */
static char s_proto[] =
  "?n?f%f .?m(file %i of %m) ..?e(END) ?x- Next\\: %x..%t";
static char m_proto[] =
  "?n?f%f .?m(file %i of %m) ..?e(END) ?x- Next\\: %x.:?pB%pB\\%:byte %bB?s/%s...%t";
static char M_proto[] =
  "?f%f .?n?m(file %i of %m) ..?ltline %lt :byte %bB?s/%s ..?e(END) ?x- Next\\: %x.:?pB%pB\\%..%t";
static char e_proto[] =
  "?f%f .?m(file %i of %m) .?ltline %lt .byte %bB?s/%s. ?e(END) :?pB%pB\\%..%t";

char *prproto[3];
char *eqproto = e_proto;

static char message[250];
static char *mp;

/*
 * Initialize the prompt prototype strings.
 */
	public void
init_prompt()
{
	prproto[0] = save(s_proto);
	prproto[1] = save(m_proto);
	prproto[2] = save(M_proto);
	eqproto = save(e_proto);
}

/*
 * Set the message pointer to the end of the message string.
 */
	static void
setmp()
{
	while (*mp != '\0')
		mp++;
}

/*
 * Append a POSITION (as a decimal integer) to the end of the message.
 */
	static void
ap_pos(pos)
	POSITION pos;
{
	sprintf(mp, "%ld", (long)pos);
	setmp();
}

/*
 * Append an integer to the end of the message.
 */
	static void
ap_int(n)
	int n;
{
	sprintf(mp, "%d", n);
	setmp();
}

/*
 * Append a question mark to the end of the message.
 */
	static void
ap_quest()
{
	*mp++ = '?';
}

/*
 * Return the "current" byte offset in the file.
 */
	static POSITION
curr_byte(where)
	int where;
{
	POSITION pos;

	pos = position(where);
	if (pos == NULL_POSITION)
		pos = ch_length();
	return (pos);
}

/*
 * Return the value of a prototype conditional.
 * A prototype string may include conditionals which consist of a 
 * question mark followed by a single letter.
 * Here we decode that letter and return the appropriate boolean value.
 */
	static int
cond(c, where)
	char c;
	int where;
{
	switch (c)
	{
	case 'a':	/* Anything in the message yet? */
		return (mp > message);
	case 'b':	/* Current byte offset known? */
		return (curr_byte(where) != NULL_POSITION);
	case 'e':	/* At end of file? */
		return (hit_eof);
	case 'f':	/* Filename known? */
		return (!ispipe);
	case 'l':	/* Line number known? */
		return (linenums);
	case 'm':	/* More than one file? */
		return (ac > 1);
	case 'n':	/* First prompt in a new file? */
		return (new_file);
	case 'p':	/* Percent into file known? */
		return (curr_byte(where) != NULL_POSITION && 
				ch_length() > 0);
	case 's':	/* Size of file known? */
		return (ch_length() != NULL_POSITION);
	case 'x':	/* Is there a "next" file? */
		return (curr_ac + 1 < ac);
	}
	return (0);
}

/*
 * Decode a "percent" prototype character.
 * A prototype string may include various "percent" escapes;
 * that is, a percent sign followed by a single letter.
 * Here we decode that letter and take the appropriate action,
 * usually by appending something to the message being built.
 */
	static void
protochar(c, where)
	int c;
	int where;
{
	POSITION pos;
	POSITION len;
	int n;

	switch (c)
	{
	case 'b':	/* Current byte offset */
		pos = curr_byte(where);
		if (pos != NULL_POSITION)
			ap_pos(pos);
		else
			ap_quest();
		break;
	case 'f':	/* File name */
		strtcpy(mp, current_file,
			(unsigned int)(&message[sizeof(message)] - mp));
		setmp();
		break;
	case 'i':	/* Index into list of files */
		ap_int(curr_ac + 1);
		break;
	case 'l':	/* Current line number */
		n = currline(where);
		if (n != 0)
			ap_int(n);
		else
			ap_quest();
		break;
	case 'm':	/* Number of files */
		ap_int(ac);
		break;
	case 'p':	/* Percent into file */
		pos = curr_byte(where);
		len = ch_length();
		if (pos != NULL_POSITION && len > 0)
			ap_int((int)(100*pos / len));
		else
			ap_quest();
		break;
	case 's':	/* Size of file */
		len = ch_length();
		if (len != NULL_POSITION)
			ap_pos(len);
		else
			ap_quest();
		break;
	case 't':	/* Truncate trailing spaces in the message */
		while (mp > message && mp[-1] == ' ')
			mp--;
		break;
	case 'x':	/* Name of next file */
		if (curr_ac + 1 < ac)
		{
			strtcpy(mp, av[curr_ac+1],
				(unsigned int)(&message[sizeof(message)] - mp));
			setmp();
		} else
			ap_quest();
		break;
	}
}

/*
 * Skip a false conditional.
 * When a false condition is found (either a false IF or the ELSE part 
 * of a true IF), this routine scans the prototype string to decide
 * where to resume parsing the string.
 * We must keep track of nested IFs and skip them properly.
 */
	static char *
skipcond(p)
	register char *p;
{
	register int iflevel = 1;

	for (;;) switch (*++p)
	{
	case '?':
		/*
		 * Start of a nested IF.
		 */
		iflevel++;
		break;
	case ':':
		/*
		 * Else.
		 * If this matches the IF we came in here with,
		 * then we're done.
		 */
		if (iflevel == 1)
			return (p);
		break;
	case '.':
		/*
		 * Endif.
		 * If this matches the IF we came in here with,
		 * then we're done.
		 */
		if (--iflevel == 0)
			return (p);
		break;
	case '\\':
		/*
		 * Backslash escapes the next character.
		 */
		++p;
		break;
	case '\0':
		/*
		 * Whoops.  Hit end of string.
		 * This is a malformed conditional, but just treat it
		 * as if all active conditionals ends here.
		 */
		return (p-1);
	}
	/*NOTREACHED*/
}

	static char *
wherechar(p, wp)
	char *p;
	int *wp;
{
	switch (*p)
	{
	case 'b': case 'l': case 'p':
		switch (*++p)
		{
		case 't':   *wp = TOP;			break;
		case 'm':   *wp = MIDDLE;		break;
		case 'b':   *wp = BOTTOM;		break;
		case 'B':   *wp = BOTTOM_PLUS_ONE;	break;
		default:    *wp = TOP;			break;
		}
	}
	return (p);
}

/*
 * Construct a message based on a prototype string.
 */
	static char *
pr_expand(proto, maxwidth)
	char *proto;
	int maxwidth;
{
	register char *p;
	register int c;
	int where;

	mp = message;

	if (*proto == '\0')
		return ("");

	for (p = proto;  *p != '\0';  p++)
	{
		switch (*p)
		{
		default:	/* Just put the character in the message */
			*mp++ = *p;
			break;
		case '\\':	/* Backslash escapes the next character */
			p++;
			*mp++ = *p;
			break;
		case '?':	/* Conditional (IF) */
			if ((c = *++p) == '\0')
				--p;
			else
			{
				p = wherechar(p, &where);
				if (!cond(c, where))
					p = skipcond(p);
			}
			break;
		case ':':	/* ELSE */
			p = skipcond(p);
			break;
		case '.':	/* ENDIF */
			break;
		case '%':	/* Percent escape */
			if ((c = *++p) == '\0')
				--p;
			else
			{
				p = wherechar(p, &where);
				protochar(c, where);
			}
			break;
		}
	}

	new_file = 0;
	if (mp == message)
		return (NULL);
	*mp = '\0';
	if (maxwidth > 0 && mp >= message + maxwidth)
	{
		/*
		 * Message is too long.
		 * Return just the final portion of it.
		 */
		return (mp - maxwidth);
	}
	return (message);
}

/*
 * Return a message suitable for printing by the "=" command.
 */
	public char *
eq_message()
{
	return (pr_expand(eqproto, 0));
}

/*
 * Return a prompt.
 * This depends on the prompt type (SHORT, MEDIUM, LONG), etc.
 * If we can't come up with an appropriate prompt, return NULL
 * and the caller will prompt with a colon.
 */
	public char *
pr_string()
{
	return (pr_expand(prproto[pr_type], sc_width-so_width-se_width-2));
}
