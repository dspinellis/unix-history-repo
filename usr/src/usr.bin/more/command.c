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
static char sccsid[] = "@(#)command.c	5.13 (Berkeley) %G%";
#endif /* not lint */

/*
 * User-level command processor.
 */

#include <sys/param.h>
#include <stdio.h>
#include <ctype.h>
#include <less.h>

#define	NO_MCA		0
#define	MCA_DONE	1
#define	MCA_MORE	2

extern int erase_char, kill_char, werase_char;
extern int ispipe;
extern int sigs;
extern int quit_at_eof;
extern int hit_eof;
extern int sc_width;
extern int sc_height;
extern int sc_window;
extern int curr_ac;
extern int ac;
extern int quitting;
extern int scroll;
extern int screen_trashed;	/* The screen has been overwritten */

static char cmdbuf[120];	/* Buffer for holding a multi-char command */
static char *cp;		/* Pointer into cmdbuf */
static int cmd_col;		/* Current column of the multi-char command */
static int longprompt;		/* if stat command instead of prompt */
static int mca;			/* The multicharacter command (action) */
static int last_mca;		/* The previous mca */
static int number;		/* The number typed by the user */
static int wsearch;		/* Search for matches (1) or non-matches (0) */

/*
 * Reset command buffer (to empty).
 */
cmd_reset()
{
	cp = cmdbuf;
}

/*
 * Backspace in command buffer.
 */
static
cmd_erase()
{
	if (cp == cmdbuf)
		/*
		 * Backspace past beginning of the string:
		 * this usually means abort the command.
		 */
		return(1);

	if (CONTROL_CHAR(*--cp)) {
		/*
		 * Erase an extra character, for the carat.
		 */
		backspace();
		--cmd_col;
	}
	backspace();
	--cmd_col;
	return(0);
}

/*
 * Set up the display to start a new multi-character command.
 */
start_mca(action, prompt)
	int action;
	char *prompt;
{
	lower_left();
	clear_eol();
	putstr(prompt);
	cmd_col = strlen(prompt);
	mca = action;
}

/*
 * Process a single character of a multi-character command, such as
 * a number, or the pattern of a search command.
 */
static
cmd_char(c)
	int c;
{
	if (c == erase_char)
		return(cmd_erase());
	/* in this order, in case werase == erase_char */
	if (c == werase_char) {
		if (cp > cmdbuf) {
			while (isspace(cp[-1]) && !cmd_erase());
			while (!isspace(cp[-1]) && !cmd_erase());
			while (isspace(cp[-1]) && !cmd_erase());
		}
		return(cp == cmdbuf);
	}
	if (c == kill_char) {
		while (!cmd_erase());
		return(1);
	}
	/*
	 * No room in the command buffer, or no room on the screen;
	 * {{ Could get fancy here; maybe shift the displayed line
	 * and make room for more chars, like ksh. }}
	 */
	if (cp >= &cmdbuf[sizeof(cmdbuf)-1] || cmd_col >= sc_width-3)
		bell();
	else {
		*cp++ = c;
		if (CONTROL_CHAR(c)) {
			putchr('^');
			cmd_col++;
			c = CARAT_CHAR(c);
		}
		putchr(c);
		cmd_col++;
	}
	return(0);
}

/*
 * Return the number currently in the command buffer.
 */
	static int
cmd_int()
{
	*cp = '\0';
	cp = cmdbuf;
	return (atoi(cmdbuf));
}

/*
 * Move the cursor to lower left before executing a command.
 * This looks nicer if the command takes a long time before
 * updating the screen.
 */
static
cmd_exec()
{
	lower_left();
	flush();
}

prompt()
{
	extern int terseprompt, linenums;
	extern char *current_name, *firstsearch, *next_name;
	off_t len, pos, ch_length(), position(), forw_line();
	char pbuf[40];

	/*
	 * if nothing is displayed yet, display starting from line 1;
	 * if search string provided, go there instead.
	 */
	if (position(TOP) == NULL_POSITION) {
		if (forw_line((off_t)0) == NULL_POSITION)
			return(0);
		if (!firstsearch || !search(1, firstsearch, 1, 1))
			jump_back(1);
	}
	else if (screen_trashed)
		repaint();

	/* if no -e flag and we've hit EOF on the last file, quit. */
	if (!quit_at_eof && hit_eof && curr_ac + 1 >= ac)
		quit();

	/* select the proper prompt and display it. */
	lower_left();
	clear_eol();
	if (longprompt) {
		so_enter();
		putstr(current_name);
		putstr(":");
		if (!ispipe) {
			(void)sprintf(pbuf, " file %d/%d", curr_ac + 1, ac);
			putstr(pbuf);
		}
		if (linenums) {
			(void)sprintf(pbuf, " line %d", currline(BOTTOM));
			putstr(pbuf);
		}
		if ((pos = position(BOTTOM)) != NULL_POSITION) {
			(void)sprintf(pbuf, " byte %ld", pos);
			putstr(pbuf);
			if (!ispipe && (len = ch_length())) {
				(void)sprintf(pbuf, "/%ld pct %ld%%",
				    len, ((100 * pos) / len));
				putstr(pbuf);
			}
		}
		so_exit();
		longprompt = 0;
	}
	else if (terseprompt)
		putchr(':');
	else {
		so_enter();
		putstr(current_name);
		if (hit_eof)
			if (next_name) {
				(void)sprintf(pbuf, ": END (%s)", next_name);
				putstr(pbuf);
			}
			else
				putstr(": END");
		else if (linenums) {
			(void)sprintf(pbuf, ": %d", currline(TOP));
			putstr(pbuf);
		}
		so_exit();
	}
	return(1);
}

/*
 * Get command character.
 */
static
getcc()
{
	extern int cmdstack;
	int ch;

	/* left over from error() routine. */
	if (cmdstack) {
		ch = cmdstack;
		cmdstack = NULL;
		return(ch);
	}
	if (cp > cmdbuf && position(TOP) == NULL_POSITION) {
		/*
		 * Command is incomplete, so try to complete it.
		 * There are only two cases:
		 * 1. We have "/string" but no newline.  Add the \n.
		 * 2. We have a number but no command.  Treat as #g.
		 * (This is all pretty hokey.)
		 */
		if (mca != A_DIGIT)
			/* Not a number; must be search string */
			return('\n');
		else
			/* A number; append a 'g' */
			return('g');
	}
	return(getchr());
}

/*
 * Execute a multicharacter command.
 */
static
exec_mca()
{
	extern int file;
	extern char *tagfile;
	register char *p;
	char *glob();

	*cp = '\0';
	cmd_exec();
	switch (mca) {
	case A_F_SEARCH:
		(void)search(1, cmdbuf, number, wsearch);
		break;
	case A_B_SEARCH:
		(void)search(0, cmdbuf, number, wsearch);
		break;
	case A_EXAMINE:
		for (p = cmdbuf; isspace(*p); ++p);
		(void)edit(glob(p));
		break;
	case A_TAGFILE:
		for (p = cmdbuf; isspace(*p); ++p);
		findtag(p);
		if (tagfile == NULL)
			break;
		if (edit(tagfile))
			(void)tagsearch();
		break;
	}
}

/*
 * Add a character to a multi-character command.
 */
static
mca_char(c)
	int c;
{
	switch (mca) {
	case 0:			/* not in a multicharacter command. */
	case A_PREFIX:		/* in the prefix of a command. */
		return(NO_MCA);
	case A_DIGIT:
		/*
		 * Entering digits of a number.
		 * Terminated by a non-digit.
		 */
		if (!isascii(c) || !isdigit(c) &&
		    c != erase_char && c != kill_char && c != werase_char) {
			/*
			 * Not part of the number.
			 * Treat as a normal command character.
			 */
			number = cmd_int();
			mca = 0;
			return(NO_MCA);
		}
		break;
	}

	/*
	 * Any other multicharacter command
	 * is terminated by a newline.
	 */
	if (c == '\n' || c == '\r') {
		/*
		 * Execute the command.
		 */
		exec_mca();
		return(MCA_DONE);
	}
	/*
	 * Append the char to the command buffer.
	 */
	if (cmd_char(c))
		/*
		 * Abort the multi-char command.
		 */
		return(MCA_DONE);
	/*
	 * Need another character.
	 */
	return(MCA_MORE);
}

/*
 * Main command processor.
 * Accept and execute commands until a quit command, then return.
 */
commands()
{
	register int c;
	register int action;

	last_mca = 0;
	scroll = (sc_height + 1) / 2;

	for (;;) {
		mca = 0;
		number = 0;

		/*
		 * See if any signals need processing.
		 */
		if (sigs) {
			psignals();
			if (quitting)
				quit();
		}
		/*
		 * Display prompt and accept a character.
		 */
		cmd_reset();
		if (!prompt()) {
			next_file(1);
			continue;
		}
		noprefix();
		c = getcc();

again:		if (sigs)
			continue;

		/*
		 * If we are in a multicharacter command, call mca_char.
		 * Otherwise we call cmd_decode to determine the
		 * action to be performed.
		 */
		if (mca)
			switch (mca_char(c)) {
			case MCA_MORE:
				/*
				 * Need another character.
				 */
				c = getcc();
				goto again;
			case MCA_DONE:
				/*
				 * Command has been handled by mca_char.
				 * Start clean with a prompt.
				 */
				continue;
			case NO_MCA:
				/*
				 * Not a multi-char command
				 * (at least, not anymore).
				 */
				break;
			}

		/*
		 * Decode the command character and decide what to do.
		 */
		switch (action = cmd_decode(c))
		{
		case A_DIGIT:
			/*
			 * First digit of a number.
			 */
			start_mca(A_DIGIT, ":");
			goto again;

		case A_F_SCREEN:
			/*
			 * Forward one screen.
			 */
			if (number <= 0)
				number = sc_window;
			if (number <= 0)
				number = sc_height - 1;
			cmd_exec();
			forward(number, 1);
			break;

		case A_B_SCREEN:
			/*
			 * Backward one screen.
			 */
			if (number <= 0)
				number = sc_window;
			if (number <= 0)
				number = sc_height - 1;
			cmd_exec();
			backward(number, 1);
			break;

		case A_F_LINE:
			/*
			 * Forward N (default 1) line.
			 */
			if (number <= 0)
				number = 1;
			cmd_exec();
			forward(number, 0);
			break;

		case A_B_LINE:
			/*
			 * Backward N (default 1) line.
			 */
			if (number <= 0)
				number = 1;
			cmd_exec();
			backward(number, 0);
			break;

		case A_F_SCROLL:
			/*
			 * Forward N lines 
			 * (default same as last 'd' or 'u' command).
			 */
			if (number > 0)
				scroll = number;
			cmd_exec();
			forward(scroll, 0);
			break;

		case A_B_SCROLL:
			/*
			 * Forward N lines 
			 * (default same as last 'd' or 'u' command).
			 */
			if (number > 0)
				scroll = number;
			cmd_exec();
			backward(scroll, 0);
			break;

		case A_FREPAINT:
			/*
			 * Flush buffers, then repaint screen.
			 * Don't flush the buffers on a pipe!
			 */
			if (!ispipe)
			{
				ch_init(0, 0);
				clr_linenum();
			}
			/* FALLTHRU */
		case A_REPAINT:
			/*
			 * Repaint screen.
			 */
			cmd_exec();
			repaint();
			break;

		case A_GOLINE:
			/*
			 * Go to line N, default beginning of file.
			 */
			if (number <= 0)
				number = 1;
			cmd_exec();
			jump_back(number);
			break;

		case A_PERCENT:
			/*
			 * Go to a specified percentage into the file.
			 */
			if (number < 0)
				number = 0;
			if (number > 100)
				number = 100;
			cmd_exec();
			jump_percent(number);
			break;

		case A_GOEND:
			/*
			 * Go to line N, default end of file.
			 */
			cmd_exec();
			if (number <= 0)
				jump_forw();
			else
				jump_back(number);
			break;

		case A_STAT:		/* print file name, etc. */
			longprompt = 1;
			continue;

		case A_QUIT:		/* exit */
			quit();

		case A_F_SEARCH:
		case A_B_SEARCH:
			/*
			 * Search for a pattern.
			 * Accept chars of the pattern until \n.
			 */
			if (number <= 0)
				number = 1;
			start_mca(action, (action==A_F_SEARCH) ? "/" : "?");
			last_mca = mca;
			wsearch = 1;
			c = getcc();
			if (c == '!') {
				/*
				 * Invert the sense of the search.
				 * Set wsearch to 0 and get a new
				 * character for the start of the pattern.
				 */
				start_mca(action, 
					(action==A_F_SEARCH) ? "!/" : "!?");
				wsearch = 0;
				c = getcc();
			}
			goto again;

		case A_AGAIN_SEARCH:
			/*
			 * Repeat previous search.
			 */
			if (number <= 0)
				number = 1;
			if (wsearch)
				start_mca(last_mca, 
					(last_mca==A_F_SEARCH) ? "/" : "?");
			else
				start_mca(last_mca, 
					(last_mca==A_F_SEARCH) ? "!/" : "!?");
			cmd_exec();
			(void)search(mca == A_F_SEARCH, (char *)NULL,
			    number, wsearch);
			break;

		case A_HELP:
			/*
			 * Help.
			 */
			lower_left();
			clear_eol();
			putstr("help");
			cmd_exec();
			help();
			break;

		case A_TAGFILE:		/* tag a new file; get the file name */
			cmd_reset();
			start_mca(A_TAGFILE, "Tag: ");
			c = getcc();
			goto again;

		case A_EXAMINE:		/* edit a new file; get the file name */
			cmd_reset();
			start_mca(A_EXAMINE, "Examine: ");
			c = getcc();
			goto again;

		case A_VISUAL:
			/*
			 * Invoke an editor on the input file.
			 */
			if (ispipe)
			{
				error("Cannot edit standard input");
				break;
			}
			cmd_exec();
			editfile();
			ch_init(0, 0);
			clr_linenum();
			break;

		case A_NEXT_FILE:
			/*
			 * Examine next file.
			 */
			if (number <= 0)
				number = 1;
			next_file(number);
			break;

		case A_PREV_FILE:
			/*
			 * Examine previous file.
			 */
			if (number <= 0)
				number = 1;
			prev_file(number);
			break;

		case A_SETMARK:
			/*
			 * Set a mark.
			 */
			lower_left();
			clear_eol();
			start_mca(A_SETMARK, "mark: ");
			c = getcc();
			if (c == erase_char || c == kill_char)
				break;
			setmark(c);
			break;

		case A_GOMARK:
			/*
			 * Go to a mark.
			 */
			lower_left();
			clear_eol();
			start_mca(A_GOMARK, "goto mark: ");
			c = getcc();
			if (c == erase_char || c == kill_char)
				break;
			gomark(c);
			break;

		case A_PREFIX:
			/*
			 * The command is incomplete (more chars are needed).
			 * Display the current char so the user knows
			 * what's going on and get another character.
			 */
			if (mca != A_PREFIX)
				start_mca(A_PREFIX, "& ");
			if (CONTROL_CHAR(c))
			{
				putchr('^');
				c = CARAT_CHAR(c);
			}
			putchr(c);
			c = getcc();
			goto again;

		default:
			bell();
			break;
		}
	}
}

static
editfile()
{
	extern char *current_file;
	static int dolinenumber;
	static char *editor;
	int c;
	char buf[MAXPATHLEN * 2 + 20], *getenv();

	if (editor == NULL) {
		editor = getenv("EDITOR");
		/* pass the line number to vi */
		if (editor == NULL || *editor == '\0') {
#define	EDIT_PGM	"/usr/ucb/vi"
			editor = EDIT_PGM;
			dolinenumber = 1;
		}
		else
			dolinenumber = 0;
	}
	if (dolinenumber && (c = currline(MIDDLE)))
		(void)sprintf(buf, "%s +%d %s", editor, c, current_file);
	else
		(void)sprintf(buf, "%s %s", editor, current_file);
	lsystem(buf);
}
