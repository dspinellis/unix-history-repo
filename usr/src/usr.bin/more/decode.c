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
static char sccsid[] = "@(#)decode.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * Routines to decode user commands.
 *
 * This is all table driven.
 * A command table is a sequence of command descriptors.
 * Each command descriptor is a sequence of bytes with the following format:
 *	<c1><c2>...<cN><0><action>
 * The characters c1,c2,...,cN are the command string; that is,
 * the characters which the user must type.
 * It is terminated by a null <0> byte.
 * The byte after the null byte is the action code associated
 * with the command string.
 *
 * The default commands are described by cmdtable.
 * User-defined commands are read into usertable.
 */

#include "less.h"
#include "cmd.h"

/*
 * Command table is ordered roughly according to expected
 * frequency of use, so the common commands are near the beginning.
 */
static char cmdtable[] =
{
	'\r',0,				A_F_LINE,
	'\n',0,				A_F_LINE,
	'e',0,				A_F_LINE,
	'j',0,				A_F_LINE,
	CONTROL('E'),0,			A_F_LINE,
	CONTROL('N'),0,			A_F_LINE,
	'k',0,				A_B_LINE,
	'y',0,				A_B_LINE,
	CONTROL('Y'),0,			A_B_LINE,
	CONTROL('K'),0,			A_B_LINE,
	CONTROL('P'),0,			A_B_LINE,
	'd',0,				A_F_SCROLL,
	CONTROL('D'),0,			A_F_SCROLL,
	'u',0,				A_B_SCROLL,
	CONTROL('U'),0,			A_B_SCROLL,
	' ',0,				A_F_SCREEN,
	'f',0,				A_F_SCREEN,
	CONTROL('F'),0,			A_F_SCREEN,
	CONTROL('V'),0,			A_F_SCREEN,
	'b',0,				A_B_SCREEN,
	CONTROL('B'),0,			A_B_SCREEN,
	CONTROL('['),'v',0,		A_B_SCREEN,
	'R',0,				A_FREPAINT,
	'r',0,				A_REPAINT,
	CONTROL('R'),0,			A_REPAINT,
	CONTROL('L'),0,			A_REPAINT,
	'g',0,				A_GOLINE,
	'<',0,				A_GOLINE,
	CONTROL('['),'<',0,		A_GOLINE,
	'p',0,				A_PERCENT,
	'%',0,				A_PERCENT,
	'G',0,				A_GOEND,
	CONTROL('['),'>',0,		A_GOEND,
	'>',0,				A_GOEND,

	'0',0,				A_DIGIT,
	'1',0,				A_DIGIT,
	'2',0,				A_DIGIT,
	'3',0,				A_DIGIT,
	'4',0,				A_DIGIT,
	'5',0,				A_DIGIT,
	'6',0,				A_DIGIT,
	'7',0,				A_DIGIT,
	'8',0,				A_DIGIT,
	'9',0,				A_DIGIT,

	'=',0,				A_STAT,
	CONTROL('G'),0,			A_STAT,
	'/',0,				A_F_SEARCH,
	'?',0,				A_B_SEARCH,
	'n',0,				A_AGAIN_SEARCH,
	'm',0,				A_SETMARK,
	'\'',0,				A_GOMARK,
	CONTROL('X'),CONTROL('X'),0,	A_GOMARK,
	'E',0,				A_EXAMINE,
	':','e',0,			A_EXAMINE,
	CONTROL('X'),CONTROL('V'),0,	A_EXAMINE,
	'N',0,				A_NEXT_FILE,
	'P',0,				A_PREV_FILE,
	':','n',0,			A_NEXT_FILE,
	':','p',0,			A_PREV_FILE,
	'-',0,				A_TOGGLE_OPTION,
	'_',0,				A_DISP_OPTION,
	'v',0,				A_VISUAL,
	'!',0,				A_SHELL,
	'+',0,				A_FIRSTCMD,

	'H',0,				A_HELP,
	'h',0,				A_HELP,
	'V',0,				A_VERSION,
	'q',0,				A_QUIT,
	':','q',0,			A_QUIT,
	'Z','Z',0,			A_QUIT
};

char *cmdendtable = cmdtable + sizeof(cmdtable);

static char usertable[MAX_USERCMD];
char *userendtable = usertable;

static char kbuf[MAX_CMDLEN+1];
static char *kp = kbuf;

/*
 * Decode a command character and return the associated action.
 */
	public int
cmd_decode(c)
	int c;
{
	register int action = A_INVALID;

	/*
	 * Append the new command character to the command string in kbuf.
	 */
	*kp++ = c;
	*kp = '\0';

	/*
	 * Look first for any user-defined commands.
	 */
	action = cmd_search(usertable, userendtable);
	/*
	 * If didn't find user-defined command,
	 * try the normal default commands.
	 */
	if (action == A_INVALID)
		action = cmd_search(cmdtable, cmdendtable);

	if (action != A_PREFIX)
		/*
		 * This is not a prefix character.
		 */
		noprefix();

	return (action);
}

/*
 * Indicate that we're not in a prefix command
 * by resetting the command buffer pointer.
 */
	public void
noprefix()
{
	kp = kbuf;
}

/*
 * Search a command table for the current command string (in kbuf).
 */
	static int
cmd_search(table, endtable)
	char *table;
	char *endtable;
{
	register char *p;
	register char *q;

	for (p = table, q = kbuf;  p < endtable;  p++, q++)
	{
		if (*p == *q)
		{
			/*
			 * Current characters match.
			 * If we're at the end of the string, we've found it.
			 * Return the action code, which is the character
			 * after the null at the end of the string
			 * in the command table.
			 */
			if (*p == '\0')
				return (p[1]);
		} else if (*q == '\0')
		{
			/*
			 * Hit the end of the user's command,
			 * but not the end of the string in the command table.
			 * The user's command is incomplete.
			 */
			return (A_PREFIX);
		} else
		{
			/*
			 * Not a match.
			 * Skip ahead to the next command in the
			 * command table, and reset the pointer
			 * to the user's command.
			 */
			while (*p++ != '\0') ;
			q = kbuf-1;
		}
	}
	/*
	 * No match found in the entire command table.
	 */
	return (A_INVALID);
}

/*
 * Initialize the user command table.
 */
	public void
init_cmd()
{
	char *filename;
	char *homedir;
	int f;
	int n;
	extern char *getenv();

	/*
	 * Try to open "$HOME/.less"
	 * If we can't, return without doing anything.
	 */
	homedir = getenv("HOME");
	if (homedir == NULL)
		return;
	filename = calloc(strlen(homedir)+7, sizeof(char));
	if (filename == NULL)
		return;
	sprintf(filename, "%s/%s", homedir, ".less");
	f = open(filename, 0);
	free(filename);
	if (f < 0)
		return;

	/*
	 * Read the file into the user table.
	 * {{ Minimal error checking is done here.
	 *    A garbage .less file will produce strange results.
	 *    To avoid a large amount of error checking code here, we
	 *    rely on the lesskey program to generate a good .less file. }}
	 */
	n = read(f, (char *)usertable, MAX_USERCMD);
	if (n < 3 || usertable[n-2] != '\0')
	{
		/*
		 * Several error cases are lumped together here:
		 * - Cannot read user file (n < 0).
		 * - User file is too short (a valid file must
		 *   have at least 3 chars: one char command string,
		 *   the terminating null byte, and the action byte).
		 * - The final entry in the user file is bad (it
		 *   doesn't have a null byte in the proper place).
		 * Many other error cases are not caught, such as
		 * invalid format in any except the last entry,
		 * invalid action codes, command strings too long, etc.
		 */
		error("invalid user key file");
		n = 0;
	}
	userendtable = usertable + n;
	close(f);
}
