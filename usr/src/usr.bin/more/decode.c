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
static char sccsid[] = "@(#)decode.c	5.6 (Berkeley) %G%";
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
 */

#include <sys/param.h>
#include <sys/file.h>
#include <stdio.h>
#include <less.h>

/*
 * Command table is ordered roughly according to expected
 * frequency of use, so the common commands are near the beginning.
 */
#define	CONTROL(c)		((c)&037)

static char cmdtable[] = {
	'\r',0,				A_F_LINE,
	'\n',0,				A_F_LINE,
	'j',0,				A_F_LINE,
	'k',0,				A_B_LINE,
	'd',0,				A_F_SCROLL,
	CONTROL('D'),0,			A_F_SCROLL,
	'u',0,				A_B_SCROLL,
	CONTROL('U'),0,			A_B_SCROLL,
	' ',0,				A_F_SCREEN,
	'f',0,				A_F_SCREEN,
	CONTROL('F'),0,			A_F_SCREEN,
	'b',0,				A_B_SCREEN,
	CONTROL('B'),0,			A_B_SCREEN,
	'R',0,				A_FREPAINT,
	'r',0,				A_REPAINT,
	CONTROL('L'),0,			A_REPAINT,
	'g',0,				A_GOLINE,
	'p',0,				A_PERCENT,
	'%',0,				A_PERCENT,
	'G',0,				A_GOEND,
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
	'E',0,				A_EXAMINE,
	'N',0,				A_NEXT_FILE,
	':','n',0,			A_NEXT_FILE,
	'P',0,				A_PREV_FILE,
	':','p',0,			A_PREV_FILE,
	'v',0,				A_VISUAL,

	'h',0,				A_HELP,
	'q',0,				A_QUIT,
	':','q',0,			A_QUIT,
	':','t',0,			A_TAGFILE,
	'Z','Z',0,			A_QUIT,
};

char *cmdendtable = cmdtable + sizeof(cmdtable);

#define	MAX_CMDLEN	16

static char kbuf[MAX_CMDLEN+1];
static char *kp = kbuf;

/*
 * Indicate that we're not in a prefix command
 * by resetting the command buffer pointer.
 */
noprefix()
{
	kp = kbuf;
}

/*
 * Decode a command character and return the associated action.
 */
cmd_decode(c)
	int c;
{
	register int action = A_INVALID;

	/*
	 * Append the new command character to the command string in kbuf.
	 */
	*kp++ = c;
	*kp = '\0';

	action = cmd_search(cmdtable, cmdendtable);

	/* This is not a prefix character. */
	if (action != A_PREFIX)
		noprefix();
	return(action);
}

/*
 * Search a command table for the current command string (in kbuf).
 */
static
cmd_search(table, endtable)
	char *table;
	char *endtable;
{
	register char *p, *q;

	for (p = table, q = kbuf;  p < endtable;  p++, q++) {
		if (*p == *q) {
			/*
			 * Current characters match.
			 * If we're at the end of the string, we've found it.
			 * Return the action code, which is the character
			 * after the null at the end of the string
			 * in the command table.
			 */
			if (*p == '\0')
				return(p[1]);
		}
		else if (*q == '\0') {
			/*
			 * Hit the end of the user's command,
			 * but not the end of the string in the command table.
			 * The user's command is incomplete.
			 */
			return(A_PREFIX);
		} else {
			/*
			 * Not a match.
			 * Skip ahead to the next command in the
			 * command table, and reset the pointer
			 * to the user's command.
			 */
			while (*p++ != '\0');
			q = kbuf-1;
		}
	}
	/*
	 * No match found in the entire command table.
	 */
	return(A_INVALID);
}
