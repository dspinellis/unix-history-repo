/*
 *	Copyright 1984, 1985 by the Regents of the University of
 *	California and by Gregory Glenn Minshall.
 *
 *	Permission to use, copy, modify, and distribute these
 *	programs and their documentation for any purpose and
 *	without fee is hereby granted, provided that this
 *	copyright and permission appear on all copies and
 *	supporting documentation, the name of the Regents of
 *	the University of California not be used in advertising
 *	or publicity pertaining to distribution of the programs
 *	without specific prior permission, and notice be given in
 *	supporting documentation that copying and distribution is
 *	by permission of the Regents of the University of California
 *	and by Gregory Glenn Minshall.  Neither the Regents of the
 *	University of California nor Gregory Glenn Minshall make
 *	representations about the suitability of this software
 *	for any purpose.  It is provided "as is" without
 *	express or implied warranty.
 */


/* this program outputs the user's 3270 mapping table in a form suitable
 *	for inclusion in the environment.  Typically, this might be used
 *	by:
 *		setenv MAP3270 "`mset`"
 */

#include <curses.h>
#include "state.h"
#define LETS_SEE_ASCII
#include "m4.out"

static char array[5000];		/* lot's of room */

char *
addString(string, character)
char *string;
char character;
{
    *string++ = character;
    return(string);
}

char *
deleteString(string)
char *string;
{
    return(string-1);
}

printString(string, character, tc)
register char *string;
char character;
int tc;
{
    register char *st1;
    register int pchar;

    *string++ = character;
    st1 = array;

    printf("%s='", TC_Ascii[tc-TC_LOWEST].tc_name);
    while (st1 != string) {
	pchar = 0xff&(*st1++);
	switch (pchar) {
	case '\\':
	case '^':
	case '\'':
	    printf("%c%c", '\\', pchar);
	    break;
	default:
	    printf("%s", unctrl(pchar));
	    break;
	}
    }
    printf("';");
}

recurse(string, head)
char *string;
state *head;
{
		/*	if there is a left,
		 *	    recurse on left,
		 *	if there is no down,
		 *	    print the string to here
		 *	else,
		 *	     add the current match to the string,
		 *	     recurse.
		 *	exit.
		 */
    if (head->next) {
	recurse(string, head->next);
    }
    if (head->result != TC_GOTO) {
	printString(string, head->match, head->result);
    } else {
	string = addString(string, head->match);
	recurse(string, head->address);
	string = deleteString(string);
    }
    return;
}
main()
{
    state *head;
    state *InitControl();
    char *termPointer;
    char *getenv();

    head = InitControl();
    if (!head) {
	return(1);
    }
    termPointer = getenv("TERM");
    if (termPointer == 0) {
	termPointer = "3a";	/* use 3a as the terminal */
    }
    printf("%s{", termPointer);
    /* now, run through the table printing out entries */
    recurse(array, head);
    printf("}\n");
    return(0);
}
