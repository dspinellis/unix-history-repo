/*
 *
 * Copyright (C) 1983, 86 by Lee Moore.  All rights reserved.
 *
 * Token package
 */

#include <stdio.h>
#include <ctype.h>
#include "token.h"

#define TRUE	1
#define FALSE	0


extern char *malloc();


/*
 * Get one character from the input.  Set flags.
 */

static int GetCharacter1(ts)
    struct TokenState *ts; {
	int c;

	c = getc(ts->Input);
	ts->NotEndOfFile = (c != EOF);
	return c; }



/*
 *  Get one character from the input.  Set Flags.  (see above)
 *	Ignore comments.
 */

static int GetCharacter(ts)
    struct TokenState *ts; {
	int c;

	c = GetCharacter1(ts);

	if( c == '\\' )		/* quote the next character? */
	    c = GetCharacter1(ts);

	else if( c == '#' )	/* if this a comment? */
	    while( c != '\n' &&  c != EOF )
		c = GetCharacter1(ts);

	return c; }


/*
 * Skip over blank space.  Set LastTokenInLine if we see at a new-line.
 */

static SkipBlankSpace(ts)
    struct TokenState *ts; {
	ts->LastTokenInLine = FALSE;

	while( isspace(ts->CurChar) ) {
	    if( ts->CurChar == '\n' )
		ts->LastTokenInLine = TRUE;

	    ts->CurChar = GetCharacter(ts); } }


/*
 * Initialize token package
 */

struct TokenState *InitTokenStream(from)
    FILE *from; {
	struct TokenState *ts;

	ts = (struct TokenState *) malloc((unsigned) sizeof(struct TokenState));
	ts->LastTokenInLine = FALSE;
	ts->NotEndOfFile = FALSE,
	ts->Input = from;
	ts->CurChar = GetCharacter(ts);
	SkipBlankSpace(ts);
	return ts; }



/*
 * Close and release resources
 */

CloseTokenStream(ts)
    struct TokenState *ts; {
	(void) fclose(ts->Input);
	free((char *)ts); }



/*
 * Get the next token in the input.  Tokens are strings of characters that are
 *	delimited by <space>, <tab> and <new-line>
 */

GetToken(ts, arg, maxSize)
    struct TokenState *ts;
    char *arg;
    int maxSize; {
	while( !isspace(ts->CurChar) && ts->CurChar != EOF && maxSize > 0 ) {
	    *arg++ = ts->CurChar;
	    ts->CurChar = GetCharacter(ts);
	    maxSize--; }

	if( maxSize > 0 )
		*arg = '\0';

	SkipBlankSpace(ts);
	return;	}


/*
 * Test if we have read all the tokens in the current line
 */

EndOfLine(ts)
    struct TokenState *ts; {
	return ts->LastTokenInLine; }

/*
 * return true if we have read all the tokens in the file
 */

EndOfFile(ts)
    struct TokenState *ts; {
	return !ts->NotEndOfFile; }
