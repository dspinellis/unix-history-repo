/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)lexi.c	5.1 (Berkeley) %G%";
#endif not lint

/*

			  Copyright (C) 1976
				by the
			  Board of Trustees
				of the
			University of Illinois

			 All rights reserved


NAME:
	lexi

FUNCTION:
	This is the token scanner for indent

ALGORITHM:
	1) Strip off intervening blanks and/or tabs.
	2) If it is an alphanumeric token, move it to the token buffer "token".
	   Check if it is a special reserved word that indent will want to
	   know about.
	3) Non-alphanumeric tokens are handled with a big switch statement.  A
	   flag is kept to remember if the last token was a "unary delimiter",
	   which forces a following operator to be unary as opposed to binary.

PARAMETERS:
	None

RETURNS:
	An integer code indicating the type of token scanned.

GLOBALS:
	buf_ptr =
	had_eof
	last_u_d =	Set to true iff this token is a "unary delimiter"

CALLS:
	fill_buffer
	printf (lib)

CALLED BY:
	main

NOTES:
	Start of comment is passed back so that the comment can be scanned by
	pr_comment.

	Strings and character literals are returned just like identifiers.

HISTORY:
	initial coding 	November 1976	D A Willcox of CAC
	1/7/77		D A Willcox of CAC	Fix to provide proper handling
						of "int a -1;"

*/

/* Here we have the token scanner for indent.  It scans off one token and
   puts it in the global variable "token".  It returns a code, indicating the
   type of token scanned. */

#include "indent_globs.h";
#include "indent_codes.h";



#define alphanum 1
#define opchar 3

struct templ {
    char   *rwd;
    int     rwcode;
};

struct templ    specials[] =
{
    "switch", 1,
    "case", 2,
    "struct", 3,
    "default", 2,
    "int", 4,
    "char", 4,
    "float", 4,
    "double", 4,
    "long", 4,
    "short", 4,
    "typdef", 4,
    "unsigned", 4,
    "register", 4,
    "static", 4,
    "global", 4,
    "extern", 4,
    "if", 5,
    "while", 5,
    "for", 5,
    "else", 6,
    "do", 6,
    "sizeof", 0,
    0, 0
};

char    chartype[128] =
{		   /* this is used to facilitate the decision of what type
		      (alphanumeric, operator) each character is */
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 3, 0, 0, 0, 3, 3, 0,
    0, 0, 3, 3, 0, 3, 3, 3,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 0, 0, 3, 3, 3, 3,
    0, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 0, 0, 0, 3, 1,
    0, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 0, 3, 0, 3, 0
};

int     last_nl = true;
 /* this is true if the last thing scanned was a newline */



int     lexi () {
    register char  *tok;
 /* local pointer to next char in token */
    register int    i;
 /* local loop counter */
    register char  *j;
 /* used for searching thru list of reserved words */
    int     unary_delim;
 /* this is set to 1 if the current token forces a following operator to be
    unary */
    static int  last_code;
 /* the last token type returned */
    static int  l_struct;
 /* set to 1 if the last token was 'struct' */
    int     found_it;
    int     code;  /* internal code to be returned */
    char    qchar; /* the delimiter character for a string */

    tok = token;	       /* point to start of place to save token */
    unary_delim = false;
    col_1 = last_nl;	       /* tell world that this token started in column
			          1 iff the last thing scanned was nl */
    last_nl = false;

    while (*buf_ptr == ' ' || *buf_ptr == '\t') {
    /* get rid of blanks */
	col_1 = false;	       /* leading blanks imply token is not in column 1
			          */
	if (++buf_ptr >= buf_end)
	    fill_buffer ();
    }

/*----------------------------------------------------------*\ 
|    Scan an alphanumeric token
\*----------------------------------------------------------*/

    if (chartype[*buf_ptr & 0177] == alphanum) {
    /* we have a character or number */
	while (chartype[*buf_ptr & 0177] == alphanum) {
	/* copy it over */
	    *tok++ = *buf_ptr++;
	    if (buf_ptr >= buf_end)
		fill_buffer ();
	}

	*tok++ = '\0';

	if (l_struct) {	       /* if last token was 'struct', then this token
			          should be treated as a declaration */
	    l_struct = false;
	    last_code = ident;
	    last_u_d = true;
	    return (decl);
	}

	last_u_d = false;      /* operator after indentifier is binary */

	for (i = 0; specials[i].rwd != 0; ++i) {
	/* this loop will check if the token is a keyword.  if so, a following
	   operator is unary */
	    last_code = ident; /* remember that this is the code we will return
			          */
	    j = specials[i].rwd;
	/* point at ith reserved word */
	    tok = token;       /* point at scanned toekn */
	    found_it = true;   /* set to false if not found */
	    do {
		if (*tok++ != *j) {
		    found_it = false;
		    break;
		}
	    } while (*j++);

	    if (found_it) {    /* we have a keyword */
		last_u_d = true;
		switch (specials[i].rwcode) {
		    case 1:    /* it is a switch */
			return (swstmt);
		    case 2:    /* a case or default */
			return (casestmt);

		    case 3:    /* a "struct" */
			l_struct = true;
		    /* Next time around, we will want to know that we have had
		       a 'struct' */
		    case 4:    /* one of the declaration keywords */
			if(p_l_follow) break;	/* inside parens: cast */
			last_code = decl;
			return (decl);

		    case 5:    /* if, while, for */
			return (sp_paren);

		    case 6:    /* do, else */
			return (sp_nparen);

		    default:   /* all others are treated like any other
			          identifier */
			return (ident);
		}	       /* end of switch */
	    }		       /* end of if (found_it) */

	}

	if (last_code == decl) /* if this is a declared variable, then
			          following sign is unary */
	    last_u_d = true;   /* will make "int a -1" work */
	last_code = ident;
	return (ident);	       /* the ident is not in the list */
    }			       /* end of procesing for alpanum character */



/*----------------------------------------------------------*\ 
|   Scan a non-alphanumeric token
\*----------------------------------------------------------*/

    *tok++ = *buf_ptr;	       /* if it is only a one-character token, it is
			          moved here */
    *tok = '\0';
    if (++buf_ptr >= buf_end)
	fill_buffer ();

    switch (*token) {
	case '\n': 
	    unary_delim = last_u_d;
	    last_nl = true;    /* remember that we just had a newline */
	    code = (had_eof ? 0 : newline);
	/* if data has been exausted, the newline is a dummy, and we should
	   return code to stop */
	    break;

	case '\'': 	       /* start of quoted character */
	    qchar = '\'';      /* remember final delimiter */
	    goto copy_lit;     /* and go to common literal code */

	case '"': 	       /* start of string */
	    qchar = '"';

    copy_lit: 
	    do {	       /* copy the string */
		while (1) {    /* move one character or [/<char>]<char> */
		    if (*buf_ptr == '\n') {
		    /* check for unterminated literal */
			printf ("%d: Unterminated literal\n", line_no);
			goto stop_lit;
		    /* Don't copy any more */
		    }

		    *tok = *buf_ptr++;
		    if (buf_ptr >= buf_end)
			fill_buffer ();
		    if (had_eof || ((tok - token) > (bufsize - 2))) {
			printf ("Unterminated literal\n");
			++tok;
			goto stop_lit;
		    /* get outof literal copying loop */
		    }

		    if (*tok == '\\') {
		    /* if escape, copy extra char */
			if (*buf_ptr == '\n')
			       /* check for escaped newline */
			    ++line_no;
			*(++tok) = *buf_ptr++;
			++tok; /* we must increment this again because we
			          copied two chars */
			if (buf_ptr >= buf_end)
			    fill_buffer ();
		    }
		    else
			break; /* we copied one character */
		}	       /* end of while (1) */
	    } while (*tok++ != qchar);

    stop_lit: 
	    code = ident;
	    break;

	case ('('): 
	case ('['): 
	    unary_delim = true;
	    code = lparen;
	    break;

	case (')'): 
	case (']'): 
	    code = rparen;
	    break;

	case '#': 
	    unary_delim = last_u_d;
	    code = preesc;
	    break;

	case '?': 
	    unary_delim = true;
	    code = question;
	    break;

	case (':'): 
	    code = colon;
	    unary_delim = true;
	    break;

	case (';'): 
	    unary_delim = true;
	    code = semicolon;
	    break;

	case ('{'): 
	    unary_delim = true;
	    code = lbrace;
	    break;

	case ('}'): 
	    unary_delim = true;
	    code = rbrace;
	    break;

	case 014: 	       /* a form feed */
	    unary_delim = last_u_d;
	    last_nl = true;    /* remember this so we can set 'col_1' right */
	    code = form_feed;
	    break;

	case (','): 
	    unary_delim = true;
	    code = comma;
	    break;

	case '.': 
	    unary_delim = false;
	    code = period;
	    break;

	case '-': 
	case '+': 	       /* check for -, +, --, ++ */
	    code = (last_u_d ? unary_op : binary_op);
	    unary_delim = true;

	    if (*buf_ptr == token[0]) {
	    /* check for doubled character */
		*tok++ = *buf_ptr++;
	    /* buffer overflow will be checked at end of loop */
		if (last_code == ident || last_code == rparen) {
		    code = (last_u_d ? unary_op : postop);
		/* check for following ++ or -- */
		    unary_delim = false;
		}
	    }
	    else
		if (*buf_ptr == '>' || *buf_ptr == '=')
			       /* check for operator -> or += */
		    *tok++ = *buf_ptr++;
	/* buffer overflow will be checked at end of switch */

	    break;

	case '=': 
	    if (chartype[*buf_ptr] == opchar) {
	    /* we have two char assignment */
		*tok++ = *buf_ptr;
	    /* move second character */
		if (++buf_ptr >= buf_end)
		    fill_buffer ();
	    }

	    code = binary_op;
	    unary_delim = true;
	    if (token[1] != '<' && token[1] != '>')
			       /* check for possible 3 char operator */
		break;
	/* can drop thru!!! */

	case '>': 
	case '<': 
	case '!': 	       /* ops like <, <<, <=, !=, etc */
	    if (*buf_ptr == '>' || *buf_ptr == '<' || *buf_ptr == '=') {
		*tok++ = *buf_ptr;
		if (++buf_ptr >= buf_end)
		    fill_buffer ();
	    }

	    if (*buf_ptr == '=')
		 *tok++ = *buf_ptr++;
	    code = (last_u_d ? unary_op : binary_op);
	    unary_delim = true;
	    break;

	default: 
	    if (token[0] == '/' && *buf_ptr == '*') {
	    /* it is start of comment */
		*tok++ = '*';

		if (++buf_ptr >= buf_end)
		    fill_buffer ();

		code = comment;
		unary_delim = last_u_d;
		break;
	    }

	    while (*(tok - 1) == *buf_ptr || *buf_ptr=='=') {
	    /* handle ||, &&, etc, and also things as in int *****i */
		*tok++ = *buf_ptr;
		if (++buf_ptr >= buf_end)
		    fill_buffer ();
	    }


	    code = (last_u_d ? unary_op : binary_op);
	    unary_delim = true;


    }			       /* end of switch */

    if (code != newline) {
	l_struct = false;
	last_code = code;
    }

    if (buf_ptr >= buf_end)    /* check for input buffer empty */
	fill_buffer ();
    last_u_d = unary_delim;
    *tok = '\0';	       /* null terminate the token */
    return (code);
};
