/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)io.c	5.1 (Berkeley) %G%";
#endif not lint

/*

			  Copyright (C) 1976
				by the
			  Board of Trustees
				of the
			University of Illinois

			 All rights reserved


FILE NAME:
	io.c

PURPOSE:
	Contains routines to handle i/o related stuff for indent.

GLOBALS:
	None

FUNCTIONS:
	dump_line
	fill_buffer
	pad_output
	count_spaces
	eqin
	cmp

*/
/*

			  Copyright (C) 1976
				by the
			  Board of Trustees
				of the
			University of Illinois

			 All rights reserved


NAME:
	dump_line

FUNCTION:
	Does the actual printing of the stored up line

ALGORITHM:
	For each of the label, code, and comment sections which are used on 
	this line:

	1) Use pad_output to get the section aligned properly.
	2) write the section

	The indentation level used for the code is set by ind_level.  After
	printing, ind_level is set to i_l_follow.

	An extra level of indentation is added if ind_stmt is 1.  After 
	printing, ind_stmt is set to 1 iff the line just printed has an
	unterminated, non-declaration statement.

PARAMETERS:
	None

RETURNS:
	Nothing

GLOBALS:
	labbuf
	s_lab
	e_lab =		Reset to s_lab

	codebuf
	s_code
	e_code =	Reset to s_code

	combuf
	s_com
	e_com =		Reset to s_com

	bl_line =	Set to true iff the line was blank
	case_ind
	code_lines =	Count lines with code
	com_col
	com_lines =	Keep track of lines with comments
	decl_on_line =	Set to in_decl after line is printed
	i_l_follow
	in_decl
	in_stmt
	ind_level =	Set to i_l_follow at completion
	ind_size
	ind_stmt =	Set to in_stmt at completion if not in declaration
	out_lines =	Count output lines
	p_l_follow
	paren_level =	Set to p_l_follow at completion
	pcase
	use_ff =	Reset to false

CALLS:
	pad_output
	printf (lib)
	write (lib)

CALLED BY:
	main
	pr_comment

HISTORY:
	initial coding 	November 1976	D A Willcox of CAC

*/
#include "indent_globs.h";



int     ff = 014;	       /* used to write a form feed */


dump_line () {		       /* dump_line is the routine that actually
			          effects the printing of the new source.
			          It prints the label section, followed by
			          the code section with the appropriate
			          nesting level, followed by any comments 
			       */
    register int    cur_col,
                    temp_col,
                    target_col;

    bl_line = true;	       /* if we don't find otherwise, assume a
			          blank line */

    if (ind_level == 0)
	ind_stmt = 0;	       /* this is a class A kludge. don't do
			          additional statement indentation if we
			          are at bracket level 0 */

    if (e_lab != s_lab || e_code != s_code)
	++code_lines;	       /* keep count of lines with code */

    if (e_lab != s_lab) {      /* print lab, if any */
	if (pcase)	       /* if the label is really a case, we must
			          indent */
	    cur_col = pad_output (1, case_ind * ind_size + 1);
	else {
	    if (*s_lab == '#') /* check for #define, etc */
		cur_col = 1;
	    else
		cur_col = pad_output (1, ind_size * (ind_level - label_offset) + 1);
	}

	write (output, s_lab, e_lab - s_lab);
	cur_col = count_spaces (cur_col, s_lab);
    /* count_spaces gives number of characters, considering tabs */
	bl_line = false;       /* line not blank after all */
    }
    else
	cur_col = 1;	       /* there is no label section */

    pcase = false;

    if (s_code != e_code) {    /* print code section, if any */
	target_col = ind_size * (ind_level + paren_level + ind_stmt) + 1;

	cur_col = pad_output (cur_col, target_col);
    /* pad_output writes enough tabs and spaces to get the current char
       position up to target_col */
	write (output, s_code, e_code - s_code);
	cur_col = count_spaces (cur_col, s_code);
	bl_line = false;       /* line not blank */
    }

    if ((cur_col - 1) > max_col && output!=1)/* check for line too long */
	printf ("%d: Code has %d chars, max is %d\n", line_no, (cur_col - 1), max_col);

    if (s_com != e_com) {      /* print comment, if any */
	if (cur_col > com_col && count_spaces (cur_col, s_com) >= max_col) {
	/* if comment can't fit on this line, put it on next line */
	    write (output, "\n", 1);
	    cur_col = 1;
	    ++out_lines;
	}
	cur_col = pad_output (cur_col, com_col);
	write (output, s_com, e_com - s_com);

	cur_col = count_spaces (cur_col, s_com);
	if ((cur_col - 1) > max_col && output!=1)/* check for too long comment */
	    printf ("%d: Comment goes to column %d.  Max is %d\n",
		line_no, (cur_col - 1), max_col);

	bl_line = false;
	++com_lines;	       /* count lines with comments */
    }

    if (use_ff)
	write (output, &ff, 1);/* end the output with a ff */
    else
	write (output, "\n", 1); /* or a newline */
    use_ff = false;
    *(e_lab = s_lab) = '\0';   /* reset buffers */
    *(e_code = s_code) = '\0';
    *(e_com = s_com) = '\0';

    ind_level = i_l_follow;
    paren_level = p_l_follow;
    ++out_lines;
    decl_on_line = in_decl;    /* if we are in the middle of a
			          declaration, remember that fact for
			          proper comment indentation */
    ind_stmt = in_stmt & ~in_decl;
 /* next line should be indented if we have not completed this stmt and if
    we are not in the middle of a declaration */

    return;
};
/*

			  Copyright (C) 1976
				by the
			  Board of Trustees
				of the
			University of Illinois

			 All rights reserved


NAME:
	fill_buffer

FUNCTION:
	Reads one block of input into input_buffer

ALGORITHM:
	Trivial

PARAMETERS:
	None

RETURNS:
	Nothing

GLOBALS:
	in_buffer =
	buf_end =	Set to 1 past last character read in
	buf_ptr =	Set to start of buffer
	be_save =	Set to zero if it was non-zero
	bp_save =	Set to zero

CALLS:
	read (lib)

CALLED BY:
	lexi
	main
	pr_comment

HISTORY:
	initial coding 	November 1976	D A Willcox of CAC
	1/7/77		D A Willcox of CAC	Added check for switch back to
						partly full input buffer from
						temporary buffer 

*/
int     fill_buffer () { /* this routine reads stuff from the input */
    int     count;
    register int    i;

    if (bp_save != 0) {	       /* there is a partly filled input buffer
			          left */
	buf_ptr = bp_save;     /* don't read anything, just switch buffers 
			       */
	buf_end = be_save;
	bp_save = be_save = 0;
	if (buf_ptr < buf_end)
	    return;	       /* only return if there is really something
			          in this buffer */
    }

    count = read (input, in_buffer, inp_bufs);

    buf_end = in_buffer + count;
    buf_ptr = in_buffer;

    if (count == 0) {	       /* count of zero means eof */
	had_eof = true;
	*buf_end++ = ' ';
	*buf_end++ = '\n';     /* insert extra newline.  it will
			          eventually get indent to stop */
    }

    return;
};
/*

			  Copyright (C) 1976
				by the
			  Board of Trustees
				of the
			University of Illinois

			 All rights reserved


NAME:
	pad_output

FUNCTION:
	Writes tabs and spaces to move the current column up to the
	desired position.

ALGORITHM:
	Put tabs and/or blanks into pobuf, then write pobuf.

PARAMETERS:
	current		integer		The current column
	target		integer		The desired column

RETURNS:
	Integer value of the new column.  (If current >= target,
	no action is taken, and current is returned.

GLOBALS:
	None

CALLS:
	write (sys)

CALLED BY:
	dump_line

HISTORY:
	initial coding 	November 1976	D A Willcox of CAC

*/
int     pad_output (current, target)/* writes tabs and blanks (if necessary) to
			          get the current output position up to
			          the target column */
int     current;	       /* the current column value */
int     target;		       /* position we want it at */
{
    register int    curr; /* internal column pointer */
    register char  *p; /* pointer into buffer of characters to be written */
    char    pobuf[256]; /* pad characters are stored here before writing */
    register int tcur;

    if (current >= target)
	return (current);      /* line is already long enough */

    curr = current;
    p = pobuf;
    while (curr < target) {
	if ((tcur = ((curr - 1) & tabmask) + tabsize + 1) <= target){
	    *p++ = '\t';       /* put a tab into buffer */
	    curr = tcur;
	}
	else {
	    while (curr++ < target)
		*p++ = ' ';    /* pad with final blanks */
	}
    }

    write (output, pobuf, p - pobuf); /* write the characters we saved */
    return (target);
};
/*

			  Copyright (C) 1976
				by the
			  Board of Trustees
				of the
			University of Illinois

			 All rights reserved


NAME:
	count_spaces

FUNCTION:
	Find out where printing of a given string will leave the current
	character position on output.

ALGORITHM:
	Run thru input string and add appropriate values to current position.

PARAMETERS:
	current		integer		  The current line character position
	buffer		ptr to character  Pointer to input string

RETURNS:
	Integer value of position after printing "buffer" starting in
	column "current".

GLOBALS:
	None

CALLS:
	None

CALLED BY:
	pr_comment

HISTORY:
	initial coding 	November 1976	D A Willcox of CAC

*/
int     count_spaces (current, buffer)
			       /* this routine figures out where the
			          character position will be after
			          printing the text in buffer starting at
			          column "current" */
int     current;
char   *buffer;
{
    register char  *buf; /* used to look thru buffer */
    register int    cur; /* current character counter */

    cur = current;

    for (buf = buffer; *buf != '\0'; ++buf) {
	switch (*buf) {

	    case '\n': 
	    case 014: 	       /* form feed */
		cur = 1;
		break;

	    case '\t': 
		cur = ((cur - 1) & tabmask) + tabsize + 1;
		break;

	    case '': 	       /* this is a backspace */
		--cur;
		break;

	    default: 
		++cur;
		break;
	}		       /* end of switch */
    }			       /* end of for loop */

    return (cur);
};
/*

			  Copyright (C) 1976
				by the
			  Board of Trustees
				of the
			University of Illinois

			 All rights reserved


NAME:
	eqin

FUNCTION:
	Returns true if the first arg matches the beginning of the second arg.

ALGORITHM:
	Trivial

PARAMETERS:
	str1	pointer to character
	str2	pointer to character

RETURNS:
	1 if first string matches start of second string
	0 otherwise

GLOBALS:
	None

CALLS:
	None

CALLED BY:
	lexi
	main

HISTORY:
	initial coding November 1976 by D A Willcox of CAC

*/
eqin (str1, str2)
char   *str1;
char   *str2;
{
    register char  *s1; /* local pointer into first string */
    register char  *s2; /* local pointer into second string */

    s1 = str1;
    s2 = str2;
    while (*s1) {	       /* compare no further than end of first
			          string */
	if (*s2 == 0)	       /* check that second string isn't too short 
			       */
	    return (false);
	if (*s1++ != *s2++)
	    return (false);
    }

    return (true);
}
/*
			  Copyright (C) 1976
				by the
			  Board of Trustees
				of the
			University of Illinois

			 All rights reserved

NAME:
	cmp

FUNCTION:
	Compares two strings

ALGORITHM:
	Trivial

PARAMETERS:
	a	Pointer to char		First string to compare
	b	Pointer to char		Second string to compare

RETURNS:
	-1 if a < b
	 0 if a = b
	 1 if a > b

GLOBALS:
	None

CALLS:
	None

CALLED BY:
	main

HISTORY:
	1/7/77		D A Willcox of CAC	Initial Coding
*/
int     cmp (a, b)
char   *a;
char   *b;
{
    register char  *ta,
                   *tb;

    ta = a;
    tb = b;

    while (*ta) {
	if (*ta > *tb)
	    return (1);
	if (*ta < *tb)
	    return (-1);
	++ta;
	++tb;
    }
    if (*tb)
	return (1);
    else
	return (0);
}
