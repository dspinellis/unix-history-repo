/*
 * Copyright (c) 1980 Regents of the University of California.
 * Copyright (c) 1976 Board of Trustees of the University of Illinois.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley and the University
 * of Illinois, Urbana.  The name of either
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)io.c	5.7 (Berkeley) %G%";
#endif /* not lint */

/*
 * FILE NAME:
 *	io.c
 * PURPOSE:
 *	Contains routines to handle i/o related stuff for indent.
 * GLOBALS:
 *	None
 * FUNCTIONS:
 *	dump_line
 *	fill_buffer
 *	pad_output
 *	count_spaces
 *	eqin
 *	cmp
 *
 */
/*-
 *
 *			  Copyright (C) 1976
 *				by the
 *			  Board of Trustees
 *				of the
 *			University of Illinois
 *
 *			 All rights reserved
 *
 *
 * NAME:
 *	dump_line
 *
 * FUNCTION:
 *	Does the actual printing of the stored up line
 *
 * ALGORITHM:
 *	For each of the label, code, and comment sections which are used on 
 *	this line:
 *
 *	1) Use pad_output to get the section aligned properly.
 *	2) write the section
 *
 *	The indentation level used for the code is set by ps.ind_level.  After
 *	printing, ps.ind_level is set to ps.i_l_follow.
 *
 *	An extra level of indentation is added if ps.ind_stmt is 1.  After 
 *	printing, ps.ind_stmt is set to 1 iff the line just printed has an
 *	unterminated, non-declaration statement.
 *
 * HISTORY:
 *	initial coding 	November 1976	D A Willcox of CAC
 *
 */
#include "indent_globs.h"



int         ff = 014;		/* used to write a form feed */
int         comment_open;
static      paren_target;

dump_line()
{				/* dump_line is the routine that actually
				 * effects the printing of the new source.
				 * It prints the label section, followed
				 * by the code section with the
				 * appropriate nesting level, followed by
				 * any comments */
    register int cur_col,
                temp_col,
                target_col;

    if (ps.procname[0]) {
	if (troff)
	    fprintf(output, ".Pr \"%s\"\n", ps.procname);
	ps.ind_level = 0;
	ps.procname[0] = 0;
    }
    if (s_code == e_code && s_lab == e_lab && s_com == e_com) {
	if (suppress_blanklines>0) suppress_blanklines--;
	else {
	ps.bl_line = true;
	n_real_blanklines++;
	}
    }
    else if (!inhibit_formatting) {
	suppress_blanklines = 0;
	ps.bl_line = false;
	if (prefix_blankline_requested)
	    if (swallow_optional_blanklines) {
		if (n_real_blanklines == 1)
		    n_real_blanklines = 0;
	    }
	    else {
		if (n_real_blanklines == 0)
		    n_real_blanklines = 1;
	    }
	while (--n_real_blanklines >= 0)
	    putc('\n', output);
	n_real_blanklines = 0;
	if (ps.ind_level == 0)
	    ps.ind_stmt = 0;	/* this is a class A kludge. dont do
				 * additional statement indentation if we
				 * are at bracket level 0 */

	if (e_lab != s_lab || e_code != s_code)
	    ++code_lines;	/* keep count of lines with code */


	if (e_lab != s_lab) {	/* print lab, if any */
	    if (comment_open) {
		comment_open = 0;
		fprintf(output, ".*/\n");
	    }
	    while (e_lab > s_lab && (e_lab[-1] == ' ' || e_lab[-1] == '\t'))
		e_lab--;
	    cur_col = pad_output(1, compute_label_target());
	    fprintf(output, "%.*s", e_lab - s_lab, s_lab);
	    cur_col = count_spaces(cur_col, s_lab);
	}
	else
	    cur_col = 1;	/* there is no label section */

	ps.pcase = false;

	if (s_code != e_code) {	/* print code section, if any */
	    register char *p;

	    if (comment_open) {
		comment_open = 0;
		fprintf(output, ".*/\n");
	    }
	    target_col = compute_code_target();
	    {
		register    i;

		for (i = 0; i < ps.p_l_follow; i++)
		    if (ps.paren_indents[i] >= 0)
			ps.paren_indents[i] = -(ps.paren_indents[i] + target_col);
	    }
	    cur_col = pad_output(cur_col, target_col);
	    for (p = s_code; p < e_code; p++)
		if (*p == (char)0200)
		    fprintf(output, "%d", target_col * 7);
		else
		    putc(*p, output);
	    cur_col = count_spaces(cur_col, s_code);
	}
	if (s_com != e_com)
	    if (troff) {
		register char *p;

		if (e_com[-1] == '/' && e_com[-2] == '*')
		    e_com -= 2;
		while (e_com > s_com && e_com[-1] == ' ')
		    e_com--;
		*e_com = 0;
		p = s_com;
		while (*p == ' ')
		    p++;
		if (p[0] == '/' && p[1] == '*')
		    p += 2;
		else if (p[0] == '*')
		    p += p[1] == '/' ? 2 : 1;
		while (*p == ' ')
		    p++;
		if (*p == 0)
		    goto inhibit_newline;
		if (!comment_open) {
		    if ('a' <= *p && *p <= 'z')
			*p = *p + 'A' - 'a';
		    if (s_code != e_code || s_lab != e_lab) {
			fprintf(output, "\\c\n./* %dp 1 %dp\n",
				ps.com_col * 7, target_col * 7);
		    }
		    else
			fprintf(output, "./* %dp 0 %dp\n",
				ps.com_col * 7, target_col * 7);
		}
		comment_open = 1;
		while (*p) {
		    if (*p == BACKSLASH)
			putc(BACKSLASH, output);
		    putc(*p++, output);
		}
	    }
	    else {		/* print comment, if any */
		register    target = ps.com_col;
		register char *com_st = s_com;

		target += ps.comment_delta;
		while (target <= 0)
		    if (*s_com == ' ')
			target++, s_com++;
		    else if (*s_com == '\t')
			target = ((target - 1) & ~7) + 9, s_com++;
		    else
			target = 1;
		if (cur_col > target) {	/* if comment cant fit on this
					 * line, put it on next line */
		    putc('\n', output);
		    cur_col = 1;
		    ++ps.out_lines;
		}
		cur_col = pad_output(cur_col, target);
		if (!ps.box_com) {
		    if (star_comment_cont && com_st[1] != '*')
			if (com_st[1] == ' ' && com_st[0] == ' ')
			    com_st[1] = '*';
			else
			    fwrite(" * ", com_st[0] == '\t' ? 2 : com_st[0] == '*' ? 1 : 3, 1, output);
		}
		fwrite(com_st, e_com - com_st, 1, output);
		ps.comment_delta = ps.n_comment_delta;
		cur_col = count_spaces(cur_col, com_st);
		++ps.com_lines;	/* count lines with comments */
	    }
	if (ps.use_ff)
	    putc('\014', output);
	else
	    putc('\n', output);
inhibit_newline:
	++ps.out_lines;
	if (ps.just_saw_decl == 1 && blanklines_after_declarations) {
	    prefix_blankline_requested = 1;
	    ps.just_saw_decl = 0;
	}
	else
	    prefix_blankline_requested = postfix_blankline_requested;
	postfix_blankline_requested = 0;
    }
    ps.decl_on_line = ps.in_decl;	/* if we are in the middle of a
					 * declaration, remember that fact
					 * for proper comment indentation */
    ps.ind_stmt = ps.in_stmt & ~ps.in_decl;	/* next line should be
						 * indented if we have not
						 * completed this stmt and
						 * if we are not in the
						 * middle of a declaration */
    ps.use_ff = false;
    ps.dumped_decl_indent = 0;
    *(e_lab = s_lab) = '\0';	/* reset buffers */
    *(e_code = s_code) = '\0';
    *(e_com = s_com) = '\0';
    ps.ind_level = ps.i_l_follow;
    ps.paren_level = ps.p_l_follow;
    paren_target = -ps.paren_indents[ps.paren_level - 1];
    return;
};

compute_code_target() {
    register    target_col = ps.ind_size * ps.ind_level + 1;

    if (ps.paren_level)
	if (!lineup_to_parens)
	    target_col += continuation_indent * ps.paren_level;
	else {
	    register    w;
	    register    t = paren_target;

	    if ((w = count_spaces(t, s_code) - max_col) > 0
		&& count_spaces(target_col, s_code) <= max_col) {
		t -= w + 1;
		if (t > target_col)
		    target_col = t;
	    }
	    else
		target_col = t;
	}
    else if (ps.ind_stmt)
	target_col += continuation_indent;
    return target_col;
}

compute_label_target()
{
    return
	ps.pcase ? (int) (case_ind * ps.ind_size) +1
	: *s_lab == '#' ? 1
	: ps.ind_size * (ps.ind_level - label_offset) +1;
}


/*
 * Copyright (C) 1976 by the Board of Trustees of the University of
 * Illinois 
 *
 * All rights reserved 
 *
 *
 * NAME: fill_buffer 
 *
 * FUNCTION: Reads one block of input into input_buffer 
 *
 * HISTORY: initial coding 	November 1976	D A Willcox of CAC 1/7/77
 * A Willcox of CAC	Added check for switch back to partly full input
 * buffer from temporary buffer 
 *
 */
int
fill_buffer()
{				/* this routine reads stuff from the input */
    int         count;
    register char *p;
    register int i;
    register FILE *f = input;

    if (bp_save != 0) {		/* there is a partly filled input buffer
				 * left */
	buf_ptr = bp_save;	/* dont read anything, just switch buffers */
	buf_end = be_save;
	bp_save = be_save = 0;
	if (buf_ptr < buf_end)
	    return;		/* only return if there is really
				 * something in this buffer */
    }
    p = in_buffer;
    buf_ptr = p;
    while ((*p++ = i = getc(f)) != EOF && i != '\n');
    if (i == EOF) {
	p[-1] = ' ';
	*p++ = '\n';
	had_eof = true;
    }
    buf_end = p;
    if (p[-2] == '/' && p[-3] == '*') {
	if (in_buffer[3] == 'I' && strncmp(in_buffer, "/**INDENT**", 11) == 0)
	    fill_buffer();	/* flush indent error message */
	else {
	    int         com = 0;

	    p = in_buffer;
	    while (*p == ' ' || *p == '\t')
		p++;
	    if (*p == '/' && p[1] == '*') {
		p += 2;
		while (*p == ' ' || *p == '\t')
		    p++;
		if (p[0] == 'I' && p[1] == 'N' && p[2] == 'D' && p[3] == 'E'
		    && p[4] == 'N' && p[5] == 'T') {
		    p += 6;
		    while (*p == ' ' || *p == '\t')
			p++;
		    if (*p == '*')
			com = 1;
		    else if (*p == 'O')
			if (*++p == 'N')
			    p++, com = 1;
			else if (*p == 'F' && *++p == 'F')
			    p++, com = 2;
		    while (*p == ' ' || *p == '\t')
			p++;
		    if (p[0] == '*' && p[1] == '/' && p[2] == '\n' && com) {
			if (s_com != e_com || s_lab != e_lab || s_code != e_code)
			    dump_line();
			if (!(inhibit_formatting = com - 1)) {
			    n_real_blanklines = 0;
			    postfix_blankline_requested = 0;
			    prefix_blankline_requested = 0;
			    suppress_blanklines = 1;
			}
		    }
		}
	    }
	}
    }
    if (inhibit_formatting) {
	p = in_buffer;
	do
	    putc(*p, output);
	while (*p++ != '\n');
    }
    return;
};

/*
 * Copyright (C) 1976 by the Board of Trustees of the University of
 * Illinois 
 *
 * All rights reserved 
 *
 *
 * NAME: pad_output 
 *
 * FUNCTION: Writes tabs and spaces to move the current column up to the
 * desired position. 
 *
 * ALGORITHM: Put tabs and/or blanks into pobuf, then write pobuf. 
 *
 * PARAMETERS: current		integer		The current column target
 * nteger		The desired column 
 *
 * RETURNS: Integer value of the new column.  (If current >= target, no
 * action is taken, and current is returned. 
 *
 * GLOBALS: None 
 *
 * CALLS: write (sys) 
 *
 * CALLED BY: dump_line 
 *
 * HISTORY: initial coding 	November 1976	D A Willcox of CAC 
 *
 */
pad_output(current, target)	/* writes tabs and blanks (if necessary)
				 * to get the current output position up
				 * to the target column */
    int         current;	/* the current column value */
    int         target;		/* position we want it at */
{
    register int curr;		/* internal column pointer */
    register int tcur;

    if (troff)
	fprintf(output, "\\h'|%dp'", (target - 1) * 7);
    else {
	if (current >= target)
	    return (current);	/* line is already long enough */
	curr = current;
	while ((tcur = ((curr - 1) & tabmask) + tabsize + 1) <= target) {
	    putc('\t', output);
	    curr = tcur;
	}
	while (curr++ < target)
	    putc(' ', output);	/* pad with final blanks */
    }
    return (target);
};

/*
 * Copyright (C) 1976 by the Board of Trustees of the University of
 * Illinois 
 *
 * All rights reserved 
 *
 *
 * NAME: count_spaces 
 *
 * FUNCTION: Find out where printing of a given string will leave the current
 * character position on output. 
 *
 * ALGORITHM: Run thru input string and add appropriate values to current
 * position. 
 *
 * RETURNS: Integer value of position after printing "buffer" starting in
 * column "current". 
 *
 * HISTORY: initial coding 	November 1976	D A Willcox of CAC 
 *
 */
int
count_spaces(current, buffer)

/*
 * this routine figures out where the character position will be after
 * printing the text in buffer starting at column "current" 
 */
    int         current;
    char       *buffer;
{
    register char *buf;		/* used to look thru buffer */
    register int cur;		/* current character counter */

    cur = current;

    for (buf = buffer; *buf != '\0'; ++buf) {
	switch (*buf) {

	    case '\n':
	    case 014:		/* form feed */
		cur = 1;
		break;

	    case '\t':
		cur = ((cur - 1) & tabmask) + tabsize + 1;
		break;

	    case '':		/* this is a backspace */
		--cur;
		break;

	    default:
		++cur;
		break;
	}			/* end of switch */
    }				/* end of for loop */
    return (cur);
};

int	found_err;
diag(level, msg, a, b)
{
    if (level)
	found_err = 1;
    if (output == stdout) {
	fprintf(stdout, "/**INDENT** %s@%d: ", level == 0 ? "Warning" : "Error", line_no);
	fprintf(stdout, msg, a, b);
	fprintf(stdout, " */\n");
    }
    else {
	fprintf(stderr, "%s@%d: ", level == 0 ? "Warning" : "Error", line_no);
	fprintf(stderr, msg, a, b);
	fprintf(stderr, "\n");
    }
}
