/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rodney Ruddock of the University of Guelph.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)g.c	8.1 (Berkeley) 5/31/93";
#endif /* not lint */

#include <sys/types.h>

#include <limits.h>
#include <regex.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef DBI
#include <db.h>
#endif

#include "ed.h"
#include "extern.h"

static int	find_line __P((LINE *));
static void	w_cmd_l_file __P((FILE *, FILE *, int *));

/*
 * Find a line that we noted matched the RE earlier in the current
 * buffer (it may have disappeared because of the commands in the
 * command list).
 */
static int
find_line(dot)
	LINE *dot;
{
	LINE *l_cl;

	l_cl = top;
	for (;;) {
		if (l_cl == dot)
			return (1);
		if (l_cl == bottom)
			return (0);
		l_cl = l_cl->below;
	}
}

/*
 * Write the command line to a STDIO tmp file. See g() below.
 * This allows us to use cmd_loop to run the command list because
 * we "trick" cmd_loop into reading a STDIO file instead of stdin.
 */

static void
w_cmd_l_file(fp, inputt, errnum)
	FILE *fp, *inputt;
	int *errnum;
{
	int sl=0, jmp_flag, l_cnt=0;

	if (jmp_flag = setjmp(ctrl_position3))
		return;

	for (;;) {
		sigspecial3 = 1;
		ss = getc(inputt);
		sigspecial3 = 0;
skip1:
		if (ss == EOF)
			goto skip2;
		else if (ss == '\n') {
			if (sl != '\\') {
skip2:
				if (l_cnt == 0)
					fputc('p', fp);
				else
					fputc(sl, fp);
				break;
			}
		}
		else if ((ss == '\\') && (sl == '\\')) {
			sigspecial3 = 1;
			sl = getc(inputt);
			sigspecial3 = 0;
			if (sl == '\\') {
				sigspecial3 = 1;
				sl = getc(inputt);
				sigspecial3 = 0;
				if (sl == EOF)
					goto skip2;
				if (sl == '\n') {
					fputc('\\', fp);
					ss = sl;
				}
				else {
					fputc('\\', fp);
					fputc('\\', fp);
					ss = sl;
					sl = '\\';
					goto skip1;
				}
			}
			else {
				fputc('\\', fp);
				fputc('\\', fp);
				if ((sl == '\n') || (sl == EOF))
					goto skip2;
				else
					ss = sl;
			}
		}
		else if (l_cnt)
			fputc(sl, fp);
		sl = ss;
		l_cnt++;
	}

	fputc('\n', fp);
	if (ss == EOF)
		clearerr(inputt);
}


/*
 * The global function. All global commands (g, G, v, and V) are handled
 * in here. The lines to be affected by the command list are 1st noted
 * and then the command list is invoked for each line matching the RE.
 * Note the trick of how the command list is executed. Saves a lot of
 * code (and allows for \n's in substitutions).
 */
void
g(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	static char *l_template_g;
	char *l_patt;
	static int l_template_flag = 0;
	int l_re_success, l_flag_v = 0, l_err, l_num;
	register l_gut_cnt, a;
	register LINE **l_gut=gut;
	FILE *l_fp;
#ifdef POSIX
	LINE *l_posix_cur;
#endif

	if (Start_default && End_default) {
		Start = top;
		End = bottom;
	} else
		if (Start_default)
			Start = End;
	if (Start == NULL) {
		strcpy(help_msg, "buffer empty");
		*errnum = -1;
		return;
	}

	if (l_template_flag == 0) {
		sigspecial++;
		l_template_flag = 1;
		l_template_g = calloc(FILENAME_LEN, sizeof(char));
		sigspecial--;
		if (sigint_flag && (!sigspecial))
			SIGINT_ACTION;
		if (l_template_g == NULL) {
			*errnum = -1;
			strcpy(help_msg, "out of memory error");
			return;
		}
	}
	/* set up the STDIO command list file */
	memmove(l_template_g, "/tmp/_4.4bsd_ed_g_XXXXXX\0", 24);
	mktemp(l_template_g);

	if ((ss == 'v') || (ss == 'V'))
		l_flag_v = 1;

	if ((ss == 'G') || (ss == 'V')) {
		/*
		 * If it's an interactive global command we use stdin, not a
		 * file.
		 */
		GV_flag = 1;
		l_fp = stdin;
	} else {
		sigspecial++;
		if ((l_fp = fopen(l_template_g, "w+")) == NULL) {
			perror("ed: file I/O error, save buffer in ed.hup");
			do_hup(); /* does not return */
		}
		sigspecial--;
		if (sigint_flag && (!sigspecial))
			goto point;
	}

	ss = getc(inputt);

	/* Get the RE for the global command. */
	l_patt = get_pattern(ss, inputt, errnum, 0);

	/* Instead of: if ((*errnum == -1) && (ss == '\n'))... */
	if (*errnum < -1)
		return;
	*errnum = 0;
	if ((l_patt[1] == '\0') && (RE_flag == 0)) {
		*errnum = -1;
		ungetc(ss, inputt);
		return;
	} else
		if (l_patt[1] || (RE_patt == NULL)) {
			sigspecial++;
			free(RE_patt);
			RE_patt = l_patt;
			sigspecial--;
			if (sigint_flag && (!sigspecial))
				goto point;
		}
	RE_sol = (RE_patt[1] == '^') ? 1 : 0;
	if ((RE_patt[1]) &&
	    (regfree(&RE_comp), l_err = regcomp(&RE_comp, &RE_patt[1], 0))) {
		regerror(l_err, &RE_comp, help_msg, 128);
		*errnum = -1;
		RE_flag = 0;
		ungetc(ss, inputt);
		return;
	}
	RE_flag = 1;

	if (GV_flag)
		ss = getc(inputt);

#ifdef POSIX
	l_posix_cur = current;
#endif
	current = Start;

	sigspecial++;

	if ((l_num = line_number(bottom)) > gut_num) {
		sigspecial++;
		gut_num = l_num + 512;
		free(l_gut);
		gut = l_gut = malloc(sizeof(LINE **) * gut_num);
		sigspecial--;
		if (l_gut == NULL) {
			*errnum = -1;
			strcpy(help_msg, "out of memory error");
#ifdef POSIX
			current = l_posix_cur;
#endif
			ungetc('\n', inputt);
			return;
		}
	}
	l_gut_cnt = 0;

	for (;;) {
		/*
		 * Find the lines in the buffer that the global command wants
		 * to work with.
		 */
		get_line(current->handle, current->len);
		if (sigint_flag && (!sigspecial))
			goto point;
		l_re_success =
		    regexec(&RE_comp, text, (size_t) RE_SEC, RE_match, 0);
		/* l_re_success=0 => success */
		if ( (l_re_success == 0 && l_flag_v == 0) ||
			(l_re_success && l_flag_v)) {
				l_gut[l_gut_cnt++] = current;
		}
		if (End == current)
			break;
		current = current->below;
	}
	sigspecial--;
	if (sigint_flag && (!sigspecial))
		goto point;

	if (l_gut_cnt == 0) {
		strcpy(help_msg, "no matches found");
#ifdef POSIX
		current = l_posix_cur;
#endif
		return;
	}
	/* if non-interactive, get the command list */
	if (GV_flag == 0) {
		sigspecial++;
		w_cmd_l_file(l_fp, inputt, errnum);
		sigspecial--;
		if (sigint_flag)
			goto point;
	}

	if (g_flag == 0)
		u_clr_stk();

	sigspecial++;
	for (a=0; a<l_gut_cnt; a++) {
		/*
		 * Execute the command list on the lines that still exist that
		 * we indicated earlier that global wants to work with.
		 */
		if (sigint_flag)
			goto point;
		if (GV_flag == 0)
			fseek(l_fp, (off_t)0, 0);
		if (find_line(l_gut[a])) {
			current = (l_gut[a]);
			get_line(current->handle, current->len);
			if (sigint_flag)
				goto point;
			if (GV_flag == 1)
				printf("%s\n", text);
			g_flag++;
			explain_flag--;
			sigspecial--;
			cmd_loop(l_fp, errnum);
			sigspecial++;
			explain_flag++;
			g_flag--;
			if ((GV_flag == 1) && (*errnum < 0)) {
				ungetc('\n', l_fp);
				break;
			}
			*errnum = 0;
		}
	}

point:
	if (GV_flag == 0) {
		fclose(l_fp);
		unlink(l_template_g);
	}
	else
		ungetc('\n', inputt);

	GV_flag = 0;

#ifdef POSIX
	current = l_posix_cur;
#endif
	sigspecial--;
	if (sigint_flag)
		SIGINT_ACTION;
}
