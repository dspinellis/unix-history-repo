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
static char sccsid[] = "@(#)sub.c	8.1 (Berkeley) 5/31/93";
#endif /* not lint */

#include <sys/types.h>

#include <regex.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef DBI
#include <db.h>
#endif

#include "ed.h"
#include "extern.h"

/*
 * The substitute command. It's big because of the backward compatability.
 */
void
s(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	static int l_count2 = 1, l_global = 0, l_print = 0;
	static int l_first_pass_flag = 0;
	static char *l_match = NULL, *l_repl = NULL;
	LINE *l_s_ret, *l_temp_line, *l_temp_line2, *l_kval, *l_last, *l_cur;
	int l_s_flag, l_count, l_matched, l_nflag, l_cnt, yy, l_sr_flag = 0;
	int l_err, l_sl, l_rep_flag, l_u_reuse_flag=0, l_local_len, l_nudge=0;
	char *l_match2 = NULL, *l_local = NULL, *l_local_temp = NULL;
#ifndef REG_STARTEND
	size_t l_offset = 0;
#endif

	if (Start_default && End_default)
		Start = End = current;
	else
		if (Start_default)
			Start = End;
	if (Start == NULL) {
		*errnum = -1;
		return;
	}
	Start_default = End_default = 0;

	l_sl = ss = getc(inputt);
	if (l_first_pass_flag == 0)
		l_match = l_repl = NULL;
	l_match2 = get_pattern(l_sl, inputt, errnum, 0);

	if (*errnum < 0) {
		if ((*errnum == -2) && (l_sl != '\n'))
			return;
		if ((l_match2 == NULL) ||
		    (strlen(l_match2) > 4) || (l_first_pass_flag == 0))
			return;
		*errnum = 0;
		l_sr_flag = -1;
		for (yy = 0; yy < (strlen(l_match2)); yy++) {
			switch (l_match2[yy]) {
			case '\n':
				ss = getc(inputt);
				goto bcg1;
				break;
			case 'r':
				l_sr_flag = 1;
				break;
			case 'p':
				l_print = (l_print) ? 0 : 1;
				break;
			case 'g':
				l_global = (l_global) ? 0 : 1;
				break;
			case 'N':
				l_count2 = 1;
				break;
			default:
				*errnum = -1;
				strcpy(help_msg, "illegal modifier to s");
				return;
			}
		}
		ss = getc(inputt);
		if (l_sr_flag == 1)
			goto bcg2;
		else
			goto bcg1;
	}
	if (l_first_pass_flag) {
		free(l_match);
		free(l_repl);
	} else
		l_first_pass_flag = 1;
	l_match = l_match2;
	*errnum = 0;
	l_repl = get_pattern(ss, inputt, errnum, 1);
	if (sigint_flag && (!sigspecial))
		SIGINT_ACTION;
	l_global = l_print = 0;
	if (*errnum < 0)
		if ((*errnum == -1) && (ss == '\n'))
			/* Note, \n still in stream for next getc. */
			l_print = 1;
		else
			return;
	*errnum = 0;

	l_count2 = 1;

	while (((ss = getc(inputt)) != '\n') && (ss != EOF))
		if (ss == 'g')
			l_global = 1;
		else
			switch (ss) {
			case 'p':
				l_print = (l_print | (int) 1);
				break;
			case 'n':
				l_print = (l_print | (int) 2);
				break;
			case 'l':
				l_print = (l_print | (int) 4);
				break;
			default:
				if ((ss > ('0' - 1)) && (ss < ('9' + 1)))
					l_count2 = dig_num_conv(inputt, errnum);
				else {
					*errnum = -1;
					strcpy(help_msg,
					    "illegal command option");
					return;
				}
		}

bcg1:
	if ((RE_flag == 0) && (l_match[1] == '\0')) {
		*errnum = -1;
		ungetc(ss, inputt);
		return;
	} else
		if ((l_sr_flag == 0) && (l_match[1] || (RE_patt == NULL))) {
			int l_m_len = 2 + strlen(l_match);
			sigspecial++;
			free(RE_patt);
			RE_patt = malloc(sizeof(char) * (l_m_len));
			memmove(RE_patt, l_match, l_m_len);
			sigspecial--;
			if (sigint_flag && (!sigspecial))
				SIGINT_ACTION;
		}
	RE_sol = (l_match[1] == '^') ? 1 : 0;
	if ((l_match[1]) &&
	    (regfree(&RE_comp), l_err = regcomp(&RE_comp, &l_match[1], 0))) {
		regerror(l_err, &RE_comp, help_msg, 128);
		*errnum = -1;
		RE_flag = 0;
		ungetc(ss, inputt);
		return;
	}
	RE_flag = 1;
	if (sigint_flag && (!sigspecial))
		SIGINT_ACTION;
bcg2:
	l_cur = current;
	current = Start;
	l_s_flag = 0;
	do {
		RE_match[0].rm_eo = 0;
		get_line(current->handle, current->len);
		if (sigint_flag && (!sigspecial))
			SIGINT_ACTION;
		l_count = l_count2;
		l_local = text;
		l_rep_flag = 1;
#ifndef REG_STARTEND
		l_offset = 0;
#else
		l_local_len = current->len;
#endif
		do {
			RE_match[0].rm_so = RE_match[0].rm_eo;
			if (l_nudge)
				RE_match[0].rm_so++;
			l_nudge = 0;
#ifdef REG_STARTEND
			l_matched = regexec_n(&RE_comp, l_local,
			    (size_t)RE_SEC, RE_match, 0, l_count,
			    (size_t)l_local_len, 0);
#else
			l_matched = regexec_n(&RE_comp, l_local,
			    (size_t)RE_SEC, RE_match, 0, l_count,
			    &l_offset, 0);
#endif
			if (l_matched == 0) {
				if ((l_s_flag == 0) && (g_flag == 0)) {
					current = l_cur;
					u_clr_stk();
					current = Start;
				}
				l_count = l_s_flag = 1;
				l_rep_flag = 0;
				/*
				 * The l_local passed into re_replace is not
				 * freed in re_replace because it is "text",
				 * the global line holder, for the first pass
				 * through this loop. The value returned by
				 * re_replace is a new string (with the first
				 * replacement in it). If the 'g' flag was
				 * set with substitute then this new string
				 * is passed in for the second pass and can
				 * be freed once re_replace is done with it.
				 * (...and so on for the rest of the 'g'
				 * passes. RE_match[0].rm_eo is changed in
				 * re_replace to be the new location of the
				 * next character immediately after the
				 * replacement since it is likely the
				 * position of that character has changed
				 * because of the replacement.
				 */
				if (RE_match[0].rm_so == RE_match[0].rm_eo)
					l_nudge = 1;
#ifdef REG_STARTEND
				l_local = re_replace(l_local,
				    (size_t)(RE_SEC - 1), RE_match, &l_repl[1]);
				l_local_len = strlen(l_local);
#else
				l_local = re_replace(l_local,
				    (size_t)(RE_SEC - 1), RE_match, &l_repl[1],
				    l_offset);
#endif
			}
			if (l_global == 0)
				break;
			if (l_local[RE_match[0].rm_eo] == '\0')
				break;
		} while (!l_matched);

		if (l_rep_flag)
			goto next;
		l_cnt = l_nflag = 0;
		l_kval = current;
		l_temp_line = current->above;
		l_temp_line2 = current->below;
		l_local_temp = l_local;
		sigspecial++;
		for (;;) {
			/*
			 * Make the new string the one for this line.  Check if
			 * it needs to be split.
			 */
			if (l_local[l_cnt] == '\n' || l_local[l_cnt] == '\0') {
				if (l_local[l_cnt] == '\0')
					l_nflag = 1;
				l_local[l_cnt] = '\0';
				l_s_ret = malloc(sizeof(LINE));
				if (l_s_ret == NULL) {
					*errnum = -1;
					strcpy(help_msg, "out of memory error");
					return;
				}
				(l_s_ret->len) = strlen(l_local);
				(l_s_ret->handle) = add_line(l_local, l_s_ret->len);
				(l_s_ret->above) = l_temp_line;
				(l_s_ret->below) = NULL;
				if (l_temp_line == NULL)
					top = l_s_ret;
				else {
					if ((current != Start) &&
						((&(current->above)) == u_stk->cell))
						l_u_reuse_flag = 1;
					else {
						u_add_stk(&(l_temp_line->below));
						l_u_reuse_flag = 0;
					}
					(l_temp_line->below) = l_s_ret;
				}
				l_temp_line = l_s_ret;
				if ((l_local[l_cnt] == '\0') && (l_nflag == 1))
					break;
				else {
					l_local = &(l_local[l_cnt + 1]);
					l_cnt = 0;
				}
			} else
				l_cnt++;
		}
		(l_s_ret->below) = l_temp_line2;
		ku_chk(current, current, l_kval->below);
		if (current == End)
			End = l_s_ret;
		current = l_s_ret;
		l_last = current;
		if (l_temp_line2 == NULL)
			bottom = l_s_ret;
		else {
			if (l_u_reuse_flag)
				u_pop_n_swap(&(l_temp_line2->above));
			else
				u_add_stk(&(l_temp_line2->above));
			(l_temp_line2->above) = current;
		}
		sigspecial--;
		if (sigint_flag && (!sigspecial))
			SIGINT_ACTION;
		if (l_local_temp != text)
			free(l_local_temp);
next:
		current = current->below;
	} while (current != (End->below));

	if (l_s_flag == 0) {
		current = Start;
		if (!g_flag) {
			strcpy(help_msg, "no matches found for substitution");
			*errnum = -1;
			ungetc('\n', inputt);
		}
		else
			*errnum = 0;
		return;
	}
	change_flag = 1;
	current = l_last;

	if (l_print > 0) {
		Start = End = l_s_ret; /*current;*/
		ungetc(ss, inputt);
		if (l_print == (l_print | (int) 1))
			p(inputt, errnum, 0);
		if (l_print == (l_print | (int) 2))
			p(inputt, errnum, 1);
		if (l_print == (l_print | (int) 4))
			l(inputt, errnum);
		if (*errnum < 0)
			return;
	}
	if (l_sr_flag == -1) {
		regfree(&RE_comp);
		regcomp(&RE_comp, &RE_patt[1], 0);
	}
	*errnum = 1;
}
