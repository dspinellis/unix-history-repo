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
static char sccsid[] = "@(#)re.c	8.1 (Berkeley) 5/31/93";
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
 * This finds the n-th occurrence of an RE in a line. If '^' was at the start
 * of the RE then look once (in case n=1). There is no standard RE interface
 * to do this.  Returns 0 for success.  NOTE: the #ifdef REG_STARTEND is if
 * the regex package has the BSD extensions to it.
 */
int
#ifdef REG_STARTEND
regexec_n(reprecomp, strg, num_subexp, reprematch, flags, n, len, pass)
#else
regexec_n(reprecomp, strg, num_subexp, reprematch, flags, n, offset, pass)
#endif
	regex_t *reprecomp;
	char *strg;
	size_t num_subexp;
	regmatch_t reprematch[];
	int flags, n;
#ifdef REG_STARTEND
	size_t len;
#else
	size_t *offset;
#endif
	int pass; /* if pass == 0 .rm_so user set, else set default */
{
	int l_cnt, l_flag=0;
#ifndef REG_STARTEND
	char *l_offset=strg, *l_end;
#endif

	if (n <= 0)
		return (REG_NOMATCH);
#ifdef REG_STARTEND
	flags = (flags | REG_STARTEND);
	if (pass)
		reprematch[0].rm_so = 0;
	reprematch[0].rm_eo = len;
#else
	strg = &strg[offset];
	l_end = &strg[strlen(strg)];
#endif
	for (l_cnt = 0;;) {
		if (regexec(reprecomp,
		    strg, num_subexp, reprematch, flags) == 0)
			l_cnt++;
		else
			return (REG_NOMATCH);

		if (l_cnt >= n)
			break;
#ifdef REG_STARTEND
		if (reprematch[0].rm_so == reprematch[0].rm_eo)
			reprematch[0].rm_eo++;
		reprematch[0].rm_so = reprematch[0].rm_eo;
		if (reprematch[0].rm_so == len)
			return (REG_NOMATCH);
		reprematch[0].rm_eo = len;
#else
		strg = &strg[reprematch[0].rm_eo];
		if (strg == l_end)
                        return (REG_NOMATCH);
#endif
		/* if a "^" started the current RE we only loop once */
		if (RE_sol)
			return (REG_NOMATCH);
	}
#ifndef REG_STARTEND
	*offset = (size_t) (strg - l_offset);
#endif
	return (0);		/* success */
}

/*
 * Replace in the line specified at the found locations with the
 * specified replacement. There is no standard RE interface to do
 * this.
 */
char *
#ifdef REG_STARTEND
re_replace(line, num_subexp, repmatch, replacer)
#else
re_replace(line, num_subexp, repmatch, replacer, offset)
#endif
	char *line;
	size_t num_subexp;
	regmatch_t repmatch[];
	char *replacer;
#ifndef REG_STARTEND
	size_t offset;
#endif
{
	static char *l_prev_r = NULL;
	static int l_prev_r_flag = 0;
	regoff_t l_len_before, l_len_whole, l_slen[RE_SEC];
	int l_cnt, l_len_new = 0, l_new_rm_eo = 0;
	char *l_string, *l_head;

	if (l_prev_r_flag == 0) {
		l_prev_r_flag = 1;
		l_prev_r = NULL;
	}
	l_head = replacer;
	/* Length of what stays the same before. */
	l_len_before = (repmatch[0].rm_so);
	l_len_whole = strlen(line);
	if (num_subexp > RE_SEC - 1)
		num_subexp = RE_SEC - 1;
	for (l_cnt = 0; l_cnt <= num_subexp; l_cnt++)
		l_slen[l_cnt] =
		    (repmatch[l_cnt].rm_eo) - (repmatch[l_cnt].rm_so);

	/*
	 * l_slen[0] == len of what is to be replaced.
	 * l_slen[1-9] == len of each backref.
	 */
	if ((*replacer == '%') && (replacer[1] == '\0')) {
		l_string = calloc(l_len_whole - l_slen[0] +
		    (strlen(l_prev_r)) + 2, sizeof(char));
		if (l_string == NULL) {
			/* *errnum = -1; */
			strcpy(help_msg, "out of memory error");
			return (NULL);
		}
#ifdef REG_STARTEND
		memmove(l_string, line, (int) l_len_before);
#else
		memmove(l_string, line, (int) l_len_before + offset);
#endif
#ifdef REG_STARTEND
		l_string[l_len_before] = '\0';
#else
		l_string[l_len_before + offset] = '\0';
#endif
		strcat(l_string, l_prev_r);
		l_new_rm_eo = strlen(l_string);
#ifdef REG_STARTEND
		strcat(l_string, &line[repmatch[0].rm_eo]);
#else
		strcat(l_string, &line[repmatch[0].rm_eo + offset]);
#endif
		repmatch[0].rm_eo = l_new_rm_eo;
		return (l_string);
	}

	/* Figure out length of new line first. */
	while (*replacer != '\0') {
		/* Add in the length of the RE match. */
		if (*replacer == '&')
			l_len_new = l_len_new + l_slen[0];
		/* Add in the length of a backref. */
		else if (*replacer == '\\') {
			replacer++;
			if ((*replacer > '0') &&
			    (*replacer < ('9' + 1)) &&
			    (repmatch[*replacer - '0'].rm_so > -1))
				/* -1 - -1 = 0 */
				l_len_new = l_len_new + l_slen[*replacer - '0'];
			else
				l_len_new++;
		} else
			l_len_new++;
		replacer++;
	}

	/* Create the line of an appropriate length. */
	l_string =
	    calloc(l_len_whole - l_slen[0] + l_len_new + 2, sizeof(char));
	if (l_string == NULL) {
		strcpy(help_msg, "out of memory error");
		return (NULL);
	}
	if (l_prev_r != NULL)
		free(l_prev_r);
	l_prev_r = calloc(l_len_new + 2, sizeof(char));
	if (l_prev_r == NULL) {
		strcpy(help_msg, "out of memory error");
		return (NULL);
	}
	/* Copy over what doesn't change before the chars to be replaced. */
#ifdef REG_STARTEND
	memmove(l_string, line, (size_t)l_len_before);
#else
	memmove(l_string, line, l_len_before + offset);
#endif
#ifdef REG_STARTEND
	l_string[l_len_before] = '\0';
#else
	l_string[l_len_before + offset] = '\0';
#endif
	l_prev_r[0] = '\0';

	/* Make the replacement. */
	replacer = l_head;
	while (*replacer != '\0') {
		/* Put what matched the RE into the replacement. */
		if (*replacer == '&') {
#ifdef REG_STARTEND
			strncat(l_string,
			    &line[repmatch[0].rm_so], (int)l_slen[0]);
			strncat(l_prev_r,
			    &line[repmatch[0].rm_so], (int) l_slen[0]);
#else
			strncat(l_string,
			    &line[repmatch[0].rm_so + offset], (int) l_slen[0]);
			strncat(l_prev_r,
			    &line[repmatch[0].rm_so + offset], (int) l_slen[0]);
#endif
		} else if (*replacer == '\\') {
			/* Likely a backref to be included. */
			replacer++;
			if ((*replacer > '0') && (*replacer < ('9' + 1)) &&
			    (repmatch[*replacer - '0'].rm_so > -1)) {
#ifdef REG_STARTEND
				strncat(l_string,
				    &line[repmatch[*replacer - '0'].rm_so],
				    (int) l_slen[*replacer - '0']);
				strncat(l_prev_r,
				    &line[repmatch[*replacer - '0'].rm_so],
				    (int) l_slen[*replacer - '0']);
#else
				strncat(l_string,
				    &line[repmatch[*replacer - '0'].rm_so +
				    offset], (int) l_slen[*replacer - '0']);
				strncat(l_prev_r,
				    &line[repmatch[*replacer - '0'].rm_so +
				    offset], (int) l_slen[*replacer - '0']);
#endif
			}
			/* Put the replacement in. */
			else {
				strncat(l_string, replacer, 1);
				strncat(l_prev_r, replacer, 1);
			}
		}
		/* Put the replacement in. */
		else {
			strncat(l_string, replacer, 1);
			strncat(l_prev_r, replacer, 1);
		}
		replacer++;
	}

	l_new_rm_eo = strlen(l_string);

	/* Copy over what was after the chars to be replaced to the new line. */
#ifdef REG_STARTEND
	strcat(l_string, &line[repmatch[0].rm_eo]);
#else
	strcat(l_string, &line[repmatch[0].rm_eo + offset]);
#endif

	repmatch[0].rm_eo = l_new_rm_eo;	/* Update rm_eo. */
#ifndef REG_STARTEND
	offset += l_new_rm_eo;			/* Update offset. */
#endif
	return (l_string);			/* Return the new line. */
}
