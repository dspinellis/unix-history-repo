/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rodney Ruddock of the University of Guelph.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)search.c	8.1 (Berkeley) %G%";
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
 * searches forward through the buffer (wrapping if necessary) for a
 * line that contains a match to the RE.
 */

LINE *
search(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	LINE *l_temp;
	int l_err;
	char *l_patt;

	if (current)
		l_temp = current->below;
	else {
		*errnum = -1;
		ungetc(ss, inputt);
		strcpy(help_msg, "buffer empty");
		return(NULL);
	}
	/* Get the RE. */
	l_patt = get_pattern(ss, inputt, errnum, 0);
	if (*errnum < -1)
		return (NULL);
	*errnum = 0;
	if ((RE_flag == 0) && (l_patt[1] == '\0')) {
		*errnum = -1;
		ungetc(ss, inputt);
		return (NULL);
	} else
		if (l_patt[1] || (RE_patt == NULL)) {
			sigspecial++;
			free(RE_patt);
			RE_patt = l_patt;
			sigspecial--;
			if (sigint_flag && (!sigspecial))
				SIGINT_ACTION;
		}
	RE_sol = (RE_patt[1] == '^') ? 1 : 0;

	/* Compile it up. */
	if ((l_patt[1]) &&
	    (regfree(&RE_comp), l_err = regcomp(&RE_comp, &RE_patt[1], 0))) {
		regerror(l_err, &RE_comp, help_msg, 128);
		*errnum = -1;
		RE_flag = 0;
		ungetc(ss, inputt);
		return (NULL);
	}
	RE_flag = 1;

	/* Find a line that has the RE in it. */
	for (;;) {		/* (l_temp != current) */
		if (l_temp == NULL) {
			if (top != NULL)
				l_temp = top;
			else
				break;
		}
		get_line(l_temp->handle, l_temp->len);
		if (sigint_flag && (!sigspecial))
			SIGINT_ACTION;
		if (regexec(&RE_comp, text, (size_t) RE_SEC, RE_match, 0)) {
			l_temp = l_temp->below;
			if (l_temp == (current->below))
				break;
		} else {
			*errnum = 0;
			return (l_temp);
		}
	}
	strcpy(help_msg, "RE not found");
	*errnum = -1;
	return (NULL);
}

/*
 * Searches backward through the buffer (wrapping if necessary) to find
 * a line that contains a match to the RE.
 */
LINE *
search_r(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	LINE *l_temp;
	int l_err;
	char *l_patt;

	if (current)
		l_temp = current->above;
	else {
		*errnum = -1;
		ungetc(ss, inputt);
		strcpy(help_msg, "buffer empty");
		return(NULL);
	}

	/* Get the RE. */
	l_patt = get_pattern(ss, inputt, errnum, 0);
	if (*errnum < -1)
		return (NULL);
	*errnum = 0;
	if ((RE_flag == 0) && (l_patt[1] == '\0')) {
		*errnum = -1;
		ungetc(ss, inputt);
		return (NULL);
	} else
		if (l_patt[1] || (RE_patt == NULL)) {
			sigspecial++;
			free(RE_patt);
			RE_patt = l_patt;
			sigspecial--;
			if (sigint_flag && (!sigspecial))
				SIGINT_ACTION;
		}
	RE_sol = (RE_patt[1] == '^') ? 1 : 0;

	/* Compile up the RE. */
	if ((l_patt[1]) &&
	    (regfree(&RE_comp), l_err = regcomp(&RE_comp, &RE_patt[1], 0))) {
		regerror(l_err, &RE_comp, help_msg, 128);
		*errnum = -1;
		RE_flag = 0;
		ungetc(ss, inputt);
		return (NULL);
	}
	RE_flag = 1;

	/* Search for a line that has the RE in it. */
	for (;;) {		/* (l_temp != (current->above)) */
		if (l_temp == NULL) {
			if (bottom != NULL)
				l_temp = bottom;
			else
				break;
		}
		get_line(l_temp->handle, l_temp->len);
		if (sigint_flag && (!sigspecial))
			SIGINT_ACTION;
		if (regexec(&RE_comp, text, (size_t) RE_SEC, RE_match, 0)) {
			l_temp = l_temp->above;
			if (l_temp == (current->above))
				break;
		} else {
			*errnum = 0;
			return (l_temp);
		}
	}
	strcpy(help_msg, "RE not found");
	*errnum = -1;
	return (NULL);
}
