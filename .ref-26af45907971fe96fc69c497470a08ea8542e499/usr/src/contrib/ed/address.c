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
static char sccsid[] = "@(#)address.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <regex.h>
#include <setjmp.h>
#include <stdio.h>
#include <string.h>

#ifdef DBI
#include <db.h>
#endif

#include "ed.h"
#include "extern.h"

/*
 * Make sure that address one comes before address two in the buffer
 */

int
address_check(one, two)
	LINE *one, *two;
{
	LINE   *l_cl;

	for (l_cl = one; l_cl != NULL; l_cl = l_cl->below)
		if (l_cl == two)
			return (0);
	return (-1);
}

/*
 * convert a number given by the user into variable
 */
int
dig_num_conv(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	int l_line = 0;

	l_line = ss - '0';
	while ((ss = getc(inputt)) != EOF) {
		if ((ss < '0') || (ss > '9'))
			break;
		l_line = (l_line * 10) + (ss - '0');
	}
	*errnum = 0;
	ungetc(ss, inputt);
	return (l_line);
}

/*
 * Convert a numeric address into a LINE address (more useful for ed)
 */
LINE *
num_to_address(num, errnum)
	int num, *errnum;
{
	int l_line = 1;
	LINE *temp1;

	if (top) {
		for (temp1 = top; temp1->below != NULL; temp1 = temp1->below) {
			/* find the matching line number in the buffer */
			if (l_line >= num)
				break;
			l_line++;
		}
	}

	if ((l_line < num) || (!top)) {
		/* the number was wacko */
		*errnum = -1;
		strcpy(help_msg, "bad line number");
		return (NULL);
	} else
		if (num == 0)	/* special case */
			return (NULL);
		else
			return (temp1);
}


/*
 * Figure out what the addresses are spec'd by the user.  Note for backward
 * compatability the most recent addresses in a chain are used by the commands
 * (i.e. 3,5,17,22d deletes lines 17 to 22 inclusive. The two leading addresses
 * 3 and 5 are dropped as cmd_loop rolls through here the extra times).  Hence,
 * the code may look a little wierder than it should.  The variable l_order is
 * used to control for legally constructed addresses as described in ed(1).  So,
 * "$-21" and "/RE/++++" are legal but /RE/-$ is not.
 */
LINE *
address_conv(tempp, inputt, errnum)
	LINE *tempp;
	FILE *inputt;
	int *errnum;
{
	LINE *l_dot;
	int l_last, l_cnt, l_num, l_order, l_pass_flag;

	l_dot = NULL;
	l_order = 0;
	l_pass_flag = 0;
	address_flag = 0;

	l_last = ss;
	if (tempp == NULL)
		l_dot = current;
	else
		l_dot = tempp;

	while ((ss = getc(inputt)) != EOF) {
		switch (ss) {
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			if (l_order == (l_order | 4)) {
				*errnum = -1;
				strcpy(help_msg, "malformed address");
				return (NULL);
			}
			l_order |= 1;
			l_num = dig_num_conv(inputt, errnum);
			/*
			 * " " (<space>), historically, gets counted as a "+"
			 * if it's between two 'addable' address forms. Goofy,
			 * but it makes it compatible for those strange old
			 * scripts (or users?)
			 */
			if ((l_last == '+') ||
			    ((l_last == ' ') && l_pass_flag)) {
				if (l_last == ' ')
					l_num++;
				for (l_cnt = 0; l_cnt < l_num - 1; l_cnt++) {
					if (l_dot == NULL) {
						*errnum = -1;
						return (NULL);
					}
					l_dot = l_dot->below;
				}
			} else
				if ((l_last == '-') || (l_last == '^')) {
					for (l_cnt = l_num - 1;
					    l_cnt > 0; l_cnt--) {
						if (l_dot == NULL) {
							*errnum = -1;
							return (NULL);
						}
						l_dot = l_dot->above;
					}
				} else
					l_dot = num_to_address(l_num, errnum);
			if (*errnum < 0)
				return (NULL);
			l_pass_flag = 1;
			break;
		case '\'':
		case '$':
		case '?':
		case '/':
		case '.':
			if (l_order != 0) {
				*errnum = -1;
				strcpy(help_msg, "malformed address");
				return (NULL);
			}
			l_order = 4;
			switch (ss) {
			case '\'':
				l_dot = get_mark(inputt, errnum);
				break;
			case '$':
				l_dot = bottom;	/* set to bottom */
				break;
			case '?':
				l_dot = search_r(inputt, errnum);
				break;
			case '/':
				l_dot = search(inputt, errnum);
				break;
			case '.':
				l_dot = current;
				break;
			}
			break;
		case '-':
		case '^':
		case '+':
			l_order = 2;
			if (ss == '+') {
				l_dot = l_dot->below;
				if (l_dot == NULL) {
					strcpy(help_msg, "at end of buffer");
					*errnum = -1;
					return (NULL);
				}
			} else {
				l_dot = l_dot->above;
				if (l_dot == NULL) {
					strcpy(help_msg, "at top of buffer");
					*errnum = -1;
					return (NULL);
				}
			}
			break;
		case ' ':
			break;	/* ignore here, but see comment above */
		default:
			ungetc(ss, inputt);
			return (l_dot);
			break;
		}

		if (*errnum < 0)
			break;	/* from the while-loop */
		l_last = ss;
	}

	if (ss == EOF)
		return (l_dot);
	*errnum = -1;
	return (NULL);
}
