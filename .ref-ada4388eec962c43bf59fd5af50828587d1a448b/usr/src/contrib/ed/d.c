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
static char sccsid[] = "@(#)d.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#ifdef DBI
#include <db.h>
#endif
#include <regex.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ed.h"
#include "extern.h"

static void d_add __P((LINE *, LINE *));

/*
 * This removes lines in the buffer from user access. The specified
 * lines are not really deleted yet(!) as they might be called back
 * by an undo. So the pointers from Start, End, and neighbours are placed
 * in a stack for deletion later when no undo can be performed on these lines.
 * The lines in the buffer are freed then as well.
 */
void
d(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	LINE *l_temp1, *l_temp2;

	if (Start_default && End_default)
		Start = End = current;
	else
		if (Start_default)
			Start = End;
	if (Start == NULL) {
		strcpy(help_msg, "buffer empty");
		*errnum = -1;
		return;
	}
	Start_default = End_default = 0;

	if (join_flag == 0) {
		if (rol(inputt, errnum))
			return;
	}
	else
		ss = getc(inputt); /* fed back from join */

	if ((u_set == 0) && (g_flag == 0))
		u_clr_stk();	/* for undo */

	if ((Start == NULL) && (End == NULL)) {	/* nothing to do... */
		*errnum = 1;
		return;
	}

	d_add(Start, End);	/* for buffer clearing later(!) */

	/*
	 * Now change & preserve the pointers in case of undo and then adjust
	 * them.
	 */
	sigspecial++;
	if (Start == top) {
		top = End->below;
		if (top != NULL) {
			u_add_stk(&(top->above));
			(top->above) = NULL;
		}
	} else {
		l_temp1 = Start->above;
		u_add_stk(&(l_temp1->below));
		(l_temp1->below) = End->below;
	}

	if (End == bottom) {
		bottom = Start->above;
		current = bottom;
	} else {
		l_temp2 = End->below;
		u_add_stk(&(l_temp2->above));
		(l_temp2->above) = Start->above;
		current = l_temp2;
	}

	/* To keep track of the marks. */
	ku_chk(Start, End, NULL);
	change_flag = 1L;

	*errnum = 1;
	sigspecial--;
	if (sigint_flag && (!sigspecial))	/* next stable spot */
		SIGINT_ACTION;
}


/*
 * This keeps a stack of the start and end lines deleted for clean-up
 * later (in d_do()). A stack is used because of the global commands.
 */
static void
d_add(top_d, bottom_d)
	LINE *top_d, *bottom_d;
{
	struct d_layer *l_temp;

	l_temp = malloc(sizeof(struct d_layer));
	(l_temp->begin) = top_d;
	(l_temp->end) = bottom_d;
	(l_temp->next) = d_stk;
	d_stk = l_temp;

}

/*
 * This cleans up the LINE structures deleted and no longer accessible
 * to undo. It performs garbage clean-up on the now non-referenced
 * text in the buffer.
 */
void
d_do()
{
	struct d_layer *l_temp;
#ifdef DBI
	DBT l_db_key;
#endif
	LINE *l_temp2, *l_temp3;
	int l_flag;

	l_temp = d_stk;
	sigspecial++;
	do {
		l_flag = 0;
		l_temp2 = l_temp->begin;
		do {
			l_temp3 = l_temp2;
#ifdef STDIO
			/* no garbage collection done currently */
#endif
#ifdef DBI
			/* garbage collection should be done iff the
			 * open was done as btree, not recno.
			 */
			l_db_key.size = sizeof(recno_t);
			l_db_key.data = &(l_temp2->handle);
			(dbhtmp->del) (dbhtmp, &l_db_key, (u_int) 0);
#endif
#ifdef MEMORY
			free(l_temp2->handle);
#endif
			if ((l_temp->end) == l_temp2)
				l_flag = 1;
			l_temp2 = l_temp3->below;
			free(l_temp3);
		} while (l_flag == 0);

		d_stk = d_stk->next;
		free(l_temp);
		l_temp = d_stk;
	} while (d_stk);

	d_stk = NULL;		/* just to be sure */
	sigspecial--;
	if (sigint_flag && (!sigspecial))
		SIGINT_ACTION;
}
