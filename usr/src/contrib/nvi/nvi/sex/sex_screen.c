/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
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
static char sccsid[] = "@(#)sex_screen.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <stdlib.h>
#include <unistd.h>

#include "vi.h"
#include "excmd.h"
#include "recover.h"
#include "sex_screen.h"

static void	sex_abort __P((void));
static int	sex_init __P((SCR *));
static int	sex_noop __P((void));

/*
 * sex --
 *	Main ex screen loop.  The ex screen is relatively uncomplicated.
 *	As long as it has a stdio FILE pointer for output, it's happy.
 */
int
sex(sp, ep, spp)
	SCR *sp, **spp;
	EXF *ep;
{
	struct termios raw;
	GS *saved_gp;

	/* Set the list of routines. */
	if (sex_init(sp))
		return (1);

	/* Initialize the terminal state. */
	if (tcgetattr(STDIN_FILENO, &raw))
		return (1);
	cfmakeraw(&raw);
	raw.c_oflag |= OPOST|ONLCR;
	if (tcsetattr(STDIN_FILENO, TCSADRAIN, &raw))
		return (1);

	/* Write to the terminal. */
	sp->stdfp = stdout;

	do {
		sp->rows = O_VAL(sp, O_LINES);
		sp->cols = O_VAL(sp, O_COLUMNS);

		F_CLR(sp, S_RESIZE);		/* XXX block SIGWINCH */
		if (ex(sp, sp->ep)) {
			F_SET(sp, S_EXIT_FORCE);
			if (F_ISSET(ep, F_RCV_ON)) {
				F_SET(ep, F_RCV_NORM);
				(void)rcv_sync(sp, sp->ep);
			}
		}

		saved_gp = sp->gp;

		switch (F_ISSET(sp, S_MAJOR_CHANGE)) {
		case S_EXIT:
			if (file_stop(sp, sp->ep, 0))
				F_CLR(sp, S_EXIT);
			else
				sp = NULL;
			break;
		case S_EXIT_FORCE:
			if (file_stop(sp, sp->ep, 1))
				F_CLR(sp, S_EXIT_FORCE);
			else
				sp = NULL;
			break;
		case S_FSWITCH_FORCE:
			F_CLR(sp, S_FSWITCH_FORCE);
			if (file_stop(sp, sp->ep, 1))
				break;
			if (sp->enext->refcnt == 0 &&
			    file_start(sp, sp->enext, NULL) == NULL)
				sp = NULL;
			else {
				sp->eprev = sp->ep;
				sp->ep = sp->enext;
			}
			break;
		case S_FSWITCH:
			F_CLR(sp, S_FSWITCH);
			if (file_stop(sp, sp->ep, 0))
				break;
			if (sp->enext->refcnt == 0 &&
			    file_start(sp, sp->enext, NULL) == NULL)
				sp = NULL;
			else {
				sp->eprev = sp->ep;
				sp->ep = sp->enext;
			}
			break;
		case 0:
			break;
		default:
			abort();
		}
	} while (sp != NULL &&
	    F_ISSET(sp, S_MODE_EX) && !F_ISSET(sp, S_EXIT | S_EXIT_FORCE));

	*spp = sp;

	/* Reset the terminal state. */
	return (tcsetattr(STDIN_FILENO,
	    TCSADRAIN, &saved_gp->original_termios) ? 1 : 0);
}

/*
 * sex_init --
 *	Initialize the ex screen.
 */
static int
sex_init(sp)
	SCR *sp;
{
	/* Initialize support routines. */
	sp->s_bell		= sex_bell;
	sp->s_busy_cursor	= (int (*)())sex_noop;
	sp->s_change		= (int (*)())sex_noop; 
	sp->s_chposition	= (size_t (*)())sex_abort; 
	sp->s_confirm		= sex_confirm;
	sp->s_down		= (int (*)())sex_abort;
	sp->s_ex_cmd		= (int (*)())sex_abort;
	sp->s_ex_run		= (int (*)())sex_abort;
	sp->s_ex_write		= (int (*)())sex_abort;
	sp->s_fill		= (int (*)())sex_abort;
	sp->s_get		= sex_get;
	sp->s_position		= (int (*)())sex_abort;
	sp->s_refresh		= sex_refresh;
	sp->s_relative		= (size_t (*)())sex_abort;
	sp->s_split		= sex_split;
	sp->s_suspend		= sex_suspend;
	sp->s_up		= (int (*)())sex_abort;
	return (0);
}

/*
 * sex_abort --
 *	Fake function.  Die.
 */
static void
sex_abort()
{
	abort();
}

/*
 * sex_noop --
 *	Fake function.  Do nothing.
 */
static int
sex_noop()
{
	return (0);
}
