/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Christos Zoulas of Cornell University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)sig.h	8.1 (Berkeley) %G%
 */

/*
 * el.sig.h: Signal handling functions
 */
#ifndef _h_el_sig
#define _h_el_sig

#include <signal.h>

#include "histedit.h"

/*
 * Define here all the signals we are going to handle
 * The _DO macro is used to iterate in the source code
 */
#define ALLSIGS 	\
    _DO(SIGINT)		\
    _DO(SIGTSTP)	\
    _DO(SIGSTOP)	\
    _DO(SIGQUIT)	\
    _DO(SIGHUP)		\
    _DO(SIGTERM)	\
    _DO(SIGCONT)	\
    _DO(SIGWINCH)

typedef sig_t *el_signal_t;

protected void	sig_end		__P((EditLine*));
protected int	sig_init	__P((EditLine*));
protected void	sig_set		__P((EditLine*));
protected void	sig_clr		__P((EditLine*));

#endif /* _h_el_sig */
