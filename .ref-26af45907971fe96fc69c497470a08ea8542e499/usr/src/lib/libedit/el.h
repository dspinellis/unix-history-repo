/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Christos Zoulas of Cornell University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)el.h	8.1 (Berkeley) %G%
 */

/*
 * el.h: Internal structures.
 */
#ifndef _h_el
#define _h_el
/*
 * Local defaults
 */
#define KSHVI
#define VIDEFAULT
#define ANCHOR

#include <stdio.h>
#include <sys/types.h>

#define EL_BUFSIZ	1024		/* Maximum line size		*/

#define HANDLE_SIGNALS	1

typedef int bool_t;			/* True or not			*/

typedef unsigned char el_action_t;	/* Index to command array	*/

typedef struct coord_t {		/* Position on the screen	*/
    int h, v;
} coord_t;

typedef struct el_line_t {
    char *buffer, 			/* Input line 			*/
	 *cursor, 			/* Cursor position 		*/
	 *lastchar,			/* Last character 		*/
	 *limit;			/* Max position			*/
} el_line_t;

/*
 * Editor state
 */
typedef struct el_state_t {
    int 	inputmode;		/* What mode are we in? 	*/
    int 	doingarg;		/* Are we getting an argument?	*/
    int	        argument;		/* Numeric argument 		*/
    int		metanext;		/* Is the next char a meta char */
    el_action_t lastcmd;		/* Previous command		*/
} el_state_t;

/*
 * Until we come up with something better...
 */
#define el_malloc(a)	malloc(a)
#define el_realloc(a,b)	realloc(a, b)
#define el_free(a)	free(a)

#include "tty.h"
#include "prompt.h"
#include "key.h"
#include "term.h"
#include "refresh.h"
#include "chared.h"
#include "common.h"
#include "search.h"
#include "hist.h"
#include "map.h"
#include "parse.h"
#include "sig.h"
#include "help.h"

struct editline {
    char	 *el_prog;	/* the program name 			*/
    FILE         *el_outfile;	/* Stdio stuff				*/
    FILE         *el_errfile;	/* Stdio stuff				*/
    int           el_infd;	/* Input file descriptor		*/
    int		  el_flags;	/* Various flags.			*/
    coord_t       el_cursor;	/* Cursor location			*/
    char        **el_display, 	/* Real screen image = what is there	*/
	        **el_vdisplay;	/* Virtual screen image = what we see	*/

    el_line_t     el_line;	/* The current line information		*/
    el_state_t	  el_state;	/* Current editor state			*/
    el_term_t     el_term;	/* Terminal dependent stuff		*/
    el_tty_t	  el_tty;	/* Tty dependent stuff			*/
    el_refresh_t  el_refresh;	/* Refresh stuff			*/
    el_prompt_t   el_prompt;	/* Prompt stuff				*/
    el_chared_t	  el_chared;	/* Characted editor stuff		*/
    el_map_t	  el_map;	/* Key mapping stuff			*/
    el_key_t	  el_key;	/* Key binding stuff			*/
    el_history_t  el_history;	/* History stuff			*/
    el_search_t	  el_search;	/* Search stuff				*/
    el_signal_t	  el_signal;	/* Signal handling stuff		*/
};

#endif /* _h_el */
