/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Christos Zoulas of Cornell University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)term.h	8.1 (Berkeley) %G%
 */

/*
 * el.term.h: Termcap header
 */
#ifndef _h_el_term
#define _h_el_term

#include "histedit.h"

typedef struct {	/* Symbolic function key bindings	*/
    char       *name;	/* name of the key			*/
    int     	key;	/* Index in termcap table		*/
    key_value_t fun;	/* Function bound to it			*/
    int	        type;	/* Type of function			*/
} fkey_t;

typedef struct {
    coord_t t_size;			/* # lines and cols	*/
    bool_t  t_flags;
#define TERM_CAN_INSERT		0x01	/* Has insert cap	*/
#define TERM_CAN_DELETE		0x02	/* Has delete cap	*/
#define TERM_CAN_CEOL		0x04	/* Has CEOL cap		*/
#define TERM_CAN_TAB		0x08	/* Can use tabs		*/
#define TERM_CAN_ME		0x10	/* Can turn all attrs.	*/
#define TERM_CAN_UP		0x20	/* Can move up		*/
#define TERM_HAS_META		0x40	/* Has a meta key	*/
    char   *t_buf;			/* Termcap buffer	*/
    int	    t_loc;			/* location used	*/
    char  **t_str;			/* termcap strings	*/
    int	   *t_val;			/* termcap values	*/
    char   *t_cap;			/* Termcap buffer	*/
    fkey_t *t_fkey;			/* Array of keys	*/
} el_term_t;

/*
 * fKey indexes
 */
#define A_K_DN		0
#define A_K_UP		1
#define A_K_LT		2
#define A_K_RT		3
#define A_K_NKEYS	4

protected void term_move_to_line	__P((EditLine *, int));
protected void term_move_to_char	__P((EditLine *, int));
protected void term_clear_EOL		__P((EditLine *, int));
protected void term_overwrite		__P((EditLine *, char *, int));
protected void term_insertwrite		__P((EditLine *, char *, int));
protected void term_deletechars		__P((EditLine *, int));
protected void term_clear_screen	__P((EditLine *));
protected void term_beep		__P((EditLine *));
protected void term_change_size		__P((EditLine *, int, int));
protected int  term_get_size		__P((EditLine *, int *, int *));
protected int  term_init		__P((EditLine *));
protected void term_bind_arrow		__P((EditLine *));
protected void term_print_arrow		__P((EditLine *, char *));
protected int  term_clear_arrow		__P((EditLine *, char *));
protected int  term_set_arrow		__P((EditLine *, char *, 
					     key_value_t *, int));
protected void term_end			__P((EditLine *));
protected int  term_set			__P((EditLine *, char *));
protected int  term_settc		__P((EditLine *, int, char **));
protected int  term_telltc		__P((EditLine *, int, char **));
protected int  term_echotc		__P((EditLine *, int, char **));

protected void term__putc		__P((int));
protected void term__flush		__P((void));

/*
 * Easy access macros
 */
#define EL_FLAGS	(el)->el_term.t_flags

#define EL_CAN_INSERT	(EL_FLAGS & TERM_CAN_INSERT)
#define EL_CAN_DELETE	(EL_FLAGS & TERM_CAN_DELETE)
#define EL_CAN_CEOL	(EL_FLAGS & TERM_CAN_CEOL)
#define EL_CAN_TAB	(EL_FLAGS & TERM_CAN_TAB)
#define EL_CAN_ME	(EL_FLAGS & TERM_CAN_ME)
#define EL_HAS_META	(EL_FLAGS & TERM_HAS_META)

#endif /* _h_el_term */
