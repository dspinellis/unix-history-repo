/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Christos Zoulas of Cornell University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)map.h	8.1 (Berkeley) %G%
 */

/*
 * el.map.h:	Editor maps
 */
#ifndef _h_el_map
#define _h_el_map

typedef struct el_bindings_t {	/* for the "bind" shell command */
    const char   *name;		/* function name for bind command */
    int     func;		/* function numeric value */
    const char   *description;	/* description of function */
} el_bindings_t;


typedef struct el_map_t {
    el_action_t   *alt;		/* The current alternate key map	*/
    el_action_t   *key;		/* The current normal key map		*/
    el_action_t   *current;	/* The keymap we are using		*/
    el_action_t   *emacs;	/* The default emacs key map		*/
    el_action_t   *vic;		/* The vi command mode key map		*/
    el_action_t   *vii;		/* The vi insert mode key map		*/
    int		   type;	/* Emacs or vi				*/
    el_bindings_t *help;	/* The help for the editor functions	*/
    el_func_t     *func;	/* List of available functions		*/
    int  	   nfunc;	/* The number of functions/help items	*/
} el_map_t;

#define MAP_EMACS	0
#define MAP_VI		1

protected int	map_bind		__P((EditLine *, int, char **));
protected int	map_init		__P((EditLine *));
protected void	map_end			__P((EditLine *));
protected void	map_init_vi		__P((EditLine *));
protected void	map_init_emacs		__P((EditLine *));
protected int	map_set_editor		__P((EditLine *, char *));
protected int	map_addfunc		__P((EditLine *, const char *, 
					     const char *, el_func_t));

#endif /* _h_el_map */
