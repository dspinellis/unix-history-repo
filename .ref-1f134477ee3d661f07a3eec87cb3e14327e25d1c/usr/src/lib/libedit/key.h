/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Christos Zoulas of Cornell University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)key.h	8.1 (Berkeley) %G%
 */

/*
 * el.key.h: Key macro header
 */
#ifndef _h_el_key
#define _h_el_key

typedef union key_value_t {
    el_action_t  cmd;	/* If it is a command the #	*/
    char        *str;	/* If it is a string...		*/
} key_value_t;

typedef struct key_node_t key_node_t;

typedef struct el_key_t {
    char        *buf;	/* Key print buffer		*/
    key_node_t  *map;	/* Key map			*/
    key_value_t  val;	/* Local conversion buffer	*/
} el_key_t;

#define XK_CMD	0
#define XK_STR	1
#define XK_NOD	2
#define XK_EXE	3

protected int 		key_init	__P((EditLine *));
protected void 		key_end		__P((EditLine *));
protected key_value_t *	key_map_cmd	__P((EditLine *, int));
protected key_value_t *	key_map_str	__P((EditLine *, char *));
protected void		key_reset	__P((EditLine *));
protected int 		key_get		__P((EditLine *, char *, 
					     key_value_t *));
protected void		key_add		__P((EditLine *, char *, key_value_t *,
					     int));
protected void		key_clear	__P((EditLine *, el_action_t *,
					     char *));
protected int		key_delete	__P((EditLine *, char *));
protected void		key_print	__P((EditLine *, char *));
protected void	        key_kprint	__P((EditLine *, char *, 
					     key_value_t *, int));
protected char 	       *key__decode_str	__P((char *, char *, char *));

#endif /* _h_el_key */
