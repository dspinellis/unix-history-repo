/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Paul Borman at Krystal Technologies.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ldef.h	5.1 (Berkeley) %G%
 */

/*
 * This should look a LOT like a _RuneEntry
 */
typedef struct rune_list {
    rune_t		min;
    rune_t 		max;
    rune_t 		map;
    u_long		*types;
    struct rune_list	*next;
} rune_list;

typedef struct rune_map {
    u_long		map[_CACHED_RUNES];
    rune_list		*root;
} rune_map;
