/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	8.1 (Berkeley) %G%
 */

BUFHEAD	*__add_ovflpage __P((HTAB *, BUFHEAD *));
int	 __addel __P((HTAB *, BUFHEAD *, const DBT *, const DBT *));
int	 __big_delete __P((HTAB *, BUFHEAD *));
int	 __big_insert __P((HTAB *, BUFHEAD *, const DBT *, const DBT *));
int	 __big_keydata __P((HTAB *, BUFHEAD *, DBT *, DBT *, int));
int	 __big_return __P((HTAB *, BUFHEAD *, int, DBT *, int));
int	 __big_split __P((HTAB *, BUFHEAD *, BUFHEAD *, BUFHEAD *,
		int, u_int, SPLIT_RETURN *));
int	 __buf_free __P((HTAB *, int, int));
void	 __buf_init __P((HTAB *, int));
u_int	 __call_hash __P((HTAB *, char *, int));
int	 __delpair __P((HTAB *, BUFHEAD *, int));
int	 __expand_table __P((HTAB *));
int	 __find_bigpair __P((HTAB *, BUFHEAD *, int, char *, int));
u_short	 __find_last_page __P((HTAB *, BUFHEAD **));
void	 __free_ovflpage __P((HTAB *, BUFHEAD *));
BUFHEAD	*__get_buf __P((HTAB *, u_int, BUFHEAD *, int));
int	 __get_page __P((HTAB *, char *, u_int, int, int, int));
int	 __init_bitmap __P((HTAB *, int, int, int));
u_int	 __log2 __P((u_int));
int	 __put_page __P((HTAB *, char *, u_int, int, int));
void	 __reclaim_buf __P((HTAB *, BUFHEAD *));
int	 __split_page __P((HTAB *, u_int, u_int));

/* Default hash routine. */
extern int	(*__default_hash) __P((u_char *, int));

#ifdef HASH_STATISTICS
extern long hash_accesses, hash_collisions, hash_expansions, hash_overflows;
#endif
