/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.1 (Berkeley) %G%
 */

BUFHEAD	*__add_ovflpage __P((BUFHEAD *));
int	 __addel __P((BUFHEAD *, const DBT *, const DBT *));
int	 __big_delete __P((BUFHEAD *, int));
int	 __big_insert __P((BUFHEAD *, const DBT *, const DBT *));
int	 __big_keydata __P((BUFHEAD *, int, DBT *, DBT *, int));
int	 __big_return __P((BUFHEAD *, int, DBT *, int));
int	 __big_split __P((BUFHEAD *, BUFHEAD *, BUFHEAD *,
		int, u_int, SPLIT_RETURN *));
int	 __buf_free __P((int, int));
void	 __buf_init __P((int));
u_int	 __call_hash __P((char *, int));
int	 __delpair __P((BUFHEAD *, int));
int	 __expand_table __P((void));
int	 __find_bigpair __P((BUFHEAD *, int, char *, int));
u_short	 __find_last_page __P((BUFHEAD **));
void	 __free_ovflpage __P((BUFHEAD *));
BUFHEAD	*__get_buf __P((u_int, BUFHEAD *, int));
int	 __get_page __P((char *, u_int, int, int, int));
u_long	*__init_bitmap __P((int, int, int));
int	 __log2 __P((int));
int	 __put_page __P((char *, u_int, int, int));
void	 __reclaim_buf __P((BUFHEAD *));
int	 __split_page __P((u_int, u_int));

extern HTAB *hashp;
#ifdef HASH_STATISTICS
extern long hash_accesses, hash_collisions, hash_expansions, hash_overflows;
#endif
extern int (*default_hash) __P((u_char *, int));
