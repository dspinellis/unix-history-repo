/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	8.1 (Berkeley) %G%
 */

struct kinfo;
struct nlist;
struct var;
struct varent;

extern VAR var[];
extern VARENT *vhead;

__BEGIN_DECLS
void	 command __P((KINFO *, VARENT *));
void	 cputime __P((KINFO *, VARENT *));
int	 donlist __P((void));
void	 evar __P((KINFO *, VARENT *));
char	*fmt_argv __P((char **, char *, int));
double	 getpcpu __P((KINFO *));
double	 getpmem __P((KINFO *));
void	 logname __P((KINFO *, VARENT *));
void	 longtname __P((KINFO *, VARENT *));
void	 lstarted __P((KINFO *, VARENT *));
void	 maxrss __P((KINFO *, VARENT *));
void	 nlisterr __P((struct nlist *));
void	 p_rssize __P((KINFO *, VARENT *));
void	 pagein __P((KINFO *, VARENT *));
void	 parsefmt __P((char *));
void	 pcpu __P((KINFO *, VARENT *));
void	 pmem __P((KINFO *, VARENT *));
void	 pri __P((KINFO *, VARENT *));
void	 printheader __P((void));
void	 pvar __P((KINFO *, VARENT *));
void	 rssize __P((KINFO *, VARENT *));
void	 runame __P((KINFO *, VARENT *));
void	 rvar __P((KINFO *, VARENT *));
void	 showkey __P((void));
void	 started __P((KINFO *, VARENT *));
void	 state __P((KINFO *, VARENT *));
void	 tdev __P((KINFO *, VARENT *));
void	 tname __P((KINFO *, VARENT *));
#ifndef NEWVM
void	 trss __P((KINFO *, VARENT *));
#endif
void	 tsize __P((KINFO *, VARENT *));
void	 ucomm __P((KINFO *, VARENT *));
void	 uname __P((KINFO *, VARENT *));
void	 uvar __P((KINFO *, VARENT *));
void	 vsize __P((KINFO *, VARENT *));
void	 wchan __P((KINFO *, VARENT *));
__END_DECLS
