/*-
 * Copyright (c) 1992 Diomidis Spinellis.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Diomidis Spinellis of Imperial College, University of London.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.4 (Berkeley) %G%
 */

extern struct s_command *prog;
extern struct s_appends *appends;
extern regmatch_t *match;
extern size_t maxnsub;
extern u_long linenum;
extern int appendnum;
extern int lastline;
extern int aflag, eflag, nflag;
extern char *fname;

void	 compile __P((void));
char	*cu_fgets __P((char *, int));
void	 err __P((int, const char *, ...));
int	 mf_fgets __P((SPACE *, enum e_spflag));
void	 process __P((void));
char	*strregerror __P((int, regex_t *));
void	*xmalloc __P((u_int));
void	*xrealloc __P((void *, u_int));
void	 cfclose __P((struct s_command *));
void	 cspace __P((SPACE *, char *, size_t, enum e_spflag));
