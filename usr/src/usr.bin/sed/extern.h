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
 *	@(#)extern.h	5.1 (Berkeley) %G%
 */

extern struct s_command *prog;
extern struct s_appends *appends;
extern u_long linenum;
extern int appendnum;
extern int compile_errors;
extern int lastline;
extern int aflag, eflag, nflag;
extern char *fname;

void	 compile __P((void));
char	*cu_fgets __P((char *, int));
void	 err __P((int, const char *, ...));
char	*mf_fgets __P((size_t *));
void	 process __P((void));
char	*strregerror __P((int, regex_t *));
void	*xmalloc __P((u_int));
void	*xrealloc __P((void *, u_int));
void	 cfclose(struct s_command *cp);
