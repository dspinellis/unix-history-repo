/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	8.1 (Berkeley) %G%
 */

#include <sys/cdefs.h>

void	 a __P((FILE *, int *));

#ifdef STDIO
long	add_line __P((char *, long));
#endif
#ifdef DBI
recno_t	 add_line __P((char *, long));
#endif
#ifdef MEMORY
char	*add_line __P((char *, long));
#endif

int	 address_check __P((LINE *, LINE *));
LINE	*address_conv __P((LINE *, FILE *, int *));
void	 bang __P((FILE *, int *));
void	 c __P((FILE *, int *));
void	 cmd_loop __P((FILE *, int *));
void	 d __P((FILE *, int *));
void	 d_do __P((void));
int	 dig_num_conv __P((FILE *, int *));
void	 e __P((FILE *, int *));
void	 e2 __P((FILE *, int *));
void	 ed_exit __P((int));
int	 edwrite __P((FILE *, LINE *, LINE *));
void	 equal __P((FILE *, int *));
void	 f __P((FILE *, int *));
char	*filename __P((FILE *, int *));
void	 g __P((FILE *, int *));
#ifdef STDIO
void     get_line __P((long, int));
#endif
#ifdef DBI
void	 get_line __P((recno_t, int));
#endif
#ifdef MEMORY
void     get_line __P((char *, int));
#endif;
LINE	*get_mark __P((FILE *, int *));
char	*get_pattern __P((int, FILE *, int *, int));
void	 i __P((FILE *, int *));
long	 input_lines __P((FILE *, int *));
void	 j __P((FILE *, int *));
void	 ku_chk __P((LINE *, LINE *, LINE *));
void	 l __P((FILE *, int *));
int	 line_number __P((LINE *));
void	 m __P((FILE *, int *));
LINE	*num_to_address __P((int, int *));
void	 p __P((FILE *, int *, int));
void	 q __P((FILE *, int *));
void	 r __P((FILE *, int *));
int	 rol __P((FILE *, int *));
void	 s __P((FILE *, int *));
LINE	*search __P((FILE *, int *));
LINE	*search_r __P((FILE *, int *));
void	 set_mark __P((FILE *, int *));
void	 t __P((FILE *, int *));
void	 u __P((FILE *, int *));
void	 u_add_stk __P((LINE **));
void	 u_clr_stk __P((void));
void	 u_pop_n_swap __P((LINE **));
void	 undo __P((void));
__dead void do_hup __P((void));
void	 w __P((FILE *, int *));
void	 z __P((FILE *, int *));

#ifdef REG_STARTEND
char	*re_replace __P((char *, size_t, regmatch_t [], char *));
int	 regexec_n __P((regex_t *,
	    char *, size_t, regmatch_t [], int, int, size_t, int));
#else
char	*re_replace __P((char *, size_t, regmatch_t [], char *, size_t));
int	 regexec_n __P((regex_t *,
	    char *, size_t, regmatch_t [], int, int, size_t *, int));
#endif
