/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)extern.h	8.1 (Berkeley) 5/31/93
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
