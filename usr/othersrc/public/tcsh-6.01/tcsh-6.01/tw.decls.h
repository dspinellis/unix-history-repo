/* $Header: /home/hyperion/mu/christos/src/sys/tcsh-6.01/RCS/tw.decls.h,v 3.0 1991/07/04 21:49:28 christos Exp $ */
/*
 * tw.decls.h: Tenex external declarations
 */
/*-
 * Copyright (c) 1980, 1991 The Regents of the University of California.
 * All rights reserved.
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
 */
#ifndef _h_tw_decls
#define _h_tw_decls

/*
 * tw.help.c
 */
extern	void		  do_help		__P((Char *));

/*
 * tw.parse.c
 */
extern	 int		  tenematch		__P((Char *, int, int, 
						     COMMAND));
extern	 int		  t_search		__P((Char *, Char *, COMMAND, 
						     int, int, int));
extern	 int		  starting_a_command	__P((Char *, Char *));
extern	 void		  copyn			__P((Char *, Char *, int));
extern	 void		  catn			__P((Char *, Char *, int));
extern	 int		  fcompare		__P((Char **, Char **));
extern	 void		  print_by_column	__P((Char *, Char *[], int, 
						     int));
extern	 int		  StrQcmp		__P((Char *, Char *));

/*
 * tw.init.c
 */
extern	 void	  	  tw_clear_comm_list	__P((void));
extern	 void	  	  tw_sort_comms		__P((void));
extern	 void	  	  tw_add_comm_name	__P((Char *));
extern	 void	  	  tw_add_builtins	__P((void));
extern	 void	  	  tw_add_aliases	__P((void));
extern	 struct	varent	 *tw_start_shell_list	__P((void));
extern	 Char	 	 *tw_next_shell_var	__P((struct varent **));
extern	 Char		**tw_start_env_list	__P((void));
extern	 Char	 	 *Getenv		__P((Char *));
extern	 Char	 	 *tw_next_env_var	__P((Char ***));

/*
 * tw.spell.c
 */
extern	 int		  spell_me		__P((Char *, int, int));
extern	 int		  spdir			__P((Char *, Char *, Char *, 
						     Char *));
extern	 int		  spdist		__P((Char *, Char *));

#endif				/* _h_tw_decls */
