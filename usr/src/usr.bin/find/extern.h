/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.4 (Berkeley) %G%
 */

#include <sys/cdefs.h>

void	 brace_subst __P((char *, char **, char *, int));
void	*emalloc __P((unsigned int));
PLAN	*find_create __P((char ***));
void	 find_execute __P((PLAN *, char **));
PLAN	*find_formplan __P((char **));
PLAN	*not_squish __P((PLAN *));
OPTION	*option __P((char *));
PLAN	*or_squish __P((PLAN *));
PLAN	*paren_squish __P((PLAN *));
struct stat;
void	 printlong __P((char *, char *, struct stat *));
int	 queryuser __P((char **));

PLAN	*c_atime __P((char *));
PLAN	*c_ctime __P((char *));
PLAN	*c_depth __P((void));
PLAN	*c_exec __P((char ***, int));
PLAN	*c_follow __P((void));
PLAN	*c_fstype __P((char *));
PLAN	*c_group __P((char *));
PLAN	*c_inum __P((char *));
PLAN	*c_links __P((char *));
PLAN	*c_ls __P((void));
PLAN	*c_name __P((char *));
PLAN	*c_newer __P((char *));
PLAN	*c_nogroup __P((void));
PLAN	*c_nouser __P((void));
PLAN	*c_path __P((char *));
PLAN	*c_perm __P((char *));
PLAN	*c_print __P((void));
PLAN	*c_prune __P((void));
PLAN	*c_size __P((char *));
PLAN	*c_type __P((char *));
PLAN	*c_user __P((char *));
PLAN	*c_xdev __P((void));
PLAN	*c_openparen __P((void));
PLAN	*c_closeparen __P((void));
PLAN	*c_mtime __P((char *));
PLAN	*c_not __P((void));
PLAN	*c_or __P((void));

extern int ftsoptions, isdeprecated, isdepth, isoutput, isxargs;
