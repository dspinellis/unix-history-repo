/*
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 *
 * %sccs.include.redist.c%
 *
 *      @(#)extern.h	1.3 (Berkeley) %G%
 */

typedef int boolean;

extern boolean	 _escaped;             /* if last character was an escape */
extern char    *_start;                /* start of the current string */
extern char    *l_acmbeg;              /* string introducing a comment */
extern char    *l_acmend;              /* string ending a comment */
extern char    *l_blkbeg;              /* string begining of a block */
extern char    *l_blkend;              /* string ending a block */
extern char    *l_chrbeg;              /* delimiter for character constant */
extern char    *l_chrend;              /* delimiter for character constant */
extern char    *l_combeg;              /* string introducing a comment */
extern char    *l_comend;              /* string ending a comment */
extern char     l_escape;              /* character used to escape characters */
extern char    *l_keywds[];    	       /* keyword table address */
extern boolean  l_onecase;             /* upper and lower case are equivalent */
extern char    *l_prcbeg;              /* regular expr for procedure begin */
extern char    *l_strbeg;              /* delimiter for string constant */
extern char    *l_strend;              /* delimiter for string constant */
extern boolean  l_toplex;              /* procedures only defined at top lex level */
extern char    *language;              /* the language indicator */

#include <sys/cdefs.h>

__BEGIN_DECLS
extern int      STRNCMP __P((char *, char *, int));
extern char    *convexp __P((char *));
extern char    *expmatch __P((char *, char *, char *));
__END_DECLS

