/*-
 * Copyright (c) 1992 Henry Spencer.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Henry Spencer of the University of Toronto.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)regex.h	5.2 (Berkeley) %G%
 */

#ifndef _REGEX_H_
#define	_REGEX_H_

/* types */
typedef off_t regoff_t;

typedef struct {
	int re_magic;
	size_t re_nsub;		/* number of parenthesized subexpressions */
	struct re_guts *re_g;	/* none of your business :-) */
} regex_t;

typedef struct {
	regoff_t rm_so;		/* start of match */
	regoff_t rm_eo;		/* end of match */
} regmatch_t;

/* regcomp() flags */
#define	REG_EXTENDED	001
#define	REG_ICASE	002
#define	REG_NOSUB	004
#define	REG_NEWLINE	010

/* regexec() flags */
#define	REG_NOTBOL	00001
#define	REG_NOTEOL	00002
#define	REG_STARTEND	00004
#define	REG_TRACE	00400	/* debugging tracing */
#define	REG_LARGE	01000	/* force large state model for debug */
#define	REG_BACKR	02000

/* errors */
#define	REG_NOMATCH	(1)
#define	REG_BADPAT	(2)
#define	REG_ECOLLATE	(3)
#define	REG_ECTYPE	(4)
#define	REG_EESCAPE	(5)
#define	REG_ESUBREG	(6)
#define	REG_EBRACK	(7)
#define	REG_EPAREN	(8)
#define	REG_EBRACE	(9)
#define	REG_BADBR	(10)
#define	REG_ERANGE	(11)
#define	REG_ESPACE	(12)
#define	REG_BADRPT	(13)
#define	REG_EMPTY	(14)	/* empty component */
#define	REG_ASSERT	(15)	/* assertion failure */

#include <sys/cdefs.h>

__BEGIN_DECLS
int	regcomp __P((regex_t *, const char *, int));
size_t	regerror __P((int, const regex_t *, char *, size_t));
int	regexec __P((const regex_t *,
	    const char *, size_t, regmatch_t [], int));
void	regfree __P((regex_t *));
__END_DECLS

#endif /* !_REGEX_H_ */
