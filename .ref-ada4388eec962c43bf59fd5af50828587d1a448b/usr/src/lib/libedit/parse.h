/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Christos Zoulas of Cornell University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)parse.h	8.1 (Berkeley) %G%
 */

/*
 * el.parse.h: Parser functions
 */
#ifndef _h_el_parse
#define _h_el_parse

protected int		 parse_line	__P((EditLine *, const char *)); 
protected int		 parse__escape	__P((const char ** const));
protected char *	 parse__string	__P((char *, const char *));
protected int		 parse_cmd	__P((EditLine *, const char *));

#endif /* _h_el_parse */
