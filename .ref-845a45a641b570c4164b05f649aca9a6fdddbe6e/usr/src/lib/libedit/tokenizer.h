/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Christos Zoulas of Cornell University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tokenizer.h	8.1 (Berkeley) %G%
 */

/*
 * tokenizer.h: Header file for tokenizer routines
 */
#ifndef _h_tokenizer
#define _h_tokenizer

typedef struct tokenizer Tokenizer;

Tokenizer 	*tok_init	__P((const char *));
void		 tok_reset	__P((Tokenizer *));
void		 tok_end	__P((Tokenizer *));
int		 tok_line	__P((Tokenizer *, const char *, 
				     int *, char ***));

#endif /* _h_tokenizer */
