/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.1 (Berkeley) %G%
 */

struct tsp;

void	bytehostorder __P((struct tsp *));
void	bytenetorder __P((struct tsp *));
void	clockdiff __P((int, char *[]));
void	help __P((int, char *[]));
void	intr __P((int));
void	makeargv __P((void));
void	msite __P((int, char *[]));
int	priv_resources __P((void));
void	quit __P((void));
void	testing __P((int, char *[]));
void	tracing __P((int, char *[]));
