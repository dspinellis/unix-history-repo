/*
 * Copyright (c) 1993 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tftpsubs.h	5.1 (Berkeley) %G%
 */

/*
 * Prototypes for read-ahead/write-behind subroutines for tftp user and
 * server.
 */
struct tftphdr *r_init __P((void));
void	read_ahead __P((FILE *, int));
int	readit __P((FILE *, struct tftphdr **, int));

int	synchnet __P((int));

struct tftphdr *w_init __P((void));
int	write_behind __P((FILE *, int));
int	writeit __P((FILE *, struct tftphdr **, int, int));
