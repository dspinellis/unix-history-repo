/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.1 (Berkeley) %G%
 */

void	doabort __P((int, char **));
void	clean __P((int, char **));
void	disable __P((int, char **));
void	down __P((int, char **));
void	enable __P((int, char **));
void	help __P((int, char **));
void	quit __P((int, char **));
void	restart __P((int, char **));
void	start __P((int, char **));
void	status __P((int, char **));
void	stop __P((int, char **));
void	topq __P((int, char **));
void	up __P((int, char **));

extern int NCMDS;
extern struct cmd cmdtab[];
