/*
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	8.1 (Berkeley) %G%
 */

struct delayval;

int	 adelay __P((int, struct delayval *));
char	*autobaud __P((void));
int	 delaybits __P((void));
void	 edithost __P((char *));
void	 gendefaults __P((void));
int	 getent __P((char *, char *));
int	 getflag __P((char *));
long	 getnum __P((char *));
char	*getstr __P((char *, char **));
void	 gettable __P((char *, char *));
void	 makeenv __P((char *[]));
char	*portselector __P((void));
void	 set_ttydefaults __P((int));
void	 setchars __P((void));
void	 setdefaults __P((void));
long	 setflags __P((int));
int	 speed __P((int));

int	 login_tty __P((int));			/* From libutil. */
