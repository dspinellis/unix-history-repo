/* 
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratories.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)sem.h	5.1 (Berkeley) %G%
 *
 * from: $Header: sem.h,v 1.5 93/01/12 03:57:14 torek Exp $
 */

void		enddefs __P((const char *));

void		setdefmaxusers __P((int, int, int));
void		setmaxusers __P((int));
int		defattr __P((const char *, struct nvlist *));
void		defdev __P((struct devbase *, int, struct nvlist *,
			struct nvlist *, struct nvlist *, struct nvlist *));
struct devbase *getdevbase __P((const char *name));
struct attr    *getattr __P((const char *name));
void		setmajor __P((struct devbase *d, int n));
void		addconf __P((struct config *));
void		setconf __P((struct nvlist **, const char *, struct nvlist *));
void		adddev __P((const char *, const char *, struct nvlist *, int));
void		addpseudo __P((const char *name, int number));
const char     *ref __P((const char *name));
const char     *starref __P((const char *name));
const char     *wildref __P((const char *name));
