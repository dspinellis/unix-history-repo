/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)tell.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

/*
 * return offset in file.
 */

long	lseek();

long tell(f)
{
	return(lseek(f, 0L, 1));
}
