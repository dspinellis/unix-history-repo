/*-
 * %sccs.include.proprietary.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)sobuf.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>

char	_sobuf[BUFSIZ];
