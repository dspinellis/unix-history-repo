/*
 * try_stdarg - try <stdarg.h> to see if it really works with vsprintf()
 *
 * On some systems that have both <stdarg.h> and <varargs.h>, vsprintf()
 * does not work well under one type of include file.  For example, some
 * System V based systems (such as UMIPS) have bugs in the <stdarg.h>
 * implementation.
 *
 * This program exit 1 is vsprintf() produces unexpected results
 * while using the <stdarg.h> include file.
 */
/*
 * Copyright (c) 1994 by Landon Curt Noll.  All Rights Reserved.
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright, this permission notice and text
 * this comment, and the disclaimer below appear in all of the following:
 *
 *	supporting documentation
 *	source copies
 *	source works derived from this source
 *	binaries derived from this source or from derived source
 *
 * LANDON CURT NOLL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO
 * EVENT SHALL LANDON CURT NOLL BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * chongo was here	/\../\
 */

#include <stdarg.h>
#include <stdio.h>

char buf[BUFSIZ];

#if defined(__STDC__) && __STDC__ == 1
# define VA_ALIST char *fmt, ...
# define VA_DCL
#else
# define VA_ALIST fmt
# define VA_DCL char *fmt;
#endif
void
try(VA_ALIST)
    VA_DCL
{
    va_list ap;

    va_start(ap, fmt);
    vsprintf(buf, fmt, ap);
    va_end(ap);
}

main()
{
    try("@%d:%s:%d@", 1, "hi", 2);
    if (strcmp(buf, "@1:hi:2@") != 0) {
	exit(1);
    }
    exit(0);
}
