/*
 * Copyright (c) 1983, 1985, 1991 Peter J. Nicklin.
 * Copyright (c) 1991 Version Technology.
 * All Rights Reserved.
 *
 * $License: VT.1.1 $
 * Redistribution and use in source and binary forms,  with or without
 * modification,  are permitted provided that the following conditions
 * are met:  (1) Redistributions of source code must retain the  above
 * copyright  notice,  this  list  of  conditions  and  the  following
 * disclaimer.  (2) Redistributions in binary form must reproduce  the
 * above  copyright notice,  this list of conditions and the following
 * disclaimer in the  documentation  and/or other  materials  provided
 * with  the  distribution.  (3) All advertising materials  mentioning
 * features or  use  of  this  software  must  display  the  following
 * acknowledgement:  ``This  product  includes  software  developed by
 * Version Technology.''  Neither the name of Version  Technology  nor
 * the  name  of  Peter J. Nicklin  may  be used to endorse or promote
 * products derived from this software without specific prior  written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY VERSION TECHNOLOGY ``AS IS''  AND  ANY
 * EXPRESS OR IMPLIED WARRANTIES,  INCLUDING,  BUT NOT LIMITED TO, THE
 * IMPLIED  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL  VERSION  TECHNOLOGY  BE
 * LIABLE  FOR ANY DIRECT,  INDIRECT,  INCIDENTAL, SPECIAL, EXEMPLARY,
 * OR  CONSEQUENTIAL DAMAGES   (INCLUDING,   BUT   NOT   LIMITED   TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;  LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF  LIABILITY,  WHETHER  IN  CONTRACT,  STRICT LIABILITY,  OR  TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING  IN ANY WAY OUT OF THE
 * USE OF THIS SOFTWARE,  EVEN  IF  ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 *
 * Report problems and direct questions to nicklin@netcom.com
 *
 * $Header: iolin.c,v 4.2 91/11/25 19:44:59 nicklin Exp $
 *
 * Author: Peter J. Nicklin
 */
#include <stdio.h>
#include "yesno.h"

char IOBUF[BUFSIZ];			/* I/O line buffer */
short CONTINUE;				/* does the line continue? */

/*
 * getlin() stores a line from input stream in IOBUF. The string is terminated
 * by a newline character which is replaced by a null character. getlin()
 * returns IOBUF, or null pointer upon end of file.
 */
char *
getlin(stream)
	register FILE *stream;		/* input stream */
{
	register int c;			/* current character */
	register char *iop;		/* IOBUF pointer */

	iop = IOBUF;
	while ((c = getc(stream)) != '\n' && c != EOF)
		*iop++ = c;
	if (c == EOF && iop == IOBUF)
		return(NULL);
	if (iop != IOBUF && iop[-1] == '\\')
		{
		iop[-1] = '\0';
		CONTINUE = YES;
		}
	else	{
		iop[0] = '\0';
		CONTINUE = NO;
		}
	return(IOBUF);
}



/*
 * purgcontinue() eats up continuation lines from an input stream.
 */
void
purgcontinue(stream)
	register FILE *stream;		/* input stream */
{
	register int c;			/* current character */
	register int lastc;		/* previous character */

	if (CONTINUE == YES)
		{
		for (;;)
			{
			while ((c = getc(stream)) != '\n' && c != EOF)
				lastc = c;
			if (c == EOF || (c == '\n' && lastc != '\\'))
				break;
			}
		CONTINUE = NO;
		}
}



/*
 * putlin() writes IOBUF to stream and appends a newline character. If
 * IOBUF holds a CONTINUE line, a `\' precedes the newline.
 */
void
putlin(stream)
	register FILE *stream;		/* output stream */
{
	register int c;			/* current character */
	register char *iop;		/* IOBUF pointer */

	iop = IOBUF;
	while (c = *iop++)
		putc(c, stream);
	if (CONTINUE == YES)
		putc('\\', stream);
	putc('\n', stream);
}
