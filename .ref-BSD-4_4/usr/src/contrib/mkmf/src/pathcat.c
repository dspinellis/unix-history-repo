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
 * $Header: pathcat.c,v 4.2 91/11/25 19:44:59 nicklin Exp $
 *
 * Author: Peter J. Nicklin
 */

/*
 * pathcat() concatenates path components p1 and p2 into character buffer
 * p1_p2. Returns p1_p2.
 */
#include <stdio.h>
#include "path.h"

extern char *PGN;			/* program name */

char *
pathcat(p1_p2, p1, p2)
	register char *p1;
	register char *p2;
	register char *p1_p2;
{
	register int plen;		/* maximum pathname length */
	char *sp1_p2;			/* start of p1_p2 */

	sp1_p2 = p1_p2;
	for (plen = PATHSIZE; plen > 0; plen--, p1_p2++, p1++)
		if ((*p1_p2 = *p1) == '\0')
			break;
	if (*p2 != '\0' && plen > 0)
		{
		if (p1_p2 != sp1_p2 && p1_p2[-1] != _PSC)
			{
			*p1_p2++ = _PSC;
			plen--;
			}
		for (; plen > 0; plen--, p1_p2++, p2++)
			if ((*p1_p2 = *p2) == '\0')
				break;
		}
	if (plen == 0)
		{
		*--p1_p2 = '\0';
		if (*PGN != '\0')
			fprintf(stderr, "%s: ", PGN);
		fprintf(stderr, "pathname too long\n");
		}
	return(sp1_p2);
}
