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
 * $Header: optpath.c,v 4.4 91/11/25 19:44:59 nicklin Exp $
 *
 * Author: Peter J. Nicklin
 */

/*
 * optpath() condenses a pathname by eliminating adjacent separator
 * characters, and current and parent directory names. If optpath()
 * encounters a parent directory, it backtracks to eliminate the
 * previous directory. If the beginning of the pathname is reached
 * during backtracking, then if the pathname is absolute, the parent
 * directory is purged, otherwise it is shifted to the beginning of
 * pathname. Special care is taken not to clobber a shifted parent
 * by using a guard pointer. Returns pathname.
 *
 * Note 1) For Apollo Domain/OS, adjacent separator characters are
 * allowed as the first two characters of the path name. This correctly
 * passes through paths containing nodenames. (eg. //nodename/myusr/include)
 */
#include "path.h"

#define absolute_path	(*pathname == _RDIRC)

char *
optpath(pathname)
	register char *pathname;	/* pathname to be optimized */
{
	register char *bp;		/* back pathname pointer */
	register char *fp;		/* forward pathname pointer */
	register char *up;		/* pathname update guard pointer */

	bp = fp = up = pathname;

	/* elimination of initial "./" causes no harmful side-effects */
	if (fp[0] == _CDIRC && fp[1] == _PSC) fp += 2;
#ifdef _HasNetRoot
	else if (fp[0] == _PSC)
		*bp++ = *fp++;	/* allow initial "//": see note 1 */
#endif
	while (*fp != '\0')
		if (fp[0] == _PSC)
			if (fp[1] == _PSC || fp[1] == '\0')
				fp += 1;	/* "//" or trailing `/' */
			else if (fp[1]==_CDIRC && (fp[2]==_PSC || fp[2]=='\0'))
				fp += 2;	/* `.' */
			else	{
				*bp++ = *fp++;
				}
		else	{
			*bp++ = *fp++;
			}
	if (bp == pathname && *pathname != '\0')
		*bp++ = (absolute_path) ? _RDIRC : _CDIRC;
	*bp = '\0';
	return(pathname);
}
