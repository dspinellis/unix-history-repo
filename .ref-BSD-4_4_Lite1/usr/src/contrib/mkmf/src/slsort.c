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
 * $Header: slsort.c,v 4.3 91/11/25 19:44:59 nicklin Exp $
 *
 * Author: Peter J. Nicklin
 */

/*
 * slsort() sorts list slist according to comparison function compar().
 * compar() is to be called with two arguments and must return an integer
 * greater than, equal to, or less than 0, depending on the lexicographic
 * relationship between the two arguments. Returns integer YES if
 * successful, otherwise NO if out of memory.
 */
#include <stdio.h>
#include "null.h"
#include "slist.h"
#include "yesno.h"

static int comparb();			/* compare 2 list blocks */
static int (*sscmp)();			/* string compare function */

slsort(compar, slist)
	int (*compar)();		/* compare two strings */
	SLIST *slist;			/* pointer to list head block */
{
	char **kp;			/* pointer to key pointer array */
	char *malloc();			/* memory allocator */
	char **skp;			/* ptr to start of key ptr array */
	SLBLK *curblk;			/* current list block */

	if (slist->nk <= 0)
		return(YES);
	else if ((skp = (char **) malloc((unsigned)slist->nk*sizeof(char *))) == NULL)
		{
		nocore();
		return(NO);
		}
	for (kp = skp, curblk = slist->head; curblk != NULL; kp++, curblk = curblk->next)
		*kp = curblk->key;

	sscmp = compar;
	qsort((char *) skp, slist->nk, sizeof(char *), comparb);

	for (kp = skp, curblk = slist->head; curblk != NULL; kp++, curblk = curblk->next)
		curblk->key = *kp;
	
	free((char *) skp);
	return(YES);
}



/*
 * comparb() compares key strings in 2 list blocks. Returns whatever
 * sscmp() returns. sscmp() is a string compare function.
 */
static int
comparb(s1, s2)
	char **s1;			/* string pointer */
	char **s2;			/* string pointer */
{
	return((*sscmp)(*s1, *s2));
}
