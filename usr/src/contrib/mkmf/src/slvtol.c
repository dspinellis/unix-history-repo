/*
 * Copyright (c) 1991 Peter J. Nicklin.
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
 * $Header: slvtol.c,v 4.2 91/11/25 19:44:59 nicklin Exp $
 *
 * slvtol() converts a vector back into a singly-linked list and deletes the
 * vector. Each non-NULL item in the vector is added to the end of the list.
 * The vector is assumed to be the same length as the original list.
 */
#include <stdio.h>
#include "null.h"
#include "slist.h"

void
slvtol(slist, slv)
	SLIST *slist;			/* pointer to list head block */
	SLBLK **slv;			/* ptr to singly-linked list vector */
{
	SLBLK *cblk;			/* current list block */

	int cbi;			/* current block index */
	int nk = 0;			/* number of items in new list */
	register SLBLK *cbp;		/* current block pointer */
	
	slist->head = slist->tail = NULL;

	for (cbi=0; cbi < SLNUM(slist); cbi++)
		{
		cbp = slv[cbi];
		if (cbp != NULL)
			{
			cbp->next = NULL;
			if (slist->tail == NULL)
				{
				slist->head = slist->tail = cbp;
				}
			else	{
				slist->tail = slist->tail->next = cbp;
				}
			nk++;
			}
		}
	slist->nk = nk;
	free((char *) slv);
}
