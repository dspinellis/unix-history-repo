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
 * $Header: slist.h,v 4.2 91/11/25 19:45:47 nicklin Exp $
 *
 * Singly-linked list definitions
 *
 * Author: Peter J. Nicklin
 */

/*
 * Singly-linked list macros
 */
#define SLNUM(slist)	(slist)->nk
/*
 * Singly-linked list block
 */
typedef struct slblk
	{
	char *key;			/* points to a key */
	struct slblk *next;		/* ptr to next list block */
	} SLBLK;
/*
 * Singly-linked list head block
 */
typedef struct slisthb
	{
	int nk;				/* number of keys in list */
	int maxkey;			/* length of longest key */
	SLBLK *head;			/* pointer to first list block */
	SLBLK *curblk;			/* pointer to current block */
	SLBLK *tail;			/* pointer to last list block */
	} SLIST;
/*
 * Functions defined for singly-linked list operations
 */
extern char *slappend();		/* append key */
extern SLIST *slinit();			/* initialize list */
extern void slrm();			/* remove list item */
extern int slsort();			/* sort list */
extern SLBLK **slvect();		/* make linked list vector */
extern void slvtol();			/* convert vector to linked list */
