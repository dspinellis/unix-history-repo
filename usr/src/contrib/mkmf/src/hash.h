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
 * $Header: hash.h,v 4.3 91/11/25 19:45:47 nicklin Exp $
 *
 * Hash table definitions
 *
 * Author: Peter J. Nicklin
 */

/*
 * Singly-linked list block containing a pointer to a hash table
 * block for an include file
 */
typedef struct _iblk
	{
	int i_loop;
	struct _hblk *i_hblk;
	struct _iblk *i_next;
	} INCBLK;
/*
 * Hash table block
 */
typedef struct _hblk
	{
	char *h_key;			/* points to key */
	char *h_def;			/* points to definition string */
	int h_val;			/* integer value */
	struct _iblk *h_sub;		/* ptr to include subchain */
	struct _hblk *h_tag;		/* ptr to auxiliary tag chain */
	struct _hblk *h_next;		/* ptr to next block */
	} HASHBLK;
/*
 * Hash pointer table struct
 */
typedef struct _hash
	{
	HASHBLK **hashtab;		/* hash pointer table */
	HASHBLK *thisblk;		/* current hash table block */
	int headblk;			/* index of head of block chain */
	int hashsiz;			/* hash table size */
	int nk;				/* number of keys in table */
	} HASH;
/*
 * Functions defined for hash tables
 */
#define htnum(ht) ((ht)->nk)		/* return number of keys in table */
#define htkey(ht) ((ht)->thisblk->h_key)/* return key from current block */
#define htdef(ht) ((ht)->thisblk->h_def)/* return definition from current block */
#define htval(ht) ((ht)->thisblk->h_val)/* return value from current block */
extern HASHBLK *htbrm();		/* remove hash table block */
extern int hthash();			/* compute hash value */
extern int htnext();			/* set hash table ptr to next block */
extern HASH *htinit();			/* initialize hash table */
extern HASHBLK *htinstall();		/* install hash table entry */
extern HASHBLK *htlookup();		/* find hash table entry */
extern void htrewind();			/* rewind hash table to first block */
extern void htrm();			/* remove hash table entry */
