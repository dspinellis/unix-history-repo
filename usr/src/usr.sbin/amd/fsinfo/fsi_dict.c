/*
 * $Id: fsi_dict.c,v 5.2 90/10/21 22:34:21 jsp Exp $
 *
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)fsi_dict.c	5.1 (Berkeley) %G%
 */

/*
 * Dictionary support
 */

#include "../fsinfo/fsinfo.h"

static int dict_hash(k)
char *k;
{
	unsigned int h;
	
	for (h = 0; *k; h += *k++)
		;
	return h % DICTHASH;
}

dict *new_dict()
{
	dict *dp = ALLOC(dict);
	return dp;
}

static void dict_add_data(de, v)
dict_ent *de;
char *v;
{
	dict_data *dd = ALLOC(dict_data);
	dd->dd_data = v;
	ins_que(&dd->dd_q, de->de_q.q_back);
	de->de_count++;
}

static dict_ent *new_dict_ent(k)
char *k;
{
	dict_ent *de = ALLOC(dict_ent);
	de->de_key = k;
	init_que(&de->de_q);
	return de;
}

dict_ent *dict_locate(dp, k)
dict *dp;
char *k;
{
	dict_ent *de = dp->de[dict_hash(k)];
	while (de && !STREQ(de->de_key, k))
		de = de->de_next;

	return de;
}

void dict_add(dp, k, v)
dict *dp;
char *k, *v;
{
	dict_ent *de = dict_locate(dp, k);
	if (!de) {
		dict_ent **dep = &dp->de[dict_hash(k)];
		de = new_dict_ent(k);
		de->de_next = *dep;
		*dep = de;
	}
	dict_add_data(de, v);
}

int dict_iter(dp, fn)
dict *dp;
int (*fn)();
{
	int i;
	int errors = 0;

	for (i = 0; i < DICTHASH; i++) {
		dict_ent *de = dp->de[i];
		while (de) {
			errors += (*fn)(&de->de_q);
			de = de->de_next;
		}
	}
	return errors;
}
