/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rodney Ruddock of the University of Guelph.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)add_line.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <regex.h>
#include <setjmp.h>
#include <stdio.h>

#ifdef DBI
#include <db.h>
#endif

#include "ed.h"
#include "extern.h"

/*
 * This is where the lines actually are put into the buffer.
 */
#ifdef STDIO
long
add_line(p, len)
	char *p;
	long len;
{
	extern int file_loc;
	long l_key;
	int l_jmp_flag;

	if (l_jmp_flag = setjmp(ctrl_position2))
		return (0);
	sigspecial2 = 1;
	if (file_seek)  /* x-ref to get_line for what this does */ {
		file_seek = 0;
		fseek(fhtmp, 0L, 2); /* set to end-to-file */
	}
	l_key = ftell(fhtmp);
					/* keeps user time down 20%approx */
	file_loc = l_key + fwrite(p, sizeof(char), len, fhtmp);
	return (l_key);
}
#endif

#ifdef DBI
recno_t
add_line(p, len)
	char *p;
	long len;
{
	DBT db_key, db_data;
	static recno_t l_key;
	int l_jmp_flag;

	if (l_jmp_flag = setjmp(ctrl_position2))
		return ((recno_t)0);
	sigspecial2 = 1;
	l_key++;
	(db_key.data) = &l_key;
	(db_key.size) = sizeof(recno_t);
	(db_data.data) = p;
	(db_data.size) = len;
	(dbhtmp->put)(dbhtmp, &db_key, &db_data, (u_int)(R_NOOVERWRITE));
	return (l_key);
}
#endif

#ifdef MEMORY
char *
add_line(p, len)
	char *p;
	long len;
{
	char *tmp;
	int l_jmp_flag;

	if (l_jmp_flag = setjmp(ctrl_position2))
		return (NULL);
	sigspecial2 = 1;
	tmp = (char *)calloc(len+1, sizeof(char));
	if (tmp) {
		bcopy(p, tmp, len);
		tmp[len] = '\0';
	}
	return (tmp);
}
#endif
