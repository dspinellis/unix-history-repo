/*
 * $Id: info_file.c,v 5.2 90/06/23 22:19:29 jsp Rel $
 *
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
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
 *	@(#)info_file.c	5.1 (Berkeley) 6/29/90
 */

/*
 * Get info from file
 */

#include "am.h"

#ifdef HAS_FILE_MAPS
#include <ctype.h>
#include <sys/stat.h>

#define	MAX_LINE_LEN	2048

static int read_line(buf, size, fp)
char *buf;
int size;
FILE *fp;
{
	int done = 0;

	do {
		while (fgets(buf, size, fp)) {
			int len = strlen(buf);
			done += len;
			if (len > 1 && buf[len-2] == '\\' &&
					buf[len-1] == '\n') {
				int ch;
				buf += len - 2;
				size -= len - 2;
				/*
				 * Skip leading white space on next line
				 */
				while ((ch = getc(fp)) != EOF &&
					isascii(ch) && isspace(ch))
						;
				(void) ungetc(ch, fp);
			} else {
				return done;
			}
		}
	} while (size > 0 && !feof(fp));

	return done;
}

/*
 * Try to locate a key in a file
 */
static int search_or_reload_file(fp, map, key, val, m, fn)
FILE *fp;
char *map;
char *key;
char **val;
mnt_map *m;
void (*fn) P((mnt_map*, char*, char*));
{
	char key_val[MAX_LINE_LEN];
	int chuck = 0;
	int line_no = 0;

	while (read_line(key_val, sizeof(key_val), fp)) {
		char *kp;
		char *cp;
		char *hash;
		int len = strlen(key_val);
		line_no++;

		/*
		 * Make sure we got the whole line
		 */
		if (key_val[len-1] != '\n') {
			plog(XLOG_WARNING, "line %d in \"%s\" is too long", line_no, map);
			chuck = 1;
		} else {
			key_val[len-1] = '\0';
		}

		/*
		 * Strip comments
		 */
		hash = strchr(key_val, '#');
		if (hash)
			*hash = '\0';

		/*
		 * Find start of key
		 */
		for (kp = key_val; *kp && isascii(*kp) && isspace(*kp); kp++)
			;

		/*
		 * Ignore blank lines
		 */
		if (!*kp)
			goto again;

		/*
		 * Find end of key
		 */
		for (cp = kp; *cp&&(!isascii(*cp)||!isspace(*cp)); cp++)
			;

		/*
		 * Check whether key matches
		 */
		if (*cp)
			*cp++ = '\0';

		if ((*key == *kp && strcmp(key, kp) == 0) || fn) {
			while (*cp && isascii(*cp) && isspace(*cp))
				cp++;
			if (*cp) {
				/*
				 * Return a copy of the data
				 */
				char *dc = strdup(cp);
				if (fn)
					(*fn)(m, kp, dc);
				else
					*val = dc;
#ifdef DEBUG
				dlog("%s returns %s", key, dc);
#endif /* DEBUG */
				if (!fn)
					return 0;
			} else {
				plog(XLOG_USER, "%s: line %d has no value field", map, line_no);
			}
		}

again:
		/*
		 * If the last read didn't get a whole line then
		 * throw away the remainder before continuing...
		 */
		if (chuck) {
			while (fgets(key_val, sizeof(key_val), fp) &&
				!strchr(key_val, '\n'))
					;
			chuck = 0;
		}
	}

	return fn ? 0 : ENOENT;
}

int file_init(map)
char *map;
{
	FILE *mapf = fopen(map, "r");
	if (mapf) {
		(void) fclose(mapf);
		return 0;
	}
	return errno;
}

int file_reload(m, map, fn)
mnt_map *m;
char *map;
void (*fn)();
{
	FILE *mapf = fopen(map, "r");
	if (mapf) {
		int error = search_or_reload_file(mapf, map, 0, 0, m, fn);
		(void) fclose(mapf);
		return error;
	}

	return errno;
}

int file_search(m, map, key, pval, tp)
mnt_map *m;
char *map;
char *key;
char **pval;
time_t *tp;
{
	FILE *mapf = fopen(map, "r");
	if (mapf) {
		struct stat stb;
		int error;
		error = fstat(fileno(mapf), &stb);
		if (!error && *tp < stb.st_mtime) {
			*tp = stb.st_mtime;
			error = -1;
		} else {
			error = search_or_reload_file(mapf, map, key, pval, 0, 0);
		}
		(void) fclose(mapf);
		return error;
	}

	return errno;
}
#endif /* HAS_FILE_MAPS */
