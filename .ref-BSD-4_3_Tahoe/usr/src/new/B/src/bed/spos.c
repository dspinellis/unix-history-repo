/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: spos.c,v 1.1 85/08/22 15:50:07 timo Exp $
*/

/*
 * B editor -- Save and restore focus position.
 */

#include "b.h"
#include "bobj.h"
#include "feat.h"
#include "file.h"

#ifdef SAVEPOS
#define MAXPATHLEN 1024 /* See getwd(3) */
#define MAXSAVE 50 /* Maximum number of entries kept in SAVEPOSFILE */

#define strval(v) Str(v)

/*
 * Keep a simple database of file name vs. line number, so that
 * when an edit session is stopped and later continued, the focus
 * is restored exactly.
 * The database is kept in most-recently-used-first order.
 * When it is rewritten, only its first MAXSAVE lines are saved,
 * thus limiting the amount of disk space wasted by files
 * that were once edited but then removed, renamed or forgotten.
 */


Visible int
getpos(file)
	register string file;
{
	register FILE *fp = fopen(posfile, "r");
	char buf[BUFSIZ];
	auto int l1;
	int nread;
	register int len = strlen(file);

	if (!fp)
		return 0;
	while (fgets(buf, sizeof buf, fp) != NULL) {
		if (strncmp(buf, file, len) == 0
			&& (buf[len] == '\t' || buf[len] == ' ')) {
			nread= sscanf(buf+len+1, "%d", &l1);
			if (nread >= 1) {
				fclose(fp);
				return l1;
			}
		}
	}
	fclose(fp);
	return 0;
}


/*
 * Save focus position for file 'file'.
 * Return Yes if save succeeded.
 */

Visible bool
savepos(file, line)
	register string file;
	int line;
{
	register int nsave = 0;
	register int i;
	register FILE *fp = fopen(posfile, "r");
	char buf[BUFSIZ];
	register int len = strlen(file);
	value saved[MAXSAVE];

	if (fp) {
		while (fgets(buf, sizeof buf, fp) != NULL && nsave < MAXSAVE) {
			if (strncmp(file, buf, len) == 0
				&& (buf[len] == ' ' || buf[len] == '\t'))
				continue;
			saved[nsave] = mk_text(buf);
			++nsave;
		}
		fclose(fp);
	}
	fp = fopen(posfile, "w");
	if (fp == NULL)
		return No;
	fprintf(fp, "%s\t%d\n", file, line);
	for (i = 0; i < nsave; ++i) {
		fputs(strval(saved[i]), fp);
		release(saved[i]);
	}
	if (fclose(fp) == EOF) return No;
	return Yes;
}

#endif SAVEPOS
