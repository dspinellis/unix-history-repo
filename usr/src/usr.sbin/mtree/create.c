/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)create.c	5.16 (Berkeley) 3/12/91";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <time.h>
#include <fts.h>
#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include "mtree.h"

#define	LABEL \
	if (label++) \
		(void)putchar(' '); \

int ftsoptions = FTS_PHYSICAL;

cwalk()
{
	extern int dflag;
	register FTS *t;
	register FTSENT *p;
	register int cnt, label, notset;
	time_t clock;
	uid_t uid;
	gid_t gid;
	mode_t mode;
	int tabs, dsort();
	char *argv[2];
	char curp[MAXPATHLEN], *inotype(), *getlogin(), *rlink();

	if (!getwd(curp)) {
		(void)fprintf(stderr, "mtree: %s\n", curp);
		exit(1);
	}
	(void)time(&clock);
	(void)printf("#\t  fs: %s\n#\t  by: %s\n#\tdate: %s\n",
	    curp, getlogin(), ctime(&clock));

	argv[0] = ".";
	argv[1] = (char *)NULL;
	if (!(t = fts_open(argv, ftsoptions, dsort))) {
		(void)fprintf(stderr,
		    "mtree: fts_open: %s.\n", strerror(errno));
		exit(1);
	}
	while (p = fts_read(t)) {
		switch(p->fts_info) {
		case FTS_D:
			if (dflag)
				notset = 1;
			else
				notset =
				    statdir(t, p, &uid, &gid, &mode, &tabs);
			if (!strcmp(p->fts_name, "."))
				continue;
			break;
		case FTS_DP:
			if (p->fts_level <= 0)
				continue;
			for (cnt = p->fts_level - 1; cnt-- > 0; )
				(void)putchar('\t');
			(void)printf("..\n");
			continue;
		case FTS_DNR:
		case FTS_ERR:
		case FTS_NS:
			(void)fprintf(stderr, "mtree: %s: %s.\n",
			    p->fts_path, strerror(errno));
			continue;
		default:
			if (dflag)
				continue;
		}

		for (cnt = p->fts_level - 1; cnt-- > 0; )
			(void)putchar('\t');
		(void)printf("%s", p->fts_name);
		if (p->fts_info == FTS_D)
			(void)putchar('\t');
		else {
			if (tabs > 1 && p->fts_namelen < 8)
				(void)putchar('\t');
			(void)putchar('\t');
		}

		label = 0;
		if (!S_ISREG(p->fts_statb.st_mode) || notset) {
			LABEL;
			(void)printf("type=%s", inotype(p->fts_statb.st_mode));
		}
		if (p->fts_statb.st_uid != uid || notset) {
			LABEL;
			(void)printf("owner=%u", p->fts_statb.st_uid);
		}
		if (p->fts_statb.st_gid != gid || notset) {
			LABEL;
			(void)printf("group=%u", p->fts_statb.st_gid);
		}
		if ((p->fts_statb.st_mode & MBITS) != mode || notset) {
			LABEL;
			(void)printf("mode=%#o", p->fts_statb.st_mode & MBITS);
		}
		if (p->fts_statb.st_nlink != 1 || notset) {
			LABEL;
			(void)printf("nlink=%u", p->fts_statb.st_nlink);
		}
		LABEL;
		(void)printf("size=%ld", p->fts_statb.st_size);
		LABEL;
		(void)printf("time=%ld", p->fts_statb.st_mtime);

		if (p->fts_info == FTS_SL || p->fts_info == FTS_SLNONE) {
			LABEL;
			(void)printf("link=%s", rlink(p->fts_accpath));
		}
		(void)putchar('\n');
	}
	(void)fts_close(t);
}

#define	MAXGID	5000
#define	MAXUID	5000
#define	MAXMODE	MBITS + 1

statdir(t, parent, puid, pgid, pmode, tabs)
	FTS *t;
	FTSENT *parent;
	uid_t *puid;
	gid_t *pgid;
	mode_t *pmode;
	int *tabs;
{
	register FTSENT *p;
	register gid_t gid;
	register uid_t uid;
	register mode_t mode;
	gid_t savegid;
	uid_t saveuid;
	mode_t savemode;
	u_short maxgid, maxuid, maxmode, g[MAXGID], u[MAXUID], m[MAXMODE];

	if (!(p = fts_children(t))) {
		if (errno) {
			(void)fprintf(stderr, "mtree: %s: %s.\n",
			    RP(parent), strerror(errno));
			exit(1);
		}
		return(1);
	}

	bzero(g, sizeof(g));
	bzero(u, sizeof(u));
	bzero(m, sizeof(m));

	*tabs = 1;
	maxuid = maxgid = maxmode = 0;
	for (; p; p = p->fts_link) {
		mode = p->fts_statb.st_mode & MBITS;
		if (mode < MAXMODE && ++m[mode] > maxmode) {
			savemode = mode;
			maxmode = m[mode];
		}
		gid = p->fts_statb.st_gid;
		if (gid < MAXGID && ++g[gid] > maxgid) {
			savegid = gid;
			maxgid = g[gid];
		}
		uid = p->fts_statb.st_uid;
		if (uid < MAXUID && ++u[uid] > maxuid) {
			saveuid = uid;
			maxuid = u[uid];
		}
		if (p->fts_namelen > 7)
			*tabs = 2;
	}
	(void)printf("\n/set group=%u mode=%#o nlink=1 owner=%u type=file\n",
	    savegid, savemode, saveuid);
	*puid = saveuid;
	*pgid = savegid;
	*pmode = savemode;
	return(0);
}

dsort(p1, p2)
	FTSENT **p1, **p2;
{
	register FTSENT *a, *b;

	a = *p1;
	b = *p2;

	if (S_ISDIR(a->fts_statb.st_mode)) {
		if (!S_ISDIR(b->fts_statb.st_mode))
			return(1);
	} else if (S_ISDIR(b->fts_statb.st_mode))
		return(-1);
	return(strcmp(a->fts_name, b->fts_name));
}
