/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)create.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <stdio.h>
#include <string.h>
#include "mtree.h"

cwalk(parent, tail)
	ENTRY *parent;
	register char *tail;
{
	extern ENTRY *root;
	extern dev_t device;
	extern int dflag, xflag, errno, alphasort();
	extern char path[];
	register ENTRY *centry, *level;
	struct dirent *dp, **dir_list;
	struct stat sbuf;
	int cnt, dir_cnt;
	char *emalloc(), *rlink();

	*tail++ = '/';
	dir_cnt = scandir(".", &dir_list, NULL, alphasort);
	if (dir_cnt == -1 || xflag && lstat(".", &sbuf)) {
		(void)fprintf(stderr, "mtree: %s: %s\n", path + 2,
		    strerror(errno));
			exit(1);
	}
	device = sbuf.st_dev;
	for (cnt = 0; cnt < dir_cnt; ++cnt) {
		dp = dir_list[cnt];
		if (dp->d_name[0] == '.' &&
		    (!dp->d_name[1] || dp->d_name[1] == '.' && !dp->d_name[2]))
			continue;
		bcopy(dp->d_name, tail, dp->d_namlen + 1);
		if (lstat(dp->d_name, &sbuf)) {
			(void)fprintf(stderr, "mtree: %s: %s\n",
			    path + 2, strerror(errno));
			exit(1);
		}
		if (dflag && !S_ISDIR(sbuf.st_mode))
			continue;

		centry = (ENTRY *)emalloc(sizeof(ENTRY));
		if (!(centry->name = strdup(dp->d_name)))
			nomem();
		centry->nlen = dp->d_namlen;

		switch(sbuf.st_mode&S_IFMT) {
		case S_IFDIR:
			centry->info.type = F_DIR;
			break;
		case S_IFCHR:
			centry->info.type = F_CHAR;
			break;
		case S_IFBLK:
			centry->info.type = F_BLOCK;
			break;
		case S_IFREG:
			centry->info.type = F_FILE;
			break;
		case S_IFLNK:
			centry->info.type = F_LINK;
			centry->info.slink = strdup(rlink(dp->d_name));
			break;
		case S_IFSOCK:
			centry->info.type = F_SOCK;
			break;
		default:
			(void)fprintf(stderr, "mtree: unknown file type %s.\n",
			    path + 2);
			exit(1);
		}
		centry->info.st_uid = sbuf.st_uid;
		centry->info.st_gid = sbuf.st_gid;
		centry->info.st_size = sbuf.st_size;
		centry->info.st_mode = sbuf.st_mode&07777;
		centry->info.st_nlink = sbuf.st_nlink;
		centry->info.st_uid = sbuf.st_uid;

		if (!root) {
			level = root = centry;
			root->parent = root->child = NULL;
		}
		else if (parent) {
			centry->parent = parent;
			parent->child = centry;
			level = centry;
			parent = NULL;
		}
		else {
			centry->parent = level->parent;
			level = level->next = centry;
		}
		stats(&centry->info);
		if (!S_ISDIR(sbuf.st_mode) || xflag && device != sbuf.st_dev)
			continue;
		if (chdir(dp->d_name)) {
			(void)fprintf(stderr, "mtree: %s: %s\n", path + 2,
			    strerror(errno));
			exit(1);
		}
		cwalk(level, tail + dp->d_namlen);
		if (chdir("..")) {
			(void)fprintf(stderr, "mtree: ..: %s\n",
			    strerror(errno));
			exit(1);
		}
	}
}

#define	LABEL { \
	if (!label++) \
		(void)printf("%s", level->nlen > 7 ? "\t" : "\t\t"); \
	else \
		(void)putchar(' '); \
}

extern mode_t dmode;				/* default directory mode */
extern mode_t fmode;				/* default file mode */
uid_t uid, gid;					/* default owner, group */
u_int type;
pwalk(level, tabs)
	ENTRY *level;
	int tabs;
{
	INFO *ip;
	register int cnt;
	int label;
	char *ftype();

	for (; level; level = level->next) {
		for (cnt = tabs; cnt--; )
			(void)putchar('\t');
		(void)printf("%s", level->name);
		label = 0;
		if ((ip = &level->info)->type != type) {
			LABEL;
			(void)printf("type=%s", ftype(ip->type));
		}
		if (ip->st_uid != uid) {
			LABEL;
			(void)printf("owner=%u", ip->st_uid);
		}
		if (ip->st_gid != gid) {
			LABEL;
			(void)printf("group=%u", ip->st_gid);
		}
		if (ip->type == F_DIR) {
			if (ip->st_mode != dmode) {
				LABEL;
				(void)printf("mode=%o", ip->st_mode);
			}
		} else {
			if (ip->st_mode != fmode) {
				LABEL;
				(void)printf("mode=%o", ip->st_mode);
			}
			if (ip->st_nlink != 1) {
				LABEL;
				(void)printf("nlink=%u", ip->st_nlink);
			}
		}
		LABEL;
		(void)printf("size=%ld", ip->st_size);
		if (ip->slink)
			(void)printf(" link=%s", ip->slink);
		(void)putchar('\n');
		if (level->child)
			pwalk(level->child, tabs + 1);
		if (ip->type == F_DIR) {
			for (cnt = tabs; cnt--; )
				(void)putchar('\t');
			(void)printf("..\n");
		}
	}
}

ID *uhead;
ID *ghead;
u_long dmodes[0777 + 1];
u_long fmodes[0777 + 1];

stats(ip)
	INFO *ip;
{
	register ID *p;

	if (ip->type == F_DIR)
		++dmodes[ip->st_mode&0777];
	else
		++fmodes[ip->st_mode&0777];
	for (p = uhead;; p = p->next)
		if (!p) {
			p = (ID *)emalloc(sizeof(ID));
			p->id = ip->st_uid;
			p->next = uhead;
			uhead = p;
			break;
		} else if (p->id == ip->st_uid) {
			++p->cnt;
			break;
		}
	for (p = ghead;; p = p->next)
		if (!p) {
			p = (ID *)emalloc(sizeof(ID));
			p->id = ip->st_gid;
			p->next = ghead;
			ghead = p;
			break;
		} else if (p->id == ip->st_gid) {
			++p->cnt;
			break;
		}
}

shostats()
{
	extern int dflag;
	register ID *p;
	register mode_t cnt;
	register u_long max;

	for (max = 0, cnt = 0777; cnt; --cnt)
		if (dmodes[cnt] > max) {
			max = dmodes[cnt];
			dmode = cnt;
		}
	(void)printf("/set dmode=%o\n", dmode);
	for (max = 0, cnt = 0777; cnt; --cnt)
		if (fmodes[cnt] > max) {
			max = dmodes[cnt];
			fmode = cnt;
		}
	(void)printf("/set fmode=%o\n", fmode);
	for (max = 0, p = uhead; p; p = p->next)
		if (p->cnt > max) {
			max = p->cnt;
			uid = p->id;
		}
	(void)printf("/set owner=%u\n", uid);
	for (max = 0, p = ghead; p; p = p->next)
		if (p->cnt > max) {
			max = p->cnt;
			gid = p->id;
		}
	(void)printf("/set group=%u\n", gid);
	(void)printf("/set nlink=1\n");
	if (dflag) {
		type = F_DIR;
		(void)printf("/set type=dir\n\n");
	} else {
		type = F_FILE;
		(void)printf("/set type=file\n\n");
	}
}
