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

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)fts.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>
#include <fts.h>
#include <strings.h>

extern int errno;

FTSENT *fts_alloc(), *fts_build(), *fts_cycle(), *fts_sort(), *fts_root();
short fts_stat();
char *malloc(), *realloc();

/*
 * Special case a root of "/" so that slashes aren't appended causing
 * paths to be written as "//foo".
 */
#define	NAPPEND(p) \
	(p->level == ROOTLEVEL && p->pathlen == 1 && \
	    p->path[0] == '/' ? 0 : p->pathlen)

#define	CHDIR(sp, path)	(!(sp->options & FTS_NOCHDIR) && chdir(path))

#define	ROOTLEVEL	0
#define	ROOTPARENTLEVEL	-1

static FTS *stream;			/* current stream pointer */

FTS *
ftsopen(argv, options, compar)
	char *argv[];
	register int options;
	int (*compar)();
{
	register FTS *sp;
	register FTSENT *p, *root;
	register int nitems, maxlen;
	FTSENT *parent, *tmp;
	char wd[MAXPATHLEN], *getwd(), *strdup();

	/* allocate/initialize the stream */
	if (!(stream = sp = (FTS *)malloc((u_int)sizeof(FTS))))
		return(NULL);
	bzero(sp, sizeof(FTS));
	sp->compar = compar;
	sp->options = options;

	/* allocate/initialize root's parent */
	if (!(parent = fts_alloc("", 0)))
		goto mem1;
	parent->level = ROOTPARENTLEVEL;

	/* allocate/initialize root(s) */
	if (options & FTS_MULTIPLE) {
		maxlen = -1;
		for (root = NULL, nitems = 0; *argv; ++argv, ++nitems) {
			if (!(p = fts_root(*argv)))
				goto mem2;
			if (maxlen < p->namelen)
				maxlen = p->namelen;
			/*
			 * if comparison routine supplied, traverse in sorted
			 * order; otherwise traverse in the order specified.
			 */
			if (compar) {
				p->link = root;
				root = p;
				p->accpath = p->name;
				p->info = fts_stat(p, 0);
			} else {
				p->link = NULL;
				if (!root)
					tmp = root = p;
				else {
					tmp->link = p;
					tmp = p;
				}
			}
			p->level = ROOTLEVEL;
			p->parent = parent;
		}
		if (compar && nitems > 1)
			root = fts_sort(root, nitems);
	} else {
		if (!(root = fts_root((char *)argv)))
			goto mem2;
		maxlen = root->namelen;
		root->link = NULL;
		root->level = ROOTLEVEL;
		root->parent = parent;
	}

	/* start out with at least 1K+ of path space */
	if (!fts_path(MAX(maxlen, MAXPATHLEN)))
		goto mem2;

	/*
	 * some minor trickiness.  Set the pointers so that ftsread thinks
	 * we've just finished the node before the root(s); set p->info to
	 * FTS_NS so that everything about the "current" node is ignored.
	 * Rather than allocate a dummy node use the root's parent's link
	 * pointer.  This is handled specially in ftsclose() as well.
	 */
	sp->cur = parent;
	parent->link = root;
	parent->info = FTS_NS;

	/*
	 * if using chdir(2), do a getwd() to insure that we can get back
	 * here; this could be avoided for some paths, but probably not
	 * worth the effort.  Slashes, symbolic links, and ".." are all
	 * fairly nasty problems.  Note, if we can't get the current
	 * working directory we run anyway, just more slowly.
	 */
	if (!(options & FTS_NOCHDIR) && (!getwd(wd) || !(sp->wd = strdup(wd))))
		sp->options |= FTS_NOCHDIR;

	return(sp);

mem2:	fts_lfree(root);
	(void)free((char *)parent);
mem1:	(void)free((char *)sp);
	return(NULL);
}

static
fts_load(p)
	register FTSENT *p;
{
	register int len;
	register char *cp;

	/*
	 * load the stream structure for the next traversal; set the
	 * accpath field specially so the chdir gets done to the right
	 * place and the user can access the first node.
	 */
	len = p->pathlen = p->namelen;
	bcopy(p->name, stream->path, len + 1);
	if ((cp = rindex(p->name, '/')) && (cp != p->name || cp[1])) {
		len = strlen(++cp);
		bcopy(cp, p->name, len + 1);
		p->namelen = len;
	}
	p->accpath = p->path = stream->path;
}

ftsclose(sp)
	FTS *sp;
{
	register FTSENT *freep, *p;
	int saved_errno;

	if (sp->cur)
		/* check for never having read anything */
		if (sp->cur->level == ROOTPARENTLEVEL)
			fts_lfree(sp->cur);
		else {
			for (p = sp->cur; p->level > ROOTPARENTLEVEL;) {
				freep = p;
				p = p->link ? p->link : p->parent;
				(void)free((char *)freep);
			}
			(void)free((char *)p);
		}

	/* free up child linked list, sort array, path buffer */
	if (sp->child)
		fts_lfree(sp->child);
	if (sp->array)
		(void)free((char *)sp->array);
	(void)free((char *)sp->path);

	/*
	 * return to original directory, save errno if necessary;
	 * free up the directory buffer
	 */
	if (!(sp->options & FTS_NOCHDIR)) {
		saved_errno = chdir(sp->wd) ? errno : 0;
		(void)free(sp->wd);
	}

	/* free up the stream pointer */
	(void)free((char *)sp);

	/* set errno and return */
	if (!(sp->options & FTS_NOCHDIR) && saved_errno) {
		errno = saved_errno;
		return(-1);
	}
	return(0);
}

FTSENT *
ftsread(sp)
	register FTS *sp;
{
	register FTSENT *p;
	register int instr;
	static int cd;
	FTSENT *tmp;
	char *cp;

	/* if finished or unrecoverable error, return NULL */
	if (!sp->cur || sp->options & FTS__STOP)
		return(NULL);

	/* set global stream pointer, and current node pointer */
	stream = sp;
	p = sp->cur;

	/* save and zero out user instructions */
	instr = p->instr;
	p->instr = 0;

	/* if used link pointer for cycle detection, restore it */
	if (sp->savelink) {
		p->link = sp->savelink;
		sp->savelink = NULL;
	}

	/* any type of file may be re-visited; re-stat and return */
	if (instr == FTS_AGAIN) {
		p->info = fts_stat(p, 0);
		return(p);
	}

	if (p->info == FTS_D)
		if (instr == FTS_SKIP) {
			if (sp->child) {
				fts_lfree(sp->child);
				sp->child = NULL;
			}
		} else {
			if (sp->child) {
				/* cd into child directory */
				if (CHDIR(sp, p->accpath)) {
					sp->options |= FTS__STOP;
					p->info = FTS_ERR;
					return(p);
				}
			} else if (!(sp->child = fts_build(sp, 1)))
				return(p);
			p = sp->child;
			sp->child = NULL;
			cp = sp->path + NAPPEND(p->parent);
			*cp++ = '/';
			bcopy(p->name, cp, p->namelen + 1);
			return(sp->cur = p);
		}
	else if (p->info == FTS_SL && instr == FTS_FOLLOW) {
		p->info = fts_stat(p, 1);
		return(p);
	}

	/*
	 * user may have called ftsset on pointer returned by
	 * ftschildren; handle it here.
	 */
	for (p = p->link; p;) {
		instr = p->instr;
		if (instr == FTS_FOLLOW) {
			p->info = fts_stat(p, 1);
			p->instr = 0;
			break;
		}
		if (instr == FTS_SKIP) {
			tmp = p;
			p = p->link;
			(void)free((char *)tmp);
			continue;
		}
		p->instr = 0;
		break;
	}

	/* go to next node on this level */
	if (p) {
		/*
		 * if root level node, set up paths and return.  If not the
		 * first time, and it's not an absolute pathname, get back
		 * to wherever we started from.
		 */
		if (p->level == ROOTLEVEL) {
			fts_load(p);
			if (cd) {
				(void)free((char *)sp->cur);
				if (p->path[0] != '/' && CHDIR(sp, sp->wd)) {
					/* return target path for error msg */
					p->path = sp->wd;
					p->info = FTS_ERR;
					sp->options |= FTS__STOP;
					return(sp->cur = p);
				}
			} else
				cd = 1;
			p->info = fts_stat(p, 0);
		} else {
			(void)free((char *)sp->cur);
			cp = sp->path + NAPPEND(p->parent);
			*cp++ = '/';
			bcopy(p->name, cp, p->namelen + 1);
			if (p->info == FTS_D && (tmp = fts_cycle(p))) {
				sp->savelink = p->link;
				p->link = tmp;
			}
		}
		return(sp->cur = p);
	}

	/* go to parent */
	p = sp->cur->parent;
	(void)free((char *)sp->cur);
	if (p->level == ROOTPARENTLEVEL) {
		/*
		 * done; free everything up and set errno to 0 so the user
		 * can distinguish between error and EOF.
		 */
		(void)free((char *)p);
		errno = 0;
		return(sp->cur = NULL);
	}

	sp->path[p->pathlen] = '\0';
	if (CHDIR(sp, "..")) {
		sp->options |= FTS__STOP;
		p->info = FTS_ERR;
	} else
		p->info = FTS_DP;
	return(sp->cur = p);
}

/*
 * ftsset takes the stream as an argument although it's not used in this
 * implementation; it would be necessary if anyone wanted to add global
 * semantics to fts using ftsset.  A possible error return is allowed for
 * similar reasons.
 */
/* ARGSUSED */
ftsset(sp, p, instr)
	FTS *sp;
	FTSENT *p;
	int instr;
{
	p->instr = instr;
	return(0);
}

FTSENT *
ftschildren(sp)
	register FTS *sp;
{
	/*
	 * set errno to 0 so that user can tell the difference between an
	 * error and a directory without entries.
	 */
	errno = 0;
	if (sp->cur->info != FTS_D || sp->options & FTS__STOP)
		return(NULL);
	if (sp->child)
		fts_lfree(sp->child);
	if (sp->child = fts_build(sp, 0)) {
		/*
		 * have to cd up so that the fields of the current node
		 * as returned from readfts will be correct.
		 */
		if (CHDIR(sp, "..")) {
			sp->options |= FTS__STOP;
			return(NULL);
		}
	}
	return(sp->child);
}

#define	ISDOT(a)	(a[0] == '.' && (!a[1] || a[1] == '.' && !a[2]))

static FTSENT *
fts_build(sp, set_node_errors)
	register FTS *sp;
	int set_node_errors;
{
	register struct dirent *dp;
	register FTSENT *p, *head;
	register int nitems;
	DIR *dirp;
	int len, level, maxlen, nlinks, saved_errno;
	char *cp;

	p = sp->cur;
	if (!(dirp = opendir(p->accpath))) {
		if (set_node_errors) {
			errno = 0;
			p->info = FTS_DNR;
		}
		return(NULL);
	}
	if (CHDIR(sp, p->accpath)) {
		if (set_node_errors) {
			errno = 0;
			p->info = FTS_DNX;
		}
		return(NULL);
	}

	/*
	 * the real slowdown in walking the tree is the stat calls.  If
	 * FTS_NOSTAT is set and it's a physical walk (so that symbolic
	 * links can't be directories), fts assumes that the number of
	 * subdirectories in a node is equal to the number of links to
	 * the parent.  This allows stat calls to be skipped in any leaf
	 * directories and for any nodes after the directories in the
	 * parent node have been found.  This empirically cuts the stat
	 * calls by about 2/3.
	 */
	nlinks = sp->options & FTS_NOSTAT && sp->options & FTS_PHYSICAL ?
	    p->statb.st_nlink - (sp->options & FTS_SEEDOT ? 0 : 2) : -1;

	/* get max file name length that can be stored in current path */
	maxlen = sp->pathlen - p->pathlen - 1;

	cp = sp->path + (len = NAPPEND(p));
	*cp++ = '/';

	level = p->level + 1;

	/* read the directory, attching each new entry to the `link' pointer */
	for (head = NULL, nitems = 0; dp = readdir(dirp);) {
		if (ISDOT(dp->d_name) && !(sp->options & FTS_SEEDOT))
			continue;

		if (!(p = fts_alloc(dp->d_name, dp->d_namlen))) {
			saved_errno = errno;
			goto mem1;
		}
		if (dp->d_namlen > maxlen) {
			if (!fts_path((int)dp->d_namlen)) {
				/* quit: this stream no longer has a path */
				sp->options |= FTS__STOP;
				saved_errno = errno;
				(void)free((char *)p);
mem1:				fts_lfree(head);
				if (set_node_errors)
					p->info = FTS_ERR;
				if (CHDIR(sp, "..")) {
					saved_errno = errno;
					sp->options |= FTS__STOP;
				}
				errno = saved_errno;
				return(NULL);
			}
			maxlen = sp->pathlen - sp->cur->pathlen - 1;
		}

		p->pathlen = len + dp->d_namlen + 1;
		p->accpath = sp->options & FTS_NOCHDIR ? p->path : p->name;

		p->parent = sp->cur;
		p->level = level;

		if (nlinks) {
			/* make sure fts_stat has a filename to stat */
			if (sp->options & FTS_NOCHDIR)
				bcopy(p->name, cp, p->namelen + 1);
			p->info = fts_stat(p, 0);
			if (nlinks > 0 && (p->info == FTS_D ||
			    p->info == FTS_DNR || p->info == FTS_DNX))
				--nlinks;
		} else
			p->info = FTS_NS;

		p->link = head;
		head = p;
		++nitems;
	}
	(void)closedir(dirp);

	if (!nitems) {
		if (CHDIR(sp, "..")) {
			sp->options |= FTS__STOP;
			p->info = FTS_ERR;
		} else if (set_node_errors)
			p->info = FTS_DP;
		*--cp = '\0';
		return(NULL);
	}

	if (sp->compar && nitems > 1)
		head = fts_sort(head, nitems);

	if (set_node_errors)
		bcopy(head->name, cp, head->namelen + 1);
	else
		*--cp = '\0';
	return(head);
}

static short
fts_stat(p, symflag)
	register FTSENT *p;
	int symflag;
{
	register int ngroups, *gp;
	int gidset[NGROUPS];

	/*
	 * detection of symbolic links w/o targets.  If FTS_FOLLOW is set,
	 * the symlink structure is overwritten with the stat structure of
	 * the target.
	 */
	if (stream->options & FTS_LOGICAL || symflag) {
		if (stat(p->accpath, &p->statb))
			return(symflag || !lstat(p->accpath, &p->statb) ?
			    FTS_SLNONE : FTS_ERR);
	} else if (lstat(p->accpath, &p->statb))
		return(FTS_ERR);

	switch(p->statb.st_mode&S_IFMT) {
	case S_IFDIR:
		/* get new uid/groups each time, they may have changed */
		if (getuid() == p->statb.st_uid) {
			if (!(p->statb.st_mode&S_IRUSR))
				return(FTS_DNR);
			if (!(p->statb.st_mode&S_IXUSR))
				return(FTS_DNX);
			return(FTS_D);
		}
		if ((ngroups = getgroups(NGROUPS, gidset)) == -1)
			return(FTS_ERR);
		for (gp = gidset; ngroups--;)
			if (*gp++ == p->statb.st_gid) {
				if (!(p->statb.st_mode&S_IRGRP))
					return(FTS_DNR);
				if (!(p->statb.st_mode&S_IXGRP))
					return(FTS_DNX);
				return(FTS_D);
			}
		if (!(p->statb.st_mode&S_IROTH))
			return(FTS_DNR);
		if (!(p->statb.st_mode&S_IXOTH))
			return(FTS_DNX);
		return(FTS_D);
	case S_IFLNK:
		return(FTS_SL);
	case S_IFREG:
		return(FTS_F);
	default:
		return(FTS_DEFAULT);
	}
	/* NOTREACHED */
}

static FTSENT *
fts_cycle(p)
	register FTSENT *p;
{
	register dev_t dev;
	register ino_t ino;

	/*
	 * cycle detection is brute force; if the tree gets deep enough or
	 * the number of symbolic links to directories is really high
	 * something faster might be worthwhile.
	 */
	dev = p->statb.st_dev;
	ino = p->statb.st_ino;
	for (p = p->parent; p->level > ROOTLEVEL; p = p->parent)
		if (ino == p->statb.st_ino && dev == p->statb.st_dev)
			return(p);
	return(NULL);
}

#define	R(type, nelem, ptr) \
	(type *)realloc((char *)ptr, (u_int)((nelem) * sizeof(type)))

static FTSENT *
fts_sort(head, nitems)
	FTSENT *head;
	register int nitems;
{
	register FTSENT **ap, *p;

	/*
	 * construct an array of pointers to the structures and call qsort(3).
	 * Reassemble the array in the order returned by qsort.  If unable to
	 * sort for memory reasons, return the directory entries in their
	 * current order.  Allocate enough space for the current needs plus
	 * 40 so we don't realloc one entry at a time.
	 */
	if (nitems > stream->nitems) {
		stream->nitems = nitems + 40;
		if (!(stream->array =
		    R(FTSENT *, stream->nitems, stream->array))) {
			stream->nitems = 0;
			return(head);
		}
	}
	for (ap = stream->array, p = head; p; p = p->link)
		*ap++ = p;
	qsort((char *)stream->array, nitems, sizeof(FTSENT *), stream->compar);
	for (head = *(ap = stream->array); --nitems; ++ap)
		ap[0]->link = ap[1];
	ap[0]->link = NULL;
	return(head);
}

static FTSENT *
fts_alloc(name, len)
	char *name;
	register int len;
{
	register FTSENT *p;

	/*
	 * variable sized structures; the name is the last element so
	 * allocate enough extra space after the structure to hold it.
	 */
	if (!(p = (FTSENT *)malloc((u_int)(sizeof(FTSENT) + len))))
		return(NULL);
	bcopy(name, p->name, len + 1);
	p->namelen = len;
	p->path = stream->path;
	p->instr = 0;
	p->local.number = 0;
	p->local.pointer = NULL;
	return(p);
}

static
fts_lfree(head)
	register FTSENT *head;
{
	register FTSENT *p;

	while (p = head) {
		head = head->link;
		(void)free((char *)p);
	}
}

/*
 * allow essentially unlimited paths; certain programs (find, remove, ls)
 * need to work on any tree.  Most systems will allow creation of paths
 * much longer than MAXPATHLEN, even though the kernel won't resolve them.
 * Add an extra 128 bytes to the requested size so that we don't realloc
 * the path 2 bytes at a time.
 */
static
fts_path(size)
	int size;
{
	stream->pathlen += size + 128;
	return((int)(stream->path = R(char, stream->pathlen, stream->path)));
}

static FTSENT *
fts_root(name)
	register char *name;
{
	register char *cp;

	/*
	 * rip trailing slashes; it's somewhat unclear in POSIX 1003.1 what
	 * /a/b/ really is, they don't talk about what a null path component
	 * resolves to.  This hopefully does what the user intended.  Don't
	 * allow null pathnames.
	 */
	for (cp = name; *cp; ++cp);
	if (cp == name) {
		errno = ENOENT;
		return(NULL);
	}
	while (--cp > name && *cp == '/');
	*++cp = '\0';
	return(fts_alloc(name, cp - name));
}
