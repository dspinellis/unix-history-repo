/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)fts.c	5.5 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>
#include <fts.h>
#include <string.h>

extern int errno;

FTSENT *fts_alloc(), *fts_build(), *fts_cycle(), *fts_sort(), *fts_root();
short fts_stat();
char *malloc(), *realloc();

/*
 * Special case a root of "/" so that slashes aren't appended causing
 * paths to be written as "//foo".
 */
#define	NAPPEND(p) \
	(p->fts_level == ROOTLEVEL && p->fts_pathlen == 1 && \
	    p->fts_path[0] == '/' ? 0 : p->fts_pathlen)

#define	CHDIR(sp, path)	(!(sp->fts_options & FTS_NOCHDIR) && chdir(path))
#define	FCHDIR(sp, fd)	(!(sp->fts_options & FTS_NOCHDIR) && fchdir(fd))

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
	sp->fts_compar = compar;
	sp->fts_options = options;

	/* allocate/initialize root's parent */
	if (!(parent = fts_alloc("", 0)))
		goto mem1;
	parent->fts_level = ROOTPARENTLEVEL;

	/* allocate/initialize root(s) */
	if (options & FTS_MULTIPLE) {
		maxlen = -1;
		for (root = NULL, nitems = 0; *argv; ++argv, ++nitems) {
			if (!(p = fts_root(*argv)))
				goto mem2;
			if (maxlen < p->fts_namelen)
				maxlen = p->fts_namelen;
			/*
			 * if comparison routine supplied, traverse in sorted
			 * order; otherwise traverse in the order specified.
			 */
			if (compar) {
				p->fts_link = root;
				root = p;
				p->fts_accpath = p->fts_name;
				p->fts_info = fts_stat(p, 0);
			} else {
				p->fts_link = NULL;
				if (!root)
					tmp = root = p;
				else {
					tmp->fts_link = p;
					tmp = p;
				}
			}
			p->fts_level = ROOTLEVEL;
			p->fts_parent = parent;
		}
		if (compar && nitems > 1)
			root = fts_sort(root, nitems);
	} else {
		if (!(root = fts_root((char *)argv)))
			goto mem2;
		maxlen = root->fts_namelen;
		root->fts_link = NULL;
		root->fts_level = ROOTLEVEL;
		root->fts_parent = parent;
	}

	/* start out with at least 1K+ of path space */
	if (!fts_path(MAX(maxlen, MAXPATHLEN)))
		goto mem2;

	/*
	 * some minor trickiness.  Set the pointers so that ftsread thinks
	 * we've just finished the node before the root(s); set p->fts_info
	 * to FTS_NS so that everything about the "current" node is ignored.
	 * Rather than allocate a dummy node use the root's parent's link
	 * pointer.  This is handled specially in ftsclose() as well.
	 */
	sp->fts_cur = parent;
	parent->fts_link = root;
	parent->fts_info = FTS_NS;

	/*
	 * if using chdir(2), do a getwd() to insure that we can get back
	 * here; this could be avoided for some paths, but probably not
	 * worth the effort.  Slashes, symbolic links, and ".." are all
	 * fairly nasty problems.  Note, if we can't get the current
	 * working directory we run anyway, just more slowly.
	 */
	if (!(options & FTS_NOCHDIR) &&
	    (!getwd(wd) || !(sp->fts_wd = strdup(wd))))
		sp->fts_options |= FTS_NOCHDIR;

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
	len = p->fts_pathlen = p->fts_namelen;
	bcopy(p->fts_name, stream->fts_path, len + 1);
	if ((cp = rindex(p->fts_name, '/')) && (cp != p->fts_name || cp[1])) {
		len = strlen(++cp);
		bcopy(cp, p->fts_name, len + 1);
		p->fts_namelen = len;
	}
	p->fts_accpath = p->fts_path = stream->fts_path;
}

ftsclose(sp)
	FTS *sp;
{
	register FTSENT *freep, *p;
	int saved_errno;

	if (sp->fts_cur)
		/* check for never having read anything */
		if (sp->fts_cur->fts_level == ROOTPARENTLEVEL)
			fts_lfree(sp->fts_cur);
		else {
			for (p = sp->fts_cur; p->fts_level > ROOTPARENTLEVEL;) {
				freep = p;
				p = p->fts_link ? p->fts_link : p->fts_parent;
				(void)free((char *)freep);
			}
			(void)free((char *)p);
		}

	/* free up child linked list, sort array, path buffer */
	if (sp->fts_child)
		fts_lfree(sp->fts_child);
	if (sp->fts_array)
		(void)free((char *)sp->fts_array);
	(void)free((char *)sp->fts_path);

	/*
	 * return to original directory, save errno if necessary;
	 * free up the directory buffer
	 */
	if (!(sp->fts_options & FTS_NOCHDIR)) {
		saved_errno = chdir(sp->fts_wd) ? errno : 0;
		(void)free(sp->fts_wd);
	}

	/* free up the stream pointer */
	(void)free((char *)sp);

	/* set errno and return */
	if (!(sp->fts_options & FTS_NOCHDIR) && saved_errno) {
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
	if (!sp->fts_cur || sp->fts_options & FTS__STOP)
		return(NULL);

	/* set global stream pointer, and current node pointer */
	stream = sp;
	p = sp->fts_cur;

	/* save and zero out user instructions */
	instr = p->fts_instr;
	p->fts_instr = 0;

	/* if used link pointer for cycle detection, restore it */
	if (sp->fts_savelink) {
		p->fts_link = sp->fts_savelink;
		sp->fts_savelink = NULL;
	}

	/* any type of file may be re-visited; re-stat and return */
	if (instr == FTS_AGAIN) {
		p->fts_info = fts_stat(p, 0);
		return(p);
	}

	if (p->fts_info == FTS_D)
		if (instr == FTS_SKIP || sp->fts_options & FTS_XDEV &&
		    p->fts_statb.st_dev != sp->sdev) {
			if (sp->fts_child) {
				fts_lfree(sp->fts_child);
				sp->fts_child = NULL;
			}
		} else {
			if (!sp->fts_child &&
			    (!(sp->fts_child = fts_build(sp, 1))))
				return(p);
			p = sp->fts_child;
			sp->fts_child = NULL;
			return(sp->fts_cur = p);
		}
	else if (p->fts_info == FTS_SL && instr == FTS_FOLLOW) {
		p->fts_info = fts_stat(p, 1);
		return(p);
	}

	/*
	 * user may have called ftsset on pointer returned by
	 * ftschildren; handle it here.
	 */
	for (p = p->fts_link; p;) {
		instr = p->fts_instr;
		if (instr == FTS_FOLLOW) {
			p->fts_info = fts_stat(p, 1);
			p->fts_instr = 0;
			break;
		}
		if (instr == FTS_SKIP) {
			tmp = p;
			p = p->fts_link;
			(void)free((char *)tmp);
			continue;
		}
		p->fts_instr = 0;
		break;
	}

	/* go to next node on this level */
	if (p) {
		/*
		 * if root level node, set up paths and return.  If not the
		 * first time, and it's not an absolute pathname, get back
		 * to wherever we started from.
		 */
		if (p->fts_level == ROOTLEVEL) {
			fts_load(p);
			if (cd) {
				(void)free((char *)sp->fts_cur);
				if (p->fts_path[0] != '/' &&
				    CHDIR(sp, sp->fts_wd)) {
					/* return target path for error msg */
					p->fts_path = sp->fts_wd;
					p->fts_info = FTS_ERR;
					sp->fts_options |= FTS__STOP;
					return(sp->fts_cur = p);
				}
			} else
				cd = 1;
			p->fts_info = fts_stat(p, 0);
			sp->sdev = p->fts_statb.st_dev;
		} else {
			(void)free((char *)sp->fts_cur);
			cp = sp->fts_path + NAPPEND(p->fts_parent);
			*cp++ = '/';
			bcopy(p->fts_name, cp, p->fts_namelen + 1);
			if (p->fts_info == FTS_D && (tmp = fts_cycle(p))) {
				sp->fts_savelink = p->fts_link;
				p->fts_link = tmp;
			}
		}
		return(sp->fts_cur = p);
	}

	/* go to parent */
	p = sp->fts_cur->fts_parent;
	(void)free((char *)sp->fts_cur);
	if (p->fts_level == ROOTPARENTLEVEL) {
		/*
		 * done; free everything up and set errno to 0 so the user
		 * can distinguish between error and EOF.
		 */
		(void)free((char *)p);
		errno = 0;
		return(sp->fts_cur = NULL);
	}

	sp->fts_path[p->fts_pathlen] = '\0';
	if (CHDIR(sp, "..")) {
		sp->fts_options |= FTS__STOP;
		p->fts_info = FTS_ERR;
	} else
		p->fts_info = FTS_DP;
	return(sp->fts_cur = p);
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
	p->fts_instr = instr;
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
	if (sp->fts_cur->fts_info != FTS_D || sp->fts_options & FTS__STOP)
		return(NULL);
	if (sp->fts_child)
		fts_lfree(sp->fts_child);
	return(sp->fts_child = fts_build(sp, 0));
}

#define	ISDOT(a)	(a[0] == '.' && (!a[1] || a[1] == '.' && !a[2]))

static FTSENT *
fts_build(sp, set_node)
	register FTS *sp;
	int set_node;
{
	register struct dirent *dp;
	register FTSENT *p, *head;
	register int nitems;
	DIR *dirp;
	int descend, len, level, maxlen, nlinks, saved_errno;
	char *cp;

	p = sp->fts_cur;
	if (!(dirp = opendir(p->fts_accpath))) {
		if (set_node) {
			errno = 0;
			p->fts_info = FTS_DNR;
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
	nlinks =
	    sp->fts_options & FTS_NOSTAT && sp->fts_options & FTS_PHYSICAL ?
	    p->fts_statb.st_nlink - (sp->fts_options & FTS_SEEDOT ? 0 : 2) : -1;

	if (nlinks || set_node) {
		if (FCHDIR(sp, dirfd(dirp))) {
			if (set_node) {
				errno = 0;
				p->fts_info = FTS_DNX;
			}
			return(NULL);
		}
		descend = 1;
	} else
		descend = 0;

	/* get max file name length that can be stored in current path */
	maxlen = sp->fts_pathlen - p->fts_pathlen - 1;

	cp = sp->fts_path + (len = NAPPEND(p));
	*cp++ = '/';

	level = p->fts_level + 1;

	/* read the directory, attching each new entry to the `link' pointer */
	for (head = NULL, nitems = 0; dp = readdir(dirp);) {
		if (ISDOT(dp->d_name) && !(sp->fts_options & FTS_SEEDOT))
			continue;

		if (!(p = fts_alloc(dp->d_name, dp->d_namlen))) {
			saved_errno = errno;
			goto mem1;
		}
		if (dp->d_namlen > maxlen) {
			if (!fts_path((int)dp->d_namlen)) {
				/* quit: this stream no longer has a path */
				sp->fts_options |= FTS__STOP;
				saved_errno = errno;
				(void)free((char *)p);
mem1:				fts_lfree(head);
				if (set_node)
					p->fts_info = FTS_ERR;
				if (descend && CHDIR(sp, "..")) {
					saved_errno = errno;
					sp->fts_options |= FTS__STOP;
				}
				errno = saved_errno;
				return(NULL);
			}
			maxlen = sp->fts_pathlen - sp->fts_cur->fts_pathlen - 1;
		}

		p->fts_pathlen = len + dp->d_namlen + 1;
		p->fts_accpath =
		    sp->fts_options & FTS_NOCHDIR ? p->fts_path : p->fts_name;

		p->fts_parent = sp->fts_cur;
		p->fts_level = level;

		if (nlinks) {
			/* make sure fts_stat has a filename to stat */
			if (sp->fts_options & FTS_NOCHDIR)
				bcopy(p->fts_name, cp, p->fts_namelen + 1);
			p->fts_info = fts_stat(p, 0);
			if (nlinks > 0 && (p->fts_info == FTS_D ||
			    p->fts_info == FTS_DNR || p->fts_info == FTS_DNX))
				--nlinks;
		} else
			p->fts_info = FTS_NS;

		p->fts_link = head;
		head = p;
		++nitems;
	}
	(void)closedir(dirp);

	if (descend && (!nitems || !set_node) && CHDIR(sp, "..")) {
		sp->fts_options |= FTS__STOP;
		p->fts_info = FTS_ERR;
		*--cp = '\0';
		return(NULL);
	}

	if (!nitems) {
		if (set_node)
			p->fts_info = FTS_DP;
		*--cp = '\0';
		return(NULL);
	}

	if (sp->fts_compar && nitems > 1)
		head = fts_sort(head, nitems);

	if (set_node)
		bcopy(head->fts_name, cp, head->fts_namelen + 1);
	else
		*--cp = '\0';
	return(head);
}

static short
fts_stat(p, symflag)
	register FTSENT *p;
	int symflag;
{
	/*
	 * detection of symbolic links w/o targets.  If FTS_FOLLOW is set,
	 * the symlink structure is overwritten with the stat structure of
	 * the target.
	 */
	if (stream->fts_options & FTS_LOGICAL || symflag) {
		if (stat(p->fts_accpath, &p->fts_statb))
			return(symflag || !lstat(p->fts_accpath,
			    &p->fts_statb) ? FTS_SLNONE : FTS_ERR);
	} else if (lstat(p->fts_accpath, &p->fts_statb))
		return(FTS_ERR);

	switch(p->fts_statb.st_mode&S_IFMT) {
	case S_IFDIR:
		return(FTS_D);
	case S_IFLNK:
		return(FTS_SL);
	case S_IFREG:
		return(FTS_F);
	}
	return(FTS_DEFAULT);
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
	dev = p->fts_statb.st_dev;
	ino = p->fts_statb.st_ino;
	for (p = p->fts_parent; p->fts_level > ROOTLEVEL; p = p->fts_parent)
		if (ino == p->fts_statb.st_ino && dev == p->fts_statb.st_dev)
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
	if (nitems > stream->fts_nitems) {
		stream->fts_nitems = nitems + 40;
		if (!(stream->fts_array =
		    R(FTSENT *, stream->fts_nitems, stream->fts_array))) {
			stream->fts_nitems = 0;
			return(head);
		}
	}
	for (ap = stream->fts_array, p = head; p; p = p->fts_link)
		*ap++ = p;
	qsort((char *)stream->fts_array, nitems, sizeof(FTSENT *),
	    stream->fts_compar);
	for (head = *(ap = stream->fts_array); --nitems; ++ap)
		ap[0]->fts_link = ap[1];
	ap[0]->fts_link = NULL;
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
	bcopy(name, p->fts_name, len + 1);
	p->fts_namelen = len;
	p->fts_path = stream->fts_path;
	p->fts_instr = 0;
	p->fts_local.number = 0;
	p->fts_local.pointer = NULL;
	return(p);
}

static
fts_lfree(head)
	register FTSENT *head;
{
	register FTSENT *p;

	while (p = head) {
		head = head->fts_link;
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
	stream->fts_pathlen += size + 128;
	return((int)(stream->fts_path =
	    R(char, stream->fts_pathlen, stream->fts_path)));
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
