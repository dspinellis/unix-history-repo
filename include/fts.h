/*
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
 *
 *	@(#)fts.h	5.14 (Berkeley) 4/3/91
 */

#ifndef	_FTS_H_
#define	_FTS_H_

typedef struct {
	struct _ftsent *fts_cur;	/* current node */
	struct _ftsent *fts_child;	/* linked list of children */
	struct _ftsent *fts_savelink;	/* saved link if node had a cycle */
	struct _ftsent **fts_array;	/* sort array */
	dev_t rdev;			/* starting device # */
	char *fts_path;			/* path for this descent */
	int fts_dfd;			/* fd for directories */
	int fts_rfd;			/* fd for root */
	int fts_pathlen;		/* sizeof(path) */
	int fts_nitems;			/* elements in the sort array */
	int (*fts_compar)();		/* compare function */

#define	FTS_LOGICAL	0x001		/* logical walk */
#define	FTS_NOCHDIR	0x002		/* don't change directories */
#define	FTS_NOSTAT	0x004		/* don't get stat info */
#define	FTS_PHYSICAL	0x008		/* physical walk */
#define	FTS_SEEDOT	0x010		/* return dot and dot-dot */
#define	FTS_STOP	0x020		/* (private) unrecoverable error */
#define	FTS_XDEV	0x040		/* don't cross devices */
	int fts_options;		/* openfts() options */
} FTS;

typedef struct _ftsent {
	struct _ftsent *fts_parent;	/* parent directory */
	struct _ftsent *fts_link;	/* cycle or next file structure */
	union {
		long number;		/* local numeric value */
		void *pointer;		/* local address value */
	} fts_local;
#define	fts_number	fts_local.number
#define	fts_pointer	fts_local.pointer
	char *fts_accpath;		/* access path */
	char *fts_path;			/* root path */
	int fts_cderr;			/* chdir failed -- errno */
	short fts_pathlen;		/* strlen(fts_path) */
	short fts_namelen;		/* strlen(fts_name) */

#define	FTS_ROOTPARENTLEVEL	-1
#define	FTS_ROOTLEVEL		 0
	short fts_level;		/* depth (-1 to N) */

#define	FTS_D		 1		/* preorder directory */
#define	FTS_DC		 2		/* directory that causes cycles */
#define	FTS_DEFAULT	 3		/* none of the above */
#define	FTS_DNR		 4		/* unreadable directory */
#define	FTS_DP		 5		/* postorder directory */
#define	FTS_ERR		 6		/* error; errno is set */
#define	FTS_F		 7		/* regular file */
#define	FTS_NS		 8		/* stat(2) failed */
#define	FTS_NSOK	 9		/* no stat(2) requested */
#define	FTS_SL		10		/* symbolic link */
#define	FTS_SLNONE	11		/* symbolic link without target */
	u_short fts_info;		/* user flags for FTSENT structure */

#define	FTS_AGAIN	 1		/* read node again */
#define	FTS_FOLLOW	 2		/* follow symbolic link */
#define	FTS_NOINSTR	 3		/* no instructions */
#define	FTS_SKIP	 4		/* discard node */
	u_short fts_instr;		/* fts_set() instructions */

	struct stat fts_statb;		/* stat(2) information */
	char fts_name[1];		/* file name */
} FTSENT;

#include <sys/cdefs.h>

__BEGIN_DECLS
FTSENT	*fts_children __P((FTS *));
int	 fts_close __P((FTS *));
FTS	*fts_open
	    __P((char * const *, int, int (*)(const FTSENT *, const FTSENT *)));
FTSENT	*fts_read __P((FTS *));
int	 fts_set __P((FTS *, FTSENT *, int));
__END_DECLS

#endif /* !_FTS_H_ */
