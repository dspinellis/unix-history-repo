/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)fts.h	5.9 (Berkeley) %G%
 */

typedef struct {
	struct _ftsent *fts_cur;	/* current node */
	struct _ftsent *fts_child;	/* linked list of children */
	struct _ftsent *fts_savelink;	/* saved link if node had a cycle */
	struct _ftsent **fts_array;	/* sort array */
	dev_t sdev;			/* starting device # */
	char *fts_path;			/* path for this descent */
	int fts_sd;			/* fd for root */
	int fts_pathlen;		/* sizeof(path) */
	int fts_nitems;			/* elements in the sort array */
	int (*fts_compar)();		/* compare function */
#define	FTS__STOP	0x001		/* private: unrecoverable error */
#define	FTS_LOGICAL	0x002		/* user: use stat(2) */
#define	FTS_NOCHDIR	0x004		/* user: don't use chdir(2) */
#define	FTS_NOSTAT	0x008		/* user: don't require stat info */
#define	FTS_PHYSICAL	0x010		/* user: use lstat(2) */
#define	FTS_SEEDOT	0x020		/* user: return dot and dot-dot */
#define	FTS_XDEV	0x040		/* user: don't cross devices */
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
	short fts_pathlen;		/* strlen(fts_path) */
	short fts_namelen;		/* strlen(fts_name) */
	short fts_level;		/* depth (-1 to N) */
#define	FTS_D		 1		/* preorder directory */
#define	FTS_DC		 2		/* directory that causes cycles */
#define	FTS_DNR		 3		/* unreadable directory */
#define	FTS_DNX		 4		/* unsearchable directory */
#define	FTS_DP		 5		/* postorder directory */
#define	FTS_ERR		 6		/* error; errno is set */
#define	FTS_F		 7		/* regular file */
#define	FTS_NS		 8		/* no stat(2) information */
#define	FTS_SL		 9		/* symbolic link */
#define	FTS_SLNONE	10		/* symbolic link without target */
#define	FTS_DEFAULT	11		/* none of the above */
	u_short fts_info;		/* flags for FTSENT structure */
#define	FTS__NOINSTR	 0		/* private: no instructions */
#define	FTS_AGAIN	 1		/* user: read node again */
#define	FTS_SKIP	 2		/* user: discard node */
#define	FTS_FOLLOW	 3		/* user: follow symbolic link */
	short fts_instr;		/* private: fts_set() instructions */
	struct stat fts_statb;		/* stat(2) information */
	char fts_name[1];		/* file name */
} FTSENT;

#if __STDC__ || c_plusplus
extern FTS *fts_open(const char **, int, int (*)(const FTSENT *, const FTSENT *));
extern FTSENT *fts_read(FTS *);
extern FTSENT *fts_children(FTS *);
extern int fts_set(FTS *, FTSENT *, int);
extern int fts_close(FTS *);
#else
extern FTS *fts_open();
extern FTSENT *fts_children(), *fts_read();
extern int fts_close(), fts_set();
#endif
