/* Copyright (c) 1983 Regents of the University of California */

/*	@(#)restore.h	3.1	(Berkeley)	83/02/18	*/

#include <stdio.h>
#include <sys/param.h>
#include <sys/inode.h>
#include <sys/fs.h>

/*
 * Flags
 */
extern int	cvtflag;	/* convert from old to new tape format */
extern int	dflag;		/* print out debugging info */
extern int	hflag;		/* restore heirarchies */
extern int	mflag;		/* restore by name instead of inode number */
extern int	vflag;		/* print out actions taken */
extern int	yflag;		/* always try to recover from tape errors */
/*
 * Global variables
 */
extern char	*dumpmap; 	/* map of inodes on this dump tape */
extern char	*clrimap; 	/* map of inodes to be deleted */
extern ino_t	maxino;		/* highest numbered inode in this file system */
extern long	dumpnum;	/* location of the dump on this tape */
extern long	volno;		/* current volume being read */
extern time_t	dumptime;	/* time that this dump was made */
extern char	command;	/* opration being performed */

/*
 * Each file in the file system is described by one of these entries
 */
struct entry {
	char	*e_name;		/* the current name of this entry */
	u_char	e_namlen;		/* length of this name */
	char	e_type;			/* type of this entry, see below */
	short	e_flags;		/* status flags, see below */
	ino_t	e_ino;			/* inode number in previous file sys */
	char	*e_newname;		/* full pathname of rename in new fs */
	struct	entry *e_parent;	/* pointer to parent directory (..) */
	struct	entry *e_sibling;	/* next element in this directory (.) */
	struct	entry *e_links;		/* hard links to this inode */
	struct	entry *e_entries;	/* for directories, their entries */
};
/* types */
#define	LEAF 1			/* non-directory entry */
#define NODE 2			/* directory entry */
#define LINK 4			/* synthesized type, stripped by addentry */
/* flags */
#define REMOVE		0x0001	/* entry to be removed */
#define REMOVED		0x0002	/* entry has been removed */
#define RENAME		0x0004	/* entry to be renamed */
#define TMPNAME		0x0008	/* entry has been given a temporary name */
#define TMPNODE		0x0010	/* entry is a temporary, to be replaced */
#define EXTRACT		0x0020	/* entry is to be extracted from the tape */
#define RENUMBER	0x0040	/* entry is to be assigned a new inode number */
#define CHANGE		0x0080	/* entry is to be deleted and extracted */
#define NEW		0x0100	/* a new entry to be extracted */
#define KEEP		0x0200	/* entry is not to change */
/*
 * functions defined on entry structs
 */
extern struct entry **entry;
extern struct entry *lookupino();
extern struct entry *lookupname();
extern struct entry *lookupparent();
extern struct entry *pathcheck();
extern struct entry *addentry();
extern char *myname();
extern char *savename();
extern ino_t lowerbnd();
extern ino_t upperbnd();
#define NIL ((struct entry *)(0))
#define lookupino(inum)		(entry[(inum)])
#define addino(inum, np)	(entry[(inum)] = (np))
#define deleteino(inum)		(entry[(inum)] = (struct entry *)NIL)
/*
 * Constants associated with entry structs
 */
#define HARDLINK 1
#define SYMLINK  2
#define TMPCHAR (0x01)

/*
 * The entry describes the next file available on the tape
 */
struct context {
	char	*name;		/* name of file */
	ino_t	ino;		/* inumber of file */
	struct	dinode *dip;	/* pointer to inode */
	char	action;		/* action being taken on this file */
} curfile;
/* actions */
#define	USING	1	/* extracting from the tape */
#define	SKIP	2	/* skipping */
#define UNKNOWN 3	/* disposition or starting point is unknown */

/*
 * Other exported routines
 */
extern ino_t psearch();
extern void listfile();
extern void addfile();
extern void markfile();
extern void verifyfile();
extern char *rindex();
extern char *index();
extern void strcat();
extern void strcpy();
extern void mktemp();

/*
 * Useful macros
 */
#define	MWORD(m,i) (m[(unsigned)(i-1)/NBBY])
#define	MBIT(i)	(1<<((unsigned)(i-1)%NBBY))
#define	BIS(i,w)	(MWORD(w,i) |=  MBIT(i))
#define	BIC(i,w)	(MWORD(w,i) &= ~MBIT(i))
#define	BIT(i,w)	(MWORD(w,i) & MBIT(i))

#define dprintf		if (dflag) fprintf
#define vprintf		if (vflag) fprintf

#define GOOD 0
#define FAIL 1
