/* Copyright (c) 1983 Regents of the University of California */

#ifndef lint
static char sccsid[] = "@(#)dirs.c	3.1	(Berkeley)	83/02/18";
#endif

#include "restore.h"
#include <dumprestor.h>
#include <file.h>
#include <dir.h>

#define HASHSIZE	1000

#define INOHASH(val) (val % HASHSIZE)
struct inotab {
	struct inotab *t_next;
	ino_t	t_ino;
	daddr_t	t_seekpt;
	long t_size;
} *inotab[HASHSIZE];
struct inotab *inotablookup();

struct modeinfo {
	ino_t ino;
	time_t timep[2];
	short mode;
	short uid;
	short gid;
};

daddr_t	seekpt;
FILE	*df, *mf;
DIR	*dirp;
char	dirfile[] = "/tmp/rstaXXXXXX";
extern	ino_t search();

#define ODIRSIZ 14
struct odirect {
	u_short	d_ino;
	char	d_name[ODIRSIZ];
};

/*
 *	Extract directory contents, building up a directory structure
 *	on disk for extraction by name.
 *	If modefile is requested, save mode, owner, and times for all
 *	directories on the tape.
 */
extractdirs(modefile)
	char *modefile;
{
	register int i;
	register struct dinode *ip;
	struct direct nulldir;
	int putdir(), null();

	vprintf(stdout, "Extract directories from tape\n");
	mktemp(dirfile);
	df = fopen(dirfile, "w");
	if (df == 0) {
		fprintf(stderr,
		    "restor: %s - cannot create directory temporary\n",
		    dirfile);
		perror("fopen");
		done(1);
	}
	if (modefile != NULL) {
		mf = fopen(modefile, "w");
		if (df == 0) {
			fprintf(stderr,
			    "restor: %s - cannot create modefile \n",
			    modefile);
			perror("fopen");
			done(1);
		}
	}
	nulldir.d_ino = 1;
	nulldir.d_namlen = 1;
	strncpy(nulldir.d_name, "/", nulldir.d_namlen);
	nulldir.d_reclen = DIRSIZ(&nulldir);
	for (;;) {
		curfile.name = "<directory file - name unknown>";
		curfile.action = USING;
		ip = curfile.dip;
		i = ip->di_mode & IFMT;
		if (i != IFDIR) {
			fclose(df);
			dirp = opendir(dirfile);
			if (dirp == NULL)
				perror("opendir");
			if (mf != NULL)
				fclose(mf);
			return;
		}
		allocinotab(curfile.ino, ip, seekpt);
		getfile(putdir, null);
		putent(&nulldir);
		flushent();
	}
}

/*
 *	Recursively find names and inumbers of all files in subtree 
 *	pname and pass them off to be processed.
 */
treescan(pname, ino, todo)
	char *pname;
	ino_t ino;
	void (*todo)();
{
	register struct inotab *itp;
	int namelen;
	daddr_t bpt;
	register struct direct *dp;
	char locname[BUFSIZ + 1];

	itp = inotablookup(ino);
	if (itp == NULL) {
		/*
		 * Pname is name of a simple file or an unchanged directory.
		 */
		(*todo)(pname, ino, LEAF);
		return;
	}
	/*
	 * Pname is a dumped directory name.
	 */
	(*todo)(pname, ino, NODE);
	/*
	 * begin search through the directory
	 * skipping over "." and ".."
	 */
	strncpy(locname, pname, BUFSIZ);
	strncat(locname, "/", BUFSIZ);
	namelen = strlen(locname);
	seekdir(dirp, itp->t_seekpt, itp->t_seekpt);
	dp = readdir(dirp); /* "." */
	dp = readdir(dirp); /* ".." */
	dp = readdir(dirp); /* first real entry */
	bpt = telldir(dirp);
	/*
	 * "/" signals end of directory
	 */
	while (dp != NULL && !(dp->d_namlen == 1 && dp->d_name[0] == '/')) {
		locname[namelen] = '\0';
		if (namelen + dp->d_namlen >= BUFSIZ) {
			fprintf(stderr, "%s%s: name exceeds %d char\n",
				locname, dp->d_name, BUFSIZ);
		} else {
			strncat(locname, dp->d_name, dp->d_namlen);
			treescan(locname, dp->d_ino, todo);
			seekdir(dirp, bpt, itp->t_seekpt);
		}
		dp = readdir(dirp);
		bpt = telldir(dirp);
	}
	if (dp == NULL)
		fprintf(stderr, "corrupted directory: %s.\n", locname);
}

/*
 * Search the directory tree rooted at inode ROOTINO
 * for the path pointed at by n
 */
ino_t
psearch(n)
	char	*n;
{
	register char *cp, *cp1;
	ino_t ino;
	char c;

	ino = ROOTINO;
	if (*(cp = n) == '/')
		cp++;
next:
	cp1 = cp + 1;
	while (*cp1 != '/' && *cp1)
		cp1++;
	c = *cp1;
	*cp1 = 0;
	ino = search(ino, cp);
	if (ino == 0) {
		*cp1 = c;
		return(0);
	}
	*cp1 = c;
	if (c == '/') {
		cp = cp1+1;
		goto next;
	}
	return(ino);
}

/*
 * search the directory inode ino
 * looking for entry cp
 */
ino_t
search(inum, cp)
	ino_t	inum;
	char	*cp;
{
	register struct direct *dp;
	register struct inotab *itp;
	int len;

	itp = inotablookup(inum);
	if (itp == NULL)
		return(0);
	seekdir(dirp, itp->t_seekpt, itp->t_seekpt);
	len = strlen(cp);
	do {
		dp = readdir(dirp);
		if (dp->d_namlen == 1 && dp->d_name[0] == '/')
			return(0);
	} while (dp->d_namlen != len || strncmp(dp->d_name, cp, len));
	return(dp->d_ino);
}

/*
 * Put the directory entries in the directory file
 */
putdir(buf, size)
	char *buf;
	int size;
{
	struct direct cvtbuf;
	register struct odirect *odp;
	struct odirect *eodp;
	register struct direct *dp;
	long loc, i;

	if (cvtflag) {
		eodp = (struct odirect *)&buf[size];
		for (odp = (struct odirect *)buf; odp < eodp; odp++)
			if (odp->d_ino != 0) {
				dcvt(odp, &cvtbuf);
				putent(&cvtbuf);
			}
	} else {
		for (loc = 0; loc < size; ) {
			dp = (struct direct *)(buf + loc);
			i = DIRBLKSIZ - (loc & (DIRBLKSIZ - 1));
			if (dp->d_reclen == 0 || dp->d_reclen > i) {
				loc += i;
				continue;
			}
			loc += dp->d_reclen;
			if (dp->d_ino != 0) {
				putent(dp);
			}
		}
	}
}

/*
 * These variables are "local" to the following two functions.
 */
char dirbuf[DIRBLKSIZ];
long dirloc = 0;
long prev = 0;

/*
 * add a new directory entry to a file.
 */
putent(dp)
	struct direct *dp;
{
	if (dp->d_ino == 0)
		return;
	if (dirloc + dp->d_reclen > DIRBLKSIZ) {
		((struct direct *)(dirbuf + prev))->d_reclen =
		    DIRBLKSIZ - prev;
		fwrite(dirbuf, 1, DIRBLKSIZ, df);
		dirloc = 0;
	}
	bcopy((char *)dp, dirbuf + dirloc, (long)dp->d_reclen);
	prev = dirloc;
	dirloc += dp->d_reclen;
}

/*
 * flush out a directory that is finished.
 */
flushent()
{

	((struct direct *)(dirbuf + prev))->d_reclen = DIRBLKSIZ - prev;
	fwrite(dirbuf, (int)dirloc, 1, df);
	seekpt = ftell(df);
	dirloc = 0;
}

dcvt(odp, ndp)
	register struct odirect *odp;
	register struct direct *ndp;
{

	bzero((char *)ndp, (long)(sizeof *ndp));
	ndp->d_ino =  odp->d_ino;
	strncpy(ndp->d_name, odp->d_name, ODIRSIZ);
	ndp->d_namlen = strlen(ndp->d_name);
	ndp->d_reclen = DIRSIZ(ndp);
	/*
	 * this quickly calculates if this inode is a directory.
	 * Currently not maintained.
	 *
	itp = inotablookup(odp->d_ino);
	if (itp != NIL)
		ndp->d_fmt = IFDIR;
	 */
}

/*
 * Open a directory.
 * Modified to allow any random file to be a legal directory.
 */
DIR *
opendir(name)
	char *name;
{
	register DIR *dirp;

	dirp = (DIR *)malloc((unsigned long)sizeof(DIR));
	dirp->dd_fd = open(name, 0);
	if (dirp->dd_fd == -1) {
		free((char *)dirp);
		return NULL;
	}
	dirp->dd_loc = 0;
	return dirp;
}

/*
 * Seek to an entry in a directory.
 * Only values returned by ``telldir'' should be passed to seekdir.
 * Modified to have many directories based in one file.
 */
void
seekdir(dirp, loc, base)
	register DIR *dirp;
	daddr_t loc, base;
{

	if (loc == telldir(dirp))
		return;
	loc -= base;
	if (loc < 0)
		fprintf(stderr, "bad seek pointer to seekdir %d\n", loc);
	(void) lseek(dirp->dd_fd, base + (loc & ~(DIRBLKSIZ - 1)), 0);
	dirp->dd_loc = loc & (DIRBLKSIZ - 1);
	if (dirp->dd_loc != 0)
		dirp->dd_size = read(dirp->dd_fd, dirp->dd_buf, DIRBLKSIZ);
}

/*
 * get next entry in a directory.
 */
struct direct *
readdir(dirp)
	register DIR *dirp;
{
	register struct direct *dp;

	for (;;) {
		if (dirp->dd_loc == 0) {
			dirp->dd_size = read(dirp->dd_fd, dirp->dd_buf, 
			    DIRBLKSIZ);
			if (dirp->dd_size <= 0)
				return NULL;
		}
		if (dirp->dd_loc >= dirp->dd_size) {
			dirp->dd_loc = 0;
			continue;
		}
		dp = (struct direct *)(dirp->dd_buf + dirp->dd_loc);
		if (dp->d_reclen == 0 ||
		    dp->d_reclen > DIRBLKSIZ + 1 - dirp->dd_loc)
			return NULL;
		dirp->dd_loc += dp->d_reclen;
		if (dp->d_ino == 0)
			continue;
		return (dp);
	}
}

/*
 * Set the mode, owner, and times for all new or changed directories
 */
setdirmodes(modefile)
	char *modefile;
{
	FILE *mf;
	struct modeinfo node;
	struct entry *ep;
	char *cp;
	
	vprintf(stdout, "Set directory mode, owner, and times.\n");
	mf = fopen(modefile, "r");
	if (mf == NULL) {
		perror("fopen");
		panic("cannot open mode file %s\n", modefile);
	}
	clearerr(mf);
	fread((char *)&node, 1, sizeof(struct modeinfo), mf);
	while (!feof(mf)) {
		ep = lookupino(node.ino);
		if (ep == NIL)
			panic("cannot find directory inode %d\n", node.ino);
		cp = myname(ep);
		chown(cp, node.uid, node.gid);
		chmod(cp, node.mode);
		utime(cp, node.timep);
		fread((char *)&node, 1, sizeof(struct modeinfo), mf);
	}
	if (ferror(mf))
		panic("error setting directory modes\n");
	fclose(mf);
}

/*
 * Generate a literal copy of a directory.
 */
genliteraldir(name, ino)
	char *name;
	ino_t ino;
{
	register struct inotab *itp;
	int ofile, dp, i, size;
	char buf[BUFSIZ];

	itp = inotablookup(ino);
	if (itp == NULL)
		panic("cannot find directory inode %d named %s\n", ino, name);
	if ((ofile = open(name, FWRONLY|FCREATE, 0666)) < 0) {
		fprintf(stderr, "%s: cannot create file\n", name);
		return (FAIL);
	}
	seekdir(dirp, itp->t_seekpt, itp->t_seekpt);
	dp = dup(dirp->dd_fd);
	for (i = itp->t_size; i > 0; i -= BUFSIZ) {
		size = i < BUFSIZ ? i : BUFSIZ;
		if (read(dp, buf, (int) size) == -1) {
			fprintf(stderr,
				"write error extracting inode %d, name %s\n",
				curfile.ino, curfile.name);
			perror("read");
			done(1);
		}
		if (write(ofile, buf, (int) size) == -1) {
			fprintf(stderr,
				"write error extracting inode %d, name %s\n",
				curfile.ino, curfile.name);
			perror("write");
			done(1);
		}
	}
	close(dp);
	close(ofile);
	return (GOOD);
}

/*
 * Allocate and initialize a directory inode entry.
 * If requested, save its pertinent mode, owner, and time info.
 */
allocinotab(ino, dip, seekpt)
	ino_t ino;
	struct dinode *dip;
	daddr_t seekpt;
{
	register struct inotab	*itp;
	struct modeinfo node;
	static int prevseekpt = 0;

	itp = (struct inotab *)calloc(1, sizeof(struct inotab));
	itp->t_next = inotab[INOHASH(ino)];
	inotab[INOHASH(ino)] = itp;
	itp->t_ino = ino;
	itp->t_seekpt = seekpt;
	itp->t_size = seekpt - prevseekpt;
	prevseekpt = seekpt;
	if (mf == NULL)
		return;
	node.ino = ino;
	node.timep[0] = dip->di_atime;
	node.timep[1] = dip->di_mtime;
	node.mode = dip->di_mode;
	node.uid = dip->di_uid;
	node.gid = dip->di_gid;
	fwrite((char *)&node, 1, sizeof(struct modeinfo), mf);
}

/*
 * Look up an inode in the table of directories
 */
struct inotab *
inotablookup(ino)
	ino_t	ino;
{
	register struct inotab *itp;

	for (itp = inotab[INOHASH(ino)]; itp != NULL; itp = itp->t_next)
		if (itp->t_ino == ino)
			return(itp);
	return ((struct inotab *)0);
}

/*
 * Clean up and exit
 */
done(exitcode)
	int exitcode;
{

	closemt();
	unlink(dirfile);
	exit(exitcode);
}
