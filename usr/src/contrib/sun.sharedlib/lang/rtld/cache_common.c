/*
 * This source code is a product of Sun Microsystems, Inc. and is provided
 * for unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this source code without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 *
 * THIS PROGRAM CONTAINS SOURCE CODE COPYRIGHTED BY SUN MICROSYSTEMS, INC.
 * SUN MICROSYSTEMS, INC., MAKES NO REPRESENTATIONS ABOUT THE SUITABLITY
 * OF SUCH SOURCE CODE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT
 * EXPRESS OR IMPLIED WARRANTY OF ANY KIND.  SUN MICROSYSTEMS, INC. DISCLAIMS
 * ALL WARRANTIES WITH REGARD TO SUCH SOURCE CODE, INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN
 * NO EVENT SHALL SUN MICROSYSTEMS, INC. BE LIABLE FOR ANY SPECIAL, INDIRECT,
 * INCIDENTAL, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING
 * FROM USE OF SUCH SOURCE CODE, REGARDLESS OF THE THEORY OF LIABILITY.
 * 
 * This source code is provided with no support and without any obligation on
 * the part of Sun Microsystems, Inc. to assist in its use, correction, 
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS
 * SOURCE CODE OR ANY PART THEREOF.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California 94043
 */

/* @(#)cache_common.c 1.10 69/12/31 SMI */

/*
 * ld.so directory caching: common code
 */

/*
 * Copyright (c) 1989, 1991 by Sun Microsystems, Inc.
 */

#include <sys/param.h>
#include <sys/file.h>
#include <sys/dirent.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/exec.h>
#include "link.h"
#include "cache.h"
#include <string.h>

extern char *getenv();
extern int stol();
extern int rest_ok();
static struct link_object *get_lo();
static	struct dbd *new_dbd();
static struct db *find_so();
static void fix_lo();
static void get_cache_file();

#define	LIB 	"lib"			/* library name prefix string */
#define SO	".so."			/* extension for shared object */
#define	LIBLEN	(sizeof (LIB) - 1)	/* lengths of same */
#define	SOLEN	(sizeof (SO) - 1)

caddr_t db_base;			/* base address from mmap() */
static	struct dbd *dbd_head = NULL;	/* head of data bases */

/* 
 * Given a directory name - give back a data base. The data base may have
 * orginated from the mmapped file or temporarily created
 */
struct db *
lo_cache(ds)
	char *ds;			/* directory to get cache for */
{
	struct db *dbp;			/* database pointer */
	struct dbd *dbdp;		/* working database descriptor */
	struct dbd **dbdpp;		/* insertion pointer */
	static int once = 1;		/* once-only flag */

	if (once) {
		get_cache_file();
		once = 0;
	}
	dbdpp = &dbd_head;
	for (dbdp = dbd_head; dbdp; dbdp = dbdp->dbd_next) {
		if (!strcmp(ds, &AP(dbdp->dbd_db)[dbdp->dbd_db->db_name]))
			return (dbdp->dbd_db);
		dbdpp = &dbdp->dbd_next;
	}
	if (dbp = find_so(ds)){
		(void) new_dbd(dbdpp, dbp);
	}
	return (dbp);
}

/* 
 * Delete from the db list those dbs that came from the mmapped file
 */
void
dbd_flush()
{
	struct dbd *dbdp;		/* working dbd ptr. */

	for (dbdp = dbd_head; dbdp; dbdp = dbdp->dbd_next) {
		if (dbdp->dbd_db >= (struct db *)db_base) {
			if (dbdp == dbd_head)
				dbd_head = dbdp->dbd_next;
			else if (dbdp->dbd_db >= (struct db *)db_base) 
				dbdp = dbdp->dbd_next;
		}
		if (dbdp->dbd_next == NULL)
			break;
	}
};

/*
 * Get the cache file, if appropriate.
 */
static void
get_cache_file()
{
	int fd;				/* descriptor on cache file */
	struct stat sb;			/* used to find size */
	struct dbf *dbf_base;		/* working cache file pointer */
	struct db *dbp;			/* working data base pointer */
	struct dbd *dbdp;		/* working dbd */
	struct dbd **dbdpp;		/* insertion point for dbd list */

	/*
	 * If cache use is suppressed, simply skip all this.
	 */
	if (!use_cache)
		return;

	/*
	 * Open, map in the file.  Failures occur silently. (XXX)
	 */
	if ((fd = open(CACHE_FILE, O_RDONLY)) == -1) 
		return;
	if (fstat(fd, &sb) == -1) {
		(void) close(fd);
		return;
	}
	dbf_base = (struct dbf *)mmap(0, sb.st_size, PROT_READ, MAP_SHARED, 
	    fd, 0);
	(void) close(fd);
	if ((dbf_base == (struct dbf *)-1) ||
	   (dbf_base->dbf_magic != LD_CACHE_MAGIC))
		return;
	if ((dbf_base->dbf_version != LD_CACHE_VERSION) ||
#if TARGET==SUN2
	  (dbf_base->dbf_machtype != M_68010))
#endif
#if TARGET==SUN3 
	    (dbf_base->dbf_machtype != M_68010) && 
	    (dbf_base->dbf_machtype != M_68020))
#endif
#if TARGET==SUN4
	    (dbf_base->dbf_machtype != M_SPARC))
#endif
		return;
	db_base = &AP(dbf_base)[(int)dbf_base->dbf_db];
	/*
	 * For each data base in the file, build a dbd and link it
	 * on to the master list.
	 */
	for (dbdpp = &dbd_head, dbp = (struct db *)db_base; 
	    (dbp < (struct db *)(db_base + sb.st_size)) && (dbp->db_chain != 0); 
	    (char *)dbp += (int)dbp->db_chain) {
		dbdp = new_dbd(dbdpp, dbp);
		dbdpp = &dbdp->dbd_next;
	}
}

/*
 * Build a database for the directory "ds".
 */
static struct db *
find_so(ds)
	char *ds;			/* directory to search */
{
	int fd;				/* descriptor on directory */
	int n;				/* bytes from getdents */
	char *cp;			/* working char * */
	struct stat sb;			/* buffer for stat'ing directory */
	struct db *dbp;			/* database */	
	static caddr_t buf = NULL;	/* buffer for doing getdents */
	static long bs;			/* cached blocksize for getdents */
	struct link_object *tlop;	/* working link object ptr. */
	struct dirent *dp;		/* directory entry ptr. */
	struct dbe *ep;			/* working db_entry ptr. */
	char *mnp;			/* where minor version begins */
	char *mjp;			/* where major version begins */
	int m;				/* the major number */
	int to_min;			/* index into string of minor */
	int cplen;			/* length of X */
	int index;			/* the hash value */

	/*
	 * Try to open directory.  Failing that, just return silently.
	 */
	if ((fd = open(ds, O_RDONLY)) == -1)
		return ((struct db *)NULL);

	/*
	 * If we have not yet gotten a buffer for reading directories,
	 * allocate it now.  Size it according to the most efficient size
	 * for the first directory we open successfully.
	 */
	if (!buf) {
		if (fstat(fd, &sb) == -1) {
			(void) close(fd);
			return ((struct db *)NULL);
		}
		buf = (*heap_malloc)(bs = sb.st_blksize);
	}

	/*
	 * Have a directory, have a buffer.  Allocate up a database
	 * and initialize it.
	 */
	dbp = (struct db *)(*db_malloc)(sizeof (struct db));
	dbp->db_name = RELPTR(dbp, (*db_malloc)(strlen(ds) + 1));
	(void) strcpy((char *)&AP(dbp)[dbp->db_name], ds);

	/*
	 * Scan the directory looking for shared libraries.  getdents()
	 * failures are silently ignored and terminate the scan.
	 */
	while ((n = getdents(fd, buf, bs)) > 0) 
		for (dp = (struct dirent *)buf; 
		    dp && (dp < (struct dirent *)(buf + n));
		    dp = (struct dirent *)((dp->d_reclen == 0) ? 
		    NULL : (char *)dp + dp->d_reclen)) {

			/*
			 * If file starts with a "lib", then extract the X
			 * from libX.
			 */
			cp = dp->d_name;
			if ((cplen = extract_name(&cp)) == -1) 
				continue;

			/* 
			 * Is the next component ".so."?
			 */
			if (strncmp(SO, cp + cplen, SOLEN))
				continue; 
	
			/*
			 * Check if next component is the major number and
			 * whether following components are legal.
			 */
			mnp = mjp = (dp->d_name + LIBLEN + cplen + SOLEN);
			if (!(stol(mjp, '.', &mnp, &m) && rest_ok(mnp + 1)))
				continue;
			to_min = mnp - dp->d_name + 1;

			/*
			 * Have libX.so.major.minor - attempt to add it to the
			 * cache. If there is another with the same major
			 * number then the chose the object with the highest
			 * minor number
			 */
			index = hash(cp, cplen, m);
			ep = &(dbp->db_hash[index]);
			if (ep->dbe_lop == NULL) {
				ep->dbe_lop = (long)get_lo(dbp, cp, 
				    cplen, m, to_min);
				tlop = (struct link_object *)
				    &AP(dbp)[ep->dbe_lop];
				strcpy(&AP(dbp)[tlop->lo_next], dp->d_name);
				continue;
			}
			for(ep = &(dbp->db_hash[index]); ep; 
			    ep = (struct dbe *) &AP(dbp)[ep->dbe_next]) {
				tlop = (struct link_object *)
				    &AP(dbp)[ep->dbe_lop];

				/* 
				 * Choose the highest minor version
				 */
				if ((tlop->lo_major == m) && 
				    (!strncmp(&AP(dbp)[tlop->lo_name],
				    cp, cplen)) &&
				    (*(&AP(dbp)[tlop->lo_name + cplen + 1]) ==
				     '\0')) {
				    	if (verscmp(dp->d_name + to_min, 
					    (char *)(&AP(dbp)[tlop->lo_next] + to_min))
					    > 0) 
						strcpy(&AP(dbp)[tlop->lo_next],
						    dp->d_name);
					break;
				}
				if (ep->dbe_next == NULL) {
					ep->dbe_next = RELPTR(dbp, 
					    (*db_malloc)(sizeof(struct dbe)));
					ep  = (struct dbe *)
					    &AP(dbp)[ep->dbe_next];
					ep->dbe_lop = (long)get_lo(dbp, 
					    cp, cplen, m, to_min);
					tlop = (struct link_object *)
					    &AP(dbp)[ep->dbe_lop];
					strcpy(&AP(dbp)[tlop->lo_next], dp->d_name);
					break;
				} 
			} 
		}
	fix_lo(dbp);
	(void) close(fd);
	return (dbp);
}

/*
 * Allocate and fill in the fields for a link_object
 */
static struct link_object *
get_lo(dbp, cp, cplen, m, n)
	struct db *dbp;			/* data base */
	char *cp;			/* ptr. to X of libX */
	int cplen;			/* length of X */
	int m;				/* major version */
	int n;				/* index to minor version */
{
	struct link_object *lop;	/* link_object to be returned */
	struct link_object *tlop;	/* working copy of the above */

	/*
	 * Allocate a link object prototype in the database heap.
	 * Store the numeric major (interface) number, but the minor
	 * number is stored in the database as an index to the string
	 * representing the minor version.  By keeping the minor version
	 * as a string, "subfields" (i.e., major.minor[.other.fields. etc.])
	 * are permitted.  Although not meaningful to the link editor, this
	 * permits run-time substitution of arbitrary customer revisions,
	 * although introducing the confusion of overloading the lo_minor
	 * field in the database (!)
	 */
	lop = (struct link_object *)RELPTR(dbp, 
	    (*db_malloc)(sizeof(struct link_object)));
	tlop = (struct link_object *)&AP(dbp)[(long)lop];
	tlop->lo_major = m;
	tlop->lo_minor = n;

	/*
	 * Allocate space for the complete path name on the host program's
	 * heap -- as we have to save it from the directory buffer which
	 * might otherwise get re-used on us.  Note that this space
	 * is wasted -- we can not assume that it can be reclaimed.
	 */
	tlop->lo_next = (long)RELPTR(dbp, (*heap_malloc)(MAXNAMLEN)); 

	/*
	 * Store the prototype name in the link object in the database.
	 */
	tlop->lo_name = (long)RELPTR(dbp, (*db_malloc)(cplen + 1)); 
	strncpy((char *)&AP(dbp)[tlop->lo_name], cp, cplen);
	return(lop);
}

/*
 * Pull the "X" from libX, set name to X and return the
 * length of X
 */
static int
extract_name(name)
	char **name;
{
	char *ls;			/* string after LIB root */
	char *dp;			/* string before first delimiter */
	
	if (strncmp(*name, LIB, LIBLEN) == 0) {
		ls = *name + LIBLEN;
		if ((dp = (char *)index(ls, '.')) != (char *)0) {
			*name = ls;
			return (dp - ls);
		}
	}
	return (-1);
}

/* 
 * Make a pass through the data base to set the dbe_name of a dbe.  This
 * is necessary because there may be several revisions of a library
 * but only one will be chosen. 
 */
static void 
fix_lo(dbp)
	struct db *dbp;
{
	int i;				/* loop temporary */
	int dirlen = strlen(&AP(dbp)[dbp->db_name]);
					/* length of directory pathname */
	char *cp;			/* working temporary */
	char *tp;			/* working temporary */
	struct dbe *ep;			/* working copy of dbe */
	struct link_object *lop;	/* working copy of link_object */

	for(i = 0; i < DB_HASH; i++) {
		for (ep = &(dbp->db_hash[i]); ep && ep->dbe_lop; 
		    (ep = ep->dbe_next == 0 ? NULL : 
		    (struct dbe *)&AP(dbp)[ep->dbe_next])) {
			lop = (struct link_object *)&AP(dbp)[ep->dbe_lop];
			tp = &AP(dbp)[lop->lo_next];
			ep->dbe_name = RELPTR(dbp, 
			    (*db_malloc)(dirlen + strlen(tp) + 2));
			lop->lo_minor += dirlen + 1;
			cp = strncpy(&AP(dbp)[ep->dbe_name], 
			    &AP(dbp)[dbp->db_name], dirlen);
			cp = strncpy(cp + dirlen, "/", 1);
			(void) strcpy(cp + 1, tp);
		}
	}
}

/*
 * Allocate a new dbd, append it after dbdpp and set the dbd_dbp to dbp.
 */
static struct dbd *
new_dbd(dbdpp, dbp)
	struct dbd **dbdpp;		/* insertion point */
	struct db *dbp;			/* db associated with this dbd */
{
	struct dbd *dbdp;		/* working dbd ptr. */

	dbdp = (struct dbd *)(*heap_malloc)(sizeof(struct dbd));
	dbdp->dbd_db = dbp;
	dbdp->dbd_next = NULL;
	*dbdpp = dbdp;
	return (dbdp);
}

/*
 * Calculate hash index for link object.
 * This is based on X.major from libX.so.major.minor.
 */
hash(np, nchrs, m) 
	char *np; 			/* X of libX */
	int nchrs;			/* no of chrs. to hash on */
	int m;				/* the major version */
{
	int h;				/* for loop counter */
	char *cp;			/* working (char *) ptr */

	for (h = 0, cp = np; h < nchrs; h++,*cp++)
		h = (h << 1) + *cp;
	h += (h << 1) + m;
	h = ((h & 0x7fffffff) % DB_HASH);
	return (h);
}
