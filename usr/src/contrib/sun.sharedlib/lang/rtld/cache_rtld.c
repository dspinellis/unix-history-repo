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

/* @(#)cache_rtld.c 1.11 91/03/16 SMI */

/*
 * ld.so directory caching: run-time link-editor specific functions.
 */

/*
 * Copyright (c) 1989, 1991 by Sun Microsystems, Inc.
 */

#include <sys/param.h>
#include "link.h"
#include "../ld/dynamic.h"
#include "rtld.h"
#include "cache.h"
#include <string.h>

static	struct dd *get_next_dir();
static	char *ask_db();
static	char *get_next_name();
static	struct dd *new_dd();
static	secure_directory();

static	struct dd *ddhead = NULL;	/* master head of list pointer */
static	char *es;			/* LD_LIBRARY_PATH cache */
static	char *default_dirs[] =		/* directories searched by default */
    {"/usr/lib", "/usr/local/lib", '\0'};
static	char *secure_dirs[] = 		/* directories we trust */
    {"/usr/lib","/usr/local/lib","/usr/5lib", '\0'};
static	char **also_secure = NULL;	/* directories contributed by main */

extern	char *library_path;		/* alternate path for library search */
extern	struct link_map *mlmp;		/* main program's link map */

/*
 * Lookup a canonical link_object, return its absolute pathname (or NULL).
 */
char *
lo_lookup(lop, lmp)
	struct link_object *lop;	/* link object searched for */
	struct link_map *lmp;		/* link map in which it occurs */
{
	int i, j, k;			/* working temporaries */
	char *r;			/* rule strings from main link map */
	char *cp;			/* working pointer */
	char *so = NULL;		/* return string */
	struct dd *ddp;			/* directory loop temporary */
	static int once = 1;		/* once-only flag */

	/*
	 * Once-only processing.  Deal with security issues here.  Enable
	 * the main program to specify additional "trusted" directories
	 * through it's link map rules.  
	 *
	 * Historical note: until late in the development of 4.1, the
	 * LD_LIBRARY_PATH environment variable was simply ignored if
	 * we were running at setxid program.  However, this check was
	 * redundant, as the elements from the path are checked for trust
	 * anyway, and it inadvertantly prevented a program from allowing
c	 * the path variable to influence the search among trusted directories.
	 * Now, the library path variable is allowed, but its elements are
	 * ignored if they do not specify trusted directories.  This assumes
	 * that the trust relationship among the directories in the path is
	 * reflexive (i.e., the ordering contributes nothing to the trust.)
	 *
	 * Additional note: "ld" allows relative path names to be put into
	 * the link rules for an executable.  This, in the case of setxid
	 * programs is simply too much rope for people -- as they blithely
	 * use relative pathnames in construction but then are surprised
	 * to find them used again at execution.  This obscures the basic
	 * problem of having relative resources when building a secure
	 * executable, but it's clear that this is just too much rope for
	 * people.  So, "also secure" directories are only those that are
	 * absolute.
	 */
	if (once) {

		/*
		 * If we're secure, and the main link_map has been established,
		 * then see if it has any embedded rules.  If so, copy them
		 * onto the heap, and make a pass through counting the number
		 * of directories that are in the list.  Then allocate an
		 * array of char *'s for "also_secure" of the appropriate
		 * length, and step through the processed string to initialize
		 * the array.  Zero the last element.  N.B.: only absolute
		 * directories (those beginning with "/") are included in
		 * this list.
		 */
		if ((*is_secure)() && (mlmp != NULL)) {
			r = &TEXTBASE(mlmp)[mlmp->lm_ld->ld_un.ld_2->ld_rules];
			if (r != TEXTBASE(mlmp)) {
				cp = (*heap_malloc)(strlen(r) + 1);
				(void) strcpy(cp, r);
				for (r = cp, i = 0; get_next_name(&r); i++)
					;
				also_secure = (char **)(*heap_malloc)((i + 1) *
				    sizeof (char *));
				for (j = k = 0; j < i; j++) {
					if (*cp == '/')
						also_secure[k++] = cp;
					while (*cp++)
						;
				}
				also_secure[k] = NULL;
			}
		}

		/*
		 * Security checks finished, set up the path contributed
		 * by the environment.  As noted above, we no longer prune
		 * this if we're running "secure" -- the lower levels take
		 * care of it for us.
		 */
		if (library_path && *library_path) {
			es = (*heap_malloc)(strlen(library_path) + 1);
			(void) strcpy(es, library_path);
		} else
			es = (char *)NULL;
		once = 0;
	}
	for (ddp = get_next_dir((struct dd *)NULL, lmp); 
	    ddp; ddp = get_next_dir(ddp, lmp))
		if (so = ask_db(ddp->dd_db, TEXTBASE(lmp), lop))
			break;
	return (so);
}

/* 
 * Delete the databases that came from the mmapped file and foreach 
 * dd element reset the database pointer
 */
void
lo_flush()
{
	struct dd *ddp;				/* working dd pointer */
	
	dbd_flush();
	for (ddp = ddhead; ddp; ddp = ddp->dd_next) 
		ddp->dd_db = lo_cache((char *)&AP(ddp->dd_db)[ddp->dd_db->db_name]);
}

/* 
 * Given a db - find the highest shared versioned object. The 
 * highest versioned object is the .so  with a matching major number
 * but the highest minor number
 */
static char *
ask_db(dbp, base, lop)
	struct db *dbp;
	caddr_t base;
	struct link_object *lop;
{
	char *name = (char *)&(base[(int)lop->lo_name]);
	struct dbe *ep;
	struct link_object *tlop;
	int index;
	char l[20];

	/*
	 * Search appropriate hash bucket for a matching entry.
	 */
	index = hash(name, strlen(name), lop->lo_major);
	for (ep = (struct dbe *)&(dbp->db_hash[index]); (ep && ep->dbe_lop); 
	    ep = ep->dbe_next == 0 ? NULL : 
	    (struct dbe *)&AP(dbp)[ep->dbe_next]) {
		tlop = (struct link_object *)&AP(dbp)[ep->dbe_lop];
		if (tlop->lo_major == lop->lo_major)
			if (!strcmp((char *)&AP(dbp)[tlop->lo_name], name))
				break;
	}

	/*
	 * If no entry was found, we've lost.
	 */
	if (!(ep && ep->dbe_lop))
		return (NULL);
	sprintf(l, "%d", lop->lo_minor);
	if (verscmp(l, &AP(dbp)[ep->dbe_name] + tlop->lo_minor) > 0)
		fprintf(stderr, 
		    "ld.so: warning: %s has older revision than expected %d\n",
		    &AP(dbp)[ep->dbe_name], lop->lo_minor);
	return (&AP(dbp)[ep->dbe_name]);
}

/*
 * Given a directory descriptor, find the next one in a list.  List
 * is lazily evaluated, and is sensitive to changes in link maps.
 */
static struct dd *
get_next_dir(ddp, lmp)
	struct dd *ddp;			/* previous directory (or NULL) */
	struct link_map *lmp;		/* link map in which it occurs */
{
	struct dd *rddp;		/* return value */
	struct dd **ddpp;		/* insertion point temporary */
	struct db *dbp;			/* points to db */
	char *ds;			/* directory name string */
	static char *ldr;		/* link_dynamic ld_rules cache */
	static int dhused = 0;		/* default list inserted */
	static int rsused = 0;		/* lmp rules have been copied */
	static char *rs;		/* lmp rules string */
	static char **dcpp = default_dirs;
					/* default directory list position */

	static struct dd *etddp = NULL;
					/* environment tail pointer */
	static struct dd *dhddp = NULL; /* defaults head pointer */
	static struct link_map *plmp = NULL;
					/* previous link map pointer */

	/*
	 * If link map has changed, must discard any old values for
	 * its entries and start over.
	 */
	if (plmp != lmp) {
		if (etddp)
			etddp->dd_next = (struct dd *)NULL;
		else
			ddhead = 0;
		plmp = lmp;
		dhused = 0;
		rs = NULL;
		if ((ldr = &TEXTBASE(lmp)[lmp->lm_ld->ld_un.ld_2->ld_rules])
		    == TEXTBASE(lmp)) 
			rsused = 1;
		else
			rsused = 0;
	}

	/*
	 * Get the next entry on the list if any.  If we're starting
	 * at the front, set pointers accordingly.
	 */
	if (ddp) {
		rddp = ddp->dd_next;
		ddpp = &ddp->dd_next;
	} else {
		rddp = ddhead;
		ddpp = &ddhead;
	}

	/*
	 * If we have a block, we're done -- return it.
	 */
	if (rddp)
		return (rddp);

	/*
	 * Ran off the end of the list, have to get another block if
	 * possible.  Try first with the environment.
	 */
	while (es)
		if (ds = get_next_name(&es))
			if (secure_directory(ds))
				if (dbp = lo_cache(ds)) 
					 return (etddp = new_dd(ddpp, dbp));

	/*
	 * Environment is exhausted, search the link_map specific rules.
	 */
	if (!rsused) {
		rs = (*heap_malloc)(strlen(ldr) + 1);
		(void) strcpy(rs, ldr);
		rsused = 1;
	}
	while (rs)
		if (ds = get_next_name(&rs))
			if (secure_directory(ds))
				if (dbp = lo_cache(ds))
					return (new_dd(ddpp, dbp));

	/*
	 * Environment and link_map specific rules exhausted, now examine
	 * the default directories.  If we've got a head and it has not
	 * yet been inserted, just use that.
	 */
	if (dhddp && !dhused) {
		*ddpp = dhddp;
		dhused = 1;
		return (dhddp);
	}

	/*
	 * Only hope now is that we have more default directories to use.
	 */
	while (ds = *dcpp++)
		if (secure_directory(ds))
			if (dbp = lo_cache(ds)) {
				rddp = new_dd(ddpp, dbp);
				/*
				 * dhused = 1 since ddhead is also
				 * dhddp
				 */
				if (!dhddp) {
					dhddp = rddp;
					dhused = 1;
				}
				return (rddp);
			}

	/*
	 * Out of directories, they lose.
	 */
	return ((struct dd *)NULL);
}

/*
 * Extract list of directories needed from colon separated string.
 */
static char *
get_next_name(list)
	char **list;
{
	char *lp = *list;
	char *cp = *list;;

	if (lp != NULL && *lp != '\0') {
		while (*lp != '\0' && *lp != ':')
			lp++;
		if (*lp == ':') {
			*lp = '\0';
			*list = lp + 1;
			if (**list == '\0')
				*list = NULL;
		} else 
			*list = NULL;
	}
	return(cp);
}

/*
 * Allocate a new dd, append it after ddpp and set the dd_dbp to dbp.
 */
static struct dd *
new_dd(ddpp, dbp)
	struct dd **ddpp;		/* insertion point */
	struct db *dbp;			/* db associated with this dd */
{
	struct dd *ddp;			/* working dd ptr. */

	ddp = (struct dd *)(*heap_malloc)(sizeof(struct dd));
	ddp->dd_db = dbp;
	ddp->dd_next = NULL;
	*ddpp = ddp;
	return (ddp);
}

/*
 * Indicate whether "ds" is a directory we trust.  If
 * security function indicates we don't care, then just return true.
 */
static int
secure_directory(ds)
	char *ds;			/* directory string */
{
	char **cpp;			/* working (char **)string */

	if ((*is_secure)()) {
		for (cpp = secure_dirs; *cpp; cpp++)
			if (strcmp(ds, *cpp) == 0)
				return (1);
		if (also_secure)
			for (cpp = also_secure; *cpp; cpp++)
				if (strcmp(ds, *cpp) == 0)
					return (1);
		return (0);
	} else 
		return (1);
}

