/* entry_load.c - load bits of the database */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/entry_load.c,v 7.6 91/03/09 11:56:52 mrose Exp $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/entry_load.c,v 7.6 91/03/09 11:56:52 mrose Exp $
 *
 *
 * $Log:	entry_load.c,v $
 * Revision 7.6  91/03/09  11:56:52  mrose
 * update
 * 
 * Revision 7.5  91/02/22  09:39:20  mrose
 * Interim 6.8
 * 
 * Revision 7.4  90/10/17  11:54:17  mrose
 * sync
 * 
 * Revision 7.3  90/07/09  14:46:10  mrose
 * sync
 * 
 * Revision 7.2  90/04/18  08:49:54  mrose
 * 6.2
 * 
 * Revision 7.1  89/12/19  16:20:34  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:17:39  mrose
 * Release 6.0
 * 
 */

/*
 *                                NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */

#include "config.h"
#include "quipu/util.h"
#include "quipu/entry.h"
#include "quipu/ds_error.h"
#include "tailor.h"
#include <sys/stat.h>
#include <errno.h>
#ifdef TURBO_AVL
#include "quipu/turbo.h"
#endif

extern char * treedir;
extern LLog * log_dsap;
extern int errno;
extern int parse_status;
struct acl_info * acl_dflt ();
#ifdef TURBO_INDEX
extern AttributeType *turbo_index_types;
#endif

static char filename [LINESIZE];
static PS ps;

#define EDBLEN	3	/* length of string "EDB" */

fileexists (fname)
char * fname;
{
struct stat buf;

	if (stat (fname,&buf) != 0) {
		if (errno != ENOENT)
			DLOG (log_dsap,LLOG_DEBUG,("File %s will not stat - %d",fname,errno));
		return FALSE;
	}
	return TRUE;
}

static read_mapped_rdn (aps,name,file)
PS aps;
char * name;
char * file;
{
FILE * mapfp;
char *ptr, *newname, *tmp, *getline();
extern int parse_line;
register int i;

	if ((mapfp = fopen (file,"r")) == (FILE *)NULL) {
		LLOG(log_dsap,LLOG_EXCEPTIONS,("Can read \"%s\" (%d)",file,errno));
		return FALSE;
	}

	parse_line = 0;
	while ( (ptr = getline(mapfp)) != NULLCP) {
		if ((newname = rindex(ptr,'#')) == NULLCP) {
			LLOG(log_dsap,LLOG_EXCEPTIONS,("Seperator missing in map file \"%s\", line %d",file,parse_line));
			(void) fclose (mapfp);			
			return FALSE;
		}
		tmp = newname;
		*newname++ = 0;
		while (isspace(*--tmp))
			*tmp = 0;

		if (lexequ (name,ptr) == 0) {
			/* got it - replace in ps*/	
			i = strlen (name);
			aps->ps_ptr -= i;
			aps->ps_cnt += i;
			ps_print (aps,SkipSpace(newname));
			(void) fclose (mapfp);			
			return TRUE;
		}
	}

	DLOG (log_dsap, LLOG_DEBUG,("%s not found in map file %s",name,file));
	(void) fclose (mapfp);			
	return FALSE;
}

static write_mapped_rdn (aps,name,file)
PS aps;
char * name;
char * file;
{
FILE * mapfp;
char mapname[LINESIZE];
char sname[LINESIZE];
char *mptr, *nptr;
register int i;

	if (strlen(name) < MAXFILENAMELEN) 
		return FALSE;

	/* Make unique name for it */
	mptr = mapname;
	if ((nptr = index (name,'=')) == NULLCP)
		return FALSE;

	for (i=0 ; (*nptr!=0) && (i < MAXFILENAMELEN-6) ; nptr++)
		if (isalpha(*nptr))
			*mptr++ = *nptr, i++;

	(void) strcpy (sname,name);
	(void) strcpy (mptr,"XXXXXX");
	i = strlen (name);
	nptr = (aps->ps_ptr -= i);
	aps->ps_cnt += i;
	ps_print (aps,mapname);
	*aps->ps_ptr = 0;

	if ((aps->ps_base = mktemp (aps->ps_base)) == NULLCP)
		return FALSE;

	DLOG(log_dsap,LLOG_DEBUG,("mapped name %s",aps->ps_base));

	if (mkdir (aps->ps_base,0700) != 0) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("map rdn mkdir failure \"%s\" (%d)",aps->ps_base,errno));
		return FALSE;
	}

	/* write it to map file */
	if (fileexists(file)) 
		mapfp = fopen (file,"a");
	else {
		int um;
		um = umask (0177);
		mapfp = fopen (file,"w");
		(void) umask (um);
	}

	if (mapfp == (FILE *)NULL) {
		LLOG(log_dsap,LLOG_EXCEPTIONS,("Can't write to \"%s\" (%d)",file,errno));
		return FALSE;
	}

	if (fprintf (mapfp,"%s#%s\n",sname,nptr) == EOF) {
		LLOG(log_dsap,LLOG_EXCEPTIONS,("Can't write to \"%s\" (%d)",file,errno));
		return FALSE;
	}

	if (fclose (mapfp) != 0) {
		LLOG(log_dsap,LLOG_EXCEPTIONS,("Can't close \"%s\" (%d)",file,errno));
		return FALSE;
	}

	return TRUE;
}

static rdn2filename (aps,rdn,make)
PS aps;
RDN rdn;
char make;
{
	char *start = aps->ps_ptr;
	char mapbuf [LINESIZE];	

	/* look for EDB.map file */
	*aps->ps_ptr = 0;
	(void) sprintf (mapbuf, "%sEDB.map",aps->ps_base);

	rdn_print (aps,rdn,DIROUT);
	*aps->ps_ptr = 0;

	if (fileexists (mapbuf) && read_mapped_rdn (aps,start,mapbuf)) {
		*aps->ps_ptr = 0;
		if (fileexists(aps->ps_base))
			return OK;
		if (make) { 
			if (mkdir (aps->ps_base,0700) != 0) {
				LLOG (log_dsap,LLOG_EXCEPTIONS,("dn2file mkdir (mapped) failure \"%s\" (%d)",aps->ps_base,errno));
				return NOTOK;
			}
			LLOG (log_dsap,LLOG_NOTICE,("WARNING: Needed to make mapped directory \"%s\"",aps->ps_base));
			return OK;
		}
		LLOG (log_dsap,LLOG_EXCEPTIONS,("mapped file missing \"%s\"",aps->ps_base));
		return NOTOK;
	}

#ifdef SYS5
	else if ( strlen(start) > MAXFILENAMELEN ) 
		LLOG (log_dsap,LLOG_EXCEPTIONS,("Potential problem with \"%s\" (name too long)",start));
#endif

	if (fileexists(aps->ps_base))
		return OK;
	
	if (make) { 
		if (write_mapped_rdn (aps,start,mapbuf)) 
			return OK;
		if (mkdir (aps->ps_base,0700) != 0) {
			LLOG (log_dsap,LLOG_EXCEPTIONS,("dn2file mkdir failure \"%s\" (%d)",aps->ps_base,errno));
			return NOTOK;
		}
		return OK;
	}

	return NOTOK;
}

static dn2filename (aps,dn,make)
PS aps;
DN dn;
char make;
{
	if (treedir != NULLCP) {
		ps_print (aps,isodefile(treedir,0));
		if (make) {
			*aps->ps_ptr = 0;
			if ((! fileexists (aps->ps_base)) &&
				(mkdir (aps->ps_base,0700) != 0)) {
				LLOG (log_dsap,LLOG_EXCEPTIONS,("dn2file mkdir failure \"%s\" (%d)",aps->ps_base,errno));
				return NOTOK;
				}
		}
		if (*(aps->ps_ptr - 1) != '/')
			ps_print (aps,"/");
	} else
		ps_print (aps,"./");

	if (dn != NULLDN) {
		if (rdn2filename (aps,dn->dn_rdn,make) == NOTOK)
			return NOTOK;
		if (dn->dn_parent != NULLDN) {
			DN eptr;
			for (eptr = dn->dn_parent; eptr != NULLDN; eptr = eptr->dn_parent) {
				ps_print (aps,"/"); 
				if (rdn2filename (aps,eptr->dn_rdn,make) == NOTOK)
					return NOTOK;
			}
		}
	}

	return OK;

}

char * dn2edbfile (dn)
DN dn;
{
PS aps;
static char result [LINESIZE];

	if ((aps = ps_alloc (str_open)) == NULLPS) {
		LLOG (log_dsap, LLOG_EXCEPTIONS, ("dn2dir ps_alloc failed"));
		return NULLCP;
	}
	if (str_setup (aps,result,LINESIZE,1) == NOTOK) {
		LLOG (log_dsap, LLOG_EXCEPTIONS, ("dn2dir ps_alloc failed"));
		return NULLCP;
	}

	if (dn2filename (aps,dn,TRUE) != OK)
		return NULLCP;

	if (*(aps->ps_ptr - 1) != '/')
		ps_print (aps,"/EDB");
	else
		ps_print (aps,"EDB");
	*aps->ps_ptr = 0;

	ps_free (aps);

	return result;
}

static file_check (offset,entryptr)
register int offset;
register Entry entryptr;
{
	ps->ps_ptr = filename + offset;
	ps->ps_cnt = LINESIZE - offset;

	if (rdn2filename (ps,entryptr->e_name,FALSE) == OK) {
		if (*(ps->ps_ptr - 1) != '/')
			ps_print (ps,"/EDB");
		else
			ps_print (ps,"EDB");
		*ps->ps_ptr = 0;
		return (OK);
	}
	return (NOTOK);
}

#ifdef TURBO_AVL

static char got_all = TRUE;

static load_a_kid(e, offset)
Entry   e;
int     offset;
{
        static int      entry_load_kids();

        if ((!e->e_external) && 
	    (e->e_master == NULLAV) && 
	    (e->e_slave == NULLAV)) {
                e->e_leaf = TRUE;
		e->e_allchildrenpresent = 2;
                return(OK);
        }

        if (file_check(offset, e) == OK) {
                if ((e->e_children = getentry_block(e, filename)) == NULLAVL) {
			if (e->e_allchildrenpresent != FALSE &&
			    e->e_leaf == FALSE)
				return(OK);
			else
				return(NOTOK) ;
		}
		if (parse_status != 0)
			return(NOTOK);

                e->e_leaf = FALSE;

                if (entry_load_kids(e->e_children, strlen( filename ) - EDBLEN)
		    == NOTOK)
                        return (NOTOK);
		if (e->e_allchildrenpresent != 2)
			got_all = FALSE;
        } else {
                LLOG (log_dsap, LLOG_TRACE, ("Sibling file %s/EDB NOT found", filename));
		e->e_allchildrenpresent = FALSE;
		got_all = FALSE;
	}

        return(OK);
}

#endif /* TURBO_AVL */

static entry_load_kids (entryptr,offset)
#ifdef TURBO_AVL
Avlnode	*entryptr;	/* in this case, entryptr is really a tree of kids */
#else
register Entry entryptr;
#endif
register int offset;
{
#ifdef TURBO_AVL
Entry	akid, parent;
#else
register Entry ptr;
char got_all = TRUE;
#endif

	ps->ps_ptr = filename + offset;
	ps->ps_cnt = LINESIZE - offset;
	*ps->ps_ptr = 0;

#ifdef TURBO_AVL

	if (entryptr == NULLAVL)
		return(OK);

	got_all = TRUE;

        if (avl_apply(entryptr, load_a_kid,  (caddr_t) offset, NOTOK, AVL_PREORDER)
            == NOTOK)
                return(NOTOK);

	akid = (Entry) avl_getone(entryptr);
	if (akid && (parent = akid->e_parent)) {
		if (got_all) {
			if (parent->e_allchildrenpresent == 1)
				parent->e_allchildrenpresent = 2;
		} else if (parent->e_allchildrenpresent == 2)
			parent->e_allchildrenpresent = 1;
	}
#else

	if (entryptr == NULLENTRY)
		return OK;

	for ( ptr = entryptr; ptr != NULLENTRY; ptr = ptr->e_sibling) {
		if ((!ptr->e_external) && 
		    (ptr->e_master == NULLAV) && 
		    (ptr->e_slave == NULLAV)) {
			ptr->e_leaf = TRUE;
			ptr->e_allchildrenpresent = 2;
			continue;
		}

		if (file_check (offset,ptr) == OK) {
			if ((ptr->e_child = getentry_block (ptr,filename)) == NULLENTRY)
			{
				if (ptr->e_allchildrenpresent != FALSE &&
				    ptr->e_leaf == FALSE)
					continue;
				else
					return (NOTOK) ;
			}
			if (parse_status != 0)
				return NOTOK;

			ptr->e_leaf = FALSE;
			if (entry_load_kids (ptr->e_child,strlen(filename) - EDBLEN) == NOTOK)
				return (NOTOK);
			if (ptr->e_allchildrenpresent != 2)
				got_all = FALSE;
		} else {
			LLOG (log_dsap,LLOG_TRACE,("WARNING, sibling file %s/EDB NOT found", filename));
			ptr->e_allchildrenpresent = FALSE;
			got_all = FALSE;
		}
	}

	if (entryptr->e_parent) {
		if (got_all) {
			if (entryptr->e_parent->e_allchildrenpresent == 1)
				entryptr->e_parent->e_allchildrenpresent = 2;
		} else if (entryptr->e_parent->e_allchildrenpresent == 2)
			entryptr->e_parent->e_allchildrenpresent = 1;
	}
#endif /* TURBO_AVL */

	return (OK);
}

#ifdef TURBO_AVL
static char got_subtree;

static check_entry_free (e)
Entry e;
{
	if (e->e_allchildrenpresent < 2)
		got_subtree = FALSE;
	entry_free(e);
}

parent_link(e, parent)
Entry   e;
Entry   parent;
{
struct DSError error;
int res;

        e->e_parent = parent;
	set_inheritance (e);
        return(OK);
}

static merge_entry(newentry, oldtree)
Entry   newentry;
Avlnode *oldtree;
{
        Entry   p;
        int     entry_cmp();

        newentry->e_parent = ((Entry) avl_getone(oldtree))->e_parent;

        if ((p = (Entry) avl_find(oldtree, (caddr_t) newentry, entry_cmp))
            != NULLENTRY ) {
                newentry->e_leaf = FALSE;
                newentry->e_allchildrenpresent = p->e_allchildrenpresent;
                newentry->e_children = p->e_children;

                (void) avl_apply(newentry->e_children, parent_link, (caddr_t) newentry,
		    NOTOK, AVL_PREORDER);

                if (p->e_edbversion != NULLCP)
                        newentry->e_edbversion = strdup(p->e_edbversion);
        } else {
		got_subtree = FALSE;	
		newentry->e_allchildrenpresent = FALSE;
	}

        return(OK);
}
#endif /* TURBO_AVL */

Entry subtree_load (parent,dn)
Entry parent;
DN dn;
{
char failed = FALSE;
#ifdef TURBO_AVL
Avlnode	*treetop;
Entry	akid;
int	entry_free();
#else
Entry temp, old_entry;
char got_subtree = TRUE;
Entry	treetop, sibl, find_sibling();
#endif

#ifdef TURBO_AVL

	got_subtree = TRUE;

	if ((parent != NULLENTRY) && (parent->e_children != NULLAVL)) {
		akid = (Entry) avl_getone(parent->e_children);
		if (akid->e_data != E_TYPE_CONSTRUCTOR)
			return (parent);
	}
#else
	if ((parent != NULLENTRY) && (parent->e_child != NULLENTRY))
		if (parent->e_child->e_data != E_TYPE_CONSTRUCTOR)
			return (parent->e_child);
#endif

	if ((ps = ps_alloc (str_open)) == NULLPS) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("file open ps_alloc failed"));
		return (NULLENTRY);
	}
	if (str_setup (ps,filename,LINESIZE,1) == NOTOK) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("file open ps_alloc failed"));
		return (NULLENTRY);
	}

	(void) dn2filename (ps,dn,FALSE);
	if (*(ps->ps_ptr - 1) != '/')
		ps_print (ps,"/EDB");
	else
		ps_print (ps,"EDB");
	*ps->ps_ptr = 0;

#ifdef TURBO_AVL
	if (parent != NULLENTRY && parent->e_children != NULLAVL) {
#else
	if ((parent != NULLENTRY) && (parent->e_child != NULLENTRY)) {
#endif
		/* yuk - already got an edb lower in the DIT ... */
		treetop = getentry_block (parent,filename);
		if (parse_status != 0)
			return NULLENTRY;

#ifdef TURBO_AVL
		/*
		 * go through the tree we just loaded, merging it with the
		 * tree previously loaded.
		 */

		(void) avl_apply(treetop, merge_entry, (caddr_t) parent->e_children,
		    NOTOK, AVL_PREORDER);

		if (got_subtree && (parent->e_allchildrenpresent == 1))
			parent->e_allchildrenpresent = 2;

		got_subtree = TRUE;

		/* free the old tree and set got_subtree */
		(void) avl_free(parent->e_children, check_entry_free);

		if (got_subtree && (parent->e_allchildrenpresent == 1))
			parent->e_allchildrenpresent = 2;

		parent->e_children = treetop;

#else
		for (temp = treetop; temp != NULLENTRY; temp=temp->e_sibling) {
			temp->e_parent = parent;

			if ((old_entry = find_sibling (temp->e_name,parent->e_child)) != NULLENTRY) {
				temp->e_leaf = FALSE;
				if ((temp->e_allchildrenpresent = old_entry->e_allchildrenpresent) < 2 )
					got_subtree = FALSE;
				temp->e_child = old_entry->e_child;
				for (sibl = temp->e_child; sibl != NULLENTRY; sibl=sibl->e_sibling) {
					sibl->e_parent = temp;
					set_inheritance (sibl);
				}
				if (old_entry->e_edbversion != NULLCP)
					temp->e_edbversion = strdup (old_entry->e_edbversion);
			} else if ( ! temp->e_leaf) {
				got_subtree = FALSE;
				temp->e_allchildrenpresent = FALSE;
			}

		}
		if (got_subtree && (treetop->e_allchildrenpresent == 1))
			treetop->e_allchildrenpresent = 2;

		got_subtree = TRUE;

		for (temp = parent->e_child; temp != NULLENTRY; temp=old_entry) {
			old_entry = temp->e_sibling;
			if (temp->e_allchildrenpresent < 2)
				got_subtree = FALSE;
			entry_free (temp);
		}
		if (got_subtree && (parent->e_allchildrenpresent == 1))
			parent->e_allchildrenpresent = 2;

		/* do we need to ripple e_allchildrenpresent higher ? */
#endif

		ps_free (ps);
#ifdef TURBO_AVL
		parent->e_children = treetop;
		return(parent);
#else
		parent->e_child = treetop;
		return (treetop);
#endif
	}

	if (dn == NULLDN) {
		parent = get_default_entry (NULLENTRY);
		parent->e_leaf = FALSE;
		parent->e_acl = acl_alloc();
		parent->e_acl->ac_child = acl_dflt ();
		parent->e_acl->ac_entry = acl_dflt ();
		parent->e_acl->ac_default = acl_dflt ();
#ifdef TURBO_AVL
		if ((treetop = getentry_block (parent,filename)) == NULLAVL) 
#else
		if ((treetop = getentry_block (parent,filename)) == NULLENTRY) 
#endif
			return (NULLENTRY);
	} else 
		treetop = getentry_block (parent,filename);

	if (parse_status != 0)
		failed = TRUE;

#ifdef TURBO_AVL
	parent->e_children = treetop;
#else
	parent->e_child = treetop;
#endif

	if (entry_load_kids (treetop,strlen (filename) - EDBLEN) == NOTOK) {
		parse_status++;
		return (NULLENTRY);
	}

	ps_free (ps);

	if (failed) {
		parse_status++;
		return (NULLENTRY);
	}

#ifdef TURBO_AVL
	return(parent);
#else
	if (dn == NULLDN)
		return (parent); /* be wary of this when calling subtree load... */
				 /* if DN == NULL - you may want the child !!! */
	else
		return (treetop);
#endif
}

int	refreshing;

refresh_from_disk(dn)
DN	dn;
{
Entry child;
Entry parent;
Entry tmp;
extern Entry database_root;

	if ((parent = local_find_entry (dn,FALSE)) == NULLENTRY)
		return (NOTOK);

	if (parent->e_data != E_DATA_MASTER)
		LLOG (log_dsap,LLOG_EXCEPTIONS, ("WARNING: refreshing SLAVE EDB file -- should not be needed"));

#ifdef TURBO_AVL
	child = (Entry) entry_cpy(parent);
	child->e_parent = parent->e_parent;
	child->e_children = parent->e_children;
	parent->e_children = NULLAVL;
#else
	child = parent->e_child;
	parent->e_child = NULLENTRY;
#endif

	refreshing = TRUE;
	if (dn == NULLDN)
		tmp = subtree_load (NULLENTRY,NULLDN);
	else
		tmp = subtree_load (parent,dn);
	refreshing = FALSE;
	if (tmp == NULLENTRY)
		return (NOTOK);

	if (dn == NULLDN) {
		database_root = child;
		directory_free (child);
		database_root = tmp;
	} else
		directory_free(child);

	return (OK);
}
