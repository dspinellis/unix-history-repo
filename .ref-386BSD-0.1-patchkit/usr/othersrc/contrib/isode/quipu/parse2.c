/* parse2.c - */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/parse2.c,v 7.5 91/02/22 09:39:38 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/quipu/RCS/parse2.c,v 7.5 91/02/22 09:39:38 mrose Interim $
 *
 *
 * $Log:	parse2.c,v $
 * Revision 7.5  91/02/22  09:39:38  mrose
 * Interim 6.8
 * 
 * Revision 7.4  90/10/17  11:54:38  mrose
 * sync
 * 
 * Revision 7.3  90/07/09  14:46:27  mrose
 * sync
 * 
 * Revision 7.2  90/01/11  18:37:23  mrose
 * real-sync
 * 
 * Revision 7.1  89/12/19  16:20:42  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:17:57  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */

#include "quipu/util.h"
#include "quipu/entry.h"
#include "quipu/config.h"
#include "cmd_srch.h"
#include "quipu/malloc.h"
#ifdef TURBO_DISK
#include <gdbm.h>
#endif

#ifdef TURBO_AVL
Avlnode *getentry_block();
#else
Entry getentry_block();
#endif
Entry get_entry_aux();
extern LLog * log_dsap;
char * getline ();
static test_duplicate ();
int rdn_print ();
int master_edbs = 0;
int slave_edbs = 0;

#ifdef TURBO_DISK
extern datum	turbo_header_key;
#endif

#ifdef TURBO_AVL
Avlnode *getentry_block (p_parent,fname)
#else
Entry getentry_block (p_parent,fname)
#endif
Entry p_parent;
char * fname;
{
extern char * parse_file;
extern int parse_status;
extern int parse_line;
int   dtype;
char *version;
#ifdef TURBO_AVL
Avlnode	*tree;
Avlnode	*get_entries_aux();
#else
Entry entryptr;
Entry get_entries_aux ();
#endif
extern int errno;

#ifdef TURBO_DISK
GDBM_FILE	file;
char		gfname[1024];
int		save_heap;
#else
FILE 		*file;
#endif

#ifdef TURBO_DISK
	strcpy(gfname, fname);
	strcat(gfname, ".gdbm");
	save_heap = mem_heap;
	GENERAL_HEAP;
	file = gdbm_open(gfname, 0, GDBM_READER, 0, 0);
	mem_heap = save_heap;
	fname = gfname;
#else
	file = fopen (fname, "r");
#endif

	if (file == NULL) {
		extern int	refreshing;

		LLOG (log_dsap,LLOG_NOTICE,("WARNING - Can't open \"%s\" (%d)- should I be able to ?",fname,errno));
		if (refreshing)
			parse_error("Can't open \"%s\" (%d)", fname, errno);
#ifdef TURBO_AVL
		return(NULLAVL);
#else
		return (NULLENTRY);
#endif
	}

	LLOG (log_dsap,LLOG_NOTICE,("Loading \"%s\"",fname));

	parse_status = 0;
	parse_line   = 0;
	parse_file = fname;

	if (get_header (file,&dtype,&version) != OK) {
		parse_line = 0;
		parse_error ("Bad HEADER - File %s not loaded",fname);
		parse_file = NULLCP;
#ifdef TURBO_DISK
		save_heap = mem_heap;
		GENERAL_HEAP;
		(void) gdbm_close (file);
		mem_heap = save_heap;
		return(NULLAVL);
#else
		(void) fclose (file);
#ifdef TURBO_AVL
		return (NULLAVL);
#else
		return (NULLENTRY);
#endif
#endif
	}

#ifdef TURBO_AVL
	tree = get_entries_aux (file,p_parent,version,dtype);
#else
	entryptr = get_entries_aux (file,p_parent,version,dtype);
#endif

#ifdef TURBO_AVL
	if ((parse_status == 0) && (tree == NULLAVL)) {
#else
	if ((parse_status == 0) && (entryptr == NULLENTRY)) {
#endif
		LLOG(log_dsap, LLOG_NOTICE,("Header OK, but null EDB File %s.",fname));
		p_parent->e_leaf = FALSE ;
		p_parent->e_allchildrenpresent = 2 ;
                master_edbs++;
#ifdef TURBO_DISK
		p_parent->e_children = NULLAVL ;
		save_heap = mem_heap;
		GENERAL_HEAP;
		(void) gdbm_close (file);
		mem_heap = save_heap;
		return(NULLAVL);
#else
#ifdef TURBO_AVL
		p_parent->e_children = NULLAVL;
		(void) fclose (file);
		return(NULLAVL) ;
#else
		p_parent->e_child = NULLENTRY ;
		(void) fclose (file);
		return(NULLENTRY) ;
#endif
#endif
	}

#ifdef TURBO_AVL
	if ((parse_status != 0) || (tree == NULLAVL)) {
#else
	if ((parse_status != 0) || (entryptr == NULLENTRY)) {
#endif
		parse_line = 0;
		parse_error ("File %s not loaded",fname);
		parse_file = NULLCP;
		p_parent->e_allchildrenpresent = FALSE;
#ifdef TURBO_DISK
		save_heap = mem_heap;
		GENERAL_HEAP;
		(void) gdbm_close (file);
		mem_heap = save_heap;
		return(NULLAVL);
#else
		(void) fclose (file);
#ifdef TURBO_AVL
		return(NULLAVL);
#else
		return (NULLENTRY);
#endif
#endif
	}

        if ( p_parent != NULLENTRY ) {
       	        p_parent->e_edbversion = version;
               	if ((dtype == E_DATA_MASTER) || (dtype == E_TYPE_SLAVE)) 
                       	p_parent->e_allchildrenpresent = 1; 	/* at least */
       	}

	parse_file = NULLCP;

	if (dtype == E_DATA_MASTER)
		master_edbs++;
	if (dtype == E_TYPE_SLAVE)
		slave_edbs++;

#ifdef TURBO_DISK
	save_heap = mem_heap;
	GENERAL_HEAP;
	(void) gdbm_close (file);
	mem_heap = save_heap;
	return(tree);
#else
	(void) fclose (file);
#ifdef TURBO_AVL
	return (tree);
#else
	return (entryptr);
#endif
#endif
}

#ifdef TURBO_DISK

get_header (db, typeptr, versionptr)
GDBM_FILE	db;
int		*typeptr;
char		**versionptr;
{
	char		*v, *p;
	datum		h;
	int		save_heap;
	static CMD_TABLE cmd_header[] =  {
		"MASTER",	E_DATA_MASTER,
		"SLAVE",	E_TYPE_SLAVE,
		"CACHE",	E_TYPE_CACHE_FROM_MASTER,
		0,		-1,
	};
	extern char	*parse_entry;

	save_heap = mem_heap;
	GENERAL_HEAP;

	parse_entry = turbo_header_key.dptr;
	if (db == NULL) {
		parse_error("NULL dbm file!!!", NULLCP);
		mem_heap = save_heap;
		return(NOTOK);
	}

	h = gdbm_fetch(db, turbo_header_key);
	if (h.dptr == NULL) {
		parse_error("File has no header!!!", NULLCP);
		mem_heap = save_heap;
		return(NOTOK);
	}

	v = index(h.dptr, '\n');
	if (v == NULLCP) {
		parse_error("Bad file header", NULLCP);
		mem_heap = save_heap;
		return(NOTOK);
	}
	*v++ = '\0';

	if ((*typeptr = cmd_srch(h.dptr, cmd_header)) == -1) {
		parse_error("File type %s not recognised", h.dptr);
		mem_heap = save_heap;
		return(NOTOK);
	}

	if (*v == '\0') {
		parse_error("No version specified", NULLCP);
		mem_heap = save_heap;
		return(NOTOK);
	}
	if ((p = index(v, '\n')) != NULLCP)
		*p = '\0';
	*versionptr = strdup(v);
	free(h.dptr);

	mem_heap = save_heap;
	return(OK);
}

#else

get_header (file,typeptr,versionptr)
FILE * file;
int * typeptr;
char ** versionptr;
{
char * ptr;
static CMD_TABLE cmd_header [] = {
	"MASTER",	E_DATA_MASTER,
	"SLAVE",	E_TYPE_SLAVE,
	"CACHE",	E_TYPE_CACHE_FROM_MASTER,
	0,		-1,
	};

	if ((ptr = getline (file)) == NULLCP) {
		parse_error ("NULL file !!!",NULLCP);
		return (NOTOK);
	}

	if ((*typeptr = cmd_srch (ptr,cmd_header)) == -1) {
		parse_error ("File type %s not recognised",ptr);
		return (NOTOK);
	}

	if ((ptr = getline (file)) == NULLCP) {
		parse_error ("No version specified",NULLCP);
		return (NOTOK);
	}
	*versionptr = strdup (ptr);
	
	return (OK);
}

#endif

/* ARGSUSED */
#ifdef TURBO_AVL
Avlnode *get_entries_aux (file,parent,version,dtype)
#else
Entry get_entries_aux (file,parent,version,dtype)
#endif
#ifdef TURBO_DISK
GDBM_FILE	file;
#else
FILE * file;
#endif
Entry parent;
char * version;
int dtype;
{
register Entry eptr = NULLENTRY;
#ifdef TURBO_AVL
Avlnode	*tree = NULLAVL;
int	entry_cmp();
#else
register Entry top = NULLENTRY;
#endif
#ifndef TURBO_INDEX
register Entry trail;
#endif
Entry find_sibling();
#ifdef TURBO_DISK
extern int dbmeof;
#endif

#ifdef TURBO_DISK
	dbmeof = 0;
	while (dbmeof == 0) {
#else
	while (feof(file) == 0) {
#endif
		if ((eptr = get_entry_aux (file,parent,dtype)) == NULLENTRY)
			continue;
#ifdef TURBO_AVL
		DATABASE_HEAP;

		if (avl_insert(&tree, (caddr_t) eptr, entry_cmp, avl_dup_error)
		    == NOTOK) {
                        pslog (log_dsap,LLOG_EXCEPTIONS,"Duplicate entry for",
                            rdn_print,(caddr_t)eptr->e_name);
                        parse_error ("Non Unique RDN",NULLCP);
		}

#ifdef TURBO_INDEX
		turbo_add2index(eptr);
#endif
#else
		if ( top == NULLENTRY) {
			top = eptr;
			trail = eptr;
		} else {
			trail->e_sibling = eptr;
			trail = eptr;
		}
#endif
	}

#ifdef TURBO_AVL
	return(tree);
#else
	test_duplicate(top);

	return (top);
#endif
}


#ifdef TURBO_AVL
Avlnode *get_entries (file,parent,version,dtype)
#else
Entry get_entries (file,parent,version,dtype)
#endif
#ifdef TURBO_DISK
GDBM_FILE	file;
#else
FILE * file;
#endif
Entry parent;
char * version;
int dtype;
{
extern int parse_status;
extern int parse_line;

	parse_status = 0;
	parse_line   = 0;

	return (get_entries_aux (file,parent,version,dtype));
}

#ifndef TURBO_AVL
Entry find_sibling (object,start)
RDN object;
Entry start;
{
	if (start == NULLENTRY) 
		return (NULLENTRY);

	while (rdn_cmp (start->e_name, object) != OK) {
		start = start->e_sibling ;
		if ( start == NULLENTRY ) 
			return (NULLENTRY);
	}
	return (start);
}

static test_duplicate (top)
register Entry top;
{
register Entry ptr;

	for (; top != NULLENTRY; top = top->e_sibling)
		for (ptr = top->e_sibling; ptr != NULLENTRY; ptr = ptr->e_sibling)
			if (rdn_cmp (ptr->e_name, top->e_name) == OK)
			{
				pslog (log_dsap,LLOG_EXCEPTIONS,"Duplicate entry for",rdn_print,(caddr_t)top->e_name);
				parse_error ("Non Unique RDN",NULLCP);
			}

}
#endif /* TURBO_AVL */
