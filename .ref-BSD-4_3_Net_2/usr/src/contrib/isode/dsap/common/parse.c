/* parse.c - */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/parse.c,v 7.3 91/02/22 09:19:51 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/common/RCS/parse.c,v 7.3 91/02/22 09:19:51 mrose Interim $
 *
 *
 * $Log:	parse.c,v $
 * Revision 7.3  91/02/22  09:19:51  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/17  11:42:27  mrose
 * sync
 * 
 * Revision 7.1  90/07/09  14:34:54  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:17:56  mrose
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


#include "general.h"
#include "manifest.h"
#include "tailor.h"
#include "quipu/config.h"
#include "quipu/util.h"
#include "quipu/entry.h"
#include "quipu/ds_error.h"
#include "quipu/malloc.h"
#ifdef TURBO_DISK
#include <gdbm.h>
#endif

#define	CONT_CHAR	'\\'	/* Continueation character */


Entry database_root = NULLENTRY;
int local_master_size = 0;
int local_slave_size = 0;
int local_cache_size = 0;

extern LLog * log_dsap;
extern int dn_print ();

#ifdef TURBO_DISK
extern char	*parse_entry;
int		dbmeof;
int		dbmfirst = 1;
datum		turbo_header_key = { "HEADER", sizeof("HEADER") };
GDBM_FILE	save_db;
#endif

extern  time_t time ();
extern char	*unesc_char();
extern char	*unesc_cont();
extern char *getstring();
char *srealloc();
char	*brkl();

#ifdef TURBO_DISK

char *getline (db)
GDBM_FILE	db;
{
	static datum	newkey, key, dat;
	static char	*line, *next, *save;
	static int	new_entry;
	char		*TidyString();
	int		save_heap;

	save_db = db;

	save_heap = mem_heap;
	GENERAL_HEAP;

	if (dbmfirst) {
		dbmfirst = 0;
		key = gdbm_firstkey(db);
		parse_entry = key.dptr;
		new_entry = 1;
	} else if (line == NULLCP || *line == '\0') {
		newkey = gdbm_nextkey(db, key);
		free(key.dptr);
		key = newkey;
		parse_entry = key.dptr;
		new_entry = 1;
	}

	if (key.dptr == NULLCP) {
		dbmeof = 1;
		dbmfirst = 1;
		mem_heap = save_heap;
		return(NULLCP);
	}

	if (new_entry) {
		/* gross, but we have to skip the header datum */
		if (strcmp(key.dptr, turbo_header_key.dptr) == 0) {
			newkey = gdbm_nextkey(db, key);
			free(key.dptr);
			key = newkey;
			if (key.dptr == NULLCP) {
				parse_entry = key.dptr;
				dbmeof = 1;
				dbmfirst = 1;
				mem_heap = save_heap;
				return(NULLCP);
			}
		}

		new_entry = 0;
		parse_entry = key.dptr;
		dat = gdbm_fetch(db, key);

		if ((line = dat.dptr) == NULLCP) {
			parse_error("edb error - null dbm data");
			return(NULLCP);
		}
	}

	if (*line == '\n') {
		line = NULLCP;
		free(dat.dptr);
		mem_heap = save_heap;
		return("");
	}

	line = SkipSpace(line);
	while (*line == '#') {
		line = index(line, '\n') + 1;
		if (*line == '\0') {
			mem_heap = save_heap;
			return(NULLCP);
		}
		line = SkipSpace(line);
	}

	next = line;
	while (next = index(next, '\n')) {
		if (*(next - 1) == CONT_CHAR) {	/* XXX do this right XXX */
			*(next - 1) = ' ';
			*next = ' ';
		} else
			break;
	}
	if (next == NULL)
		return(NULLCP);
	*next++ = '\0';
	save = line;
	line = next;

	mem_heap = save_heap;
	return(TidyString(save));
}

char *getnextline ()
{
	return getline (save_db);
}


#endif /* TURBO_DISK */

#define PARSE_INCR 512
#define MAXLINE    130

/* Testing
#define PARSE_INCR 5
#define MAXLINE    3
*/
    static char *parse_buffer = NULLCP;
    static int	parse_len = 0;
    static char	*buf;
    static int	buflen;
    static int	curlen;
    static int	size;

/*
 * get a physical line - handle arbitary long physical line
 */
static char *
getphyline (file)
FILE * file;
{
    extern int parse_line;
    extern char * TidyString ();


    buf = parse_buffer;
    buflen = parse_len;
    curlen = 0;

    for (;;) {
	if ( buflen <= MAXLINE) {
	    if (parse_len <= 0) {
		buflen = parse_len = PARSE_INCR;
		buf = parse_buffer = smalloc(PARSE_INCR);
	    } else {
		parse_len += PARSE_INCR;
		buflen += PARSE_INCR;
		if ((parse_buffer = realloc(parse_buffer, (unsigned)parse_len)) == NULLCP)
		    exit (2);	/* ??? */
		buf = parse_buffer + curlen;
	    }
	}

	if (fgets (buf, buflen,file) == NULLCP)
	    return (NULLCP);

	size = strlen(buf);
	if (buf[size - 1] == '\n') {
	    buf[--size] = '\0';
	    break;
	}
	buf += size;
	buflen -= size;
	curlen += size;

    }

    parse_line++;
    return (parse_buffer);

}

/*
 * Append the next physical line to the current physical line
 * Assuming that only characters from buf onwards have been
 * altered. We recalculate the end of buffer from buf
 */
static char *
catphyline (file, str)
FILE * file;
char	*str;
{
    extern int parse_line;
    extern char * TidyString ();


    if (str && str > parse_buffer) {
	/* size will be negative if a long line has been trimmed back
	 * beyond the last buffer length
	 */
	size = str - buf;	/* saves us calling strlen here */
	buf += size;
	buflen -= size;
	curlen += size;
    } else {
	buf = parse_buffer;
	buflen = parse_len;
	curlen = 0;
    }

    for (;;) {
	if ( buflen <= MAXLINE) {
	    if (parse_len <= 0) {
		buflen = parse_len = PARSE_INCR;
		buf = parse_buffer = smalloc(PARSE_INCR);
	    } else {
		parse_len += PARSE_INCR;
		buflen += PARSE_INCR;
		if ((parse_buffer = realloc(parse_buffer, (unsigned)parse_len)) == NULLCP)
		    exit (2);	/* ??? */
		buf = parse_buffer + curlen;
	    }
	}

	if (fgets (buf, buflen,file) == NULLCP)
	    return (NULLCP);

	size = strlen(buf);
	if (buf[size - 1] == '\n') {
	    buf[--size] = '\0';
	    break;
	}
	buf += size;
	buflen -= size;
	curlen += size;

    }

    parse_line++;
    return (parse_buffer);

}

extern char	*unesc_cont();
FILE * savefile;

#ifdef TURBO_DISK
char * fgetline (file)
#else
char * getline (file)
#endif
FILE * file;
{
    extern char * TidyString ();

    int		len;
    int		ocurlen;
    char	*p;
    char	*npart;

    savefile = file;

    do {
	if ((p = getphyline(file)) == NULLCP)
	    return (NULLCP);

    } while (*SkipSpace (buf) == '#');

    len = curlen + size;

    npart = p;
    while ((npart = unesc_cont(npart, len))) {/* continued line keep going */
	ocurlen = npart - p;
	if ((p = catphyline(file, npart)) == NULLCP)
	    return (NULLCP);
	len = curlen + size - ocurlen;
	npart = p + ocurlen;
    }

    return (TidyString(p));

}

#ifdef TURBO_DISK
char * fgetnextline ()
#else
char * getnextline ()
#endif
{
	return getline (savefile);
}

/*
 * un-escape a continued line and return pointer to end of the buffer
 * if the line is continued
 */
char	*
unesc_cont(ptr, len)
char	*ptr;
int	len;
{
    char	*p;
    int		cnt;

    for (cnt = 0, p = ptr + len - 1; p >= ptr; p--) {
	if (*p != CONT_CHAR)
	    break;
       cnt++;
    }
    if (cnt) {
	p += (cnt / 2) + 1;
	*p = '\0';
    }

    return (cnt % 2 ? p : NULLCP);

}


/*
 * write no more than wl characters of the line out escaping any
 * characters at the end to a file pointer.
 */
fpwr_esc(fp, line, wl)
FILE	*fp;
char	*line;
int	wl;
{
    int		len;	/* length of line left */
    int		pos;	/* position we are going to break line at */
    int		m;	/* number of CONT_CHARs at end of string */
    int		n;	/* number of CONT_CHARs we can put on this line */
    int		i;

    len = strlen(line);
    while (len > 0) {
	if (len < wl)
	    pos = len;
	else
	    pos = wl;
	
	m = cnt_escp(line, pos);

	if (m > 0) {
	    if (pos + 2*m <= wl) {	/* can fit them in the line */
		n = m;	/* need an extra escape for each one */
	    } else  {
		n = (wl - 1 - pos + m)/2;
		pos -= m - n;
		n++;	/* Need an extra one to make the line continue */
	    }
	} else
	    n = 0;
	for (i = pos; i > 0; i--, line++)
	    (void) putc(*line, fp);
	for (i = n; i > 0; i--)
	    (void) putc(CONT_CHAR, fp);
	(void) putc('\n', fp);
	len -= pos;
    }
}
/*
 * write no more than wl characters of the line out escaping any
 * characters at the end.
 */
pswr_esc(ps, line, wl)
PS	ps;
char	*line;
int	wl;
{
    int		len;	/* length of line left */
    int		pos;	/* position we are going to break line at */
    int		m;	/* number of CONT_CHARs at end of string */
    int		n;	/* number of CONT_CHARs we can put on this line */
    int		i;

    len = strlen(line);
    while (len > 0) {
	if (len < wl)
	    pos = len;
	else
	    pos = wl;
	
	m = cnt_escp(line, pos);

	if (m > 0) {
	    if (pos + 2*m <= wl) {	/* can fit them in the line */
		n = m;	/* need an extra escape for each one */
	    } else  {
		n = (wl - 1 - pos + m)/2;
		pos -= m - n;
		n++;	/* Need an extra one to make the line continue */
	    }
	} else
	    n = 0;
	(void) ps_write(ps, (PElementData)line, pos);
	if (n > 0) {
	    char nbuf[MAXLINE];

	    /* unlikely this will ever be run but just in case */
	    while (n + 1 > MAXLINE) {
		for (i = MAXLINE - 2; i >= 0; i--)
		    nbuf[i] = CONT_CHAR;
		nbuf[MAXLINE - 1] = '\n';
		(void) ps_write(ps, (PElementData)line, pos);
		n -= MAXLINE;
	    }
	    for (i = n - 1; i >= 0; i--)
		nbuf[i] = CONT_CHAR;
	    nbuf[n] = '\n';
	    (void) ps_write(ps, (PElementData)line, pos);
	}
	len -= pos;
    }
}

cnt_escp(ptr, len)
register char	*ptr;
int	len;
{
    register char	*p;
    register int		cnt;

    for (cnt = 0, p = ptr + len - 1; p >= ptr; p--) {
	if (*p != CONT_CHAR)
	    break;
       cnt++;
    }
    return (cnt);
}

char *
srealloc(p, nsize)
char	*p;
int     nsize;
{
        register char *ptr;

        if ((ptr = realloc(p, (unsigned) nsize)) == NULL){
            LLOG (compat_log,LLOG_FATAL, ("realloc() failure"));
            abort ();
            /* NOTREACHED */
        }

        return(ptr);
}

#ifdef TURBO_DISK

Attr_Sequence fget_attributes_aux (file)
FILE * file;
{
register Attr_Sequence as = NULLATTR;
Attr_Sequence as_combine ();
register char * ptr;

	if ((ptr = fgetline (file)) == NULLCP)
		return (NULLATTR);

	while ( *ptr != 0 ) {
		as = as_combine (as,ptr,FALSE);
		if ((ptr = fgetline (file)) == NULLCP)
			break;
	}
	return (as);
}

Attr_Sequence fget_attributes (file)
FILE * file;
{
extern int parse_status;
extern int parse_line;

	parse_status = 0;
	parse_line   = 0;

	return (fget_attributes_aux (file));
}

#endif

Attr_Sequence get_attributes_aux (file)
#ifdef TURBO_DISK
GDBM_FILE	file;
#else
FILE * file;
#endif
{
register Attr_Sequence as = NULLATTR;
Attr_Sequence as_combine ();
register char * ptr;

	if ((ptr = getline (file)) == NULLCP)
		return (NULLATTR);

	while ( *ptr != 0 ) {
		as = as_combine (as,ptr,FALSE);
		if ((ptr = getline (file)) == NULLCP)
			break;
	}
	return (as);
}

Attr_Sequence get_attributes (file)
#ifdef TURBO_DISK
GDBM_FILE	file;
#else
FILE * file;
#endif
{
extern int parse_status;
extern int parse_line;

	parse_status = 0;
	parse_line   = 0;

	return (get_attributes_aux (file));
}


Entry get_entry_aux (file,parent,dtype)
#ifdef TURBO_DISK
GDBM_FILE	file;
#else
FILE * file;
#endif
Entry parent;
int dtype;
{
Entry eptr;
char * ptr;
extern RDN parse_rdn;
struct DSError err;
extern int print_parse_errors;
extern int parse_line;
int save; 
extern PS opt;
char check = TRUE;

	DATABASE_HEAP;

	eptr = get_default_entry (parent);
	eptr->e_data = dtype;

	if ((ptr = getline (file)) == NULLCP) {
		GENERAL_HEAP;
		return (NULLENTRY);
	}
		
	while (*ptr == 0)
		if ((ptr = getline (file)) == NULLCP) {
			GENERAL_HEAP;
			return (NULLENTRY);
		}

	if ((eptr->e_name = str2rdn (ptr)) == NULLRDN) {
		parse_error ("invalid rdn %s",ptr);
		check = FALSE;
	}
	
	parse_rdn = eptr->e_name;
	eptr->e_attributes = get_attributes_aux (file);

	if (check) {
		save = parse_line;
		parse_line = 0;
		if (unravel_attribute (eptr,&err) != OK) {
			parse_error ("Error in entry ending line %d...",(char *) save);
			if (print_parse_errors)
				ds_error (opt,&err);
		} 
		if (check_schema (eptr,NULLATTR,&err) != OK) {
			parse_error ("Schema error in entry ending line %d...",(char *) save);
			if (print_parse_errors)
				ds_error (opt,&err);
		} 
		parse_line = save;
	}
	parse_rdn = NULLRDN;

	GENERAL_HEAP;

	switch (dtype) {
	case E_TYPE_SLAVE:
		local_slave_size++; break;
	case E_DATA_MASTER:
		local_master_size++; break;
	case E_TYPE_CACHE_FROM_MASTER:
		eptr->e_age = time ((time_t *)0);
		local_cache_size++; break;
	}

	return (eptr);
}


Entry get_entry (file,parent,dtype)
#ifdef TURBO_DISK
GDBM_FILE	file;
#else
FILE * file;
#endif
Entry parent;
int dtype;
{
extern int parse_status;
extern int parse_line;

	parse_status = 0;
	parse_line   = 0;

	return (get_entry_aux (file,parent,dtype));
}


Entry new_constructor (parent)
Entry parent;
{
Entry constructor;

	constructor = get_default_entry (parent);
#ifdef TURBO_AVL
	constructor->e_children = NULLAVL;
#endif
	constructor->e_leaf = FALSE;
	constructor->e_allchildrenpresent = FALSE;
	constructor->e_complete = FALSE;
	constructor->e_data = E_TYPE_CONSTRUCTOR;
	constructor->e_acl = acl_alloc ();
	constructor->e_acl->ac_child = acl_dflt ();
	constructor->e_acl->ac_entry = acl_dflt ();
	constructor->e_acl->ac_default = acl_dflt ();
	constructor->e_acl->ac_attributes = NULLACL_ATTR;
	return (constructor);
}


Entry make_path (dn)
DN dn;
{
Entry ptr;
register RDN    b_rdn;
#ifdef TURBO_AVL
Entry	parent, new;
Avlnode	*kids;
int	entryrdn_cmp(), entry_cmp();
#else
register RDN	a_rdn;
#endif

#ifdef TURBO_AVL
	if (database_root == NULLENTRY || database_root->e_children == NULLAVL) {
#else
	if ((database_root == NULLENTRY) || (database_root->e_child == NULLENTRY)) {
#endif
		database_root = new_constructor(NULLENTRY);
		ptr = database_root;
		for (; dn!= NULLDN; dn=dn->dn_parent) {
#ifdef TURBO_AVL
			new = new_constructor(ptr);
			new->e_name = rdn_cpy(dn->dn_rdn);
			(void) avl_insert(&ptr->e_children, (caddr_t) new, entry_cmp, avl_dup_error);
			ptr = (Entry) avl_getone(ptr->e_children);
#else
			ptr->e_child = new_constructor(ptr);
			ptr = ptr->e_child;
			ptr->e_name = rdn_cpy (dn->dn_rdn);
#endif /* TURBO_AVL */
		}
		return (ptr);
	} else {
		/* follow links as far as poss, then add new bits */

		if (dn == NULLDN)
			return (database_root);

#ifdef TURBO_AVL
		kids = database_root->e_children;
		parent = database_root;
#else
		ptr = database_root->e_child;
		a_rdn = ptr->e_name ;
#endif
		b_rdn = dn->dn_rdn;

		for(;;) { /* return out */
#ifdef TURBO_AVL
			if ((ptr = (Entry) avl_find(kids, (caddr_t) b_rdn, entryrdn_cmp))
			    == NULLENTRY ) {
				for (; dn != NULLDN; dn = dn->dn_parent) {
					new = new_constructor(parent);
					new->e_name = rdn_cpy(dn->dn_rdn);
					(void) avl_insert(&parent->e_children, (caddr_t) new,
					    entry_cmp, avl_dup_error);
					parent = (Entry) avl_find(parent->e_children, (caddr_t) dn->dn_rdn, entryrdn_cmp);
				}
				return(parent);
			}
#else
			while (rdn_cmp (a_rdn, b_rdn) != OK) {
				if (ptr->e_sibling == NULLENTRY) {
				   if ((ptr->e_data == E_DATA_MASTER) || 
					(ptr->e_data == E_TYPE_SLAVE)) {
					pslog(log_dsap, LLOG_EXCEPTIONS,
						"WARNING: Not loading entry that does not exist (any more)",
						dn_print, (caddr_t) dn);
					return NULLENTRY;
					}
				   ptr->e_sibling = new_constructor(ptr->e_parent);
				   ptr = ptr->e_sibling;
				   ptr->e_name = rdn_cpy (dn->dn_rdn);
				   for (dn=dn->dn_parent; dn!= NULLDN; dn=dn->dn_parent) {
					ptr->e_child = new_constructor(ptr);
					ptr = ptr->e_child;
					ptr->e_name = rdn_cpy (dn->dn_rdn);
				   }
				   return (ptr);
				}
				ptr = ptr->e_sibling;
				a_rdn = ptr->e_name ;
			}
#endif /* TURBO_AVL */

			if (dn->dn_parent == NULLDN)
				return (ptr);

			dn = dn->dn_parent;
			b_rdn = dn->dn_rdn;

#ifdef TURBO_AVL
			if (ptr->e_children == NULLAVL) {
				for (; dn!= NULLDN; dn=dn->dn_parent) {
					new = new_constructor(ptr);
					new->e_name = rdn_cpy(dn->dn_rdn);
					(void) avl_insert(&ptr->e_children, (caddr_t) new,
					    entry_cmp, avl_dup_error);
					ptr = (Entry) avl_find(ptr->e_children,
					    (caddr_t) dn->dn_rdn, entryrdn_cmp);
				}
				return(ptr);
			}

			kids = ptr->e_children;
			parent = ptr;
#else
			if ( ptr->e_child == NULLENTRY) {
				for (; dn!= NULLDN; dn=dn->dn_parent) {
					ptr->e_child = new_constructor(ptr);
					ptr = ptr->e_child;
					ptr->e_name = rdn_cpy (dn->dn_rdn);
				}
				return (ptr);
			}

			ptr = ptr->e_child;
			a_rdn = ptr->e_name;
#endif
		}

	}
	/* NOTREACHED */

}

