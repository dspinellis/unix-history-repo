/* objects.c - SMI object handling */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/snmp/RCS/objects.c,v 7.14 91/02/22 09:43:44 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/snmp/RCS/objects.c,v 7.14 91/02/22 09:43:44 mrose Interim $
 *
 * Contributed by NYSERNet Inc.  This work was partially supported by the
 * U.S. Defense Advanced Research Projects Agency and the Rome Air Development
 * Center of the U.S. Air Force Systems Command under contract number
 * F30602-88-C-0016.
 *
 *
 * $Log:	objects.c,v $
 * Revision 7.14  91/02/22  09:43:44  mrose
 * Interim 6.8
 * 
 * Revision 7.13  90/12/11  21:56:34  mrose
 * again
 * 
 * Revision 7.12  90/09/26  18:47:15  mrose
 * more-compile
 * 
 * Revision 7.11  90/08/30  15:11:09  mrose
 * ho-hum
 * 
 * Revision 7.10  90/07/09  14:48:54  mrose
 * sync
 * 
 * Revision 7.9  90/06/21  21:26:23  mrose
 * 0.0
 * 
 * Revision 7.8  90/06/20  21:38:21  mrose
 * update
 * 
 * Revision 7.7  90/06/12  09:22:06  mrose
 * write-only
 * 
 * Revision 7.6  90/05/28  21:49:51  mrose
 * not-accessible
 * 
 * Revision 7.5  90/05/13  16:18:14  mrose
 * views
 * 
 * Revision 7.4  90/05/12  17:37:04  mrose
 * typo
 * 
 * Revision 7.3  90/02/19  15:54:04  mrose
 * touch-up
 * 
 * Revision 7.2  90/02/17  10:38:22  mrose
 * smux
 * 
 * Revision 7.1  90/01/11  18:34:23  mrose
 * real-sync
 * 
 * Revision 7.0  89/11/23  22:23:18  mrose
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


#include <ctype.h>
#include <stdio.h>
#include "objects.h"
#include "tailor.h"

/*    DATA */

#define	TBUCKETS	0x80

static	int	once_only = 0;
static	int	compile_flag;
static	OT      compile_heap1;
static	char   *compile_heap2;
static	unsigned int *compile_heap3;
static	OID	compile_heap4;
static	OT	Tbuckets[TBUCKETS];

static	OT	anchor;
static	OT	chain;


OID	resolve ();


extern	int	errno;

/*    OBJECTS */

static int THASH (name)
char   *name;
{
    char    c;
    register char *cp,
		  *dp;

    for (c = *(dp = cp = name); *cp; cp++)
	if (isupper (*cp))
	    c = tolower (*(dp = cp));
    if (*++dp)
	return (((c - *dp) & 0x1f) + (*(dp + 1) & 0x5f));
    else
	return (c & 0x7f);
}


#define	OT_XXX	0x04

static int	ot_compar (a, b)
OT    *a,
      *b;
{
    int	    i = oid_cmp ((*a) -> ot_name, (*b) -> ot_name);

    if (i == 0
	    && (!((*a) -> ot_access & OT_XXX)
		    || !((*b) -> ot_access & OT_XXX))) {
	OID	oid = (*a) -> ot_name;
	register unsigned int *ip = oid -> oid_elements;

					       /* XXX: 0.0 is a special case */
	if (oid -> oid_nelem == 2 && ip[0] == 0 && ip[1] == 0)
	    return 0;

	if (PY_pepy[0])
	    (void) sprintf (PY_pepy + strlen (PY_pepy), ", ");
	else
	    (void) sprintf (PY_pepy, "duplicate objects: ");
	(void) sprintf (PY_pepy + strlen (PY_pepy), "\"%s\" and \"%s\"",
			(*a) -> ot_text, (*b) -> ot_text);
	(*a) -> ot_access |= OT_XXX, (*b) -> ot_access |= OT_XXX;
    }

    return i;
}


static char *roots[] = { "ccitt", "iso", "joint-iso-ccitt" };

int	readobjects (file)
char   *file;
{
    register char *cp,
		 **ap;
    char    buffer[BUFSIZ],
	    line[BUFSIZ],
	   *vec[NVEC + NSLACK + 1];
    register OT	   ot;
    FILE   *fp;

    if (file == NULL)
	file = "objects.defs";
    if ((fp = fopen (file, "r")) == NULL
	    && (fp = fopen (cp = isodefile (file, 0), "r")) == NULL) {
	(void) sprintf (PY_pepy, "unable to read %s: %s",
			cp, sys_errname (errno));
	return NOTOK;
    }

    if (once_only == 0) {
	bzero ((char *) Tbuckets, sizeof Tbuckets);

	readsyntax ();

	compile_flag = -1;
	if (fgets (buffer, sizeof buffer, fp)
	        && strncmp (buffer, "--* compiled",
			    sizeof "--* compiled" - 1) == 0) {
	    int	    i,
		    j,
		    k;

	    compile_flag = 1;
	    anchor = chain = NULL;

	    if (sscanf (buffer, "--* compiled %d %d %d", &i, &j, &k) == 3) {
		if ((compile_heap1 = (OT) calloc ((unsigned) i,
						  sizeof *compile_heap1))
		        == NULL) {
		    (void) sprintf (PY_pepy,
				    "out of memory allocating %d objects", i);
		    goto you_lose;
		}

		if ((compile_heap2 = malloc ((unsigned) j)) == NULL) {
		    (void) sprintf (PY_pepy,
				    "out of memory allocating %d octets", j);
		    goto you_lose;
		}

		if ((compile_heap3 = (unsigned int *)
				calloc ((unsigned) k, sizeof *compile_heap3))
		        == NULL) {
		    (void) sprintf (PY_pepy,
				    "out of memory allocating %d sub-identitifers",
				    k);
		    goto you_lose;
		}

		if ((compile_heap4 = (OID) calloc ((unsigned) i,
						   sizeof *compile_heap4))
		        == NULL) {
		    (void) sprintf (PY_pepy,
				    "out of memory allocating %d identitifers",
				    i);
		    goto you_lose;
		}
	    }
	    else
		compile_heap1 = NULLOT,
		    compile_heap2 = NULL,
			compile_heap3 = NULL,
			    compile_heap4 = NULL;
	}
	else {
	    compile_flag = 0;
	    (void) rewind (fp);

	    for (ap = roots + (sizeof roots / sizeof roots[0]) - 1;
		     ap >= roots;
		     ap--) {
		(void) sprintf (buffer, "%d", ap - roots);
		if (read_name (*ap, buffer) == NOTOK) {
you_lose: ;
		    (void) fclose (fp);
		    return NOTOK;
		}
	    }
	}

	once_only = 1;
    }
    else
	if (compile_flag > 0) {
	    (void) sprintf (PY_pepy, "only one compiled file is allowed");
	    goto you_lose;
	}

    while (fgets (buffer, sizeof buffer, fp)) {
	if (*buffer == '#' || strncmp (buffer, "--", 2) == 0)
	    continue;
	if (cp = index (buffer, '\n'))
	    *cp = NULL;
	(void) strcpy (line, buffer);

	bzero ((char *) vec, sizeof vec);
	switch (str2vec (buffer, vec)) {
	    case 0:
	        break;

	    case 2:
		if (read_name (vec[0], vec[1]) == NOTOK)
		    goto you_lose;
		break;

	    case 5:
		if (read_type (vec) == NOTOK)
		    goto you_lose;
		break;

	    default:
		(void) sprintf (PY_pepy, "malformed line: \"%s\"", line);
		goto you_lose;
	}
    }

    (void) fclose (fp);

    PY_pepy[0] = NULL;
    if (compile_flag < 1) {
	int	again,
		hit,
		i;
	register int   j = 0;
	register OT   *op,
		      *ep;
	OT     *base,
		oz;

	hit = 1, oz = NULLOT;
	do {
	    if (!hit) {
		(void) sprintf (PY_pepy, "unable to resolve object \"%s\"",
				oz -> ot_text);
		return NOTOK;
	    }

	    again = hit = 0;

	    for (i = 0; i < TBUCKETS; i++)
		for (ot = Tbuckets[i];
		         ot && ot -> ot_text;
		         ot = ot -> ot_chain)
		    if (ot -> ot_name == NULLOID)
			if (ot -> ot_name = resolve (ot -> ot_id, ot))
			    hit = 1;
			else
			    oz = ot, again = 1;
	} while (again);

	for (i = 0; i < TBUCKETS; i++)
	    for (ot = Tbuckets[i]; ot && ot -> ot_text; ot = ot -> ot_chain)
		j++;

	/* j > 1 ALWAYS */

	if ((base = (OT *) malloc ((unsigned) (j * sizeof *base))) == NULL) {
	    (void) sprintf (PY_pepy, "out of memory");
	    return NOTOK;
	}

	op = base;
	for (i = 0; i < TBUCKETS; i++)
	    for (ot = Tbuckets[i]; ot && ot -> ot_text; ot = ot -> ot_chain)
		*op++ = ot;
	ep = op;

	qsort ((char *) base, j, sizeof *base, ot_compar);

	op = base;
	anchor = ot = *op++;
	while (op < ep) {
	    ot -> ot_next = *op;
	    ot = *op++;
	}
	(chain = ot) -> ot_next = NULL;

	free ((char *) base);
    }
    else {
	for (ot = anchor; ot; ot = ot -> ot_next) {
	    OID	    oid;

	    if ((oid = str2oid (ot -> ot_id)) == NULLOID) {
		(void) sprintf (PY_pepy,
				"invalid OID (%s) for \"%s\" in compiled file",
				ot -> ot_id, ot -> ot_text);
		return NOTOK;
	    }
	    if (compile_heap3) {
		ot -> ot_name = compile_heap4++;
		bcopy ((char *) oid -> oid_elements,
		       (char *) (ot -> ot_name -> oid_elements
							    = compile_heap3),
		       (ot -> ot_name -> oid_nelem = oid -> oid_nelem)
		       		* sizeof *oid -> oid_elements);
		compile_heap3 += oid -> oid_nelem + 1;
	    }
	    else
		if ((ot -> ot_name = oid_cpy (oid)) == NULLOID) {
		    (void) sprintf (PY_pepy, "out of memory");
		    return NOTOK;
		}
	}

	if (add_objects_aux () == NOTOK)
	    return NOTOK;
    }

#ifdef	notdef
#ifdef	DEBUG
	dump_objects_by_tree ();
#endif
#endif

    return (PY_pepy[0] ? NOTOK : OK);
}

/*  */

static int  read_name (name, value)
char   *name,
       *value;
{
    int	    i;
    register OT	   ot;

    if (text2obj (name)) {
	(void) sprintf (PY_pepy, "duplicate object \"%s\"", name);
	return NOTOK;
    }

    if (compile_flag > 0 && compile_heap1)
	ot = compile_heap1++;
    else
	if ((ot = (OT) calloc (1, sizeof *ot)) == NULL) {
	    (void) sprintf (PY_pepy, "out of memory");
	    return NOTOK;
	}

    if (compile_flag > 0 && compile_heap2) {
	(void) strcpy (ot -> ot_text = compile_heap2, name);
	compile_heap2 += strlen (compile_heap2) + 1;

	(void) strcpy (ot -> ot_id = compile_heap2, value);
	compile_heap2 += strlen (compile_heap2) + 1;
    }
    else
	if ((ot -> ot_text = strdup (name)) == NULL
	        || (ot -> ot_id = strdup (value)) == NULL) {
	    (void) sprintf (PY_pepy, "out of memory");
	    return NOTOK;
	}
    
    ot -> ot_chain = Tbuckets[i = THASH (name)];
    Tbuckets[i] = ot;

    if (compile_flag) {
	if (chain)
	    chain -> ot_next = ot;
	else
	    anchor = ot;
	chain = ot;
    }

    return OK;
}

/*  */

static int  read_type (vec)
char  **vec;
{
    int	    i;
    register OT	   ot;

    if (text2obj (*vec)) {
	(void) sprintf (PY_pepy, "duplicate object \"%s\"", *vec);
	return NOTOK;
    }

    if (compile_flag > 0 && compile_heap1)
	ot = compile_heap1++;
    else
	if ((ot = (OT) calloc (1, sizeof *ot)) == NULL) {
	    (void) sprintf (PY_pepy, "out of memory");
	    return NOTOK;
	}

    if (compile_flag > 0 && compile_heap2) {
	(void) strcpy (ot -> ot_text = compile_heap2, *vec++);
	compile_heap2 += strlen (compile_heap2) + 1;

	(void) strcpy (ot -> ot_id = compile_heap2, *vec++);
	compile_heap2 += strlen (compile_heap2) + 1;
    }
    else
	if ((ot -> ot_text = strdup (*vec++)) == NULL
		|| (ot -> ot_id = strdup (*vec++)) == NULL) {
	    (void) sprintf (PY_pepy, "out of memory");
	    return NOTOK;
	}

    if ((ot -> ot_syntax = text2syn (*vec)) == NULL
	    && lexequ (*vec, "Aggregate")
	    && debug)
	fprintf (stderr,
		 "warning: object \"%s\" has unknown SYNTAX \"%s\"\n",
		 ot -> ot_text, *vec);
    vec++;
    
    if (lexequ (*vec, "read-only") == 0)
	ot -> ot_access = OT_RDONLY;
    else
	if (lexequ (*vec, "read-write") == 0)
	    ot -> ot_access = OT_RDWRITE;
        else
	    if (lexequ (*vec, "write-only") == 0)
		ot -> ot_access = OT_WRONLY;
	    else
		if (lexequ (*vec, "not-accessible") && debug)
		    fprintf (stderr,
			     "warning: object \"%s\" has unknown ACCESS \"%s\"\n",
			     ot -> ot_text, *vec);
    vec++;

    if (lexequ (*vec, "mandatory") == 0)
	ot -> ot_status = OT_MANDATORY;
    else
	if (lexequ (*vec, "optional") == 0)
	    ot -> ot_status = OT_OPTIONAL;
	else
	    if (lexequ (*vec, "deprecated") == 0)
		ot -> ot_status = OT_DEPRECATED;
	    else
		if (lexequ (*vec, "obsolete") && debug)
		    fprintf (stderr,
			     "warning: object \"%s\" has unknown STATUS \"%s\"\n",
			     ot -> ot_text, *vec);
    vec++;

    ot -> ot_chain = Tbuckets[i = THASH (ot -> ot_text)];
    Tbuckets[i] = ot;

    if (compile_flag > 0) {
	chain -> ot_next = ot;
	chain = ot;
    }

    return OK;
}

/*  */

/* does not insert into THASH table... */

int	add_objects (ot)
register OT	ot;
{
    register OID oid = ot -> ot_name;
    register OT	 ot2,
		*otp;

    if (oid_cmp (chain -> ot_name, oid) < 0) {
	chain -> ot_next = ot;
	(chain = ot) -> ot_next = NULLOT;
    }
    else {
	for (otp = &anchor; ot2 = *otp; otp = &ot2 -> ot_next)
	    if (oid_cmp (ot2 -> ot_name, oid) > 0)
		break;
	ot -> ot_next = ot2;
	*otp = ot;
    }

    for (ot = anchor; ot; ot = ot -> ot_next)
	ot -> ot_sibling = ot -> ot_children = NULLOT;

    return add_objects_aux ();
}


static int  add_objects_aux ()
{
    register OT	    ot,
		    ot2;

    for (ot = anchor; ot; ot = ot -> ot_next) {
	OIDentifier oids;

	if (ot -> ot_name -> oid_nelem <= 1)
	    continue;
	ot2 = NULLOT;
	for (oids.oid_elements = ot -> ot_name -> oid_elements,
		    oids.oid_nelem = ot -> ot_name -> oid_nelem - 1;
		oids.oid_nelem > 0;
	         oids.oid_nelem--)
	    if (ot2 = name2obj (&oids))
		break;
	if (ot2) {
	    ot -> ot_sibling = ot2 -> ot_children;
	    ot2 -> ot_children = ot;
	}
	else
	    if (debug)
		fprintf (stderr, "no distant parent for %s",
			 sprintoid (ot -> ot_name));
    }

    return OK;
}

/*  */

OID	text2oid (name)
char   *name;
{
    int	    i,
	    j;
    register unsigned int  *ip,
			   *jp;
    unsigned int elements[NELEM + 1];
    register char *cp;
    OID	    oid,
	    new;

    for (cp = name + strlen (name) - 1; cp >= name; cp--)
	if (!isdigit (*cp) && *cp != '.')
	    break;
    cp++;
    if (isdigit (*cp) && cp != name)
	for (cp++; *cp; cp++)
	    if (*cp == '.')
		break;

    if (*cp == NULL)		/* name */
	i = 0;
    else
	if (cp == name) {	/* 1.3.6.1.2.1.1.1.0 */
	    if ((i = str2elem (cp, elements)) < 2)
		return NULL;
	}
	else			/* name.numbers */
	    if ((i = str2elem (cp + 1, elements)) < 1)
		return NULL;

    if (cp != name) {
	*cp = NULL;
	if ((oid = resolve (name, NULLOT)) == NULLOID)
	    return NULL;
	if (i == 0)
	    return oid;

	j = oid -> oid_nelem;
    }
    else
	oid = NULL, j = 0;

    if ((new = (OID) malloc (sizeof *new)) == NULLOID) {
	oid_free (oid);
	return NULL;
    }
    if ((ip = (unsigned int *) malloc ((unsigned) (i + j + 1) * sizeof *ip))
		== NULL) {
	oid_free (oid);
	free ((char *) new);
    }
    new -> oid_elements = ip, new -> oid_nelem = i + j;

    if (oid) {
	for (j = 0, jp = oid -> oid_elements; j < oid -> oid_nelem; j++, jp++)
	    *ip++ = *jp;

	oid_free (oid);
    }
    if (i > 0)
	for (j = 0, jp = elements; j < i; j++, jp++)
	    *ip++ = *jp;

    new -> oid_nelem = ip - new -> oid_elements;

    return new;
}


static OID  resolve (id, ot)
char   *id;
register OT	ot;
{
    int	    i;
    unsigned int elements[NELEM + 1];
    register char *cp;
    register OT	   ot2;
    struct OIDentifier oids;
    register OID   oid = &oids;

    oid -> oid_elements = elements;

    if (cp = index (id, '.'))
	*cp = NULL;
    if (isdigit (*id)) {
	ot2 = NULLOT;
	oid -> oid_nelem = 1;
	oid -> oid_elements[0] = atoi (id);
	if (cp)
	    *cp = '.';
    }
    else {
	ot2 = text2obj (id);
	if (cp)
	    *cp = '.';
	if (ot2 == NULLOT || ot2 -> ot_name == NULLOID)
	    return NULLOID;

	oid -> oid_nelem = ot2 -> ot_name -> oid_nelem;
	bcopy ((char *) ot2 -> ot_name -> oid_elements,
	       (char *) oid -> oid_elements,
	       oid -> oid_nelem * sizeof *elements);
    }

    if (cp) {
	if ((i = str2elem (++cp, oid -> oid_elements + oid -> oid_nelem)) < 1)
	    return NULLOID;
	oid -> oid_nelem += i;
	if (ot && ot2) {	/* XXX: not normalized... */
	    ot -> ot_sibling = ot2 -> ot_children;
	    ot2 -> ot_children = ot;
	}
    }

    return oid_cpy (oid);
}

/*  */

/* partial matches are made only on leaf nodes... */

OT	name2obj (oid)
OID	oid;
{
    register int    i,
    		    j;
    register unsigned *ip;
    register OID   nm;
    register OT	   ot;

    if (oid == NULLOID
	    || oid -> oid_nelem < 1
	    || (i = (ip = oid -> oid_elements)[0])
		    >= (sizeof roots / sizeof roots[0])
	    || (ot = text2obj (roots[i])) == NULL)
	return NULLOT;

    i = 0;
    while (ot) {
	if ((j = (nm = ot -> ot_name) -> oid_nelem) > oid -> oid_nelem)
	    return NULLOT;

	if (bcmp ((char *) ip, (char *) (nm -> oid_elements + i),
		  (j - i) * sizeof *ip))
	    ot = ot -> ot_sibling;
	else
	    if (oid -> oid_nelem == j
		    || ot -> ot_children == NULLOT
		    || ot -> ot_smux)
		break;
	    else {
		ot = ot -> ot_children;
		ip = oid -> oid_elements + j, i = j;
	    }
    }

    return ot;
}

/*  */

OT	text2obj (text)
char   *text;
{
    register OT	   ot;

    if (text == NULL || once_only == 0)
	return NULLOT;

    for (ot = Tbuckets[THASH (text)];
	     ot && strcmp (ot -> ot_text, text);
	     ot = ot -> ot_chain)
	continue;

    return ot;
}

/*  */

/* ARGSUSED */

char   *oid2ode_aux (oid, quoted)
OID	oid;
int	quoted;
{
    register int    i;
    register char  *bp;
    register unsigned int *ip;
    register OID    oid2;
    register OT	    ot;
    static char buffer[BUFSIZ];

    if ((oid -> oid_nelem == 2		       /* XXX: 0.0 is a special case */
		&& oid -> oid_elements[0] == 0
		&& oid -> oid_elements[1] == 0)
	    || (ot = name2obj (oid)) == NULLOT)
	return sprintoid (oid);

    (void) strcpy (bp = buffer, ot -> ot_text);
    bp += strlen (bp);
    for (ip = oid -> oid_elements + (oid2 = ot -> ot_name) -> oid_nelem,
	 	i = oid -> oid_nelem - oid2 -> oid_nelem;
	     i-- > 0;
	     ip++) {
	(void) sprintf (bp, ".%u", *ip);
	bp += strlen (bp);
    }

    return buffer;
}

/*  */

OI	name2inst (oid)
OID	oid;
{
    static object_instance ois;
    register OI oi = &ois;

    if ((oi -> oi_type = name2obj (oi -> oi_name = oid)) == NULLOT)
	return NULLOI;

    return oi;    
}


OI	next2inst (oid)
OID	oid;
{
    static object_instance ois;
    register OI oi = &ois;
    register OT ot;

    for (ot = anchor; ot; ot = ot -> ot_next) {
	if (ot -> ot_smux) {
	    if (oid -> oid_nelem < ot -> ot_name -> oid_nelem
		    || bcmp ((char *) oid -> oid_elements,
			     (char *) ot -> ot_name -> oid_elements,
			     ot -> ot_name -> oid_nelem
			         * sizeof oid -> oid_elements[0]))
		continue;
	}
	else
	    if (oid_cmp (oid, ot -> ot_name) > 0)
		continue;

	oi -> oi_name = (oi -> oi_type = ot) -> ot_name;
	return oi;
    }

    return NULLOI;
}

/*  */

OI	text2inst (text)
char   *text;
{
    static object_instance ois;
    register OI oi = &ois;
    static OID oid = NULLOID;

    if (oid)
	oid_free (oid), oid = NULLOID;

    if ((oid = text2oid (text)) == NULLOID)
	return NULLOI;
    if ((oi -> oi_type = name2obj (oi -> oi_name = oid)) == NULLOT) {
	if (debug)
	    fprintf (stderr, "got name \"%s\", but not object\n", text);
	return NULLOI;
    }

    return oi;    
}

/*    DUMP */

#ifdef	DEBUG
dump_objects_by_text ()
{
    int	    hit;
    register int    i;
    register OT	    ot;

    for (i = 0; i < TBUCKETS; i++) {
	hit = 0;
	for (ot = Tbuckets[i]; ot && ot -> ot_text; ot = ot -> ot_chain) {
	    if (!hit)
		printf ("Bucket %d:\n", i), hit = 1;
	    dump_object (ot, 2);
	}
    }
	
    printf ("///////\n");
}


dump_objects_by_tree ()
{
    register char **ap;
    char  **bp;
    register OT	    ot;

    for (bp = (ap = roots) + (sizeof roots / sizeof roots[0]); ap < bp; ap++) {
	if (ot = text2obj (*ap))
	    dump_object_by_tree (ot, 0);
	else
	    printf ("no object for root \"%s\"\n", *ap);
    }
	
    printf ("///////\n");
}


dump_object_by_tree (ot, i)
register OT	ot;
int	i;
{
    if (ot == NULL)
	return;

    dump_object (ot, i);
    dump_object_by_tree (ot -> ot_children, i + 1);
    dump_object_by_tree (ot -> ot_sibling, i);
}


dump_objects_by_xxx ()
{
    register OT	    ot;

    for (ot = anchor; ot; ot = ot -> ot_next)
	dump_object (ot, 0);
	
    printf ("///////\n");
}


static	dump_object (ot, i)
register OT	ot;
int	i;
{
    printf ("%*.*s%s %s %s %s %d %d 0x%x\n", i, i, "",
	    ot -> ot_text, ot -> ot_id, sprintoid (ot -> ot_name),
	    ot -> ot_syntax ? ot -> ot_syntax -> os_name : "NULL",
	    ot -> ot_access, ot -> ot_status, ot -> ot_smux);
}
#endif

/*    MISCELLANY */

char   *strdup (s)
char   *s;
{
    char   *p;

    if (p = malloc ((unsigned) (strlen (s) + 1)))
	(void) strcpy (p, s);

    return p;
}
