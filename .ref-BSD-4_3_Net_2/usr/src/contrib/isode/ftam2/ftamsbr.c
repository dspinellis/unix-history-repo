/* ftamsbr.c - FTAM subroutines */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam2/RCS/ftamsbr.c,v 7.6 91/02/22 09:24:06 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam2/RCS/ftamsbr.c,v 7.6 91/02/22 09:24:06 mrose Interim $
 *
 *
 * $Log:	ftamsbr.c,v $
 * Revision 7.6  91/02/22  09:24:06  mrose
 * Interim 6.8
 * 
 * Revision 7.5  91/01/13  12:27:04  mrose
 * NBS
 * 
 * Revision 7.4  90/12/23  18:40:16  mrose
 * update
 * 
 * Revision 7.3  90/11/21  11:30:50  mrose
 * sun
 * 
 * Revision 7.2  90/11/05  13:29:54  mrose
 * nist
 * 
 * Revision 7.1  90/07/01  21:03:31  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  21:54:37  mrose
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
#include "ftamsbr.h"

#ifdef	BRIDGE
extern int vfs_fdf;
#endif

extern struct vfsmap vfs[];

/*  */

#ifdef	BRIDGE
/* ARGSUSED */
#endif

struct vfsmap *st2vfs (fd, file, st, proposed, ftamfd)
int	fd;
char   *file;
struct stat *st;
OID	proposed;
int	ftamfd;
{
#ifndef	BRIDGE
    register int    fmt;
    register struct vfsmap *lf;
#endif
    register struct vfsmap *vf;

#ifndef	BRIDGE
    fmt = st -> st_mode & S_IFMT;
#else
/* Return the suggested TYPE or Unstructured Text type for FTP bridge */
/* if during ftp_exist got multiple listing, return directory type */
    if (ftp_directory)
	return &vfs[vfs_fdf];
#endif

    if (proposed) {
	for (vf = vfs; vf -> vf_entry; vf++)
	    if (vf -> vf_oid && oid_cmp (vf -> vf_oid, proposed) == 0) {
#ifdef	BRIDGE
		return vf;
	    }
#else
		if ((vf -> vf_flags & VF_OK) && vf -> vf_mode == fmt) {
		    if (vf -> vf_peek
			    && (*vf -> vf_peek) (vf, fd, file, st, ftamfd)
					== NOTOK)
			break;

		    return vf;
		}

		break;
	    }

	if (!vf -> vf_entry)
	    return NULL;
#endif
    }
#ifndef	BRIDGE
    else {
	for (lf = vfs; lf -> vf_entry; lf++)
	    continue;
	lf--;

	for (vf = lf; vf >= vfs; vf--)
	    if ((vf -> vf_flags & VF_OK) && vf -> vf_mode == fmt) {
		if (vf -> vf_peek
			&& (*vf -> vf_peek) (vf, fd, file, st, ftamfd) != DONE)
		    continue;

		return vf;
	    }

	for (vf = lf; vf >= vfs; vf--)
	    if ((vf -> vf_flags & VF_OK)
		    && vf -> vf_mode == fmt
		    && vf -> vf_simplify != VFS_XXX)
		break;
	if (vf < vfs)
	    return NULL;
    }

/* let's hope there aren't any simplification loops! */

    while (vf -> vf_simplify != VFS_XXX) {
	vf = &vfs[vf -> vf_simplify];

	if (vf -> vf_flags & VF_OK) {
	    if (vf -> vf_peek)
		(void) (*vf -> vf_peek) (vf, fd, file, st, ftamfd);
	    return vf;
	}
    }

    return NULL;
#else
    return &vfs[ftp_default];
#endif
}

/*  */

int	binarycheck (param, data)
caddr_t param;
char   *data;
{
    register struct type_DOCS_FTAM__3__Parameters *p3 =
			(struct type_DOCS_FTAM__3__Parameters *) param;

    if (p3 -> optionals
	      & opt_DOCS_FTAM__3__Parameters_maximum__string__length) {
	if (getenv ("UNISYS-FTAM"))
	    p3 -> maximum__string__length = 0;
	else
	    p3 -> optionals &=
			~opt_DOCS_FTAM__3__Parameters_maximum__string__length;
    }

    if ((p3 -> optionals
		& opt_DOCS_FTAM__3__Parameters_string__significanz)
	    && p3 -> string__significanz
		    == int_DOCS_string__significanz_fixed) {
	(void) strcpy (data,
		       "filestore does not support fixed-length strings");
	return NOTOK;
    }
    

    return OK;
}

/*  */

int	textcheck (param, data)
caddr_t param;
char   *data;
{
    register struct type_DOCS_FTAM__1__Parameters *p1 =
			(struct type_DOCS_FTAM__1__Parameters *) param;

    if (!(p1 -> optionals
	      & opt_DOCS_FTAM__1__Parameters_universal__class__number)) {
	p1 -> optionals |= opt_DOCS_FTAM__1__Parameters_universal__class__number;
	p1 -> universal__class__number = PE_DEFN_GENS;
    }
    switch (p1 -> universal__class__number) {
	case PE_DEFN_GFXS:
	case PE_DEFN_IA5S:
	case PE_DEFN_GENS:
	    break;

       default:
	    (void) sprintf (data,
			    "filestore does not support strings of universal class number %d",
			    p1 -> universal__class__number);
	    return NOTOK;
    }

    if (p1 -> optionals
	      & opt_DOCS_FTAM__1__Parameters_maximum__string__length) {
	if (getenv ("UNISYS-FTAM")) {
	    p1 -> maximum__string__length = 0;
	}
	else
	    p1 -> optionals &=
			~opt_DOCS_FTAM__1__Parameters_maximum__string__length;
    }

    if ((p1 -> optionals
		& opt_DOCS_FTAM__1__Parameters_string__significance)
	    && p1 -> string__significance
		    == int_DOCS_string__significance_fixed) {
	(void) strcpy (data,
		       "filestore does not support fixed-length strings");
	return NOTOK;
    }
    

    return OK;
}

/*  */

/* ARGSUSED */

int	binarypeek (vf, fd, file, st, ftamfd)
register struct vfsmap *vf;
int	fd;
char   *file;
struct stat *st;
int	ftamfd;
{
    static struct type_DOCS_FTAM__3__Parameters p3s;
    register struct type_DOCS_FTAM__3__Parameters *p3 = &p3s;

    if (vf -> vf_parameter && (vf -> vf_flags & VF_PARM))
	(void) fre_obj (vf -> vf_parameter,
			_ZDOCS_mod.md_dtab[vf -> vf_number], &_ZDOCS_mod, 1);

    vf -> vf_parameter = (caddr_t) p3, vf -> vf_flags &= ~VF_PARM;

    p3 -> optionals = 0;

    if (getenv ("UNISYS-FTAM")) {
	p3 -> optionals |=opt_DOCS_FTAM__3__Parameters_maximum__string__length;
	p3 -> maximum__string__length = 0;
    }

    p3 -> optionals |= opt_DOCS_FTAM__3__Parameters_string__significanz;
    p3 -> string__significanz = int_DOCS_string__significanz_not__significant;

    return DONE;
}

/*  */

/* Various textual repetories.  In addition to the prohibited characters, on
   UNIX we disallow CRs.  This avoids funny CR-LF mappings. */

/* Here's the assumptions we make for whether format-effectors are used:

   	PrintableString	-	no
	TeletexString	-	yes
	VideotexString	-	yes (?)
	IA5String	-	yes
	GraphicString	-	no (?)
	VisibleString	-	no (?)
	GeneralString	-	yes (?)

*/


#define	isIA5(c) (isprint ((u_char) c) || (isspace ((u_char)c) && (c) != '\r'))


/* ARGSUSED */

int	textpeek (vf, fd, file, st, ftamfd)
register struct vfsmap *vf;
int	fd;
char   *file;
struct stat *st;
int	ftamfd;
{
#ifndef	BRIDGE
    int     gd,
	    n;
    register char *cp;
    char    buffer[BLKSIZE];
    long    pos;
#endif
    static struct type_DOCS_FTAM__1__Parameters p1s;
    register struct type_DOCS_FTAM__1__Parameters *p1 = &p1s;

    if (vf -> vf_parameter && (vf -> vf_flags & VF_PARM))
	(void) fre_obj (vf -> vf_parameter,
			_ZDOCS_mod.md_dtab[vf -> vf_number], &_ZDOCS_mod, 1);

    vf -> vf_parameter = (caddr_t) p1, vf -> vf_flags &= ~VF_PARM;

    p1 -> optionals = 0;

    p1 -> optionals |= opt_DOCS_FTAM__1__Parameters_universal__class__number;
    p1 -> universal__class__number = PE_DEFN_GENS;

    if (getenv ("UNISYS-FTAM")) {
	p1 -> optionals |=opt_DOCS_FTAM__1__Parameters_maximum__string__length;
	p1 -> maximum__string__length = 0;
    }

    p1 -> optionals |= opt_DOCS_FTAM__1__Parameters_string__significance;
    switch (p1 -> universal__class__number) {
	case PE_DEFN_GFXS:
	    if (getenv ("HP-FTAM")) {
		p1 -> string__significance =
				int_DOCS_string__significance_not__significant;
		break;
	    }	/* else fall... */
	case PE_DEFN_PRTS:
	case PE_DEFN_VISS:
	    p1 -> string__significance =
				int_DOCS_string__significance_variable;
	    break;

	case PE_DEFN_T61S:
	case PE_DEFN_VTXS:
	case PE_DEFN_IA5S:
	case PE_DEFN_GENS:
	    p1 -> string__significance =
				int_DOCS_string__significance_not__significant;
	    break;
    }

#ifndef	BRIDGE
    if ((gd = fd) == NOTOK
	    && (file == NULLCP || (gd = open (file, O_RDONLY)) == NOTOK))
	return OK;

    if (fd != NOTOK) {
	pos = lseek (gd, 0L, L_INCR);
	(void) lseek (gd, 0L, L_SET);
    }
#ifndef	MAXBSIZE
    n = read (gd, buffer, sizeof buffer);
#else
    n = 0 < st -> st_blksize && st -> st_blksize <= sizeof buffer 
		? st -> st_blksize : sizeof buffer;
    n = read (gd, buffer, n);
#endif
    if (fd != NOTOK && pos != -1L)
	(void) lseek (gd, pos, L_SET);

    if (fd == NOTOK)
	(void) close (gd);

    for (cp = buffer + n - 1; cp >= buffer; cp--)
	if (!isIA5 (*cp))
	    return NOTOK;
    return DONE;
#else
    return OK;
#endif
}

/*  */

/* ARGSUSED */

int	fdfpeek (vf, fd, file, st, ftamfd)
register struct vfsmap *vf;
int	fd;
char   *file;
struct stat *st;
int	ftamfd;
{
    struct type_DOCS_NBS__9__Parameters *p9;
    struct FTAMindication ftis;

    if (vf -> vf_parameter && (vf -> vf_flags & VF_PARM))
	(void) fre_obj (vf -> vf_parameter,
			_ZDOCS_mod.md_dtab[vf -> vf_number], &_ZDOCS_mod, 1);

    vf -> vf_parameter = NULLCP, vf -> vf_flags &= ~VF_PARM;

    if (fdf_names2p (ftamfd, FA_RDATTR, &p9, &ftis) == NOTOK)
	return NOTOK;

    vf -> vf_parameter = (caddr_t) p9;

    return DONE;
}

/*  */

/* If text, then need to worry about ESCape sequences for the various
   repetoires (thank you, Digital!)  For now, we'll recognize G0 and G1 from
   the 8859-1 (latin) alphabet.  Note that when sending a file, we do not
   generate escape sequences...
 */


int	de2fd (fd, pe, text, effector)
int	fd;
PE	pe;
int	text,
	effector;
{
    register int    i,
		    n;
		register char  *bp,
			       *cp,
			       *ep;
    register PE	    p;

    if (pe -> pe_form == PE_FORM_CONS) {
	    for (p = pe -> pe_cons, n = 0; p; p = p -> pe_next, n += i)
		if ((i = de2fd (fd, p, text, 1)) == NOTOK)
		    return NOTOK;
	goto outside;
    }

    if (!text) {
	n = pe -> pe_len;
	if (write (fd, (char *) pe -> pe_prim, n) != n)
	    return NOTOK;
	goto outside;
    }

    n = 0;
    cp = (char *) pe -> pe_prim;
    for (ep = (bp = cp) + pe -> pe_len; bp < ep;)
	switch (*bp) {
#ifndef	BRIDGE
	     case '\r':
		if (!effector) {
		    bp++;
		    break;
		}
		*bp++ = '\n';
		i = bp - cp;
		if (write (fd, cp, i) != i)
		    return NOTOK;
		cp = ++bp, n += i;
		break;
#endif

	    case 033:
		switch (*++bp) {
		    case 0x28:	/* G0: 02/08 04/02 */
			if (*++bp == 0x42) {
			    register char *dp;

write_it: ;
			    dp = bp - 2;
			    if ((i = (dp - cp)) > 0
				    && write (fd, cp, i) != i)
				return NOTOK;
			    cp = ++bp, n += i;
			}
			else
			    bp--;
			break;

		    case 0x2d:	/* G1: 02/13 04/01 */
			if (*++bp == 0x41)
			    goto write_it;
			else
			    bp--;
			break;

		    default:	/* unknown, pass it on... */
			break;
		}
		break;

	    default:
		bp++;
		break;
	}

    if (i = bp - cp) {
	if (write (fd, cp, i) != i)
	    return NOTOK;

	n += i;
    }

outside: ;
    if (text && !effector) {
#ifndef	BRIDGE
	if (write (fd, "\n", 1) != 1)
#else
	if (write (fd, "\r\n", 2) != 2)
#endif
	    return NOTOK;
    }

    return n;
}

/*  */

/* right from MH's sbr/path.c... */

#define	CWD	"./"
#define	NCWD	(sizeof CWD - 1)
#define	DOT	"."
#define	DOTDOT	".."
#define	PWD	"../"
#define	NPWD	(sizeof PWD - 1)


int	compath (f)
register char  *f;
{
    register char  *cp,
                   *dp;

    if (*f != '/')
	return;

    for (cp = f; *cp;)
	if (*cp == '/') {
	    switch (*++cp) {
		case NULL: 
		    if (--cp > f)
			*cp = NULL;
		    break;

		case '/': 
#ifdef apollo
                    if ((f+1) == cp) {
			cp++;
			continue;
                    }
#endif                    
		    for (dp = cp; *dp == '/'; dp++)
			continue;
		    (void) strcpy (cp--, dp);
		    continue;

		case '.': 
		    if (strcmp (cp, DOT) == 0) {
			if (cp > f + 1)
			    cp--;
			*cp = NULL;
			break;
		    }
		    if (strcmp (cp, DOTDOT) == 0) {
			for (cp -= 2; cp > f; cp--)
			    if (*cp == '/')
				break;
			if (cp <= f)
			    cp = f + 1;
			*cp = NULL;
			break;
		    }
		    if (strncmp (cp, PWD, NPWD) == 0) {
			for (dp = cp - 2; dp > f; dp--)
			    if (*dp == '/')
				break;
			if (dp <= f)
			    dp = f;
			(void) strcpy (dp, cp + NPWD - 1);
			cp = dp;
			continue;
		    }
		    if (strncmp (cp, CWD, NCWD) == 0) {
			(void) strcpy (cp - 1, cp + NCWD - 1);
			cp--;
			continue;
		    }
		    continue;

		default: 
		    cp++;
		    continue;
	    }
	    break;
	}
	else
	    cp++;
}
