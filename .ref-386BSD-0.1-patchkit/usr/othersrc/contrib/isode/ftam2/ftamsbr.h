/* ftamsbr.h - include file for FTAM initiator/responder subroutines */

/* 
 * $Header: /f/osi/ftam2/RCS/ftamsbr.h,v 7.2 91/02/22 09:24:07 mrose Interim $
 *
 *
 * $Log:	ftamsbr.h,v $
 * Revision 7.2  91/02/22  09:24:07  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/01  21:03:34  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  21:54:39  mrose
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


#include "ftam.h"		/* definitions for FS-USERs */
#include "DOCS-types.h"
#ifdef	NULL
#undef	NULL
#endif
#include <sys/param.h>
#ifndef	NULL
#define	NULL	0
#endif
#ifndef	SYS5
#include <sys/file.h>
#else
#define	L_SET		0	/* absolute offset */
#define	L_INCR		1	/* relative to current offset */
#define	L_XTND		2	/* relative to end of file */

#define	F_OK		0	/* file exists */
#define	X_OK		1	/* executable by caller */
#define	W_OK		2	/* writable by caller */
#define	R_OK		4	/* readable by caller */

#if	!defined(AIX) && !defined(HPUX) && !defined(AUX)
#include <sys/fcntl.h>
#else
#include <fcntl.h>
#endif
#endif
#include <sys/stat.h>
#include "usr.dirent.h"


#ifndef	MAXPATHLEN
#define	MAXPATHLEN	MAXNAMLEN
#endif

#ifdef	MAXBSIZE
#define	BLKSIZE	MAXBSIZE
#else
#define	BLKSIZE	BUFSIZ
#endif

/*
   Used to calculate the estimated integral FADU size:


   FTAM-3 transfers -

	An FADU maps onto a single PSDU with the P-DATA service.  Because the
	DCS is non-empty and the FADU is not in the default context, the
	Fully-encoded-data encoding is used.  Further, since only one PSDU is
	present, the single-ASN1-type option is used.  Hence, the outer ASN.1
	wrapper consists of

	    [APPLICATION 1] IMPLICIT
		SEQUENCE OF {	-- 4 octets for the outer structure
		    SEQUENCE {	-- 4 octets for the one and only PDV-list

				-- 3 octets for the PCI
		        presentation-context-identifier
		            INTEGER,

		        presentation-data-values {
				-- 4 octets for the single-ASN1-type wrapper
		            single-ASN1-type[0]
				-- 4 octets for the id/length of the FADU
				-- n octets for the data in the FADU
			        ANY
			}
		    }
		}

     4 + 4 + 3 + 4 + 4 = 19

     For each structure, 4 octets is used for non-data encodings
		1 octet for the ID
		1 octet for an indefinite form
		2 octets for the EOC
    If the data portion is smaller, then the definite form might be used which
    requires 3 octets, not 4.


   FTAM-1 transfers -

	FADUs are batched to the P-DATA service.  This means that the
	octet-aligned option is used.  Hence, the outer ASN.1 wrapper
	consists of  

	    [APPLICATION 1] IMPLICIT
		SEQUENCE OF {	-- 4 octets for the outer structure

-- this sequence is repeated for each member of the batch

		    SEQUENCE {	-- 4 octets for the one and only PDV-list

				-- 3 octets for the PCI
		        presentation-context-identifier
		            INTEGER,

		        presentation-data-values {
				-- 4 octets for the octet-aligned wrapper
		            octet-aligned[1]
				-- 4 octets for the id/length of the FADU
				-- n octets for the data in the FADU
			        IMPLICIT OCTET STRING
			}
		    }


		}

     4 + N*(4 + 3 + 4 + 4)
 */
#define	MAGIC_SINGLE	19
#define	MAGIC_OCTET1	4
#define	MAGIC_OCTET2	15

/*  */

struct vfsmap {
    char   *vf_entry;		/* document entry */
    OID	    vf_oid;		/* object identifier */
    caddr_t vf_parameter;	/* parameter, filled-in by vf_peek */

    int     vf_flags;		/* flags */
#define	VF_NULL	0x00
#define	VF_OK	0x01		/* negotiated */
#define	VF_WARN	0x02		/* warn if loses */
#define	VF_PARM	0x04		/* parameter dynamically allocated */

    int	    vf_id;		/* presentation context */

    int	    vf_mode;		/* st.st_mode & S_IFMT bits */
    IFP	    vf_peek;		/* sees if really this type of file */
    char    vf_stat;		/* stat character for 'ls' */

    int	    vf_simplify;	/* the next document type to try */
#define	VFS_XXX	(-1)

    int	    vf_context;		/* access context */
				/* really should have entire constraint set */

    int	    vf_mandatory;	/* > 0 parameter required
				   < 0 parameter optional
				  == 0 parameter illegal */
    IFP	    vf_check;		/*   .. check */
    int	    vf_number;		/* encode/decode index */

    char  *vf_text;		/* textual description */
};

struct vfsmap *st2vfs ();


/* WATCHP is one pepsy people should use as the macro which is
 * not expansion order dependant
 */

#ifndef DEBUG
#define	WATCH(fnx, pe, rw)
#define	WATCHP(args, pe, rw)
#else
#ifdef __STDC__
#define	WATCHP(args, pe, rw) \
    pvpdu (ftam_log, print_##args##_P, pe, \
	rw ? "F-DATA.INDICATION" : "F-DATA.REQUEST", rw)
#define	WATCH(fnx, pe, rw) \
    pvpdu (ftam_log, fnx/**/_P, pe, \
	rw ? "F-DATA.INDICATION" : "F-DATA.REQUEST", rw)
#else
#define	WATCHP(args, pe, rw) \
    pvpdu (ftam_log, print_/**/args/**/_P, pe, \
	rw ? "F-DATA.INDICATION" : "F-DATA.REQUEST", rw)
#define	WATCH(fnx, pe, rw) \
    pvpdu (ftam_log, fnx/**/_P, pe, \
	rw ? "F-DATA.INDICATION" : "F-DATA.REQUEST", rw)
#endif
#endif


int	binarypeek (), textpeek (), fdfpeek ();

int	binarycheck (), textcheck ();

/*  */

#define	FA_RDATTR \
    (FA_FILENAME | FA_ACTIONS | FA_CONTENTS | FA_ACCOUNT | FA_DATE_CREATE \
	| FA_DATE_MODIFY | FA_DATE_READ | FA_DATE_ATTR | FA_ID_CREATE \
	| FA_ID_MODIFY | FA_ID_READ | FA_ID_ATTR | FA_AVAILABILITY \
	| FA_FILESIZE)

/*  */

#ifdef	BRIDGE
extern int  ftp_default;
extern int  ftp_directory;
#endif

int	de2fd ();

int	compath ();
