/* pass2.h */

/* 
 * $Header: /f/osi/pepsy/RCS/pass2.h,v 7.7 91/02/22 09:49:23 mrose Interim $
 *
 *
 * $Log:	pass2.h,v $
 * Revision 7.7  91/02/22  09:49:23  mrose
 * Interim 6.8
 * 
 * Revision 7.6  90/12/23  17:24:56  mrose
 * patches
 * 
 * Revision 7.5  90/11/11  10:54:03  mrose
 * update
 * 
 * Revision 7.4  90/11/04  19:19:01  mrose
 * update
 * 
 * Revision 7.3  90/07/27  08:49:28  mrose
 * update
 * 
 * Revision 7.2  90/07/09  14:52:47  mrose
 * sync
 * 
 * Revision 7.1  90/07/01  20:01:13  mrose
 * update
 * 
 * Revision 7.0  90/07/01  19:54:43  mrose
 * *** empty log message ***
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


/* Change the version number only important features change - so people can
 * #ifdef on the version number. Also used to provide backwards compatible
 * macro definitions for posy/pepy.
 */
#define PEPSY_VERSION_NUMBER	2
#define NBPC	8	/* Number of Bits per character - machine dependant */
#define NBPI	sizeof (int)*NBPC	/* Number of bits per integer */

#define PSAP_DOT_H	"\"psap.h\""
#define I_PSAP_DOT_H	"<isode/psap.h>"
#define UNIV_TYPES_DOT_H	"\"UNIV-types.h\""
#define I_UNIV_TYPES_DOT_H	"<isode/pepsy/UNIV-types.h>"
#define	HFILE1	"_defs.h"
#define HFILE2	"_pre_defs.h"
#define ACTIONDEFS	"_action.h"

#define GENTYPES	"-types.h"
#define INCFILE1	"pepsy.h"
/* #define INCFILE2	"pepdefs.h" - not used any more */

#define	ACT1	"_act1"
#define	ACT2	"_act2"
#define	ACT3	"_act3"
#define	ACT4	"_act4"
#define	ACT05	"_act05"

#define PREFIX	"_Z"

#define ETABLE	"et_"
#define DTABLE	"dt_"
#define PTABLE	"pt_"

#define	ENCFNCNAME	"enc_f"
#define	DECFNCNAME	"dec_f"
#define	PRNTFNCNAME	"prnt_f"

#define	ENC_FNCNAME	"enc_f_"
#define	DEC_FNCNAME	"dec_f_"
#define	PRNT_FNCNAME	"prnt_f_"

#define	ENCFILENAME	"_enc.c"
#define	DECFILENAME	"_dec.c"
#define	PRNTFILENAME	"_prnt.c"

#define TBLNAME		"_tables.c"
#define MODTYP_SUFFIX	"_mod"

#define MAXPTRS		200   /* maximum number of pointers in pointer table */

#define PTR_TABNAME	"ptrtab"
extern	int	p_debug;

#define DEB 1

#ifdef DEB
#define Printf(x, y) if (x <= p_debug) printf y
#else
#define Printf(x, y)
#endif

/*
 * info for handling a Universal type
 */
struct	univ_typ	{
	char	*univ_name;	/* Name of this Universal type */
	char	*univ_data;	/* type to generate for it */
	char	*univ_tab;	/* type of table entry it needs */
	int	univ_id;	/* tag of the type */
	int	univ_class;	/* class - probably Universal primative */
	char	*univ_mod;	/* Name of its module if it has one  */
	int	univ_flags;	/* Information about entry */
	int	univ_type;	/* Type we can use for its contents */
#define UNF_EXTMOD	1	/* Use an external module reference */
#define UNF_HASDATA	2	/* Has data structure - allocate data for it */
    };

extern struct univ_typ *univtyp();

/* How many entries in an array */
#define NENTRIES(x)	(sizeof (x)/sizeof ((x)[0]))

/* used to specify which tables a routine is to generate */
#define G_ENC	0	/* encoding */
#define G_DEC	1	/* decoding */
#define G_PNT	2	/* printing */

#define	hflag	(options[0])
#define	Hflag	(options[1])
#define h2flag	(options[2])
#define	NOPTIONS	3
extern int	options[];

#define STRSIZE 128	/* general buffer size */

extern char   *proc_name();

extern char *getfield(), *getfldbit();
extern char *class2str();

extern int	gen_ventry();	/* generate a Value Passing Entry */
extern int	gen_fnentry();	/* generate a function calling entry */
/* extern Action	start_action, final_action; */
extern char	*int2tstr();	/* integer to temporary string */

extern char *getfield(), *getfldbit();
extern char *class2str();

extern int	gen_ventry();	/* generate a Value Passing Entry */
extern int	gen_fnentry();	/* generate a function calling entry */
/* extern Action	start_action, final_action; */
