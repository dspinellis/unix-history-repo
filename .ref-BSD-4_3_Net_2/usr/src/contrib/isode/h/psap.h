/* psap.h - include file for presentation users (PS-USER) */

/* 
 * $Header: /f/osi/h/RCS/psap.h,v 7.7 91/02/22 09:24:54 mrose Interim $
 *
 *
 * $Log:	psap.h,v $
 * Revision 7.7  91/02/22  09:24:54  mrose
 * Interim 6.8
 * 
 * Revision 7.6  91/02/20  17:26:47  mrose
 * update
 * 
 * Revision 7.5  91/01/07  12:39:28  mrose
 * update
 * 
 * Revision 7.4  90/10/23  20:35:50  mrose
 * update
 * 
 * Revision 7.3  90/07/27  08:44:43  mrose
 * update
 * 
 * Revision 7.2  90/07/01  21:03:50  mrose
 * pepsy
 * 
 * Revision 7.1  90/03/22  08:39:21  mrose
 * touch-up
 * 
 * Revision 7.0  89/11/23  21:55:52  mrose
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


#ifndef	_PSAP_
#define	_PSAP_

#ifndef	_MANIFEST_
#include "manifest.h"
#endif
#ifndef	_GENERAL_
#include "general.h"
#endif
#ifndef	OSX
#ifndef	BSD42
#include <time.h>
#else
#ifndef	timerisset
#include <sys/time.h>
#endif
#endif
#endif
#include <math.h>

/*  */

typedef struct OIDentifier {
    int	    oid_nelem;		/* number of sub-identifiers */

    unsigned int *oid_elements;	/* the (ordered) list of sub-identifiers */
}			OIDentifier, *OID;
#define	NULLOID	((OID) 0)


OID	ode2oid ();
int	oid_cmp (), elem_cmp ();
OID	oid_cpy ();
int	oid_free ();
#define	oid2ode(i)	oid2ode_aux ((i), 1)
char   *oid2ode_aux ();
char   *sprintoid ();
OID	str2oid ();

/*  */

typedef	u_char	   PElementClass;

typedef	u_char	   PElementForm;

typedef u_short    PElementID;	/* 0..16383 are meaningful (14 bits) */
#define	PE_ID_BITS	14

#define	PE_ID(class,code) \
	((int) ((((code) & 0x3fff) << 2) | ((class) & 0x0003)))

typedef	int	   PElementLen;


typedef u_char	  byte, *PElementData;
#define	NULLPED	((PElementData) 0)

#define	PEDalloc(s)		((PElementData) malloc ((unsigned int) (s)))

#define	PEDrealloc(p, s)	((PElementData) \
				    realloc ((char *) (p), (unsigned int) (s)))

#define	PEDfree(p)		free ((char *) (p))

#define	PEDcmp(b1, b2, length)	\
			bcmp ((char *) (b1), (char *) (b2), (int) (length))

#define	PEDcpy(b1, b2, length)	\
			bcopy ((char *) (b1), (char *) (b2), (int) (length))

/*  */

typedef struct PElement {
    int	    pe_errno;		/* Error codes */
#define	PE_ERR_NONE	0	/*   No error */
#define	PE_ERR_OVER	1	/*   Overflow */
#define	PE_ERR_NMEM	2	/*   Out of memory */
#define	PE_ERR_BIT	3	/*   No such bit */
#define	PE_ERR_UTCT	4	/*   Malformed universal timestring */
#define	PE_ERR_GENT	5	/*   Malformed generalized timestring */
#define	PE_ERR_MBER	6	/*   No such member */
#define	PE_ERR_PRIM	7	/*   Not a primitive form */
#define	PE_ERR_CONS	8	/*   Not a constructor form */
#define	PE_ERR_TYPE	9	/*   Class/ID mismatch in constructor */
#define	PE_ERR_OID	10	/*   Malformed object identifier */
#define	PE_ERR_BITS	11	/*   Malformed bitstring */
#define PE_ERR_NOSUPP	12	/*   Type not supported */
#define	PE_ERR_SIGNED	13	/*   Signed integer not expected */

				/* for the PSAP */
    int	    pe_context;		/* indirect reference */
#define	PE_DFLT_CTX	0	/*   the default context */

#define	PE_ID_XTND	0x1f	/* distinguished ID for extension bits */
#define	PE_ID_MORE	0x80	/* more to come flag */
#define	PE_ID_MASK	0x7f	/* value in extension byte */
#define	PE_ID_SHIFT	7

#define	PE_CLASS_MASK	0xc0	/* identifier class bits (8-7) */
#define	PE_CLASS_SHIFT	6
#define	PE_FORM_MASK	0x20	/* identifier form bit (6) */
#define	PE_FORM_SHIFT	5
#define	PE_CODE_MASK	0x1f	/* identifier code bits (5-1) + ... */
#define	PE_CODE_SHIFT	0

    PElementClass	pe_class;
#define	PE_CLASS_UNIV	0x0	/*   Universal */
#define	PE_CLASS_APPL	0x1	/*   Application-wide */
#define	PE_CLASS_CONT	0x2	/*   Context-specific */
#define	PE_CLASS_PRIV	0x3	/*   Private-use */

    PElementForm	pe_form;
#define	PE_FORM_PRIM	0x0	/*   PRIMitive */
#define	PE_FORM_CONS	0x1	/*   CONStructor */
#define	PE_FORM_ICONS	0x2	/*   internal: Inline CONStructor */

    PElementID pe_id;		/* should be extensible, 14 bits for now */
				/* Pseudo Types */
#define	PE_UNIV_EOC	0x000	/*   End-of-contents */

				/* Built-in Types */
#define	PE_PRIM_BOOL	0x001	/*   Boolean */
#define	PE_PRIM_INT	0x002	/*   Integer */
#define	PE_PRIM_BITS	0x003	/*   Bitstring */
#define	PE_PRIM_OCTS	0x004	/*   Octetstring */
#define	PE_PRIM_NULL	0x005	/*   Null */
#define	PE_PRIM_OID	0x006	/*   Object identifier */
#define	PE_PRIM_ODE	0x007	/*   Object descriptor */
#define	PE_CONS_EXTN	0x008	/*   External */
#define PE_PRIM_REAL	0x009	/*   Real */
#define PE_PRIM_ENUM	0x00a	/*   Enumerated type */
#define PE_PRIM_ENCR	0x00b	/*   Encrypted */
#define	PE_CONS_SEQ	0x010	/*   Sequence */
#define	PE_CONS_SET	0x011	/*   Set */

				/* Defined Types */
#define	PE_DEFN_NUMS	0x012	/*   Numeric String */
#define	PE_DEFN_PRTS	0x013	/*   Printable String */
#define	PE_DEFN_T61S	0x014	/*   T.61 String */
#define	PE_DEFN_VTXS	0x015	/*   Videotex String */
#define	PE_DEFN_IA5S	0x016	/*   IA5 String */
#define	PE_DEFN_UTCT	0x017	/*   UTC Time */
#define	PE_DEFN_GENT	0x018	/*   Generalized Time */
#define	PE_DEFN_GFXS	0x019	/*   Graphics string (ISO2375) */
#define	PE_DEFN_VISS	0x01a	/*   Visible string */
#define	PE_DEFN_GENS	0x01b	/*   General string */
#define	PE_DEFN_CHRS	0x01c	/*   Character string */

    PElementLen	pe_len;
#define	PE_LEN_XTND	0x80	/* long or indefinite form */
#define	PE_LEN_SMAX	127	/* largest short form */
#define	PE_LEN_MASK	0x7f	/* mask to get number of bytes in length */
#define	PE_LEN_INDF	(-1)	/* indefinite length */

    PElementLen	pe_ilen;

    union {
	PElementData	 un_pe_prim;	/* PRIMitive value */
	struct PElement *un_pe_cons;	/* CONStructor head */
    }                       pe_un1;
#define	pe_prim	pe_un1.un_pe_prim
#define	pe_cons	pe_un1.un_pe_cons

    union {
	int	    un_pe_cardinal;	/* cardinality of list */
	int	    un_pe_nbits;	/* number of bits in string */
    }			    pe_un2;
#define	pe_cardinal	pe_un2.un_pe_cardinal
#define	pe_nbits	pe_un2.un_pe_nbits

    int	    pe_inline;		/* for "ultra-efficient" PElements */
    char   *pe_realbase;	/*   .. */

    int	    pe_offset;		/* offset of element in sequence */

    struct PElement *pe_next;

    int	    pe_refcnt;		/* hack for ANYs in pepy */

#ifdef	DEBUG
    struct PElement *pe_link;	/* malloc debugging... */
#endif
}			PElement, *PE;
#define	NULLPE	((PE) 0)
#define	NULLPEP	((PE *) 0)

#define	pe_seterr(pe, e, v)	((pe) -> pe_errno = (e), (v))


extern	int	pe_allocs;
extern	int	pe_frees;
extern	int	pe_most;
extern	PE	pe_list;
#ifdef	DEBUG
extern	PE	pe_active;
#endif

PE	pe_alloc ();
int	pe_free ();
int	pe_cmp ();
PE	pe_cpy ();
int	pe_pullup ();
PE	pe_expunge ();
int	pe_extract ();

PE	str2pe ();
PE	qb2pe ();

extern int    pe_maxclass;
extern char  *pe_classlist[];

extern int    pe_maxuniv;
extern char  *pe_univlist[];

extern int    pe_maxappl;
extern char **pe_applist;

extern int    pe_maxpriv;
extern char **pe_privlist;

/*  */

typedef struct UTCtime {
    int	    ut_year;
    int	    ut_mon;
    int	    ut_mday;
    int	    ut_hour;
    int	    ut_min;
    int	    ut_sec;

    int	    ut_usec;

    int	    ut_zone;

    int	    ut_flags;
#define	UT_NULL		0x00
#define	UT_ZONE		0x01
#define	UT_SEC		0x02
#define	UT_USEC		0x04
}			UTCtime, *UTC;
#define	NULLUTC	((UTC) 0)


void	tm2ut ();
long	gtime ();
struct tm *ut2tm ();

/*  */

extern char *psapversion;


int	prim2flag ();
PE	flag2prim ();
#define	bool2prim(b)		flag2prim ((b), PE_CLASS_UNIV, PE_PRIM_BOOL)

integer	prim2num ();
PE	num2prim ();
#define	int2prim(i)		num2prim ((integer) (i), PE_CLASS_UNIV, PE_PRIM_INT)


#define	prim2enum(i)		prim2num((i))
#define enum2prim(a,b,c)	num2prim((a), (b), (c))
#define enumint2prim(i)		enum2prim ((i), PE_CLASS_UNIV, PE_PRIM_ENUM)


/* psap REAL parameters - tons of 'em */
#define PE_REAL_FLAGS	0300	/* flag bits of real */
#define 	PE_REAL_BINENC	0200	/* binary encoding */
#define 	PE_REAL_DECENC	0000	/* decimal encoding */
#define 	PE_REAL_SPECENC	0100	/* special encoding */

#define PE_REAL_B_S	0100	/* sign bit */
#define PE_REAL_B_BASE	0060	/* base bits */
#define 	PE_REAL_B_B2	0000
#define 	PE_REAL_B_B8	0020
#define 	PE_REAL_B_B16	0040

#define PE_REAL_B_F	0014	/* factor bits */
#define PE_REAL_B_EXP	0003	/* exponent type bits */
#define		PE_REAL_B_EF1	0000
#define 	PE_REAL_B_EF2	0001
#define		PE_REAL_B_EF3	0002
#define		PE_REAL_B_EF4	0003

#define	PE_REAL_PLUSINF		0200
#define PE_REAL_MINUSINF	0201

#ifndef HUGE
#ifdef MAXFLOAT
#define PE_REAL_INFINITY	MAXFLOAT
#else
#define	PE_REAL_INFINITY	99.e99
#endif
#else
#define PE_REAL_INFINITY	HUGE
#endif

double	prim2real ();
PE	real2prim ();
#define double2prim(i)		real2prim ((i), PE_CLASS_UNIV, PE_PRIM_REAL)


char   *prim2str ();
PE	str2prim ();
struct qbuf *prim2qb ();
PE	qb2prim_aux ();		/* really should be qb2pe () */
#define	qb2prim(q,c,i)		qb2prim_aux ((q), (c), (i), 0)
#define	oct2prim(s,len)		str2prim ((s), (len), \
					    PE_CLASS_UNIV, PE_PRIM_OCTS)
#define	ia5s2prim(s,len)	str2prim ((s), (len), \
					    PE_CLASS_UNIV, PE_DEFN_IA5S)
#define	nums2prim(s,len)	str2prim ((s), (len), \
					    PE_CLASS_UNIV, PE_DEFN_NUMS)
#define	prts2prim(s,len)	str2prim ((s), (len), \
					    PE_CLASS_UNIV, PE_DEFN_PRTS)
#define	t61s2prim(s,len)	str2prim ((s), (len), \
					    PE_CLASS_UNIV, PE_DEFN_T61S)
#define	vtxs2prim(s,len)	str2prim ((s), (len), \
					    PE_CLASS_UNIV, PE_DEFN_VTXS)
#define	gfxs2prim(s,len)	str2prim ((s), (len), \
					    PE_CLASS_UNIV, PE_DEFN_GFXS)
#define	viss2prim(s,len)	str2prim ((s), (len), \
					    PE_CLASS_UNIV, PE_DEFN_VISS)
#define	gens2prim(s,len)	str2prim ((s), (len), \
					    PE_CLASS_UNIV, PE_DEFN_GENS)
#define	chrs2prim(s,len)	str2prim ((s), (len), \
					    PE_CLASS_UNIV, PE_DEFN_CHRS)
#define	ode2prim(s,len)		str2prim ((s), (len), \
					    PE_CLASS_UNIV, PE_PRIM_ODE)

PE	prim2bit ();
PE	bit2prim ();

int	bit_on (), bit_off ();
int	bit_test ();

OID	prim2oid ();
PE	obj2prim ();
#define	oid2prim(o)		obj2prim ((o), PE_CLASS_UNIV, PE_PRIM_OID)

UTC	prim2time ();
#define	prim2utct(pe)		prim2time ((pe), 0)
#define	prim2gent(pe)		prim2time ((pe), 1)
PE	time2prim ();
#define	utct2prim(u)		time2prim ((u), 0, PE_CLASS_UNIV, PE_DEFN_UTCT)
#define	gent2prim(u)		time2prim ((u), 1, PE_CLASS_UNIV, PE_DEFN_GENT)
char   *time2str ();
#define	utct2str(u)		time2str ((u), 0)
#define	gent2str(u)		time2str ((u), 1)
UTC	str2utct (), str2gent ();

PE	prim2set ();
#define	set2prim(pe)		(pe)
int	set_add (), set_addon (), set_del ();
PE	set_find ();
#define	first_member(pe)	((pe) -> pe_cons)
#define	next_member(pe,p)	((p) -> pe_next)

#define	prim2seq(pe)		(prim2set (pe))
#define	seq2prim(pe)		(pe)
int	seq_add (), seq_addon (), seq_del ();
PE	seq_find ();


char   *pe_error ();

/*  */

typedef struct {
    int	    ps_errno;		/* Error codes */
#define	PS_ERR_NONE	 0	/*   No error */
#define	PS_ERR_OVERID	 1	/*   Overflow in ID */
#define	PS_ERR_OVERLEN	 2	/*   Overflow in length */
#define	PS_ERR_NMEM	 3	/*   Out of memory */
#define	PS_ERR_EOF	 4	/*   End of file */
#define	PS_ERR_EOFID	 5	/*   End of file reading extended ID */
#define	PS_ERR_EOFLEN	 6	/*   End of file reading extended length */
#define	PS_ERR_LEN	 7	/*   Length mismatch */
#define	PS_ERR_TRNC	 8	/*   Truncated */
#define	PS_ERR_INDF	 9	/*   Indefinite length in primitive form */
#define	PS_ERR_IO	10	/*   I/O error */
#define	PS_ERR_EXTRA	11	/*   Extraneous octets */
#define	PS_ERR_XXX	12	/*   XXX */

    union {
	caddr_t un_ps_addr;
	struct {
	    char   *st_ps_base;
	    int	    st_ps_cnt;
	    char   *st_ps_ptr;
	    int	    st_ps_bufsiz;
	}			un_ps_st;
	struct {
	    struct udvec *uv_ps_head;
	    struct udvec *uv_ps_cur;
	    struct udvec *uv_ps_end;
	    int	    uv_ps_elems;
	    int	    uv_ps_slop;
	    int	    uv_ps_cc;
	}			un_ps_uv;
    }                       ps_un;
#define	ps_addr	ps_un.un_ps_addr
#define	ps_base	ps_un.un_ps_st.st_ps_base
#define	ps_cnt	ps_un.un_ps_st.st_ps_cnt
#define	ps_ptr	ps_un.un_ps_st.st_ps_ptr
#define	ps_bufsiz	ps_un.un_ps_st.st_ps_bufsiz
#define	ps_head	ps_un.un_ps_uv.uv_ps_head
#define	ps_cur	ps_un.un_ps_uv.uv_ps_cur
#define	ps_end	ps_un.un_ps_uv.uv_ps_end
#define	ps_elems	ps_un.un_ps_uv.uv_ps_elems
#define	ps_slop	ps_un.un_ps_uv.uv_ps_slop
#define	ps_cc	ps_un.un_ps_uv.uv_ps_cc

    caddr_t ps_extra;		/* for George's recursive PStreams */

    int	    ps_inline;		/* for "ultra-efficient" PStreams */

    int	    ps_scratch;		/* XXX */

    int	    ps_byteno;		/* byte position */

    IFP	    ps_primeP;
    IFP	    ps_readP;
    IFP	    ps_writeP;
    IFP	    ps_flushP;
    IFP	    ps_closeP;
}			PStream, *PS;
#define	NULLPS	((PS) 0)

#define	ps_seterr(ps, e, v)	((ps) -> ps_errno = (e), (v))


PS	ps_alloc ();
void	ps_free ();

int	ps_io ();
#define	ps_read(ps, data, cc)	ps_io ((ps), (ps) -> ps_readP, (data), (cc), 0)
#define	ps_write(ps, data, cc)	ps_write_aux ((ps), (data), (cc), 0)
#define	ps_write_aux(ps, data, cc, in_line) \
    	ps_io ((ps), (ps) -> ps_writeP, (data), (cc), (in_line))

int	ps_flush ();

int	std_open ();
#define	std_setup(ps, fp)	((ps) -> ps_addr = (caddr_t) (fp), OK)

int	str_open ();
int	str_setup ();

int	dg_open ();
int	dg_setup ();

int	fdx_open ();
int	fdx_setup ();

int	qbuf_open ();
#define	qbuf_setup(ps, qb)	((ps) -> ps_addr = (caddr_t) (qb), OK)

#define	ts_open	dg_open
#define	ts_setup(p,f,s)		dg_setup ((p), (f), (s), ts_read, ts_write)
int	ts_read (), ts_write ();

int	uvec_open ();
int	uvec_setup ();


#define	ps2pe(ps)		ps2pe_aux ((ps), 1, 1)
PE	ps2pe_aux ();
#define	pe2ps(ps, pe)		pe2ps_aux ((ps), (pe), 1)
int	pe2ps_aux ();


PE	pl2pe ();
int	pe2pl ();


extern int    ps_len_strategy;
#define	PS_LEN_SPAG	0
#define	PS_LEN_INDF	1
#define	PS_LEN_LONG	2

int	ps_get_abs ();


char   *ps_error ();

/*  */

struct isobject {
    char   *io_descriptor;

    OIDentifier io_identity;
};

int	setisobject (),	endisobject ();

struct isobject *getisobject ();

struct isobject *getisobjectbyname ();
struct isobject *getisobjectbyoid ();

/*  */

extern	int	Len;
extern	char   *Qcp;
extern	char   *Ecp;

int	pe2qb_f ();


extern	int	Byteno;
extern	int	Qbrefs;
extern struct qbuf *Hqb;
extern struct qbuf *Fqb;
extern struct qbuf *Qb;

#define qbuf2pe(qb, len, result) (Byteno = 0, Hqb = qb, \
                                        Fqb = (Qb = (qb) -> qb_forw), \
                                        qbuf2pe_f (result))
PE	qbuf2pe_f ();
char   *qb2str ();
struct qbuf *str2qb ();
int	qb_free ();

int	pe2ssdu ();
PE	ssdu2pe ();

void	pe2text (), text2pe ();

int	pe2uvec ();

char   *int2strb ();
int	strb2int ();

PE	strb2bitstr ();
char   *bitstr2strb ();

/*  */

extern char PY_pepy[];

void	PY_advise ();
int	PY_pp ();

int	testdebug ();

int	vpush (), vpop ();
int	vname (), vtag ();
int	vprint ();
int	vstring (), vunknown ();

char   *bit2str ();

int	vpushfp (), vpopfp ();

int	vpushstr (), vpopstr ();

int	vpushpp (), vpopp ();

#ifdef PEPSY_VERSION
/* handle calls to the vunknown print routine */
#define vunknown_P 	0, ((modtyp *) 0)

/*
 * vpdu now comes out of the closet as a real pepsy function "pvpdu" we
 * keep the #define for vpdu for backwards compatiability of other peoples'
 * code
 */
#ifdef __STDC__
#define	vpdu(lp,fnx,pe,text,rw) \
{ \
    pvpdu ((lp), fnx##_P, (pe), (text), (rw)); \
}
#else   /* __STDC__ */
#define	vpdu(lp,fnx,pe,text,rw) \
{ \
    pvpdu ((lp), fnx/**/_P, (pe), (text), (rw)); \
}
#endif /* __STDC__ */

#else
	/* Backwards compatibility with posy/pepy */
#define vpdu(lp,fnx,pe,text,rw) \
{ \
    int    fnx (); \
 \
    _vpdu ((lp), fnx, (pe), (text), (rw)); \
}

int     _vpdu ();
#endif

/* pepy string definitions */
extern char *pepy_strings[];

#define	 PEPY_ERR_BAD 			(pepy_strings[0])
#define	 PEPY_ERR_BAD_BITS 		(pepy_strings[1])
#define	 PEPY_ERR_BAD_BOOLEAN 		(pepy_strings[2])
#define	 PEPY_ERR_BAD_CLASS 		(pepy_strings[3])
#define	 PEPY_ERR_BAD_CLASS_FORM_ID 	(pepy_strings[4])
#define	 PEPY_ERR_BAD_FORM 		(pepy_strings[5])
#define	 PEPY_ERR_BAD_INTEGER 		(pepy_strings[6])
#define	 PEPY_ERR_BAD_OID 		(pepy_strings[7])
#define	 PEPY_ERR_BAD_OCTET 		(pepy_strings[8])
#define	 PEPY_ERR_BAD_REAL 		(pepy_strings[9])
#define	 PEPY_ERR_BAD_SEQ 		(pepy_strings[10])
#define	 PEPY_ERR_BAD_SET 		(pepy_strings[11])
#define	 PEPY_ERR_TOO_MANY_BITS 	(pepy_strings[12])
#define	 PEPY_ERR_TOO_MANY_ELEMENTS 	(pepy_strings[13])
#define	 PEPY_ERR_UNKNOWN_CHOICE 	(pepy_strings[14])
#define	 PEPY_ERR_UNK_COMP 		(pepy_strings[15])
#define	 PEPY_ERR_INIT_FAILED 		(pepy_strings[16])
#define	 PEPY_ERR_INVALID_CHOICE 	(pepy_strings[17])
#define	 PEPY_ERR_MISSING 		(pepy_strings[18])
#define	 PEPY_ERR_NOMEM  		(pepy_strings[19])
#define	 PEPY_ERR_TOO_MANY_TAGGED 	(pepy_strings[20])
#define	 PEPY_ERR_EXTRA_MEMBERS 	(pepy_strings[21])

#endif
