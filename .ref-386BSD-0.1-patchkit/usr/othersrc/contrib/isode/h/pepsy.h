/* pepsy.h */

/* 
 * $Header: /f/osi/h/RCS/pepsy.h,v 7.9 91/02/22 09:24:51 mrose Interim $
 *
 *
 * $Log:	pepsy.h,v $
 * Revision 7.9  91/02/22  09:24:51  mrose
 * Interim 6.8
 * 
 * Revision 7.8  90/12/23  18:41:51  mrose
 * update
 * 
 * Revision 7.7  90/12/11  10:33:01  mrose
 * sync
 * 
 * Revision 7.5  90/11/11  10:01:21  mrose
 * touch-up
 * 
 * Revision 7.4  90/11/04  19:17:54  mrose
 * update
 * 
 * Revision 7.3  90/08/18  00:43:47  mrose
 * touch-up
 * 
 * Revision 7.2  90/08/08  14:12:29  mrose
 * update
 * 
 * Revision 7.1  90/07/09  14:37:55  mrose
 * sync
 * 
 * Revision 7.0  90/07/01  19:52:37  mrose
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


#ifndef PEPSY_DEFINITIONS
#define PEPSY_DEFINITIONS

#ifndef	PEPYPARM
#define PEPYPARM	char *
#endif

/*
 * Definitions for pep tables
 */

typedef struct	{
	int	pe_type;	/* Type of entry */
	integer	pe_ucode;	/* index to user's code if any */
	int	pe_tag;		/* Tag of this entry if any */
	int	pe_flags;	/* Flags */
}	tpe;
#define NULLTPE	((tpe *)0)

typedef struct	{
	int	pe_type;	/* Type of entry */
	integer	pe_ucode;	/* index to user's code if any */
	int	pe_tag;		/* Tag of this entry if any */
	int	pe_flags;	/* Flags */
	char *pe_typename; /* User defined name of variable */
}	ptpe;
#define NULLPTPE	((ptpe *)0)

/* extract a pointer from the pointer table */
#define GPTR(mod, ind, type)	  ((type )(mod)->md_ptrtab[ind])
/* tricky situation with the "type" - it must not contain the brackets of the
 * cast because we supply them here 
 */

/* macros for getting values of default flexibly */
#define IVAL(mod, x)      ((x)->pe_ucode)         /* Integer value */
#define PVAL(mod, x)      (GPTR(mod, (x)->pe_tag, char *)) /* (char *) */
#define TVAL(mod, x)      ((x)->pe_flags)         /* Type - Integer value */
#define RVAL(mod, x)	  (*GPTR(mod, (x)->pe_tag, double *))  /* double */

/* macros for getting the function pointer (for a FN_CALL entry) */
#define FN_PTR(mod, x)	  (*GPTR(mod, (x)->pe_ucode, IFP ))  /* function ptr */

/* macros for getting other more general pointers transparently */
#define EXT2MOD(mod, x)	   (GPTR(mod, (x)->pe_ucode, modtyp *))

/* Types */
#define PE_START	(-1)
#define	PE_END		0
#define XOBJECT		3
#define	UCODE		6
#define MEMALLOC	7
#define	SCTRL		8
#define	CH_ACT		9
#define OPTL		10	/* Optionals field offset */
#define BOPTIONAL	11 /* optional test for next */
#define FFN_CALL        12 /* call the free function */
#define FREE_ONLY       13 /* the next item is only for freeing code */

/* types that generate data */
#define TYPE_DATA	20

#define ANY			(TYPE_DATA + 0)
#define INTEGER		(TYPE_DATA + 1)
#define BOOLEAN		(TYPE_DATA + 2)
#define OBJECT		(TYPE_DATA + 3)		/* This generates data */
#define BITSTRING	(TYPE_DATA + 4)
#define OCTETSTRING	(TYPE_DATA + 5)
#define SET_START	(TYPE_DATA + 6)
#define	SEQ_START	(TYPE_DATA + 7)
#define SEQOF_START	(TYPE_DATA + 8)
#define SETOF_START	(TYPE_DATA + 9)
#define CHOICE_START	(TYPE_DATA + 10)
#define REALTYPE	(TYPE_DATA + 11)	/* ASN.1 Real */
#define T_NULL		(TYPE_DATA + 12)
#define T_OID		(TYPE_DATA + 13)
#define ETAG		(TYPE_DATA + 14) /*so set_find in pr_set is executed*/
#define IMP_OBJ		(TYPE_DATA + 15) /*so set_find in pr_set is executed*/
#define EXTOBJ		(TYPE_DATA + 16) /* External reference object */
#define EXTMOD		(TYPE_DATA + 17) /* External module for above object */
#define OBJID		(TYPE_DATA + 18) /* Object Identifier */
#define DFLT_F		(TYPE_DATA + 19)/* Default value for following entry */
#define DFLT_B		(TYPE_DATA + 20)/* default value for previous entry */
#define T_STRING	(TYPE_DATA + 21)/* [[ s ptr ]] supporting entry */
#define OCTET_PTR	(TYPE_DATA + 22)/* [[ o ptr $ len]] str entry */
#define OCTET_LEN	(TYPE_DATA + 23)/* [[ o ptr $ len]] len entry */
#define BITSTR_PTR	(TYPE_DATA + 24)/* [[ x ptr $ len]] str entry */
#define BITSTR_LEN	(TYPE_DATA + 25)/* [[ x ptr $ len]] len entry */
#define FN_CALL         (TYPE_DATA + 26)/* call a function to do the work */

#if PEPSY_VERSION >= 2
#define STYPE_DATA	(TYPE_DATA + 50) /* Below here are the S* items */
#else
#define STYPE_DATA	(TYPE_DATA + 30) /* Below here are the S* items */
#endif
/* Optimised - No indirection to cope with ISODE optimisation */
#define SBITSTRING	(STYPE_DATA + 0) /* No offset */
#define SOBJID		(STYPE_DATA + 1) /* Object Identifier - No offset */
#define SREALTYPE	(STYPE_DATA + 2) /* ASN.1 Real */
#define SANY		(STYPE_DATA + 3)
#define SEXTOBJ		(STYPE_DATA + 4) /* External reference object */
#define SOBJECT		(STYPE_DATA + 5)	/* This generates data */
#define SOCTETSTRING	(STYPE_DATA + 6) /* No offset */
#define SEXTERNAL	(STYPE_DATA + 7) /* External to be encoded */
#define SSEQ_START	(STYPE_DATA + 8) /* SEQUENCE don't indirect */
#define SSET_START	(STYPE_DATA + 9) /* SET don't do an indirection */
#define SSEQOF_START	(STYPE_DATA + 10) /* SEQOF - no indirection */
#define SSETOF_START	(STYPE_DATA + 11) /* SETOF - no indirection */
#define SCHOICE_START	(STYPE_DATA + 12) /* CHOICE - no indirection */

#define ISDTYPE(p)	(p->pe_type >= TYPE_DATA)

/* User code indexes */

#define NONE		0		/* No User code */
/* Standard Tags */

#define T_BOOL		1
#define T_INTEGER	2
#define T_OCTETSTRING	4
#define T_OBJIDENT	6
#define T_EXTERNAL	8
#define T_REAL		9
#define T_ENUMERATED	10
#define T_SEQ		16
#define T_SET		17
#define T_IA5		22
#define T_GRAPHIC	25

/* Flags */

/* Use values 0-3 bottom two bits at the moment */
/* This has to be changed if the values for the 4 below are changed in ISODE */
#define FL_CLASS	0xf	
/* Class specification */
#define FL_UNIVERSAL	PE_CLASS_UNIV
#define FL_APPLICATION	PE_CLASS_APPL
#define FL_CONTEXT	PE_CLASS_CONT
#define FL_PRIVATE	PE_CLASS_PRIV


#define FL_IMPLICIT	0100
#define FL_DEFAULT	0200
#define FL_OPTIONAL	0400

#define OPTIONAL(p)		((p)->pe_flags & FL_OPTIONAL)
#define DEFAULT(p)		((p)->pe_flags & FL_DEFAULT)

#define FL_PRTAG	01000	/* only for printing - print the tag */

#define PRINT_TAG(p)		((p)->pe_flags & FL_PRTAG)

#define FL_USELECT	02000	/* user code selects */

#define IF_USELECT(p)		((p) -> pe_flags & FL_USELECT)


#define CLASS(p)	((PElementClass)((p)->pe_flags & FL_CLASS))
#define TAG(p)		((PElementID)((p)->pe_tag))

/* signed version of above - needed for tag == -1 */
#define STAG(p)		((p)->pe_tag)

#define TESTBIT(p, bit)	((p) & (1 << (bit)))
#define SETBIT(p, bit)	((p) |= (1 << (bit)))
#define CLRBIT(p, bit)	((p) &= ~(1 << (bit)))

/* To support the OPTIONAL << field $ bitno >> construct */
#define BITTEST(p, bit)	(((p)[(bit)/8]) & (0x80 >> (bit) % 8))
#define BITSET(p, bit)	(((p)[(bit)/8]) |= (0x80 >> (bit) % 8))
#define BITCLR(p, bit)	(((p)[(bit)/8]) &= ~(0x80 >> (bit) % 8))

/* compute the number of char's required to support x bits */
#define NBITS2NCHARS(x)		(((x) + 7)/8)

/*
 * The module table. One per module which gives access to everything one needs
 * to know about the modules types
 */

typedef	struct	{
	char	*md_name;	/* Name of this module */
	int	md_nentries;	/* Number of entries */
	tpe	**md_etab;	/* Pointer to encoding tables */
	tpe	**md_dtab;	/* Pointer to decoding tables */
	ptpe    **md_ptab;	/* Pointer to printing tables */
	int	(*md_eucode)();	/* User code for encoding */
	int	(*md_ducode)();	/* User code for decoding */
	int	(*md_pucode)();	/* User code for printing */
	caddr_t	*md_ptrtab;	/* pointer table */

	}	modtyp;
#define NULLMODTYP	((modtyp *)0)


#define	LOTSOFBITS	128


int	enc_f (), dec_f (), fre_obj ();
#endif
