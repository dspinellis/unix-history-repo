/* attrvalue.h - */

/*
 * $Header: /f/osi/h/quipu/RCS/attrvalue.h,v 7.3 91/02/22 09:25:25 mrose Interim $
 *
 *
 *
 * $Log:	attrvalue.h,v $
 * Revision 7.3  91/02/22  09:25:25  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/11/20  15:30:18  mrose
 * cjr
 * 
 * Revision 7.1  90/10/17  11:46:14  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:56:22  mrose
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


#ifndef ATTRVALUE
#define ATTRVALUE
#include "quipu/name.h"

typedef struct avseqcomp {      /* attribute may have multiple values   */
				/* respresents SET OF AttributeValue    */
    attrVal      	avseq_av;
    struct avseqcomp    *avseq_next;
} avseqcomp, *AV_Sequence;

#define NULLAV ((AV_Sequence) 0)
#define avs_comp_alloc()	(AV_Sequence) smalloc(sizeof(avseqcomp))
#define avs_comp_print(x,y,z)	AttrV_print (x,&(y)->avseq_av,z)
#define avs_cmp_comp(x,y)	AttrV_cmp (&x->avseq_av ,&y->avseq_av)

AV_Sequence  avs_comp_new ();
AV_Sequence  avs_comp_cpy ();
AV_Sequence  avs_cpy ();
AV_Sequence  avs_merge ();
AV_Sequence  str2avs ();

typedef struct attrcomp {       /* A sequence of attributes             */
				/* represents Attribute                 */
				/* and SET OF Attribute                 */
    attrType       	attr_type;
    AV_Sequence         attr_value;
    struct attrcomp     *attr_link;
				/* ACL is NOT for use by DUA            */
				/* this must be done by use of ACL      */
				/* attribute                            */
    struct acl_info     *attr_acl;
} attrcomp, *Attr_Sequence;

#define NULLATTR ((Attr_Sequence) 0)
#define as_comp_alloc()          (Attr_Sequence) smalloc(sizeof(attrcomp))
#define as_comp_cmp(x,y)      (((oid_cmp (&x->attr_type ,&y->attr_type) == OK) && (avs_cmp (x->attr_value ,y->attr_value) == OK)) ? OK : NOTOK)

Attr_Sequence  as_comp_new ();
Attr_Sequence  as_comp_cpy ();
Attr_Sequence  as_cpy ();
Attr_Sequence  as_find_type ();
Attr_Sequence  as_merge ();
Attr_Sequence  str2as();


				/* ACL is defined here as it is         */
				/* referenced.   it is only used by     */
				/* DSA                                  */
				/* represents ACLInfo defined by INCA   */
struct acl_info {
    int acl_categories;
#define ACL_NONE        0
#define ACL_DETECT      1
#define ACL_COMPARE     2
#define ACL_READ        3
#define ACL_ADD         4
#define ACL_WRITE       5
    int  acl_selector_type;
#define ACL_ENTRY       1
#define ACL_OTHER       2
#define ACL_PREFIX      3
#define ACL_GROUP       4
    struct dn_seq       *acl_name;    /* prefix and group only         */
    struct acl_info     *acl_next;
};

#define NULLACL_INFO (struct acl_info *) NULL
#define acl_info_alloc()        (struct acl_info *) smalloc (sizeof  (struct acl_info))
#define acl_info_fill(w,x,y,z)    w -> acl_categories = x ;  \
				  w -> acl_selector_type = y; \
				  w -> acl_name = z;
struct acl_info *acl_info_new ();
struct acl_info *acl_info_cpy();
struct acl_info *acl_default();
struct acl_info *acl_dflt();

struct mailbox {
	char *	mtype;
	char * 	mbox;
};

struct fax {
    char    *number;
    PE     bits;
    char *   fax_bits;
    int	     fax_len;
};

struct postaddr {
	int  addrtype;   /* 1 == T61, 2 == Printstr */
	char * addrcomp;
	struct postaddr * pa_next;
};

struct telex {
	char * telexnumber;
	char * countrycode;
	char * answerback;
};

struct teletex {
	char    *terminal;
	char    *graphic;
	char    *control;
	char    *page;
	char    *misc;
	char    *t_private;
};

struct pref_deliv {
    	int	deliv;
	struct pref_deliv * pd_next;
};


struct Guide {
	OID objectClass;
	struct Criteria * criteria;
};

struct Criteria {
    int     offset;
#define	Criteria_type	1
#define	Criteria_and	2
#define	Criteria_or	3
#define	Criteria_not	4

    union {
            struct CriteriaItem *type;
            struct and_or_set {
                    struct Criteria *and_or_comp;
                    struct and_or_set *and_or_next;
            } *and_or;
            struct Criteria *not;
    }       un;
};

struct CriteriaItem {
    int  offset;
#define	choice_equality		1
#define	choice_substrings	2
#define	choice_greaterOrEqual	3
#define	choice_lessOrEqual	4
#define	choice_approximateMatch	5
    AttributeType attrib;
};


/* Upper bounds */
#define UB_POSTAL_LINE		6
#define UB_POSTAL_STRING 	30
#define UB_TELETEX_TERMINAL_ID 	1024
#define UB_TELEPHONE_NUMBER	32
#define UB_TELEX_NUMBER		14
#define UB_ANSWERBACK		8
#define UB_COUNTRY_CODE		4

typedef struct _InheritAttr {
	Attr_Sequence 	i_default;	
	Attr_Sequence 	i_always;	
	OID		i_oid;
} * InheritAttr;
#define NULLINHERIT ((InheritAttr)NULL)

struct CIList {
	char *	l_str;
	struct CIList * l_next;
	int 	l_type;   /* 1 == T61, 2 == Printstr */
};
#define NULLCILIST (struct CIList *)NULL

struct documentStore {
    int	    ds_method;
#define	DS_UNK	(-1)
#define	DS_FTP	0
#define	DS_FTAM	1

    char   *ds_host;
    char   *ds_dir;
    char   *ds_file;
};

#endif
