/* oid.h - object identifier stuff */

/*
 * $Header: /f/osi/h/quipu/RCS/oid.h,v 7.4 91/02/22 09:26:01 mrose Interim $
 *
 *
 * $Log:	oid.h,v $
 * Revision 7.4  91/02/22  09:26:01  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/11/20  15:30:55  mrose
 * cjr
 * 
 * Revision 7.2  90/10/17  11:46:34  mrose
 * sync
 * 
 * Revision 7.1  90/07/09  14:38:37  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:56:41  mrose
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


#ifndef QUIPUOID
#define QUIPUOID

#include "psap.h"

/* Definitions of OID's the DSA needs to know */
	/* X500 defined attributes */
#define OBJECTCLASS_OID "2.5.4.0"
#define ALIAS_OID	"2.5.4.1"
#define CN_OID		"2.5.4.3"
#define DSAADDRESS_OID	"2.5.4.29"
#define APPLCTX_OID 	"2.5.4.30"
#define PASSWORD_OID 	"2.5.4.35"
#define CERTIFICATE_OID	"2.5.4.36"
	/* QUIPU defined attributes */
#define SCHEMA_OID	"0.9.2342.19200300.99.1.1"	
#define ACL_OID		"0.9.2342.19200300.99.1.2"
#define EDBINFO_OID	"0.9.2342.19200300.99.1.3"
#define MASTERDSA_OID	"0.9.2342.19200300.99.1.4"
#define SLAVEDSA_OID	"0.9.2342.19200300.99.1.5"
#define CONTROL_OID 	"0.9.2342.19200300.99.1.15"
#define VERSION_OID 	"0.9.2342.19200300.99.1.16"
#define PROTECTED_OID	"0.9.2342.19200300.99.1.17"
#define INHERIT_OID	"0.9.2342.19200300.99.1.21"
#define RELAYDSA_OID	"0.9.2342.19200300.99.1.23"
#define SUBORD_OID	"0.9.2342.19200300.99.1.25"
#define XREF_OID	"0.9.2342.19200300.99.1.26"
#define NSSR_OID	"0.9.2342.19200300.99.1.27"
#define LISTEN_OID	"0.9.2342.19200300.99.1.28"
	/* THORN defined attribute */
#define MANAGER_OID	"0.9.2342.19200300.100.1.10"
#define LAST_MOD_OID	"0.9.2342.19200300.100.1.23"
#define MOD_BY_OID	"0.9.2342.19200300.100.1.24"
	/* NON leaf object class */
#define QUIPU_DSA	"0.9.2342.19200300.99.3.1"
#define NONLEAFOBJECT	"0.9.2342.19200300.99.3.6"
#define EXTERNOBJECT	"0.9.2342.19200300.99.3.9"
	/* alias objectclass */
#define ALIAS_OC	"2.5.6.1"
#define TOP_OC		"2.5.6.0"

	/* X500 defined protocol oids */
#ifdef USE_BUILTIN_OIDS
#define DIR_ACCESS_AC	str2oid("2.5.3.1")
#define DIR_SYSTEM_AC	str2oid("2.5.3.2")
#define DIR_QUIPU_AC	str2oid("0.9.2342.19200300.99.4")
#define DIR_ACCESS_AS	str2oid("2.5.9.1")
#define DIR_SYSTEM_AS	str2oid("2.5.9.2")
#define DIR_QUIPU_AS	str2oid("0.9.2342.19200300.99.5")
#define DIR_ACSE	str2oid("2.2.1.0.1")
#else 	/* use isobjects */
#define DIR_ACCESS_AC	ode2oid("directory directoryAccessAC")
#define DIR_SYSTEM_AC	ode2oid("directory directorySystemAC")
#define DIR_QUIPU_AC	str2oid("0.9.2342.19200300.99.4")
#define DIR_ACCESS_AS	ode2oid("directory directoryAccessAS")
#define DIR_SYSTEM_AS	ode2oid("directory directorySystemAS")
#define DIR_QUIPU_AS	str2oid("0.9.2342.19200300.99.5")
#define DIR_ACSE	ode2oid("acse pci version 1")
#endif

/* Wrong file for the following, but they are connected to the above so... */
#define DIR_ACCESS_PC_ID	1
#define DIR_SYSTEM_PC_ID	1
#define DIR_QUIPU_PC_ID		1
#define DIR_ACSE_PC_ID		3

/* oid table lookup definitions */
#define SEPERATOR ':'
#define DOT '.'
#define COMMA ','
#define COMMENT '#'

#define BUFSIZE 40
#define TABLESIZE 300

#define OIDPART         1
#define OIDFULL         2
#define OIDNUM          3

typedef struct {
	char            *ot_name;
	char            *ot_stroid;
	OID             ot_oid;
	OID             ot_aliasoid;
} oid_table;
#define NULLTABLE ((oid_table * )0)

typedef struct {
	oid_table       oa_ot;
	short		oa_syntax;
} oid_table_attr;
#define NULLTABLE_ATTR ((oid_table_attr *)0)


typedef struct seq_tab {
	oid_table_attr * ts_oa;
	struct seq_tab * ts_next;
} * table_seq;
#define NULLTABLE_SEQ ((table_seq)0)

struct oc_seq {
   struct _objclass *os_oc;
   struct oc_seq    *os_next;
};
#define NULLOCSEQ ((struct oc_seq*) 0)

typedef struct _objclass {
	oid_table        oc_ot;
	struct oc_seq *  oc_hierachy;
	table_seq        oc_must;
	table_seq        oc_may;
} objectclass;
#define NULLOBJECTCLASS ((objectclass * )0)

#define objclass_cmp(x,y)	( x == y ? 0 : ( x > y ? -1 : 1 ))

oid_table_attr * oid2attr();
oid_table_attr * name2attr();
char * attr2name();
#define attr2name_aux(x)	((x) ? (x)->oa_ot.ot_name : NULLCP)

objectclass * oid2oc();
objectclass * name2oc();
char * oc2name();

char * oid2name();      /* find oid wherever it is hiding !!! */
OID    name2oid();

char * SkipSpace ();
void   StripSpace ();

#endif
