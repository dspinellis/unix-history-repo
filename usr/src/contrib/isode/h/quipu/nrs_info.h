/* nrs_info.h - attribute structure for representing NRS information */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#ifndef QUIPUNRSINFO
#define QUIPUNRSINFO

#include "psap.h"

struct str_seq
{
	char		* ss_str;
	struct str_seq	* ss_next;
};

/* Defined values for contexts */
#define	NRS_Context_UNKNOWN	-1
#define	NRS_Context_X29	0
#define	NRS_Context_TS29	1
#define	NRS_Context_NIFTP	2
#define	NRS_Context_MAIL_NIFTP	3
#define	NRS_Context_NOT_USED	4
#define	NRS_Context_MAIL_TELEX	5
#define	NRS_Context_JTMP	6
#define	NRS_Context_JTMP_FILES	7
#define	NRS_Context_JTMP_REG	8
#define	NRS_Context_YBTS_NODE	9
#define	NRS_Context_YBTS	10
#define	NRS_Context_FTAM	11
#define	NRS_Context_JTM	12
#define	NRS_Context_JTM_REG	13
#define	NRS_Context_VT	14
#define	NRS_Context_MOTIS	15

/* Defined values for address space identifiers */
#define	NRS_Address_Space_Id_PSS	0
#define	NRS_Address_Space_Id_JANET	1
#define	NRS_Address_Space_Id_TELEX	2
#define	NRS_Address_Space_Id_OSI_CONS	3

struct addr_info
{
	int			  addr_info_type;
#define	ADDR_INFO_DTE_ONLY			1
#define	ADDR_INFO_DTE_APPLIC_INFO		2
#define	ADDR_INFO_DTE_CUDF			3
#define	ADDR_INFO_DTE_CUDF_APPLIC_INFO		4
#define	ADDR_INFO_DTE_YBTS			5
#define	ADDR_INFO_DTE_YBTS_APPLIC_INFO		6
#define	ADDR_INFO_DTE_YBTS_APPLIC_RELAY		7
#define	ADDR_INFO_NONE_NEEDED			8
#define	ADDR_INFO_OSI_ADDRESSING		9
#define	ADDR_INFO_OSI_NSAP_ONLY			10
#define	ADDR_INFO_OSI_NSAP_APPLIC_INFO		11
#define	ADDR_INFO_OSI_NSAP_APPLIC_RELAY		12
#define	ADDR_INFO_DTE_YBTS_OSI_ADDRESSING	13

	char		* dte_number;
	char		* cudf;
	char		* ybts_string;
	char		* nsap;
	char		* tselector;
	char		* sselector;
	char		* pselector;
	PE		  place_holder;
	PE		  application_title;
	PE		  per_app_context_info;
	struct str_seq	* applic_info;
	struct str_seq	* applic_relay;
};

struct nrs_routes
{
	PE			  cost;
	struct addr_info	* addr_info;
	struct nrs_routes	* next;
};

struct nrs_info
{
	int			  context;
	int			  addr_sp_id;
	struct nrs_routes	* routes;
};

#endif
