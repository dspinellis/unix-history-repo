/* sym.h */

/* 
 * $Header: /f/osi/pepsy/RCS/sym.h,v 7.1 91/02/22 09:50:03 mrose Interim $
 *
 *
 * $Log:	sym.h,v $
 * Revision 7.1  91/02/22  09:50:03  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/07/01  19:54:45  mrose
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


typedef struct symlist {
    char   *sy_encpref;
    char   *sy_decpref;
    char   *sy_prfpref;
    char   *sy_module;
    char   *sy_name;

    YP	    sy_type;

    struct symlist *sy_next;
}		symlist, *SY;
#define	NULLSY	((SY) 0)

