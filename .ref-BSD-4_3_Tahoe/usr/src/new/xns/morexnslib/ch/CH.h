/* $Header: CH.h,v 1.1 87/05/11 10:00:00 ed Exp $ */
/*
 * $Log:	CH.h,v $
 * Revision 1.1  87/05/11  10:00:00  ed
 * Initial revision
 * 
 * Revision 2.2  87/04/01  11:03:03  jqj
 * added CH_NameToString and CH_NameToUser
 * 
 * Revision 2.1  86/12/10  16:34:39  ed
 * Webster changes
 * 
 * Revision 2.1  86/12/10  16:34:39  ed
 * Decide on correct Clearinghouse version until V3 is implemented
 * consistently.
 * 
 * Revision 2.0  85/11/21  07:22:27  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.1  85/11/21  07:01:27  root
 * Initial revision
 * 
 */
/*
 * definitions for routines distributed as part of the Clearinghouse
 * support package.  See clearinghouse(3) for documentation.
 */
/*
 * other include files needed:
 * #include <sys/types.h>
 * #include <netns/ns.h>
 * #include <courier/Clearinghouse2.h>
 */
extern struct ns_addr *CH_LookupAddr(), *CH_LookupAddrDN(); 
extern CourierConnection* CH_GetFirstCH(),
	*CH_GetOtherCH();
#ifdef __Clearinghouse2
extern Clearinghouse2_ObjectName CH_StringToName();
#endif
#ifdef __Clearinghouse3
extern Clearinghouse3_ObjectName CH_StringToName();
#endif
extern CH_Enumerate(), CH_EnumerateAliases();
extern char *CH_NameToString(), *CH_NameToUser();
