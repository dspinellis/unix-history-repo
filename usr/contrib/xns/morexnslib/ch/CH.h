/* $Header: CH.h,v 2.0 85/11/21 07:22:27 jqj Exp $ */
/*
 * $Log:	CH.h,v $
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
extern Clearinghouse2_ObjectName CH_StringToName();
extern CH_Enumerate(), CH_EnumerateAliases();
