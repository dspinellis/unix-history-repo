/*
 * This file implements functions for dealing with names in the XNS
 * environment.
 */

/*
 $Log:	names.c,v $
 * Revision 2.1  86/07/29  06:44:22  jqj
 * Most code in this module is now superseded by ns_nameof() and ns_addr()
 * routines, which are part of the standard libc in 4.3bsd.
 * 
 * Revision 2.0  85/11/21  07:22:14  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.2  85/10/21  12:58:41  root
 * Gould version:  eliminate various uses of ns_netof(), which doesn't
 * work on Gould.
 * 
 * Revision 1.3  85/03/11  16:37:13  jqj
 * Public alpha-test version, released 11 March 1985
 * 
 * Revision 1.2  85/01/27  07:37:32  jqj
 * finished but undebugged version
 * 
 */

#ifndef lint
static char rcsid[] = "$Header: names.c,v 2.1 86/07/29 06:44:22 jqj Exp $";
#endif

#include <sys/types.h>		/* for ns.h and socket.h */
#include <sys/socket.h>		/* for ns.h */
#include <netns/ns.h>		/* for XNS addresses */


struct ns_addr *
getXNSaddr(name)
	char *name;
{
	static struct ns_addr result;
	extern struct ns_addr ns_addr();

	result = ns_addr(name);
	return &result;
}
