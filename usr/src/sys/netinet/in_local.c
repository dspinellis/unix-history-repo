/*	in_local.c	4.1	83/03/13	*/

/*
 * Site specific Internet routines.
 */

#ifndef if_localmakeaddr
/*
 * Formulate an Internet address from network + host.  Handle
 * local subnet interpretation.  Used in building addresses
 * stored in the ifnet structure.
 */
struct in_addr
if_localmakeaddr(net, host)
	int net, host;
{
	u_long addr, subnet;

	subnet = net >> 24, net &= 0xffffff;
	if (net < 128)
		addr = (net << IN_CLASSA_NSHIFT) | host |
		  (subnet << IN_CLASSA_NSHIFT - 8);
	else if (net < 65536)
		addr = (net << IN_CLASSB_NSHIFT) | host |
		  (subnet << IN_CLASSB_NSHIFT - 8);
	else
		addr = (net << IN_CLASSC_NSHIFT) | host;
	addr = htonl(addr);
	return (*(struct in_addr *)&addr);
}
#endif
