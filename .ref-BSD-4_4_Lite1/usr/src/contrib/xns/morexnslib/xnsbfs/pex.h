/* $Header: pex.h,v 1.1 86/06/27 13:14:37 jqj Exp $ */
/* $Log:	pex.h,v $
 * Revision 1.1  86/06/27  13:14:37  jqj
 * Initial revision
 * 
 */

/*	pex.h	1.1	85/01/26	*/

/*
 * Packet Exchange protocol header.
 * See XSIS 028112, December 1981
 */
struct pex {
	u_short	ph_idh, ph_idl;		/* `unique' transaction identifier */
	u_short	ph_client;		/* client type field */
};
