/*	in_local.h	4.2	83/03/13	*/

/*
 * Internet definitions and variables
 * specific to installation at Berkeley.
 */

/*
 * Local subnetwork address mapping.
 * The standard scheme is:
 *	internet address = network.local-part
 * where
 *	network is between 8 and 34 bits
 *	local-part is between 8 and 24 bits
 *
 * This is modified by interpreting network as 32 bits
 * and local-part as something between 8 and 23 bits
 * depending on the high bit in the local-part.  When
 * the high bit in the local-part is a 1, the upper byte
 * is interpreted as a local network extension, and used
 * as the high byte in the network (extending it to 32 bits).
 * The additional 8 bits of network number are administered
 * locally and are not visible outside Berkeley, since
 * they're part of the local-part.
 */
/* network mappings */
#define	CLASSA_LOCALNETMAP(n, in) \
	(in) & 0x800000 ? (n) | (((in) & 0xef0000) << 8) : (n)
#define	CLASSB_LOCALNETMAP(n, in) \
	(in) & 0x8000 ? (n) | (((in) & 0xef00) << 16) : (n)
#define	CLASSC_LOCALNETMAP(n, in)	(n)

/* local-part mappings */
#define	CLASSA_LOCALHOSTMAP(h, in) \
	(in) & 0x800000 ? (h) & ~0xef0000 : (h)
#define	CLASSB_LOCALHOSTMAP(h, in) \
	(in) & 0x8000 ? (h) & ~0xef00 : (h)
#define	CLASSC_LOCALHOSTMAP(h, in)	(h)
