/*
 *			THIS CODE IS NOT FOR DISTRIBUTION!
 *	KEEP YOUR GRUBBY HANDS OFF UNLESS AUTHORIZED BY VAN JACOBSON TO COPY!
 *			ASK SAM, MIKE, OR BILL ABOUT IT.
 */

/*
 * Definitions for tcp compression routines.
 *
 * Copyright (c) 1988, 1989 by Van Jacobson, Lawrence Berkeley Laboratory
 * All rights reserved.
 *
 * $Header: slcompress.h,v 1.3 89/03/19 18:10:38 van Locked $
 */

#define MAX_STATES 16		/* must be > 2 and < 256 */
#define MAX_HDR MLEN		/* XXX 4bsd-ism: should really be 128 */

/*
 * Compressed packet format:
 *
 * The first octet contains the packet type (top 3 bits), TCP
 * 'push' bit, and flags that indicate which of the 4 TCP sequence
 * numbers have changed (bottom 5 bits).  The next octet is a
 * conversation number that associates a saved IP/TCP header with
 * the compressed packet.  The next two octets are the TCP checksum
 * from the original datagram.  The next 0 to 15 octets are
 * sequence number changes, one change per bit set in the header
 * (there may be no changes and there are two special cases where
 * the receiver implicitly knows what changed -- see below).
 *
 * { Note that the ip version number field this overlays is 4 bits
 *   wide, and that we use type 4 to pass thru unaltered packets,
 *   type 5 to pass thru uncompressed packets that will be state
 *   information indexed by conversation. If msb (e.g type 8) is
 *   set, the other type bits are stolen to encode the difference
 *   information of a compressed TCP packet. -wfj }
 * 
 * There are 5 numbers which can change (they are always inserted
 * in the following order): TCP urgent pointer, window,
 * acknowlegement, sequence number and IP ID.  (The urgent pointer
 * is different from the others in that its value is sent, not the
 * change in value.)  Since typical use of SLIP links is biased
 * toward small packets (see comments on MTU/MSS below), changes
 * use a variable length coding with one octet for numbers in the
 * range 1 - 255 and 3 octets (0, MSB, LSB) for numbers in the
 * range 256 - 65535 or 0.  (If the change in sequence number or
 * ack is more than 65535, an uncompressed packet is sent.)
 */

/*
 * Packet types (must not conflict with IP protocol version)
 *
 * The top nibble of the first octet is the packet type.  There are
 * three possible types: IP (not proto TCP or tcp with one of the
 * control flags set); uncompressed TCP (a normal IP/TCP packet but
 * with the 8-bit protocol field replaced by an 8-bit connection id --
 * this type of packet syncs the sender & receiver); and compressed
 * TCP (described above).
 *
 * LSB of 4-bit field is TCP "PUSH" bit (a worthless anachronism) and
 * is logically part of the 4-bit "changes" field that follows.  Top
 * three bits are actual packet type.  For backward compatibility
 * and in the interest of conserving bits, numbers are chosen so the
 * IP protocol version number (4) which normally appears in this nibble
 * means "IP packet".
 */

/* packet types */
#define TYPE_IP 0x40
#define TYPE_UNCOMPRESSED_TCP 0x50
#define TYPE_COMPRESSED_TCP 0x80
#define TYPE_ERROR 0x00

/* Bits in first octet of compressed packet */
#define NEW_C	0x40	/* flag bits for what changed in a packet */
#define NEW_I	0x20
#define NEW_S	0x08
#define NEW_A	0x04
#define NEW_W	0x02
#define NEW_U	0x01

/* reserved, special-case values of above */
#define SPECIAL_I (NEW_S|NEW_W|NEW_U)		/* echoed interactive traffic */
#define SPECIAL_D (NEW_S|NEW_A|NEW_W|NEW_U)	/* unidirectional data */
#define SPECIALS_MASK (NEW_S|NEW_A|NEW_W|NEW_U)

#define TCP_PUSH_BIT 0x10


/*
 * "state" data for each active tcp conversation on the wire.  This is
 * basically a copy of the entire IP/TCP header from the last packet
 * we saw from the conversation together with a small identifier
 * the transmit & receive ends of the line use to locate saved header.
 */
struct cstate {
	struct cstate *cs_next;	/* next most recently used cstate (xmit only) */
	u_short cs_hlen;	/* size of hdr (receive only) */
	u_char cs_id;		/* connection # associated with this state */
	u_char cs_filler;
	union {
		char hdr[MAX_HDR];
		struct ip csu_ip;	/* ip/tcp hdr from most recent packet */
	} u;
};
#define cs_ip u.csu_ip

/*
 * all the state data for one serial line (we need one of these
 * per line).
 */
struct slcompress {
	struct cstate *last_cs;			/* most recently used tstate */
	u_char last_recv;			/* last rcvd conn. id */
	u_char last_xmit;			/* last sent conn. id */
	u_short flags;
	struct cstate tstate[MAX_STATES];	/* xmit connection states */
	struct cstate rstate[MAX_STATES];	/* receive connection states */
};
/* flag values */
#define SLF_TOSS 1		/* tossing rcvd frames because of input err */

extern void sl_compress_init(/* struct slcompress * */);
extern u_char sl_compress_tcp(/* struct mbuf *, struct ip *, struct slcompress * */);
extern struct mbuf *sl_uncompress_tcp(/* struct mbuf *, u_char, struct slcompress * */);
