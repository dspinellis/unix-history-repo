/*
 * Copyright (c) University of British Columbia, 1984
 * Copyright (c) 1990, 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Laboratory for Computation Vision and the Computer Science Department
 * of the University of British Columbia.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pk.h	7.10 (Berkeley) %G%
 */

/*
 *
 *  X.25 Packet Level Definitions:
 *
 */

/* Packet type identifier field defintions. */

#define X25_CALL                         11
#define X25_CALL_ACCEPTED                15   
#define X25_CLEAR                        19
#define X25_CLEAR_CONFIRM                23  
#define X25_DATA                          0   
#define X25_INTERRUPT                    35   
#define X25_INTERRUPT_CONFIRM            39   

#define X25_RR                            1   
#define X25_RNR                           5   
#define X25_REJECT			  9
#define X25_RESET                        27 
#define X25_RESET_CONFIRM                31   
#define X25_DIAGNOSTIC			241

#define X25_RESTART                     251     
#define X25_RESTART_CONFIRM		255 

/* Restart cause field definitions. */

#define X25_RESTART_DTE_ORIGINATED	  0
#define X25_RESTART_LOCAL_PROCEDURE_ERROR 1
#define X25_RESTART_NETWORK_CONGESTION	  3
#define X25_RESTART_NETWORK_OPERATIONAL	  7
#define X25_RESTART_DTE_ORIGINATED2	  128


/* Miscellaneous definitions. */

#define DATA_PACKET_DESIGNATOR		0x01
#define RR_OR_RNR_PACKET_DESIGNATOR	0x02
#define RR_PACKET_DESIGNATOR		0x04

#define DEFAULT_WINDOW_SIZE		2
#define MODULUS				8

#define ADDRLN				1
#define MAXADDRLN			15
#define FACILITIESLN			1
#define MAXFACILITIESLN			10
#define MAXUSERDATA			16
#define MAXCALLINFOLN			1+15+1+10+16

#define PACKET_OK			0
#define IGNORE_PACKET			1
#define ERROR_PACKET			2

typedef char    bool;
#define FALSE	0
#define TRUE	1

/*
 *  X.25 Packet format definitions
 *  This will eventually have to be rewritten without reference
 *  to bit fields, to be ansi C compliant and allignment safe.
 */

typedef u_char octet;

struct x25_calladdr {
	octet addrlens;
	octet address_field[MAXADDRLN];
};

struct x25_packet {
	octet bits;
	octet logical_channel_number;
	octet packet_type;
	octet packet_data;
};
#define packet_cause packet_data

struct data_packet {
	octet bits;
};

#define FACILITIES_REVERSE_CHARGE	0x1
#define FACILITIES_THROUGHPUT		0x2
#define FACILITIES_PACKETSIZE		0x42
#define FACILITIES_WINDOWSIZE		0x43

#define PKHEADERLN	3

#define DP(xp)          (((struct data_packet *)&(xp) -> packet_type) -> bits)
#define PS(xp)           X25GBITS(DP(xp), p_s)
#define PR(xp)           X25GBITS(DP(xp), p_r)
#define MBIT(xp)         X25GBITS(DP(xp), m_bit)
#define SPR(xp, v)       X25SBITS(DP(xp), p_r, (v))
#define SPS(xp, v)       X25SBITS(DP(xp), p_s, (v))
#define SMBIT(xp, v)     X25SBITS(DP(xp), m_bit, (v))

#define LCN(xp)		(xp -> logical_channel_number + \
	(X25GBITS(xp -> bits, lc_group_number) ? (X25GBITS(xp -> bits, lc_group_number) << 8) : 0))
#define SET_LCN(xp, lcn) ((xp -> logical_channel_number = lcn), \
	(X25SBITS(xp -> bits, lc_group_number, lcn > 255 ? lcn >> 8 : 0)))

struct mbuf *pk_template ();

/* Define X.25 packet level states. */

/* Call setup and clearing substates.  */

#define LISTEN           0
#define READY            1
#define RECEIVED_CALL    2
#define SENT_CALL        3
#define DATA_TRANSFER    4
#define RECEIVED_CLEAR   5
#define SENT_CLEAR       6

/* DTE states. */

#define DTE_WAITING		7
#define DTE_RECEIVED_RESTART	8
#define DTE_SENT_RESTART	9
#define DTE_READY		0

/* Cleaning out ... */

#define LCN_ZOMBIE 		10

#define MAXSTATES		11

/*
 *  The following definitions are used in a switch statement after
 *  determining the packet type.  These values are returned by the
 *  pk_decode procedure. 
 */

#define CALL             0 * MAXSTATES
#define CALL_ACCEPTED    1 * MAXSTATES
#define CLEAR            2 * MAXSTATES
#define CLEAR_CONF       3 * MAXSTATES
#define DATA             4 * MAXSTATES
#define INTERRUPT        5 * MAXSTATES
#define INTERRUPT_CONF   6 * MAXSTATES
#define RR               7 * MAXSTATES
#define RNR              8 * MAXSTATES
#define RESET            9 * MAXSTATES
#define RESET_CONF      10 * MAXSTATES
#define RESTART         11 * MAXSTATES
#define RESTART_CONF    12 * MAXSTATES
#define REJECT          13 * MAXSTATES
#define DIAG_TYPE       14 * MAXSTATES
#define INVALID_PACKET  15 * MAXSTATES
#define DELETE_PACKET	INVALID_PACKET

/*
 * The following definitions are used by the restart procedures
 * for noting wether the PLE is supposed to behave as DTE or DCE
 * (essentially necessary for operation over LLC2)
 */
#define	DTE_DXERESOLVING	0x0001
#define	DTE_PLAYDTE		0x0002
#define	DTE_PLAYDCE		0x0004
#define DTE_CONNECTPENDING	0x0010
#define	DTE_PRETENDDTE		0x0020

#define MAXRESTARTCOLLISIONS	10
