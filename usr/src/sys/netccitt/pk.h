/* Copyright (c) University of British Columbia, 1984 */

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
#define X25_RESET                        27 
#define X25_RESET_CONFIRM                31   

#define X25_RESTART                     251     
#define X25_RESTART_CONFIRM		255 

/* Restart cause field definitions. */

#define X25_RESTART_LOCAL_PROCEDURE_ERROR 1
#define X25_RESTART_NETWORK_CONGESTION	  3
#define X25_RESTART_NETWORK_OPERATIONAL	  7

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
 */

typedef u_char octet;

struct x25_calladdr {
#ifdef vax
	unsigned called_addrlen:4;
	unsigned calling_addrlen:4;
#endif 
#ifdef sun
	unsigned calling_addrlen:4;
	unsigned called_addrlen:4;
#endif
	char address_field[MAXADDRLN];
};

#define FACILITIES_REVERSE_CHARGE	0x1
#define FACILITIES_THROUGHPUT		0x2
#define FACILITIES_PACKETSIZE		0x42
#define FACILITIES_WINDOWSIZE		0x43

#define PKHEADERLN	3

struct x25_packet {
#ifdef vax
	unsigned lc_group_number:4;
	unsigned fmt_identifier:3;
	unsigned q_bit:1;
#endif
#ifdef sun
	unsigned q_bit:1;
	unsigned fmt_identifier:3;
	unsigned lc_group_number:4;
#endif
	octet logical_channel_number;
	octet packet_type;
	octet packet_data;
};

struct data_packet {
#ifdef vax
	unsigned z:1;
	unsigned ps:3;
	unsigned m_bit:1;
	unsigned pr:3;
#endif
#ifdef sun
	unsigned pr:3;
	unsigned m_bit:1;
	unsigned ps:3;
	unsigned z:1;
#endif
};

#define PR(xp)		(((struct data_packet *)&xp -> packet_type)->pr)
#define PS(xp)		(((struct data_packet *)&xp -> packet_type)->ps)
#define MBIT(xp)	(((struct data_packet *)&xp -> packet_type)->m_bit)

struct x25_packet *pk_template ();

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

#define MAXSTATES		10

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
#define INVALID_PACKET  13 * MAXSTATES
#define DELETE_PACKET	INVALID_PACKET
