/**************************************************************************/
/*                                                                        */
/*    there seems to exist no standard for the contents of HMP traps      */
/*  RFC 869 simply says that it should "usually contain an identifier,..  */
/*  the local time..., and data pertinent to the event."                  */
/*                                                                        */
/* Being unwilling to create a defacto (non)standard, since I am not a    */
/* montoring wizard, I mostly punted.  There is a trap routine which will */
/* send message with the structure below at the start of the data section,*/
/* followed by whatever data you passed to it up to the size of one mbuf  */
/*                                                                        */
/* I've also included some trap codes -- which I believe are descended    */
/* from 4.1??   You will find them used in the code but commented out..   */
/*                                                                        */
/**************************************************************************/

#if HMP && HMPTRAPS
struct hmp_trap
{
    u_long ht_type;
    u_long ht_time;
};

/*
 * need more host information traps -- like if the machine is going down
 */
#define	T_MEM_DROP	0x0		/* packet drop due to memory */

#define	T_IP_CKSUM	0x10		/* IP checksum */
#define	T_IP_ADDRS	0x11		/* address error */
#define	T_IP_FDROP	0x12		/* fragment dropped (timeout) */
#define	T_IP_TRUNC	0x13		/* truncated packet */
#define	T_IP_OVFLO	0x14		/* header overflow */
#define	T_IP_HLEN	0x15		/* header length */
#define	T_MEM_FGLEAN	0x16		/* IP frag reclaimed for memory */

#define	T_TCP_CKSUM	0x20		/* TCP checksum */
#define	T_TCP_OVFLO	0x21		/* header overflow */
#define	T_TCP_HLEN	0x22		/* header length */
#define	T_TCP_REXMTTL	0x23		/* retransmit too long */
#define	T_TCP_DUP	0x24		/* duplicate drop */
#define	T_TCP_ORDER	0x25		/* out of order receipt */
#define	T_TCP_WINDOW	0x26		/* out of window receipt */
#define	T_TCP_RDROP	0x27		/* "" end of new segment */
#define	T_TCP_UDROP	0x28		/* data dropped from end of q */
#define	T_MEM_TGLEAN	0x29		/* TCP unacked segment "" */

#define	T_UDP_CKSUM	0x30		/* UDP checksum */

#define	T_ICMP_CKSUM	0x40		/* ICMP checksum */
#define	T_ICMP_SRCQ	0x41		/* source quench rcvd */
#define	T_ICMP_REDIR	0x42		/* redirect rcvd */
#define	T_ICMP_TIMEX	0x43		/* time exceeded */
#define	T_ICMP_PARM	0x44		/* parameter problem */

#define	T_HMP_CKSUM	0x50		/* HMP checksum */
#define	T_HMP_STYPE	0x51		/* wrong system type */
#define	T_HMP_MTYPE	0x52		/* bad message type */
#define	T_HMP_PASSWD	0x53		/* bad password */

#endif
