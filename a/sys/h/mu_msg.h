/*	mu_msg.h	Melb 4.2	82/10/20	*/

/*
 * Melb Uni trivial IPC mechanism
 */

#ifdef MUSH
#ifndef	DATA_T		/* so people can make a union/struct if they want */
#define	DATA_T	long
#endif

typedef	struct {
#ifdef	vax		/* this isn't important, just nice */
	u_short	msg_uid : 15;	/* effective sender uid (or code if pid==0) */
	u_short	msg_val : 1;	/* msg is valid, ie: really sent by someone */
	u_short msg_pid : 15;	/* rcv: proc id of sender, snd: dest addr */
	u_short	msg_rply: 1;	/* rcv: reply required, snd: recv reply only */
#else
	u_short	msg_rply: 1;
	u_short	msg_pid : 15;
	u_short msg_val : 1;
	u_short	msg_uid : 15;
#endif
	DATA_T	msg_data;
} mmsgbuf;

/*
 * The following relate to the mu_msg() sys call
 */

typedef enum {
	MSG_ENAB,		/* enable input msgs to process */
	MSG_DISB,		/* no more messages */
	MSG_RECV,		/* get a message */
	MSG_SEND,		/* send a message */
	MSG_SNDW,		/* send a msg then get one */
	MSG_RPLY,		/* send a reply */
} msg_type;

#define	MSG_W_RCV	0x0001	/* wait for message to be received */
#define	MSG_W_POST	0x0002	/* wait till queued to receiver */
#define	MSG_W_DLV	0x0004	/* wait till delivered (not impl) */
#define	MSG_W_Q		0x0008	/* wait if process queues full (not impl) */
#define	MSG_W_ENAB	0x0010	/* wait till rcvr enables messages (not impl) */

#ifdef	KERNEL
	/*
	 * Some of this stuff belongs other places, but ...
	 */
#define MSGENAB		0x0001	/* process has messages enabled */
#define	MSGOK		0x0002	/* process is waiting for msg */
#define	MSGWRPLY	0x0004	/* message process is waiting for is a reply */
#define	MSGRPLY		0x0008	/* process must reply to last msg */
#define	MSGWAIT		0x0010	/* some process is waiting on our msgbuf */

#define	MSGPRI	(PZERO+4)	/* Interruptible */

struct proc *mu_send();
#endif

#define	SIGMESG		28	/* sig sent to MSGENAB'd proc if msg arrives */
#endif
