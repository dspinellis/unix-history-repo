/*
 * This file describes the structures passed back and forth
 * between the API client and API server on a Unix-based
 * tn3270 implementation.
 *
 * @(#)api_exch.h	1.6 (Berkeley) %G%
 */

/*
 * The following are the low-level opcodes exchanged between the
 * two sides.  These are designed to allow for type, sequence number,
 * and direction checking.
 *
 * We enforce conversation flow.  There are three states: CONTENTION,
 * SEND, and RECEIVE.  Both sides start in CONTENTION.
 * We never leave RECEIVE state without first reading a TURNAROUND
 * opcode.  We never leave SEND state without first writing a TURNAROUND
 * opcode.  This scheme ensures that we always have conversation flowing
 * in a synchronized direction (or detect an application error), and that
 * we never hang with both sides trying to read from the "wire".
 *
 * State	event			action
 *
 * CONTENTION	read request		send TURNAROUND
 *					read RTS
 *					enter RECEIVE
 * CONTENTION	write request		send RTS
 *					read TURNAROUND
 *					enter SEND
 *
 * RECEIVE	read request		read whatever
 * RECEIVE	write request		read TURNAROUND
 *
 * SEND		read request		send TURNAROUND
 * SEND		write			write whatever
 */

#define	EXCH_EXCH_COMMAND	0	/* The following is a command */
#define	EXCH_EXCH_TURNAROUND	1	/* Your turn to send */
#define	EXCH_EXCH_RTS		2	/* Request to send */
#define	EXCH_EXCH_TYPE		3	/* The following is a type */

struct exch_exch {
    unsigned char
	opcode,			/* COMMAND, TURNAROUND, or TYPE */
	my_sequence,		/* 0-ff, initially zero */
	your_sequence,		/* 0-ff, initially zero */
	command_or_type;	/* Application level command or type */
    unsigned short
	length;			/* The length of any following data */
};

/*
 * The following are the command codes which the higher level protocols
 * send and receive.
 */

#define	EXCH_CMD_ASSOCIATE	0	/* Connect [client->server] */
	/*
	 * struct storage_desc
	 * char key[]
	 */
#define	EXCH_CMD_DISASSOCIATE	1	/* Disconnect [client->server] */
#define	EXCH_CMD_SEND_AUTH	2	/* Send password [server->client] */
	/*
	 * struct storage_desc
	 * char prompt[]
	 * struct storage_desc
	 * char seed[]
	 */
#define	EXCH_CMD_AUTH		3	/* Authorization [client->server] */
	/*
	 * struct storage_desc
	 * char authenticator[]
	 */
#define	EXCH_CMD_ASSOCIATED	4	/* Connected [server->client] */
#define	EXCH_CMD_REJECTED	5	/* Too bad [server->client] */
	/*
	 * struct storage_desc
	 * char message[]
	 */

#define	EXCH_CMD_REQUEST	6	/* A request [client->server] */
	/* struct regs,
	 * struct sregs,
	 * struct storage_desc
	 * char bytes[]
	 */
#define	EXCH_CMD_GIMME		7	/* Send storage [server->client] */
	/*
	 * struct storage_desc
	 */
#define	EXCH_CMD_HEREIS		8	/* Here is storage [BOTH WAYS] */
	/*
	 * struct storage_desc
	 * char bytes[]
	 */
#define	EXCH_CMD_REPLY		9	/* End of discussion */
	/*
	 * struct regs,
	 * struct sregs,
	 */

/*
 * The following are typed parameters sent across the wire.
 *
 * This should be done much more generally, with some form of
 * XDR or mapped conversation ability.
 */

#define	EXCH_TYPE_REGS		0
#define	EXCH_TYPE_SREGS		1
#define	EXCH_TYPE_STORE_DESC	2
#define	EXCH_TYPE_BYTES		3

/*
 * each parameter that comes over looks like:
 *
 *	char			type of following
 *	short (2 bytes)		length of following (network byte order)
 *	following
 */

struct storage_descriptor {
    long	location;	/* In network byte order */
    short	length;		/* In network byte order */
};
