/*
 * This file contains header information used by the PC API routines.
 */

#define	API_INTERRUPT_NUMBER	0x7A		/* API Interrupt Number */

/*
 * Define the gate numbers.  These are returned via the Name Resolution
 * service.
 */

#define	GATE_SESSMGR	1
#define	GATE_KEYBOARD	2
#define	GATE_COPY	3
#define	GATE_OIAM	4

/*
 * Name Resolution is specified in AH.
 */

#define	NAME_RESOLUTION	0x81

/*
 * Codes specified in AL for various services.
 */

#define	QUERY_SESSION_ID	0x01
#define	QUERY_SESSION_PARMS	0x02
#define	QUERY_SESSION_CURSOR	0x0b

#define	CONNECT_TO_KEYBOARD	0x01
#define	DISCONNECT_FROM_KEYBOARD	0x02
#define	WRITE_KEYSTROKE		0x04
#define	DISABLE_INPUT		0x05
#define	ENABLE_INPUT		0x06

#define	COPY_STRING		0x01

#define	READ_OIA_GROUP		0x02
