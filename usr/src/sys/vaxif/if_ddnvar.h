/*	@(#)if_ddnvar.h	7.1 (Berkeley) 6/5/86 */



/************************************************************************\

     ________________________________________________________
    /                                                        \
   |          AAA          CCCCCCCCCCCCCC    CCCCCCCCCCCCCC   |
   |         AAAAA        CCCCCCCCCCCCCCCC  CCCCCCCCCCCCCCCC  |
   |        AAAAAAA       CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |
   |       AAAA AAAA      CCCC              CCCC              |
   |      AAAA   AAAA     CCCC              CCCC              |
   |     AAAA     AAAA    CCCC              CCCC              |
   |    AAAA       AAAA   CCCC              CCCC              |
   |   AAAA  AAAAAAAAAAA  CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |
   |  AAAA    AAAAAAAAAAA CCCCCCCCCCCCCCCC  CCCCCCCCCCCCCCCC  |
   | AAAA      AAAAAAAAA   CCCCCCCCCCCCCC    CCCCCCCCCCCCCC   |
    \________________________________________________________/

	Copyright (c) 1985 by Advanced Computer Communications
	720 Santa Barbara Street, Santa Barbara, California  93101
	(805) 963-9431

	This software may be duplicated and used on systems
	which are licensed to run U.C. Berkeley versions of
	the UNIX operating system.  Any duplication of any
	part of this software must include a copy of ACC's
	copyright notice.


File:
		if_ddnvar.h

Author:
		Art Berggreen

Project:
		4.2 DDN X.25 network driver

Function:
		This file contains definitions used to control and
		track the status of the ACP625 (IF-11/X25).

Components:

Revision History:
		16-May-1985:	V1.0 - First release.
				Art Berggreen.

\************************************************************************/


/*	if_ddnvar.h	 V1.0	5/16/85	*/

/* program parameters */

#define DDNMTU		1006		/* maximum IP msg length */
#define	DDN_OQMAX	8		/* max IP msgs on LCN output q */
#define NDDNCH		32		/* number of X.25 channels */
#define DELAY_CNT	50000
#define DDN_TIMEOUT	10

#define	DC_OBUSY	0x01

typedef	unsigned char	byte;
typedef int		boolean;

/* X25 LCN state definitions */
#define LC_DOWN		   0		/* X25 circuit down		*/
#define LC_RESTART	   1		/* X25 circuit restarting	*/
#define LC_IDLE		   2		/* X25 circuit not in use	*/
#define LC_CALL_PENDING	   3		/* X25 circuit call pending	*/
#define LC_DATA_IDLE	   4		/* X25 circuit open		*/
#define LC_CLR_PENDING	   5		/* X25 circuit clear pending	*/

/* Timeout definitions */
#define	TMO_OFF		   0			/* timer off		*/
#define	TMO_RESTART	 ( 90/DDN_TIMEOUT)	/* restart timeout	*/
#define	TMO_CALL_PENDING (180/DDN_TIMEOUT)	/* call timeout		*/
#define	TMO_DATA_IDLE	 ( 30/DDN_TIMEOUT)	/* idle circuit timeout	*/
#define	TMO_CLR_PENDING	 ( 30/DDN_TIMEOUT)	/* clear timeout	*/

/* Link status codes */
#define LINK_DOWN	0x00		/* Link layer is down		*/
#define LINK_UP		0x01		/* Link layer is up		*/

/* Line control codes */
#define LINK_DISABLE	0x00		/* Disable link layer		*/
#define LINK_ENABLE	0x01		/* Enable link layer		*/
#define LINK_LOOPBACK	0x42		/* Link layer loopback mode	*/
#define   LOOP_OFF	0x00		/*   Loopback off		*/
#define   LOOP_EXTERNAL	0x01		/*   Loopback external		*/
#define   LOOP_INTERNAL	0x03		/*   Loopback internal		*/
#define	PKT_SIZE	0x90		/* Packet size			*/
#define	PKT_WINDOW	0x4f		/* Packet window		*/

/* X25 supervisor message codes */
#define CALL		0x00		/* outgoing call		*/
#define RING		0x01		/* incoming call		*/
#define CLEARVC		0x02		/* clear by VCN			*/
#define	ANSWER		0x03		/* answer call			*/
#define CLEARLC		0x04		/* clear by LCN			*/
#define	RESET		0x20		/* reset LCN			*/
#define RESET_ACK	0x21		/* reset ack			*/
#define INTERRUPT	0x22		/* X25 interrupt		*/
#define	READY		0x23		/* flow control ready		*/
#define INTR_ACK	0x24		/* interrupt ack		*/
#define RESTART		0x40		/* X25 restart			*/
#define	RSTRT_ACK	0x41		/* restart ack			*/
#define SYS_STATUS	0x42		/* system status msg		*/
#define LINE_CNTL	0x60		/* link control cmnd		*/
#define LINE_STATUS	0x61		/* link status resp		*/

/* X25 facilities */
#define	X25_FACIL_DDN	0x04		/* DDN mode facility		*/
#define FAC_DDNSTD	0x01		/*   DDN standard mode		*/

/* X25 protocols */
#define	X25_PROTO_IP	0xcc		/* X25 IP protocol type code	*/
