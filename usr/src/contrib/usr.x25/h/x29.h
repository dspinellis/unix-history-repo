/* ---------------------------------------------------------------------

      X.29 Manifest Constants

   --------------------------------------------------------------------- */

/* Message types for level-1 messages. */

#define X29_SET_PARMS               2     /* Host to Pad */
#define X29_READ_PARMS              4     /* Host to Pad */
#define X29_SET_AND_READ_PARMS      6     /* Host to Pad */
#define X29_INVITATION_TO_CLEAR     1     /* Host to Pad */
#define X29_PARAMETER_INDICATION    0     /* Pad to Host */
#define X29_INDICATION_OF_BREAK     3     /* Both directions */
#define X29_ERROR                   5     /* Pad to Host */

/* Parameter code definitions as per CCITT 1978 recommendation */

#define X29_NATIONAL_PARAMETER_MARKER	0
#define X29_ESCAPE_TO_CMD_CODE		1
#define X29_ECHO_CODE			2
#define X29_FORWARDING_SIGNAL_CODE	3
#define X29_IDLE_TIMER_CODE		4
#define X29_AUX_DEV_CONTROL_CODE	5
#define X29_RECEIVE_NET_MSGS_CODE	6
#define X29_BREAK_PROCEDURE_CODE	7
#define X29_DISCARD_OUTPUT_CODE		8
#define X29_PADDING_CODE		9
#define X29_LINE_FOLDING_CODE		10
#define X29_TRANSMISSION_SPEED_CODE	11
#define X29_XON_XOFF_CODE		12

/* PAD parameters specific to 1980 CCITT X.3 recommendation */

#define X29_LF_AFTER_CR			13
#define X29_PADDING_AFTER_LF		14
#define X29_EDITING			15
#define X29_CHARACTER_DELETE		16
#define X29_LINE_DELETE			17
#define X29_LINE_DISPLAY		18

#define NX29_1978_PARMS		12	/* # of parameters in 1978 spec */
#define NX29_1980_PARMS		18	/* # of parameters in 1980 spec */
#define NX29_PARMS		18	/* # of parameters from any spec */

#define PACKET_SIZE		512	/* maximum network packet size */

/*
 * Standard ITI call protocol type.
 * This should really be extracted from /etc/services
 * but since it is not likely to ever change we define
 * it here to save cpu cycles.
 */

#define ITI_CALL	1

struct	x25packet {
	u_char	p_x25flag;	/* flag byte, either M_BIT or Q_BIT */
#define Q_BIT			0x80
#define M_BIT			0x40
	u_char	p_x25data[PACKET_SIZE];	/* actual size varies */
};

/*
 * structure of x.29 parameter entry
 */

struct	x29param {
	u_char	x29_pnum;	/* x29 parameter number */
	u_char	x29_value;	/* parameter value */
};

struct	x29packet {
	u_char	p_x29flag;	/* flag byte, either M_BIT or Q_BIT */
	u_char	p_x29code;	/* message type */
	union	{
		struct	x29param x29paramlist[NX29_PARMS];
		struct	{
			u_char	x29errno;
			u_char	x29mtype;
		} x29error;
	} x29_un;
};

#define p_x29param	x29_un.x29paramlist
#define p_x29errno	x29_un.x29error.x29errno
#define p_x29mtype	x29_un.x29error.x29mtype
