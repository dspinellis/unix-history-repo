/*
 * Where the fields fall on the formatted screen used by tncomp, tnrecv,
 * and tnsend.
 */

#define	SEND_SEQUENCE		1
#define	SEND_SEQUENCE_LENGTH	23

#define	ACK_SEQUENCE	(SEND_SEQUENCE+SEND_SEQUENCE_LENGTH+1)
#define	ACK_SEQUENCE_LENGTH	22

#define	CHECKSUM	(ACK_SEQUENCE+ACK_SEQUENCE_LENGTH+1)
#define	CHECKSUM_LENGTH		32

#define	DATA		(CHECKSUM+CHECKSUM_LENGTH+1)
#define	DATA_LENGTH		((80*22)+79)
