/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)tncomp.h	3.2 (Berkeley) %G%
 */

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
