/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kpasswd_proto.h	5.1 (Berkeley) %G%
 */

/*
 * kpasswd_proto
 *
 * definitions for the kpasswd "protocol"
 * (We hope this to be temporary until a real admin protocol is worked out.)
 */

struct kpasswd_data {
	des_cblock random_key;
	char secure_msg[_PASSWORD_LEN];
};

struct update_data {
	char pw[_PASSWORD_LEN];
	char secure_msg[_PASSWORD_LEN];
};
#define	SERVICE		"kpasswd"
#define	SECURE_STRING \
	"Kerberos password update program -- 12/9/88 UC Berkeley"
