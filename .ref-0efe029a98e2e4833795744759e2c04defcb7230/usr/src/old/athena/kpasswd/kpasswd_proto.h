
/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

/*
 * kpasswd_proto
 *
 * definitions for the kpasswd "protocol"
 * (We hope this to be temporary until a real admin protocol is worked out)
 */

#define	MSGSIZE	255
struct kpasswd_data {
	C_Block	random_key;
	char	secure_msg[MSGSIZE];
};
struct update_data {
	char	pw[255];
	char	secure_msg[MSGSIZE];
};
#define	SERVICE		"kpasswd"
#define	SECURE_STRING	"Kerberos password update program -- 12/9/88 UC Berkeley"
