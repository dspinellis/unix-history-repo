/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)authenc.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#if	defined(ENCRYPTION) || defined(AUTHENTICATION)
#include <sys/types.h>
#include <arpa/telnet.h>
#include <libtelnet/encrypt.h>
#include <libtelnet/misc.h>

#include "general.h"
#include "ring.h"
#include "externs.h"
#include "defines.h"
#include "types.h"

	int
net_write(str, len)
	unsigned char *str;
	int len;
{
	if (NETROOM() > len) {
		ring_supply_data(&netoring, str, len);
		if (str[0] == IAC && str[1] == SE)
			printsub('>', &str[2], len-2);
		return(len);
	}
	return(0);
}

	void
net_encrypt()
{
#if	defined(ENCRYPTION)
	if (encrypt_output)
		ring_encrypt(&netoring, encrypt_output);
	else
		ring_clearto(&netoring);
#endif
}

	int
telnet_spin()
{
	return(-1);
}

	char *
telnet_getenv(val)
	char *val;
{
	return((char *)env_getvalue((unsigned char *)val));
}

	char *
telnet_gets(prompt, result, length, echo)
	char *prompt;
	char *result;
	int length;
	int echo;
{
	extern char *getpass();
	extern int globalmode;
	int om = globalmode;
	char *res;

	TerminalNewMode(-1);
	if (echo) {
		printf("%s", prompt);
		res = fgets(result, length, stdin);
	} else if (res = getpass(prompt)) {
		strncpy(result, res, length);
		res = result;
	}
	TerminalNewMode(om);
	return(res);
}
#endif
