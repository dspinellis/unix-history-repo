/*
 *	Copyright (c) 1984-1987 by the Regents of the
 *	University of California and by Gregory Glenn Minshall.
 *
 *	Permission to use, copy, modify, and distribute these
 *	programs and their documentation for any purpose and
 *	without fee is hereby granted, provided that this
 *	copyright and permission appear on all copies and
 *	supporting documentation, the name of the Regents of
 *	the University of California not be used in advertising
 *	or publicity pertaining to distribution of the programs
 *	without specific prior permission, and notice be given in
 *	supporting documentation that copying and distribution is
 *	by permission of the Regents of the University of California
 *	and by Gregory Glenn Minshall.  Neither the Regents of the
 *	University of California nor Gregory Glenn Minshall make
 *	representations about the suitability of this software
 *	for any purpose.  It is provided "as is" without
 *	express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)api_bsd.c	1.9 (Berkeley) %G%";
#endif	/* not lint */

#if	defined(unix)

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>

#include "../ctlr/api.h"
#include "api_exch.h"


int
api_close_api()
{
    if (api_exch_outcommand(EXCH_CMD_DISASSOCIATE) == -1) {
	return -1;
    } else if (api_exch_flush() == -1) {
	return -1;
    } else {
	return 0;
    }
}


int
api_open_api(string)
char	*string;		/* if non-zero, where to connect to */
{
    struct sockaddr_in server;
    struct hostent *hp;
    struct storage_descriptor sd;
    char *getenv();
    char thehostname[100];
    char keyname[100];
    char inkey[100];
    FILE *keyfile;
    int sock;
    int port;
    int i;

    if (string == 0) {
	string = getenv("API3270");	/* Get API */
	if (string == 0) {
	    fprintf(stderr,
			"API3270 environmental variable not set - no API.\n");
	    return -1;			/* Nothing */
	}
    }

    if (sscanf(string, "%[^:]:%d:%s", thehostname, &port, keyname) != 3) {
	fprintf(stderr, "API3270 environmental variable has bad format.\n");
	return -1;
    }
    /* Now, try to connect */
    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0) {
	perror("opening API socket");
	return -1;
    }
    server.sin_family = AF_INET;
    hp = gethostbyname(thehostname);
    if (hp == 0) {
	fprintf(stderr, "%s specifies bad host name.\n", string);
	return -1;
    }
    bcopy(hp->h_addr, &server.sin_addr, hp->h_length);
    server.sin_port = htons(port);

    if (connect(sock, &server, sizeof server) < 0) {
	perror("connecting to API server");
	return -1;
    }
    /* Now, try application level connection */
    if (api_exch_init(sock, "client") == -1) {
	return -1;
    }
    if (api_exch_outcommand(EXCH_CMD_ASSOCIATE) == -1) {
	return -1;
    }
    keyfile = fopen(keyname, "r");
    if (keyfile == 0) {
	perror("fopen");
	return -1;
    }
    if (fscanf(keyfile, "%s\n", inkey) != 1) {
	perror("fscanf");
	return -1;
    }
    sd.length = strlen(inkey)+1;
    if (api_exch_outtype(EXCH_TYPE_STORE_DESC, sizeof sd, (char *)&sd) == -1) {
	return -1;
    }
    if (api_exch_outtype(EXCH_TYPE_BYTES, sd.length, inkey) == -1) {
	return -1;
    }
    while ((i = api_exch_nextcommand()) != EXCH_CMD_ASSOCIATED) {
	int passwd_length;
	char *passwd, *getpass();
	char buffer[200];

	switch (i) {
	case EXCH_CMD_REJECTED:
	    if (api_exch_intype(EXCH_TYPE_STORE_DESC,
					sizeof sd, (char *)&sd) == -1) {
		return -1;
	    }
	    if (api_exch_intype(EXCH_TYPE_BYTES, sd.length, buffer) == -1) {
		return -1;
	    }
	    buffer[sd.length] = 0;
	    fprintf(stderr, "%s\n", buffer);
	    if (api_exch_outcommand(EXCH_CMD_ASSOCIATE) == -1) {
		return -1;
	    }
	    break;
	case EXCH_CMD_SEND_AUTH:
	    if (api_exch_intype(EXCH_TYPE_STORE_DESC, sizeof sd, (char *)&sd) == -1) {
		return -1;
	    }
	    if (api_exch_intype(EXCH_TYPE_BYTES, sd.length, buffer) == -1) {
		return -1;
	    }
	    buffer[sd.length] = 0;
	    passwd = getpass(buffer);		/* Go to terminal */
	    passwd_length = strlen(passwd);
	    if (api_exch_intype(EXCH_TYPE_STORE_DESC, sizeof sd, (char *)&sd) == -1) {
		return -1;
	    }
	    if (api_exch_intype(EXCH_TYPE_BYTES, sd.length, buffer) == -1) {
		return -1;
	    }
	    buffer[sd.length] = 0;
	    if (sd.length) {
		char *ptr;

		ptr = passwd;
		i = 0;
		while (*ptr) {
		    *ptr++ ^= buffer[i++];
		    if (i >= sd.length) {
			i = 0;
		    }
		}
	    }
	    sd.length = passwd_length;
	    if (api_exch_outcommand(EXCH_CMD_AUTH) == -1) {
		return -1;
	    }
	    if (api_exch_outtype(EXCH_TYPE_STORE_DESC, sizeof sd, (char *)&sd) == -1) {
		return -1;
	    }
	    if (api_exch_outtype(EXCH_TYPE_BYTES, passwd_length, passwd) == -1) {
		return -1;
	    }
	    break;
	case -1:
	    return -1;
	default:
	    fprintf(stderr,
		    "Waiting for connection indicator, received 0x%x.\n", i);
	    break;
	}
    }
    /* YEAH */
    return 0;		/* Happiness! */
}


api_exch_api(regs, sregs, parms, length)
union REGS *regs;
struct SREGS *sregs;
char *parms;
int length;
{
    struct storage_descriptor sd;
    int i;

    if (api_exch_outcommand(EXCH_CMD_REQUEST) == -1) {
	return -1;
    }
    if (api_exch_outtype(EXCH_TYPE_REGS, sizeof *regs, (char *)regs) == -1) {
	return -1;
    }
    if (api_exch_outtype(EXCH_TYPE_SREGS, sizeof *sregs, (char *)sregs) == -1) {
	return -1;
    }
    sd.length = length;
    sd.location = (long) parms;
    if (api_exch_outtype(EXCH_TYPE_STORE_DESC, sizeof sd, (char *)&sd) == -1) {
	return -1;
    }
    if (api_exch_outtype(EXCH_TYPE_BYTES, length, parms) == -1) {
	return -1;
    }
    while ((i = api_exch_nextcommand()) != EXCH_CMD_REPLY) {
	switch (i) {
	case EXCH_CMD_GIMME:
	    if (api_exch_intype(EXCH_TYPE_STORE_DESC, sizeof sd, (char *)&sd)
					== -1) {
		return -1;
	    }
	    /*XXX validity check GIMME? */
	    if (api_exch_outcommand(EXCH_CMD_HEREIS) == -1) {
		return -1;
	    }
	    if (api_exch_outtype(EXCH_TYPE_STORE_DESC, sizeof sd, (char *)&sd)
				== -1) {
		return -1;
	    }
	    if (api_exch_outtype(EXCH_TYPE_BYTES, sd.length,
			    sd.location) == -1) {
		return -1;
	    }
	    break;
	case EXCH_CMD_HEREIS:
	    if (api_exch_intype(EXCH_TYPE_STORE_DESC, sizeof sd, (char *)&sd)
					== -1) {
		return -1;
	    }
	    /* XXX Validty check HEREIS? */
	    if (api_exch_intype(EXCH_TYPE_BYTES, sd.length,
			    sd.location) == -1) {
		return -1;
	    }
	    break;
	default:
	    fprintf(stderr, "Waiting for reply command, we got command %d.\n",
			i);
	    return -1;
	}
    }
    if (api_exch_intype(EXCH_TYPE_REGS, sizeof *regs, (char *)regs) == -1) {
	return -1;
    }
    if (api_exch_intype(EXCH_TYPE_SREGS, sizeof *sregs, (char *)sregs) == -1) {
	return -1;
    }
    /* YEAH */
    return 0;		/* Happiness! */
}

#endif	/* unix */
