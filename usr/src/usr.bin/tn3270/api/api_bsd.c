#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>

#include "../api/api.h"
#include "api_exch.h"


api_open_api(string)
char	*string;		/* if non-zero, where to connect to */
{
    struct sockaddr_in server;
    struct hostent *hp;
    char *getenv();
    char thehostname[100];
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

    if (sscanf(string, "%[^:]:%d", thehostname, &port) != 2) {
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
    if (api_exch_init(sock) == -1) {
	return -1;
    }
    if (api_exch_outcommand(EXCH_ASSOCIATE) == -1) {
	return -1;
    }
    while ((i = api_exch_inbyte()) != EXCH_ASSOCIATED) {
	struct storage_descriptor sd;
	int passwd_length;
	char *passwd, *getpass();
	char buffer[200];

	switch (i) {
	case EXCH_REJECTED:
	    if (api_exch_intype(EXCH_TYPE_STORE_DESC,
					sizeof sd, (char *)&sd) == -1) {
		return -1;
	    }
	    sd.length = ntohs(sd.length);
	    if (api_exch_intype(EXCH_TYPE_BYTES, sd.length, buffer) == -1) {
		return -1;
	    }
	    buffer[sd.length] = 0;
	    fprintf(stderr, "%s\n", buffer);
	    if (api_exch_outcommand(EXCH_ASSOCIATE) == -1) {
		return -1;
	    }
	    break;
	case EXCH_SEND_AUTH:
	    if (api_exch_intype(EXCH_TYPE_STORE_DESC, sizeof sd, (char *)&sd) == -1) {
		return -1;
	    }
	    sd.length = ntohs(sd.length);
	    if (api_exch_intype(EXCH_TYPE_BYTES, sd.length, buffer) == -1) {
		return -1;
	    }
	    buffer[sd.length] = 0;
	    passwd = getpass(buffer);		/* Go to terminal */
	    passwd_length = strlen(passwd);
	    if (api_exch_intype(EXCH_TYPE_STORE_DESC, sizeof sd, (char *)&sd) == -1) {
		return -1;
	    }
	    sd.length = ntohs(sd.length);
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
	    sd.length = htons(passwd_length);
	    if (api_exch_outcommand(EXCH_AUTH) == -1) {
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


api_exch_api(regs, sregs)
union REGS *regs;
struct SREGS *sregs;
{
}
