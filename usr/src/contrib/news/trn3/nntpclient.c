/* $Id: nntpclient.c,v 3.0 1991/11/22 04:12:21 davison Trn $
*/

#include "EXTERN.h"
#include "common.h"

#ifdef USE_NNTP

#include "INTERN.h"
#include "nntpclient.h"

#define CANTPOST	\
	"NOTE:  This machine does not have permission to post articles.\n"
#define CANTUSE		\
	"This machine does not have permission to use the %s news server.\n"

void
nntp_connect()
{
    char *server;
    int response;

    if ((server = getenv("NNTPSERVER")) == Nullch)
	server = SERVER_NAME;
    if (server[0] == '/') {
	register FILE *fp;
	if ((fp = fopen(server, "r")) != Nullfp) {
	    server = Nullch;
	    while (fgets(ser_line, sizeof ser_line, fp) != Nullch) {
		if (*ser_line == '\n' || *ser_line == '#')
		    continue;
		if ((server = index(ser_line, '\n')) != Nullch)
		    *server = '\0';
		server = ser_line;
		break;
	    }
	    fclose(fp);
	} else
	    server = Nullch;
	if (server == Nullch) {
	    sprintf(ser_line, "\
Couldn't get name of news server from %s\n\
Either fix this file, or put NNTPSERVER in your environment.\n", SERVER_NAME);
	    fatal_error(ser_line);
	}
    }

    switch (response = server_init(server)) {
    case NNTP_GOODBYE_VAL:
	if (atoi(ser_line) == response) {
	    char tmpbuf[LBUFLEN];
	    sprintf(tmpbuf,"News server %s unavailable: %s\n",server,&ser_line[4]);
	    fatal_error(tmpbuf);
	}
    case -1:
	sprintf(ser_line,"News server %s unavailable, try again later.\n",server);
	fatal_error(ser_line);
    case NNTP_ACCESS_VAL:
	sprintf(ser_line,CANTUSE,server);
	fatal_error(ser_line);
	/* NOT REACHED */
    case NNTP_NOPOSTOK_VAL:
	advise(CANTPOST);
	/* FALL THROUGH */
    case NNTP_POSTOK_VAL:
	break;
    default:
	sprintf(ser_line,"Unknown response code %d from %s.\n", response, server);
	fatal_error(ser_line);
    }
}

void
nntp_command(buf)
char *buf;
{
#ifdef DEBUG
    if (debug & DEB_NNTP)
	printf(">%s\n", buf) FLUSH;
#endif
    fprintf(ser_wr_fp, "%s\r\n", buf);
    fflush(ser_wr_fp);
}

char
nntp_check(strict)
bool_int strict;
{
    int n;

#ifdef HAS_SIGHOLD
    sighold(SIGINT);
#endif
    n = (fgets(ser_line, sizeof ser_line, ser_rd_fp) == NULL)? -1 : 0;
#ifdef HAS_SIGHOLD
    sigrelse(SIGINT);
#endif
    if (n < 0)
	fatal_error("\nUnexpected close of server socket.\n");
    n = strlen(ser_line);
    if (n >= 2 && ser_line[n-1] == '\n' && ser_line[n-2] == '\r')
	ser_line[n-2] = '\0';
#ifdef DEBUG
    if (debug & DEB_NNTP)
	printf("<%s\n", ser_line) FLUSH;
#endif
    if (strict && *ser_line == NNTP_CLASS_FATAL) {	/* Fatal error */
	char tmpbuf[LBUFLEN];
	sprintf(tmpbuf,"\n%s\n",ser_line);
	fatal_error(tmpbuf);
    }
    return *ser_line;
}

int
nntp_gets(buf, len)
char *buf;
int  len;
{
    int n;

#ifdef HAS_SIGHOLD
    sighold(SIGINT);
#endif
    n = (fgets(buf, len, ser_rd_fp) == NULL)? -1 : 0;
#ifdef HAS_SIGHOLD
    sigrelse(SIGINT);
#endif
    if (n < 0)
	fatal_error("\nUnexpected close of server socket.\n");
    n = strlen(buf);
    if (n >= 2 && buf[n-1] == '\n' && buf[n-2] == '\r')
	buf[n-2] = '\0';
    return 0;
}

void
nntp_close()
{
    if (ser_wr_fp != NULL && ser_rd_fp != NULL) {
	nntp_command("QUIT");
	fclose(ser_wr_fp);
	ser_wr_fp = NULL;

	nntp_check(FALSE);
	fclose(ser_rd_fp);
	ser_rd_fp = NULL;
    }
}

#endif /* USE_NNTP */
