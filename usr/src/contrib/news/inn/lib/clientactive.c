/*  $Revision: 1.9 $
**
*/
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include "configdata.h"
#include "paths.h"
#include "clibrary.h"
#include "libinn.h"
#include "nntp.h"
#include "macros.h"


STATIC char	CApathname[sizeof _PATH_TEMPACTIVE];
STATIC FILE	*CAfp;


/*
**  Get a copy of the active file for a client host to use, locally or
**  remotely.
*/
FILE *
CAopen(FromServer, ToServer)
    FILE	*FromServer;
    FILE	*ToServer;
{
    /* Use a local (or NFS-mounted) copy if available.  Make sure we don't
     * try to delete it when we close it. */
    if ((CAfp = fopen(_PATH_CLIENTACTIVE, "r")) != NULL) {
	CApathname[0] = '\0';
	return CAfp;
    }

    /* Use the active file from the server */
    return CAlistopen(FromServer, ToServer, (char *)NULL);
}


/*
**  Internal library routine.
*/
FILE *
CA_listopen(pathname, FromServer, ToServer, request)
    char	*pathname;
    FILE	*FromServer;
    FILE	*ToServer;
    char	*request;
{
    char	buff[BUFSIZ];
    char	*p;
    int		oerrno;
    FILE	*F;

    (void)unlink(pathname);
    if ((F = fopen(pathname, "w")) == NULL)
	return NULL;

    /* Send a LIST command to and capture the output. */
    if (request == NULL)
	(void)fprintf(ToServer, "list\r\n");
    else
	(void)fprintf(ToServer, "list %s\r\n", request);
    (void)fflush(ToServer);

    /* Get the server's reply to our command. */
    if (fgets(buff, sizeof buff, FromServer) == NULL
     || !EQn(buff, NNTP_LIST_FOLLOWS, STRLEN(NNTP_LIST_FOLLOWS))) {
	oerrno = errno;
	CAclose();
	errno = oerrno;
	return NULL;
    }

    /* Slurp up the rest of the response. */
    while (fgets(buff, sizeof buff, FromServer) != NULL) {
	if ((p = strchr(buff, '\r')) != NULL)
	    *p = '\0';
	if ((p = strchr(buff, '\n')) != NULL)
	    *p = '\0';
	if (buff[0] == '.' && buff[1] == '\0') {
	    if (ferror(F) || fflush(F) == EOF || fclose(F) == EOF)
		break;
	    return fopen(pathname, "r");
	}
	(void)fprintf(F, "%s\n", buff);
    }

    /* Ran out of input before finding the terminator; quit. */
    oerrno = errno;
    (void)fclose(F);
    CAclose();
    errno = oerrno;
    return NULL;
}


/*
**  Use the NNTP list command to get a file from a server.  Default is
**  the active file, otherwise ask for whatever is in the request param.
*/
FILE *
CAlistopen(FromServer, ToServer, request)
    FILE	*FromServer;
    FILE	*ToServer;
    char	*request;
{
    /* Gotta talk to the server -- see if we can. */
    if (FromServer == NULL || ToServer == NULL)
	return NULL;

    (void)strcpy(CApathname, _PATH_TEMPACTIVE);
    (void)mktemp(CApathname);
    return CAfp = CA_listopen(CApathname, FromServer, ToServer, request);
}



/*
**  Close the file opened by CAopen or CAlistopen.
*/
void
CAclose()
{
    if (CAfp) {
	(void)fclose(CAfp);
	CAfp = NULL;
    }
    if (CApathname[0]) {
	(void)unlink(CApathname);
	CApathname[0] = '\0';
    }
}
