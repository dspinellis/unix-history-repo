/*  $Revision: 1.9 $
**
*/
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include "configdata.h"
#include "nntp.h"
#include "paths.h"
#include "libinn.h"
#include "clibrary.h"
#include "macros.h"


/*
**  Send authentication information to an NNTP server.
*/
int
NNTPsendpassword(server, FromServer, ToServer)
    char		*server;
    FILE		*FromServer;
    FILE		*ToServer;
{
    register FILE	*F;
    register char	*p;
    char		buff[SMBUF];
    char		input[SMBUF];
    char		*user;
    char		*pass;
    char		*style;
    int			oerrno;

    /* What server are we interested in?  Default to the campus one. */
    if (server == NULL
     && (server = GetConfigValue(_CONF_SERVER)) == NULL)
	return -1;

    /* Open the password file; coarse check on errno, but good enough. */
    if ((F = fopen(_PATH_NNTPPASS, "r")) == NULL)
	return errno == EPERM ? -1 : 0;

    /* Scan the file, skipping blank and comment lines. */
    while (fgets(buff, sizeof buff, F) != NULL) {
	if ((p = strchr(buff, '\n')) != NULL)
	    *p = '\0';
	if (buff[0] == '\0' || buff[0] == COMMENT_CHAR)
	    continue;

	/* Parse the line. */
	if ((user = strchr(buff, ':')) == NULL)
	    continue;
	*user++ = '\0';
	if ((pass = strchr(user, ':')) == NULL)
	    continue;
	*pass++ = '\0';
	if ((style = strchr(pass, ':')) != NULL) {
	    *style++ = '\0';
	    if (!EQ(style, "authinfo")) {
		errno = EDOM;
		break;
	    }
	}

	if (!caseEQ(server, buff))
	    continue;

	if (*user) {
	    /* Send the first part of the command, get a reply. */
	    (void)fprintf(ToServer, "authinfo user %s\r\n", user);
	    if (fflush(ToServer) == EOF || ferror(ToServer))
		break;
	    if (fgets(input, sizeof input, FromServer) == NULL
	     || atoi(input) != NNTP_AUTH_NEXT_VAL)
		break;
	}

	if (*pass) {
	    /* Send the second part of the command, get a reply. */
	    (void)fprintf(ToServer, "authinfo pass %s\r\n", pass);
	    if (fflush(ToServer) == EOF || ferror(ToServer))
		break;
	    if (fgets(input, sizeof input, FromServer) == NULL
	     || atoi(input) != NNTP_AUTH_OK_VAL)
		break;
	}

	/* Authenticated. */
	(void)fclose(F);
	return 0;
    }

    /* End of file without finding a password, that's okay. */
    if (feof(F)) {
	(void)fclose(F);
	return 0;
    }

    /* Save errno, close the file, fail. */
    oerrno = errno;
    (void)fclose(F);
    errno = oerrno;
    return -1;
}
