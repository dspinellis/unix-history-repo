/*  $Revision: 1.8 $
**
*/
#include <stdio.h>
#include <sys/types.h>
#include <netdb.h>
#include "configdata.h"
#include "paths.h"
#include "clibrary.h"
#include "libinn.h"


/*
**  Get the fully-qualified domain name for this host.
*/
char *
GetFQDN()
{
    static char		buff[SMBUF];
    struct hostent	*hp;
    char		*p;
    char		temp[SMBUF + 2];

    /* Return any old results. */
    if (buff[0])
	return buff;

    /* Try gethostname. */
    if (gethostname(buff, (int)sizeof buff) < 0)
	return NULL;
    if (strchr(buff, '.') != NULL)
	return buff;

    /* See if DNS (or /etc/hosts) gives us a full domain name. */
    if ((hp = gethostbyname(buff)) == NULL)
	return NULL;
    if (strchr(hp->h_name, '.') == NULL) {
	/* Try to force DNS lookup if NIS/whatever gets in the way. */
	(void)strncpy(temp, buff, sizeof buff);
	(void)strcat(temp, ".");
	hp = gethostbyname(temp);
    }
    if (hp != NULL && strchr(hp->h_name, '.') != NULL) {
	if (strlen(hp->h_name) < sizeof buff - 1)
	    return strcpy(buff, hp->h_name);
	/* Doesn't fit; make sure we don't return bad data next time. */
	buff[0] = '\0';
	return hp->h_name;
    }

    /* Get the domain name config parameter and append it. */
    if ((p = GetFileConfigValue(_CONF_DOMAIN)) == NULL || *p == '\0')
	return NULL;
    if (strlen(buff) + 1 + strlen(p) > sizeof buff - 1)
	/* Doesn't fit. */
	return NULL;
    (void)strcat(buff, ".");
    (void)strcat(buff, p);
    return buff;
}
