/*  $Revision: 1.10 $
**
*/
#include <stdio.h>
#include <sys/types.h>
#include "configdata.h"
#include "paths.h"
#include "libinn.h"
#include "clibrary.h"
#include "macros.h"


/*
**  Read the moderators file, looking for a moderator.
*/
char *
GetModeratorAddress(group)
    char		*group;
{
    static char		address[SMBUF];
    register FILE	*F;
    register char	*p;
    char		*save;
    char		buff[BUFSIZ];
    char		name[SMBUF];

    (void)strcpy(name, group);
    address[0] = '\0';
    if ((F = fopen(_PATH_MODERATORS, "r")) != NULL) {
	while (fgets(buff, sizeof buff, F) != NULL) {
	    /* Skip blank and comment lines. */
	    if ((p = strchr(buff, '\n')) != NULL)
		*p = '\0';
	    if (buff[0] == '\0' || buff[0] == COMMENT_CHAR)
		continue;

	    /* Snip off the first word. */
	    if ((p = strchr(buff, ':')) == NULL)
		/* Malformed line... */
		continue;
	    *p++ = '\0';

	    /* If it pattern-matches the newsgroup, the second field is a
	     * format for mailing, with periods changed to dashes. */
	    if (wildmat(name, buff)) {
		for (save = p; ISWHITE(*save); save++)
		    continue;
		for (p = name; *p; p++)
		    if (*p == '.')
			*p = '-';
		(void)sprintf(address, save, name);
		break;
	    }
	}

	(void)fclose(F);
	if (address[0])
	    return address;
    }

    /* If we don't have an address, see if the config file has a default. */
    if ((save = GetConfigValue(_CONF_MODMAILER)) == NULL)
	return NULL;

    for (p = name; *p; p++)
	if (*p == '.')
	    *p = '-';
    (void)sprintf(address, save, name);
    return address;
}
