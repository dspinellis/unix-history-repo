/*  $Revision: 1.3 $
**
*/
#include "configdata.h"
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include "paths.h"
#include "clibrary.h"
#include "libinn.h"
#include "macros.h"


typedef struct _PAIR {
    char	First;
    char	*Key;
    char	*Value;
} PAIR;

STATIC PAIR	*MAPdata;
STATIC PAIR	*MAPend;


/*
**  Free the map.
*/
void
MAPfree()
{
    register PAIR	*mp;

    for (mp = MAPdata; mp < MAPend; mp++) {
	DISPOSE(mp->Key);
	DISPOSE(mp->Value);
    }
    DISPOSE(MAPdata);
    MAPdata = NULL;
}


/*
**  Read the map file.
*/
void
MAPread(name)
    char		*name;
{
    register FILE	*F;
    register int	i;
    register PAIR	*mp;
    register char	*p;
    char		buff[BUFSIZ];

    if (MAPdata != NULL)
	MAPfree();

    /* Open file, count lines. */
    if ((F = fopen(name, "r")) == NULL) {
	(void)fprintf(stderr, "Can't open %s, %s\n", name, strerror(errno));
	exit(1);
    }
    for (i = 0; fgets(buff, sizeof buff, F) != NULL; i++)
	continue;
    mp = MAPdata = NEW(PAIR, i + 1);

    /* Read each line; ignore blank and comment lines. */
    (void)fseek(F, (OFFSET_T)0, SEEK_SET);
    while (fgets(buff, sizeof buff, F) != NULL) {
	if ((p = strchr(buff, '\n')) != NULL)
	    *p = '\0';
	if (buff[0] == '\0'
	 || buff[0] == COMMENT_CHAR
	 || (p = strchr(buff, ':')) == NULL)
	    continue;
	*p++ = '\0';
	mp->First = buff[0];
	mp->Key = COPY(buff);
	mp->Value = COPY(p);
	mp++;
    }
    (void)fclose(F);
    MAPend = mp;
}


/*
**  Look up a name in the map, return original value if not found.
*/
char *
MAPname(p)
    register char	*p;
{
    register PAIR	*mp;
    register char	c;

    for (c = *p, mp = MAPdata; mp < MAPend; mp++)
	if (c == mp->First && EQ(p, mp->Key))
	    return mp->Value;
    return p;
}
