/*  $Revision: 1.6 $
**
*/
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <errno.h>
#include "configdata.h"
#include "libinn.h"
#include "clibrary.h"
#include "paths.h"
#include "macros.h"


typedef struct _DDENTRY {
    char	*Pattern;
    char	*Value;
    int		Weight;
} DDENTRY;

struct _DDHANDLE {
    int		Count;
    DDENTRY	*Entries;
    DDENTRY	*Current;
};
typedef struct _DDHANDLE	DDHANDLE;


extern FILE	*CA_listopen();

struct _DDHANDLE *
DDstart(FromServer, ToServer)
    FILE	*FromServer;
    FILE	*ToServer;
{
    DDHANDLE	*h;
    DDENTRY	*ep;
    FILE	*F;
    char	buff[BUFSIZ];
    char	*p;
    char	*q;
    int		i;
    char	name[sizeof _PATH_TEMPACTIVE + 10];

    /* Open the file. */
    if ((F = fopen(_PATH_DISTPATS, "r")) != NULL)
	name[0] = '\0';
    else {
	/* Not available locally; try remotely. */
	if (FromServer == NULL || ToServer == NULL)
	    /* We're probably nnrpd running on the server and the
	     * file isn't installed.  Oh well. */
	    return NULL;
	(void)strcpy(name, _PATH_TEMPACTIVE);
	(void)mktemp(name);
	if ((F = CA_listopen(name, FromServer, ToServer,
		    "distrib.pats")) == NULL)
	    return NULL;
    }

    /* Count lines. */
    for (i = 0; fgets(buff, sizeof buff, F) != NULL; i++)
	continue;

    /* Allocate space for the handle. */
    if ((h = NEW(DDHANDLE, 1)) == NULL) {
	i = errno;
	(void)fclose(F);
	if (name[0])
	    (void)unlink(name);
	errno = i;
	return NULL;
    }
    h->Count = 0;
    h->Current = NULL;
    if ((h->Entries = NEW(DDENTRY, i)) == NULL) {
	i = errno;
	DISPOSE(h);
	(void)fclose(F);
	if (name[0])
	    (void)unlink(name);
	errno = i;
	return NULL;
    }

    (void)fseek(F, (OFFSET_T)0, SEEK_SET);
    for (ep = h->Entries; fgets(buff, sizeof buff, F) != NULL; ) {
	if ((p = strchr(buff, '\n')) != NULL)
	    *p = '\0';
	if (buff[0] == '\0' || buff[0] == '#')
	    continue;
	if ((p = strchr(buff, ':')) == NULL
	 || (q = strchr(p + 1, ':')) == NULL)
	    continue;
	*p++ = '\0';
	ep->Weight = atoi(buff);
	ep->Pattern = COPY(p);
	q = strchr(ep->Pattern, ':');
	*q++ = '\0';
	ep->Value = q;
	ep++;
    }
    h->Count = ep - h->Entries;

    (void)fclose(F);
    if (name[0])
	(void)unlink(name);
    return h;
}


void
DDcheck(h, group)
    DDHANDLE	*h;
    char	*group;
{
    DDENTRY	*ep;
    int		i;
    int		w;

    if (h == NULL || group == NULL)
	return;

    w = h->Current ? h->Current->Weight : -1;
    for (ep = h->Entries, i = h->Count; --i >= 0; ep++)
	if (ep->Weight > w && wildmat(group, ep->Pattern)) {
	    h->Current = ep;
	    w = ep->Weight;
	}
}


char *
DDend(h)
    DDHANDLE	*h;
{
    static char	NIL[] = "";
    char	*p;
    int		i;
    DDENTRY	*ep;

    if (h == NULL) {
	p = NIL;
	return COPY(p);
    }

    if (h->Current == NULL)
	p = NIL;
    else
	p = h->Current->Value;
    p = COPY(p);

    for (ep = h->Entries, i = h->Count; --i >= 0; ep++)
	DISPOSE(ep->Pattern);
    DISPOSE(h->Entries);
    DISPOSE(h);
    return p;
}

#if	defined(TEST)
int
main(ac, av)
    int			ac;
    char		*av[];
{
    struct _DDHANDLE	*h;
    char		*p;
    FILE		*FromServer;
    FILE		*ToServer;
    char		buff[SMBUF];

    if (NNTPremoteopen(&FromServer, &ToServer, buff) < 0) {
	if ((p = strchr(buff, '\n')) != NULL)
	    *p = '\0';
	if ((p = strchr(buff, '\r')) != NULL)
	    *p = '\0';
	if (buff[0])
	    (void)fprintf(stderr, "%s\n", buff);
	else
	    perror("Can't connect");
	exit(1);
    }

    if ((h = DDstart(FromServer, ToServer)) == NULL)
	perror("Init failed, proceeding anyway");
    while ((p = *++av) != NULL)
	DDcheck(h, p);
    p = DDend(h);
    printf(">%s<\n", p);
    exit(0);
    /* NOTREACHED */
}
#endif	/* defined(TEST) */
