/* netmail.c - queue mail for the network server */

/* LINTLIBRARY */

#include "../h/strings.h"
#include <stdio.h>
#include "../zotnet/mts.h"


#define	NOTOK	(-1)
#define	OK	0

#define	NBYTES	8


static int  files = 0;

static char hex[] = "0123456789ABCDEF";

static  union {
    char    nbytes[NBYTES];

    struct {
	long    clock;
	short   pid;
	short   id;
    }       stamp;
}       template;

static char quefil[BUFSIZ];
static char wrkfil[BUFSIZ];
static FILE * fp;


long    time ();
char   *cdate (), *ctime ();

/*  */

int     nm_init (user, clock)
register char   *user;
register long   *clock;
{
    quefile (quefil, wrkfil);

    (void) unlink (wrkfil);
    if ((fp = fopen (wrkfil, "w")) == NULL)
	return NOTOK;
    (void) chmod (wrkfil, 0600);

    fprintf (fp, "%s %s\n", user, cdate (clock));

    return OK;
}


int     nm_wadr (mbox, host)
register char   *mbox,
 	        *host;
{
    fprintf (fp, "/%s %s\n", host, mbox);

    return OK;
}


int     nm_waend () {
    putc ('\n', fp);

    return OK;
}


int     nm_wtxt (buffer, cnt)
register char   *buffer;
register int     cnt;
{
    if (fwrite (buffer, sizeof *buffer, cnt, fp) != cnt)
	return NOTOK;

    return OK;
}


int     nm_wtend () {
    (void) fclose (fp);
    if (link (wrkfil, quefil) == NOTOK || unlink (wrkfil) == NOTOK)
	return NOTOK;

    return OK;
}

/*  */

static  quefile (que, lnk)
register char   *que,
 	        *lnk;
{
    register char  *p,
                   *q;
    char    buffer[BUFSIZ];

    template.stamp.pid = getpid ();
    template.stamp.id = files++;
    if (files >= 256) {
	files = 0;
	sleep (1);
    }
    (void) time (&template.stamp.clock);

    p = buffer;
    for (q = template.nbytes; q < &template.nbytes[NBYTES]; q++) {
	*p++ = hex[(*q >> 4) & 0xf];
	*p++ = hex[(*q) & 0xf];
    }
    *p = NULL;

    (void) sprintf (que, "%s/%s", Mailqdir, buffer);
    (void) sprintf (lnk, "%s/%s", TMailqdir, buffer);
}

/*  */

static char   *cdate (clock)
register long   *clock;
{
    char   *cp;

    cp = ctime (clock);
    cp[1] = cp[8];
    cp[2] = cp[9];
    cp[3] = '-';
    cp[7] = '-';
    cp[8] = cp[22];
    cp[9] = cp[23];
    cp[10] = '@';
    cp[19] = NULL;

    return cp + 1;
}
