/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)df12.c	4.4 (Berkeley) %G%";
#endif /* not lint */

#include "condevs.h"

/*
 *	df12popn(telno, flds, dev) connect to df12 modem (pulse call)
 *	df12topn(telno, flds, dev) connect to df12 modem (tone call)
 *	char *flds[], *dev[];
 *
 *	return codes:
 *		>0  -  file number  -  ok
 *		CF_DIAL,CF_NODEV  -  failed
 */

df12popn (telno, flds, dev)
char   *telno,
       *flds[];
struct Devices *dev;
{
    return df12opn (telno, flds, dev, 0);
}

df12topn (telno, flds, dev)
char   *telno,
       *flds[];
struct Devices *dev;
{
    return df12opn (telno, flds, dev, 1);
}

/* ARGSUSED */
df12opn (telno, flds, dev, toneflag)
char   *telno;
char   *flds[];
struct Devices *dev;
int     toneflag;
{
    int     phindex, dh = -1;
    extern  errno;
    char    dcname[20], newphone[64];

    sprintf (dcname, "/dev/%s", dev -> D_line);
    DEBUG (4, "dc - %s\n", dcname);
    if (setjmp (Sjbuf))
    {
	logent (dcname, "TIMEOUT");
	if (dh >= 0)
	    close (dh);
	return CF_DIAL;
    }
    signal (SIGALRM, alarmtr);
    getnextfd ();
    alarm (10);
    dh = open (dcname, 2);/* read/write */
    alarm (0);

 /* modem is open */

 /* First, adjust our phone number string.  These modems don't 
  * like any characters but digits and "=" signs (for delay)
  */
    for (phindex = 0; *telno; telno++)
    {
	if (*telno == '=' || (*telno >= '0' && *telno <= '9'))
	    newphone[phindex++] = *telno;
	if (phindex == 64)
	{
	    logent (dcname, "Phone number too long");
	    close (dh);
	    return CF_DIAL;
	}
    }
    newphone[phindex] = '\0';
    next_fd = -1;
    if (dh >= 0)
    {
	fixline (dh, dev -> D_speed);
	if (dochat (dev, flds, dh))
	{
	    logent (dcname, "CHAT FAILED");
	    close (dh);
	    return CF_DIAL;
	}
	slowrite (dh, "\02");
	if (expect ("Ready\r\n", dh) != 0)
	{
	    DEBUG (4, "Didn't get 'Ready' response.\n", NULL);
	    logent (dcname, "Modem not responding");
	    close (dh);
	    return CF_DIAL;
	}
	DEBUG (4, "Got 'Ready' response\n", NULL);
	DEBUG (7, "Writing control select flag %c\n", toneflag ? 'T' : 'P');
	slowrite (dh, toneflag ? "T" : "P");
	DEBUG (4, "Writing telephone number %s\n", newphone);
	slowrite (dh, newphone);
	DEBUG (7, "Telephone number written\n", NULL);
	slowrite (dh, "#");
	DEBUG (7, "Writing # sign\n", NULL);

	if (expect ("Attached\r\n", dh) != 0)
	{
	    logent (dcname, "No carrier");
	    strcpy (devSel, dev -> D_line);
	    df12cls (dh);
	    return CF_DIAL;
	}

    }
    if (dh < 0)
    {
	logent (dcname, "CAN'T OPEN");
	return CF_NODEV;
    }
    else
    {
	DEBUG (4, "df12 ok\n", CNULL);
	return dh;
    }
}

df12cls (fd)
int     fd;
{
    char    dcname[20];
    struct sgttyb   hup,
                    sav;

    if (fd > 0)
    {
	sprintf (dcname, "/dev/%s", devSel);
	DEBUG (4, "Hanging up fd = %d\n", fd);
    /* 
     * code to drop DTR -- change to 0 baud then back to default.
     */
	gtty (fd, &hup);
	gtty (fd, &sav);
	hup.sg_ispeed = B0;
	hup.sg_ospeed = B0;
	stty (fd, &hup);
	sleep (2);
	stty (fd, &sav);
    /* 
     * now raise DTR -- close the device & open it again.
     */
	sleep (2);
	close (fd);
	sleep (2);
	delock (devSel);
    }
}
