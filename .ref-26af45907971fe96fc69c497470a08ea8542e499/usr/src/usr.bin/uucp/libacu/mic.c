/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)mic.c	4.4 (Berkeley) %G%";
#endif /* not lint */

#include "condevs.h"
#ifdef MICOM

/*
 *	micopn: establish connection through a micom.
 *	Returns descriptor open to tty for reading and writing.
 *	Negative values (-1...-7) denote errors in connmsg.
 *	Be sure to disconnect tty when done, via HUPCL or stty 0.
 */
micopn(flds)
register char *flds[];
{
	extern errno;
	char *rindex(), *fdig(), dcname[20];
	int dh, ok = 0, speed;
	register struct condev *cd;
	register FILE *dfp;
	struct Devices dev;

	dfp = fopen(DEVFILE, "r");
	ASSERT(dfp != NULL, "Can't open", DEVFILE, 0);

	signal(SIGALRM, alarmtr);
	dh = -1;
	for(cd = condevs; ((cd->CU_meth != NULL)&&(dh < 0)); cd++) {
		if (snccmp(flds[F_LINE], cd->CU_meth) == SAME) {
			fseek(dfp, (off_t)0, 0);
			while(rddev(dfp, &dev) != FAIL) {
				if (strcmp(flds[F_CLASS], dev.D_class) != SAME)
					continue;
				if (snccmp(flds[F_LINE], dev.D_type) != SAME)
					continue;
				if (mlock(dev.D_line) == FAIL)
					continue;

				sprintf(dcname, "/dev/%s", dev.D_line);
				getnextfd();
				alarm(10);
				if (setjmp(Sjbuf)) {
					delock(dev.D_line);
					logent(dev.D_line,"micom open TIMEOUT");
					dh = -1;
					break;
				}
				dh = open(dcname, 2);
				alarm(0);
				next_fd = -1;
				if (dh > 0) {
					break;
				}
				devSel[0] = '\0';
				delock(dev.D_line);
			}
		}
	}
	fclose(dfp);
	if (dh < 0)
		return CF_NODEV;

	speed = atoi(fdig(flds[F_CLASS]));
	fixline(dh, speed);
	sleep(1);

	/* negotiate with micom */
	if (speed != 4800)	/* damn their eyes! */
		write(dh, "\r", 1);
	else
		write(dh, " ", 1);

	DEBUG(4, "wanted %s ", "SELECTION");
	ok = expect("SELECTION", dh);
	DEBUG(4, "got %s\n", ok ? "?" : "that");
	if (ok == 0) {
		write(dh, flds[F_PHONE], strlen(flds[F_PHONE]));
		sleep(1);
		write(dh, "\r", 1);
		DEBUG(4, "wanted %s ", "GO");
		ok = expect("GO", dh);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
	}

	if (ok != 0) {
		if (dh > 2)
			close(dh);
		DEBUG(4, "micom failed\n", "");
		delock(dev.D_line);
		return(CF_DIAL);
	} 
	else
		DEBUG(4, "micom ok\n", "");

	CU_end = cd->CU_clos;
	strcat(devSel, dev.D_line);	/* for later unlock */
	return dh;
}

miccls(fd)
register int fd;
{

	if (fd > 0) {
		close(fd);
		delock(devSel);
	}
}
#endif MICOM
