/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)v831.c	5.3 (Berkeley) 6/1/90";
#endif /* not lint */

/*
 * Routines for dialing up on Vadic 831
 */
#include <sys/time.h>

#include "tip.h"

int	v831_abort();
static	int alarmtr();
extern	errno;

static jmp_buf jmpbuf;
static int child = -1;

v831_dialer(num, acu)
        char *num, *acu;
{
        int status, pid, connected = 1;
        register int timelim;

        if (boolean(value(VERBOSE)))
                printf("\nstarting call...");
#ifdef DEBUG
        printf ("(acu=%s)\n", acu);
#endif
        if ((AC = open(acu, O_RDWR)) < 0) {
                if (errno == EBUSY)
                        printf("line busy...");
                else
                        printf("acu open error...");
                return (0);
        }
        if (setjmp(jmpbuf)) {
                kill(child, SIGKILL);
                close(AC);
                return (0);
        }
        signal(SIGALRM, alarmtr);
        timelim = 5 * strlen(num);
        alarm(timelim < 30 ? 30 : timelim);
        if ((child = fork()) == 0) {
                /*
                 * ignore this stuff for aborts
                 */
                signal(SIGALRM, SIG_IGN);
		signal(SIGINT, SIG_IGN);
                signal(SIGQUIT, SIG_IGN);
                sleep(2);
                exit(dialit(num, acu) != 'A');
        }
        /*
         * open line - will return on carrier
         */
        if ((FD = open(DV, O_RDWR)) < 0) {
#ifdef DEBUG
                printf("(after open, errno=%d)\n", errno);
#endif
                if (errno == EIO)
                        printf("lost carrier...");
                else
                        printf("dialup line open failed...");
                alarm(0);
                kill(child, SIGKILL);
                close(AC);
                return (0);
        }
        alarm(0);
#ifdef notdef
        ioctl(AC, TIOCHPCL, 0);
#endif
        signal(SIGALRM, SIG_DFL);
        while ((pid = wait(&status)) != child && pid != -1)
                ;
        if (status) {
                close(AC);
                return (0);
        }
        return (1);
}

static
alarmtr()
{

        alarm(0);
        longjmp(jmpbuf, 1);
}

/*
 * Insurance, for some reason we don't seem to be
 *  hanging up...
 */
v831_disconnect()
{
        struct sgttyb cntrl;

        sleep(2);
#ifdef DEBUG
        printf("[disconnect: FD=%d]\n", FD);
#endif
        if (FD > 0) {
                ioctl(FD, TIOCCDTR, 0);
                ioctl(FD, TIOCGETP, &cntrl);
                cntrl.sg_ispeed = cntrl.sg_ospeed = 0;
                ioctl(FD, TIOCSETP, &cntrl);
                ioctl(FD, TIOCNXCL, (struct sgttyb *)NULL);
        }
        close(FD);
}

v831_abort()
{

#ifdef DEBUG
        printf("[abort: AC=%d]\n", AC);
#endif
        sleep(2);
        if (child > 0)
                kill(child, SIGKILL);
        if (AC > 0)
                ioctl(FD, TIOCNXCL, (struct sgttyb *)NULL);
                close(AC);
        if (FD > 0)
                ioctl(FD, TIOCCDTR, 0);
        close(FD);
}

/*
 * Sigh, this probably must be changed at each site.
 */
struct vaconfig {
	char	*vc_name;
	char	vc_rack;
	char	vc_modem;
} vaconfig[] = {
	{ "/dev/cua0",'4','0' },
	{ "/dev/cua1",'4','1' },
	{ 0 }
};

#define pc(x)	(c = x, write(AC,&c,1))
#define ABORT	01
#define SI	017
#define STX	02
#define ETX	03

static
dialit(phonenum, acu)
	register char *phonenum;
	char *acu;
{
        register struct vaconfig *vp;
	struct sgttyb cntrl;
        char c, *sanitize();
        int i, two = 2;

        phonenum = sanitize(phonenum);
#ifdef DEBUG
        printf ("(dial phonenum=%s)\n", phonenum);
#endif
        if (*phonenum == '<' && phonenum[1] == 0)
                return ('Z');
	for (vp = vaconfig; vp->vc_name; vp++)
		if (strcmp(vp->vc_name, acu) == 0)
			break;
	if (vp->vc_name == 0) {
		printf("Unable to locate dialer (%s)\n", acu);
		return ('K');
	}
        ioctl(AC, TIOCGETP, &cntrl);
        cntrl.sg_ispeed = cntrl.sg_ospeed = B2400;
        cntrl.sg_flags = RAW | EVENP | ODDP;
        ioctl(AC, TIOCSETP, &cntrl);
	ioctl(AC, TIOCFLUSH, &two);
        pc(STX);
	pc(vp->vc_rack);
	pc(vp->vc_modem);
	while (*phonenum && *phonenum != '<')
		pc(*phonenum++);
        pc(SI);
	pc(ETX);
        sleep(1);
        i = read(AC, &c, 1);
#ifdef DEBUG
        printf("read %d chars, char=%c, errno %d\n", i, c, errno);
#endif
        if (i != 1)
		c = 'M';
        if (c == 'B' || c == 'G') {
                char cc, oc = c;

                pc(ABORT);
                read(AC, &cc, 1);
#ifdef DEBUG
                printf("abort response=%c\n", cc);
#endif
                c = oc;
                v831_disconnect();
        }
        close(AC);
#ifdef DEBUG
        printf("dialit: returns %c\n", c);
#endif
        return (c);
}

static char *
sanitize(s)
	register char *s;
{
        static char buf[128];
        register char *cp;

        for (cp = buf; *s; s++) {
		if (!isdigit(*s) && *s == '<' && *s != '_')
			continue;
		if (*s == '_')
			*s = '=';
		*cp++ = *s;
	}
        *cp++ = 0;
        return (buf);
}
