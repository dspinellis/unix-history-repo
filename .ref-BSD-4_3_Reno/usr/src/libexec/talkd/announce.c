/*
 * Copyright (c) 1983 Regents of the University of California.
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
static char sccsid[] = "@(#)announce.c	5.8 (Berkeley) 6/1/90";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <sgtty.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <stdio.h>
#include <sys/wait.h>
#include <errno.h>
#include <syslog.h>

#include <protocols/talkd.h>
#include <paths.h>

extern	int errno;
extern	char hostname[];

/*
 * Announce an invitation to talk.
 *
 * Because the tty driver insists on attaching a terminal-less
 * process to any terminal that it writes on, we must fork a child
 * to protect ourselves
 */
announce(request, remote_machine)
	CTL_MSG *request;
	char *remote_machine;
{
	int pid, val, status;

	if (pid = fork()) {
		/* we are the parent, so wait for the child */
		if (pid == -1)		/* the fork failed */
			return (FAILED);
		do {
			val = wait(&status);
			if (val == -1) {
				if (errno == EINTR)
					continue;
				/* shouldn't happen */
				syslog(LOG_WARNING, "announce: wait: %m");
				return (FAILED);
			}
		} while (val != pid);
		if (status&0377 > 0)	/* we were killed by some signal */
			return (FAILED);
		/* Get the second byte, this is the exit/return code */
		return ((status >> 8) & 0377);
	}
	/* we are the child, go and do it */
	_exit(announce_proc(request, remote_machine));
}
	
/*
 * See if the user is accepting messages. If so, announce that 
 * a talk is requested.
 */
announce_proc(request, remote_machine)
	CTL_MSG *request;
	char *remote_machine;
{
	int pid, status;
	char full_tty[32];
	FILE *tf;
	struct stat stbuf;

	(void)sprintf(full_tty, "%s/%s", _PATH_DEV, request->r_tty);
	if (access(full_tty, 0) != 0)
		return (FAILED);
	if ((tf = fopen(full_tty, "w")) == NULL)
		return (PERMISSION_DENIED);
	/*
	 * On first tty open, the server will have
	 * it's pgrp set, so disconnect us from the
	 * tty before we catch a signal.
	 */
	ioctl(fileno(tf), TIOCNOTTY, (struct sgttyb *) 0);
	if (fstat(fileno(tf), &stbuf) < 0)
		return (PERMISSION_DENIED);
	if ((stbuf.st_mode&020) == 0)
		return (PERMISSION_DENIED);
	print_mesg(tf, request, remote_machine);
	fclose(tf);
	return (SUCCESS);
}

#define max(a,b) ( (a) > (b) ? (a) : (b) )
#define N_LINES 5
#define N_CHARS 120

/*
 * Build a block of characters containing the message. 
 * It is sent blank filled and in a single block to
 * try to keep the message in one piece if the recipient
 * in in vi at the time
 */
print_mesg(tf, request, remote_machine)
	FILE *tf;
	CTL_MSG *request;
	char *remote_machine;
{
	struct timeval clock;
	struct timezone zone;
	struct tm *localtime();
	struct tm *localclock;
	char line_buf[N_LINES][N_CHARS];
	int sizes[N_LINES];
	char big_buf[N_LINES*N_CHARS];
	char *bptr, *lptr;
	int i, j, max_size;

	i = 0;
	max_size = 0;
	gettimeofday(&clock, &zone);
	localclock = localtime( &clock.tv_sec );
	(void)sprintf(line_buf[i], " ");
	sizes[i] = strlen(line_buf[i]);
	max_size = max(max_size, sizes[i]);
	i++;
	(void)sprintf(line_buf[i], "Message from Talk_Daemon@%s at %d:%02d ...",
	hostname, localclock->tm_hour , localclock->tm_min );
	sizes[i] = strlen(line_buf[i]);
	max_size = max(max_size, sizes[i]);
	i++;
	(void)sprintf(line_buf[i], "talk: connection requested by %s@%s.",
		request->l_name, remote_machine);
	sizes[i] = strlen(line_buf[i]);
	max_size = max(max_size, sizes[i]);
	i++;
	(void)sprintf(line_buf[i], "talk: respond with:  talk %s@%s",
		request->l_name, remote_machine);
	sizes[i] = strlen(line_buf[i]);
	max_size = max(max_size, sizes[i]);
	i++;
	(void)sprintf(line_buf[i], " ");
	sizes[i] = strlen(line_buf[i]);
	max_size = max(max_size, sizes[i]);
	i++;
	bptr = big_buf;
	*bptr++ = ''; /* send something to wake them up */
	*bptr++ = '\r';	/* add a \r in case of raw mode */
	*bptr++ = '\n';
	for (i = 0; i < N_LINES; i++) {
		/* copy the line into the big buffer */
		lptr = line_buf[i];
		while (*lptr != '\0')
			*(bptr++) = *(lptr++);
		/* pad out the rest of the lines with blanks */
		for (j = sizes[i]; j < max_size + 2; j++)
			*(bptr++) = ' ';
		*(bptr++) = '\r';	/* add a \r in case of raw mode */
		*(bptr++) = '\n';
	}
	*bptr = '\0';
	fprintf(tf, big_buf);
	fflush(tf);
	ioctl(fileno(tf), TIOCNOTTY, (struct sgttyb *) 0);
}
