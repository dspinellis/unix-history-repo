
/* 	pitest.c		V1.0		      30 Aug 1988        */

/*************************************************************************/
/*                                                                       */
/*                                                                       */
/*       ________________________________________________________        */
/*      /                                                        \       */
/*     |          AAA          CCCCCCCCCCCCCC    CCCCCCCCCCCCCC   |      */
/*     |         AAAAA        CCCCCCCCCCCCCCCC  CCCCCCCCCCCCCCCC  |      */
/*     |        AAAAAAA       CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |      */
/*     |       AAAA AAAA      CCCC              CCCC              |      */
/*     |      AAAA   AAAA     CCCC              CCCC              |      */
/*     |     AAAA     AAAA    CCCC              CCCC              |      */
/*     |    AAAA       AAAA   CCCC              CCCC              |      */
/*     |   AAAA  AAAAAAAAAAA  CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |      */
/*     |  AAAA    AAAAAAAAAAA CCCCCCCCCCCCCCCC  CCCCCCCCCCCCCCCC  |      */
/*     | AAAA      AAAAAAAAA   CCCCCCCCCCCCCC    CCCCCCCCCCCCCC   |      */
/*      \________________________________________________________/       */
/*                                                                       */
/*  	Copyright (c) 1988 by Advanced Computer Communications           */
/*  	720 Santa Barbara Street, Santa Barbara, California  93101       */
/*  	(805) 963-9431                                                   */
/*                                                                       */
/*                                                                       */
/*  File:		pitest.c                                         */
/*                                                                       */
/*  Author:		Brad Engstrom                                    */
/*                                                                       */
/*  Project:		Installation verification program for ACC        */
/*			ACP 5250/6250 Programmers Interface.             */
/*                                                                       */
/*  Function:                                                            */
/*	This program fork a child process, establishes an X.25 connection*/
/*	with it and sends data packets to the child.   The child echoes  */
/*	the packets back to the sender. The sender verifies the packets. */
/*	The program will print PASSED or FAILED to indicate the results  */
/*	of the test.							 */
/*                                                                       */
/*  Components:		pitest.c, /sys/vaxif/if_pivar.h                  */
/*                                                                       */
/*  Revision History:                                                    */
/*                                                                       */
/* 30-AUG-1988  Brad Engstrom:  first generated.                         */
/*************************************************************************/

#include <stdio.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/file.h>
#include <signal.h>
#include <ctype.h>
#include "/sys/vaxif/if_pivar.h"

#define PIPROTOCOL	7

#define CALL	0x0
#define ANSWER 	0x3
#define RING   	0x1
#define CLEAR	0x4

/* PREDEFINED SUPERVISORY MESSAGES */
u_char          callmsg[] =
{CALL, 0x4, 0x0, 0x22,
 0xe, 3, 1, 1, 0, 8, 0, 5, 0, 0, 1, 0, 1, 1, 1,
 0xe, 3, 1, 1, 0, 8, 0, 5, 0, 0, 1, 0, 1, 1, 1,
 1, PIPROTOCOL,
 0,
 0};

u_char		clearmsg[] = {CLEAR, 0x0, 0, 1, 0};


#define DATABUFSIZE 1024
u_char          databuffer[DATABUFSIZE];
u_char          readbuffer[DATABUFSIZE];

#define PACKETSTOSEND	75

main (argc, argv)
int             argc;
char           *argv[];

{
    int             fd1, fd2;

    if (argc != 3)
    {
	fprintf (stderr, "Usage: pitest xxdev1 xxdev2\n");
	return (1);
    }
    if ((fd1 = open (argv[1], O_RDWR)) == -1)
	error (argv[0]);
    if ((fd2 = open (argv[2], O_RDWR)) == -1)
	error (argv[0]);

    if (fork ())
    {
	caller (fd1);
        fprintf(stderr,"pitest: PASSED\n");
    }
    else
	callee (fd2);
    return (0);
}

caller (fd)
int             fd;
{
    int             lcn, i;
    struct pi_dblock pd, rd;
    caddr_t cp;
    char c;

    sleep (5);			/* give callee chance to set up */

    /*
     * RESERVE THE LOGICAL CIRCUIT TO USE 
     */
    if (ioctl (fd, XIOGETLCN, &lcn) == -1)
	error ("caller: XIOGETLCN");
    lcn &= 0xff;

    /*
     * MAKE THE CALL 
     */
    fprintf(stderr,"Making X.25 call...\n");
    callmsg[1] = lcn * 2;
    pd.dataptr = (caddr_t) callmsg;
    pd.length = sizeof (callmsg);
    pd.lcn = 0;
    pd.subfunc = 0;
    pd.flags = 0;
    if (ioctl (fd, XIOWRITE, &pd) == -1)
	error ("caller: XIOWRITE");

    /*
     * READ AND CHECK THAT WE GOT AN ANSWER MESSAGE 
     */
    pd.dataptr = (caddr_t) databuffer;
    pd.length = DATABUFSIZE;
    pd.flags = 0;
    if (ioctl (fd, XIOREAD, &pd) == -1)
	error ("caller: XIOREAD");
    if (pd.lcn != 0 || pd.dataptr[0] != ANSWER)
    {
	fprintf (stderr, "caller: did not get answer message\n");
	exit (1);
    }
    fprintf(stderr,"X.25 connection established\n");

    /*
     * SEND 25 DATA PACKETS, EXPECT THEM TO BE ECHOED 
     */
    fprintf(stderr,"Sending and verifying data packets\n");
    pd.dataptr = (caddr_t) databuffer;
    pd.subfunc = 0;
    pd.flags = 0;
    pd.lcn = lcn;
    /* fill buffer with data */
    for (cp = pd.dataptr, c = '0'; cp < pd.dataptr + DATABUFSIZE; cp++)
    {
	*cp = c;
	if (++c > '9')
	    c = '0';
    }
    srandom (getpid ());
    for (i = 0; i < PACKETSTOSEND; i++)
    {
	pd.length = random () & 0x3ff;
	if (pd.length == 0)
	    pd.length = 1;
	if (ioctl (fd, XIOWRITE, &pd) == -1)
	    error ("caller: XIOWRITE");
	rd.dataptr = (caddr_t) readbuffer;
	rd.length = DATABUFSIZE;
	rd.flags = 0;
	if (ioctl (fd, XIOREAD, &rd) == -1)
	    error ("caller: XIOREAD");
	if (rd.lcn != lcn)
	{
	    fprintf (stderr, "caller: mismatched lcn (%d != %d)\n",rd.lcn,lcn);
	    exit (1);
	}
	if (rd.length != pd.length)
	{
	    fprintf (stderr, "caller: mismatched length\n");
	    exit (1);
	}
	if (bcmp (pd.dataptr, rd.dataptr, rd.length))
	{
	    fprintf (stderr, "caller: data does not match\n");
	    exit (1);
	}
        fprintf(stderr,"+");
    }
    fprintf(stderr,"\n");

    /*
     * CLEAR THE CALL. 
     */
    clearmsg[1] = lcn * 2;
    pd.dataptr = (caddr_t) clearmsg;
    pd.length = sizeof (clearmsg);
    pd.lcn = 0;
    pd.subfunc = 0;
    pd.flags = 0;
    if (ioctl (fd, XIOWRITE, &pd) == -1)
	error ("caller: XIOWRITE");

    /*
     * WAIT FOR CLEAR CONFIRMATION 
     */
    for (;;)
    {
	pd.dataptr = (caddr_t) databuffer;
	pd.length = DATABUFSIZE;
	pd.flags = 0;
	if (ioctl (fd, XIOREAD, &pd) == -1)
	    error ("caller: XIOREAD");
	if (pd.lcn == 0 && pd.dataptr[0] == CLEAR)
		break;
	else
	{
	    fprintf (stderr, "caller: packet was not a clear\n");
	    exit(1);
	}
    }

    /*
     * FREE THE LOGICAL CHANNEL AND RESOURCES 
     */
    if (ioctl (fd, XIOFREELCN, &lcn) == -1)
	error("caller: XIOFREELCN");

    /*
     * CLOSE THE DEVICE 
     */
    close (fd);
}

callee (fd)
int             fd;
{
    struct pi_dblock pd, rd;
    proto_range     pr;
    int             lcn, i;

    /*
     * RESERVE THE LOGICAL CIRCUIT TO USE 
     */
    if (ioctl (fd, XIOGETLCN, &lcn) == -1)
	error ("callee: XIOGETLCN");
    lcn &= 0xff;

    /*
     * TELL THE DRIVER WHICH PROTOCOLS THIS CHANNEL WILL ACCEPT 
     */
    pr.lower = PIPROTOCOL;
    pr.upper = PIPROTOCOL;
    if (ioctl (fd, XIOACCRING, &pr) == -1)
	error ("callee: XIOACCRING");

    /*
     * WAIT FOR A RING TO COME IN AND THEN ANSWER IT. 
     */
    for (;;)
    {
	pd.dataptr = (caddr_t) databuffer;
	pd.length = DATABUFSIZE;
	pd.flags = 0;
	if (ioctl (fd, XIOREAD, &pd) == -1)
	    error ("XIOREAD");
	if (pd.lcn == 0 && pd.dataptr[0] == RING)
	{
	    databuffer[0] = ANSWER;
	    databuffer[1] = lcn * 2;
	    /* databuffer[2] already has data */
	    databuffer[3] = 6;
	    databuffer[4] = 0;	/* length of called addr */
	    databuffer[5] = 0;	/* length of calling addr */
	    databuffer[6] = 1;	/* length of protocol */
	    databuffer[7] = PIPROTOCOL;	/* use protocol 7 */
	    databuffer[8] = 0;	/* length of facilities */
	    databuffer[9] = 0;	/* length of user data */
	    pd.length = 10;
	    pd.flags = 0;
	    pd.lcn = 0;
	    pd.subfunc = 0;
	    if (ioctl (fd, XIOWRITE, &pd) == -1)
		error ("XIOWRITE");
	    break;
	}
	else
	    fprintf (stderr, "callee: packet was not a ring\n");
    }

    /*
     * ECHO 25 PACKETS 
     */
    for (i = 0; i < PACKETSTOSEND; i++)
    {
	rd.dataptr = (caddr_t) readbuffer;
	rd.length = DATABUFSIZE;
	rd.flags = 0;
	if (ioctl (fd, XIOREAD, &rd) == -1)
	    error ("callee: XIOREAD");
	if (rd.lcn == 0)
	{
	    fprintf (stderr, "callee: unexpected supervisor message\n");
	    exit (1);
	}
	if (ioctl (fd, XIOWRITE, &rd) == -1)
	    error ("callee: XIOWRITE");
    }

    /*
     * WAIT FOR THE CLEAR
     */
    for (;;)
    {
	pd.dataptr = (caddr_t) databuffer;
	pd.length = DATABUFSIZE;
	pd.flags = 0;
	if (ioctl (fd, XIOREAD, &pd) == -1)
	    error ("callee: XIOREAD");
	if (pd.lcn == 0 && pd.dataptr[0] == CLEAR)
		break;
	else
	{
	    fprintf (stderr, "callee: packet was not a clear\n");
	    exit(1);
	}
    }

    /*
     * SEND A CLEAR CONFIRMATION
     */
    pd.dataptr[1] = lcn * 2; /* patch up clear message and send it back */
    if (ioctl (fd, XIOWRITE, &pd) == -1)
        error ("callee: XIOREAD");

    /*
     * FREE THE LOGICAL CHANNEL AND RESOURCES 
     */
    if (ioctl (fd, XIOFREELCN, &lcn) == -1)
	error("callee: XIOFREELCN");

    /*
     * CLOSE THE DEVICE 
     */
    close (fd);
}

error(s)
char *s;
{
	perror(s);
	fprintf("pitest: FAILED\n");
	exit(1);
}
