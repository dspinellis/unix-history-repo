/* unbind.c - dish shell unbind and squid commands */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/uips/dish/RCS/unbind.c,v 7.2 91/02/22 09:30:31 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/quipu/uips/dish/RCS/unbind.c,v 7.2 91/02/22 09:30:31 mrose Interim $
 *
 *
 * $Log:	unbind.c,v $
 * Revision 7.2  91/02/22  09:30:31  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/03/15  11:20:40  mrose
 * quipu-sync
 * 
 * Revision 7.0  89/11/23  22:08:35  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#include <stdio.h>
#include "quipu/util.h"
#include "tailor.h"
#include "general.h"

extern int      errno;


#ifdef SOCKETS   	/* USE INTERNET SOCKETS */

#include "internet.h"

main(argc,argv)
int	argc;
char	*argv[];
{
	int 			sd,res,status;
	struct sockaddr_in	sin_buf;
	struct sockaddr_in	* sin = &sin_buf;
	char			buffer [BUFSIZ];
	char 			* ptr;
	
	if ((sd = start_tcp_client ((struct sockaddr_in *) 0, 0)) == NOTOK) {
		perror("start_tcp_client");
		exit(-20);
	}

	if (get_dish_sock (sin, 0, 1) != 0)
		exit (-21);

	if (join_tcp_server (sd, sin) == NOTOK) {
		(void) fprintf (stderr,"No connection and no cache !!!\n");
		(void) close_tcp_socket (sd);
		exit (0);
	}

	if ((ptr = rindex (argv[0], '/')) == NULLCP)
		(void) strcpy (buffer,argv[0]);
	else
		(void) strcpy (buffer,++ptr);

	argc--,argv++;

	while (argc--) {
		(void) strcat (buffer, " \"");
		(void) strcat (buffer, *argv++);
		(void) strcat (buffer, "\"");
	}
	(void) strcat (buffer, "\n");

	if (send(sd, buffer, strlen(buffer), 0) == -1) {
		perror("send");
		(void) close_tcp_socket (sd);
		exit (-25);
	}

	if ((res = recv(sd, buffer, BUFSIZ-1, 0)) == -1) {
		perror ("recv");
		(void) close_tcp_socket (sd);
		exit (-26);
	}
	*(buffer + res) = 0;

	if (*buffer == '2') {
		status = 1;
		if (res > 1)
			(void) write (2,&buffer[1],--res);
		while ( (res = recv(sd, buffer, BUFSIZ, 0)) > 0)
			(void) write (2,buffer,res);
	} else if (*buffer == '1') {
		status = 0;
		if (res > 1)
			(void) write (1,&buffer[1],--res);
		while ( (res = recv(sd, buffer, BUFSIZ, 0)) > 0)
			(void) write (1,buffer,res);
	}

	(void) close_tcp_socket (sd);

	exit (status);
}


#else	/* USE UNIX NAMED PIPES */

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>

char retfile [LINESIZE];
int fd;

main (argc,argv)
int argc;
char ** argv;
{
int res;
char buffer [BUFSIZ];
char sendfile [LINESIZE];
int i;
char * ptr;
void pipe_quit ();
char * getenv(), *sprintf();

	(void) umask(0);
	(void) sprintf (retfile,"/tmp/dish%d",getpid());
	if ( (ptr = getenv ("DISHPROC")) == NULLCP ) {
	    (void) sprintf (sendfile, "/tmp/dish-%d", getppid ());
	    (void) setenv ("DISHPROC", sendfile);
	}
	else
	    (void) strcpy (sendfile, ptr);
	
	setbuf (stdout,NULLCP);
	setbuf (stderr,NULLCP);

	if (mknod (retfile,S_IFIFO|0660,0) == -1) {
		(void) fprintf (stderr,"Can't create result file %s\n",retfile);
		exit (-5);
	}
	
	for (i=1; i<=15; i++)
		(void) signal(i,pipe_quit);

	if ((fd = open (sendfile,O_WRONLY|O_NDELAY)) == -1) {
		(void) fprintf (stderr,"No connection and no cache !!!\n");
		(void) unlink (retfile);
		exit (0);
	}

	argc--;
	if ((ptr = rindex (argv[0],'/')) == NULLCP)
		(void) sprintf (buffer,"%s:%s",retfile,argv[0]);
	else
		(void) sprintf (buffer,"%s:%s",retfile,++ptr);
	*argv++;
	
	while (argc--) {
		(void) strcat (buffer," ");
		(void) strcat (buffer,*argv++);
	}
	
	if (( res =write (fd, buffer,strlen (buffer))) == -1) {
		(void) fprintf (stderr,"Write failed\n");
		(void) close (fd);
		(void) unlink (retfile);		
		exit (-2);
	}
	(void) close (fd);


	/* get results */
	if (( fd = open (retfile,O_RDONLY)) < 0) {
		(void) fprintf (stderr,"Can't read results\n");
		(void) unlink (retfile);
		exit (-3);
	}
	
	if (( res = read (fd,buffer,BUFSIZ)) == -1) {
		(void) fprintf (stderr,"Read failed (%d)\n",errno);
		(void) unlink (retfile);		
		(void) close (fd);
		exit (-4);
	}

	*(buffer+res) = 0;

	if (*buffer == '2')
		fputs (&buffer[1], stderr);
	else if (*buffer == '1')
		fputs (&buffer[1], stdout);

	(void) close (fd);
	(void) unlink (retfile);

	if (*buffer == '2')
		exit (-1);
}

void pipe_quit (sig)
int     sig;
{
	(void) unlink (retfile);
	(void) fprintf (stderr,"(signal %d) exiting...\n",sig);
	exit (0);
}

#endif
