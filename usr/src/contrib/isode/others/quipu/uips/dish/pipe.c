/* pipe.c - Dish shell command handler */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/uips/dish/RCS/pipe.c,v 7.3 91/02/22 09:30:25 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/quipu/uips/dish/RCS/pipe.c,v 7.3 91/02/22 09:30:25 mrose Interim $
 *
 *
 * $Log:	pipe.c,v $
 * Revision 7.3  91/02/22  09:30:25  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/02/12  18:33:00  mrose
 * upate
 * 
 * Revision 7.1  90/03/15  11:20:36  mrose
 * quipu-sync
 * 
 * Revision 7.0  89/11/23  22:08:32  mrose
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
#include "usr.dirent.h"

extern int      errno;

#ifdef SOCKETS   	/* USE INTERNET SOCKETS */

#include "internet.h"

main(argc,argv)
int	argc;
char	*argv[];
{
	int 			sd,res;
	struct sockaddr_in	sin_buf;
	struct sockaddr_in	* sin = &sin_buf;
	char			buffer [BUFSIZ];
	char			dishname [BUFSIZ];
	char 			* ptr;
	
	isodetailor (argv[0], 1);

	if ((sd = start_tcp_client ((struct sockaddr_in *) 0, 0)) == NOTOK) {
		perror("start_tcp_client");
		exit(-20);
	}

	if (get_dish_sock (sin, 0, 1) != 0)
		exit (-21);

	if (join_tcp_server (sd, sin) == NOTOK) {
	        int	pid;
		(void) close_tcp_socket (sd);

fork_again: ;
		switch (pid = vfork ()) {
		case 0:
			/* child */
			(void) close_tcp_socket (sd);
			(void) strcpy (dishname,
				       _isodefile (isodebinpath, "dish"));
			{
				int i, nfds = getdtablesize ();

				for (i = fileno(stderr) + 1; i < nfds; i++)
					(void) close (i);
			}
 			execl (dishname, "dish","-pipe",NULLCP);
 			(void) fprintf (stderr, "unable to exec ");
 			perror (dishname);
			_exit (-21);
		case -1:
			perror ("fork");
			(void) close_tcp_socket (sd);
			exit (-22);
		default:
			/* parent */
			for (;;) {
			    if ((sd = start_tcp_client ((struct sockaddr_in *) 0,
							0)) == NOTOK) {
				perror("start_tcp_client");
				exit(-23);
			    }
			    if (join_tcp_server (sd, sin) != NOTOK)
				break;

			    /* need to introduce a timeout !!! */
			    (void) close_tcp_socket (sd);

			    sleep (5);

			    if (kill (pid, 0) == NOTOK) {
				(void) fprintf (stderr,"Trying again...\n");
				goto fork_again;
			    }
			}
			break;
		}
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

	for (;;) {
		if ((res = recv(sd, buffer, BUFSIZ - 1, 0)) == -1) {
err_recv: ;
			perror ("recv");
			(void) close_tcp_socket (sd);
			exit (-26);
		}
		*(buffer + res) = 0;
		if (res == 0) {
			(void) close_tcp_socket (sd);
			exit (0);
		}

		if (*buffer == '2') {
			if (res > 1)
				(void) write (2,&buffer[1],--res);
			while ( (res = recv(sd, buffer, BUFSIZ, 0)) > 0)
				(void) write (2,buffer,res);
			(void) close_tcp_socket (sd);
			exit (1);			
		} else if ((*buffer == '1') || (*buffer == '3')) {
			int eval;
			eval = (*buffer == '1' ? 0 : 2);
			if (res > 1)
	                        (void) write (1,&buffer[1],--res);
                        while ( (res = recv(sd, buffer, BUFSIZ, 0)) > 0)
                                (void) write (1,buffer,res);
                        (void) close_tcp_socket (sd);
                        exit (eval);
                } else {		/* 'e', 'y', 'm', or 'p' */
		        register char  *cp, *ep;
			char            where[BUFSIZ];

			cp = buffer + res - 1;
			ep = buffer + sizeof buffer - 1;
			while (*cp != '\n') {
			    ++cp;
			    switch (res = recv (sd, cp, ep - cp, 0)) {
				case NOTOK:
				    goto err_recv;
				case OK:
				    fprintf (stderr,
					     "eof reading '%c' directive\n",
					     *buffer);
				    exit (-28);
				default:
				    cp += res - 1;
				    if (cp < ep)
					continue;
				    fprintf (stderr,
					     "'%c' directive exceeds %d octets\n",
					     *buffer, sizeof buffer - 1);
				    exit(-29);
			    }
			}
			*cp = NULL;

			if (*buffer == 'e') {
				if (system (&buffer[1]))
				    (void) strcpy (where, "e");
				else
				    (void) getcwd (where, sizeof where);
			} else if (*buffer == 'm') {
				(void) fprintf (stderr, "\n%s\n", buffer + 1);
				(void) strcpy (where, "m");
			} else if (*buffer == 'y') {
				(void) fprintf (stderr,"%s",buffer + 1);
				(void) fgets (where, sizeof where, stdin);
				if (cp = index (where, '\n'))
				    *cp = NULL;
			} else {	/* 'p' */
				(void) sprintf (where,
					        "Enter password for \"%s\": ",
						buffer + 1);
				(void) sprintf (where, "p%s",
						getpassword (where));
			}
			(void) strcat (where, "\n");

			if (send(sd, where, strlen(where), 0) == -1) {
				perror("send");
				(void) close_tcp_socket (sd);
				exit (-27);
			}

		}

	}
}


#else	/* USE UNIX NAMED PIPES */

#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

char            retfile[BUFSIZ];
int             fd;
int		wfd;

main (argc, argv)
int             argc;
char          **argv;
{
	int             res;
	char            buffer[BUFSIZ];
	char            sendfile[BUFSIZ];
	char            dishname[BUFSIZ];
	int             i;
	char           *ptr, *getenv(), *sprintf(), *getpassword ();
	void            pipe_quit ();


	(void) umask (0);
	(void) sprintf (retfile, "/tmp/dish%d", getpid ());
	if ((ptr = getenv ("DISHPROC")) == NULLCP) {
	    (void) sprintf (sendfile, "/tmp/dish-%d", getppid ());
	    (void) setenv ("DISHPROC", sendfile);
	}
	else
	    (void) strcpy (sendfile, ptr);

	setbuf (stdout,NULLCP);
	setbuf (stderr,NULLCP);

	if (mknod (retfile, S_IFIFO | 0600, 0) == -1) {
		(void) fprintf (stderr,"Can't create pipe '%s'\n",retfile);
		exit (-12);
	}
	
	for (i = 1; i <= 15; i++)
		(void) signal (i, pipe_quit);

	if ((fd = open (sendfile, O_WRONLY | O_NDELAY)) == -1) {
		(void) fprintf (stderr, "(Dish starting)\n");
		if (mknod (sendfile, S_IFIFO | 0600, 0) == -1) {
			(void) fprintf (stderr,"Can't create pipe '%s'\n",sendfile);
			exit (-11);
		}
		(void) strcpy (dishname, _isodefile (isodebinpath, "dish"));
		if (vfork () == 0) {
			/* child */
			execl (dishname, "dish","-pipe",NULLCP);
			(void) fprintf (stderr, "unable to exec ");
			perror (dishname);
			(void) unlink (retfile);
			_exit (-2);
		} else {
			fd = open (sendfile, O_WRONLY);
			setbuf (stderr,NULLCP);
		}
	}
	argc--;
	if ((ptr = rindex (argv[0], '/')) == NULLCP)
		(void) sprintf (buffer, "%s:%s", retfile, argv[0]);
	else
		(void) sprintf (buffer, "%s:%s", retfile, ++ptr);
	argv++;

	while (argc--) {
		(void) strcat (buffer, " \"");
		(void) strcat (buffer, *argv++);
		(void) strcat (buffer, "\"");
	}

	if ((res = write (fd, buffer, strlen (buffer))) == -1) {
		(void) fprintf (stderr, "Write to DUA failed... Please retry\n");
		(void) close (fd);
		(void) unlink (retfile);
		exit (-3);
	}
	(void) close (fd);

	/* get results */
	if ((fd = open (retfile, O_RDONLY)) < 0) {
		(void) fprintf (stderr, "Can't read results\n");
		(void) unlink (retfile);
		exit (-4);
	}

	if ((wfd = open (retfile, O_WRONLY)) < 0) {
		(void) fprintf (stderr, "Can't lock results failed\n");
		(void) unlink (retfile);
		(void) close (fd);
		exit (-5);
	}

	for (;;) {
		if ((res = read (fd, buffer, BUFSIZ - 1)) == -1) {
			(void) fprintf (stderr, "Read failed (%d)\n", errno);
			(void) unlink (retfile);
			(void) close (fd);
			(void) close (wfd);
			exit (-6);
		}
		*(buffer + res) = 0;

		if (*buffer == '2') {
			(void) write (2,&buffer[1],--res);
			while ( (res = read (fd, buffer, BUFSIZ)) > 0)
				(void) write (2,buffer,res);
			(void) close (fd);
			(void) close (wfd);
			(void) unlink (retfile);
			exit (-1);			
		} else if ((*buffer == '1') || (*buffer == '3')) {
			int eval;
			eval = (*buffer == '1' ? 0 : 2);
                        (void) write (1,&buffer[1],--res);
                        while ( (res = read (fd, buffer, BUFSIZ)) > 0)
                                (void) write (1,buffer,res);
			(void) close (fd);
			(void) close (wfd);
			(void) unlink (retfile);
                        exit (eval);
		else {		/* 'e', 'y', 'm', or 'p' */
			int             res2,
			                nfd;
			char            where[BUFSIZ];

			if (*buffer == 'e') {
				if (system (&buffer[1]))
				    (void) strcpy (where, "e");
				else
				    (void) getcwd (where, sizeof where);
			} else if (*buffer == 'm') {
				(void) fprintf (stderr, "\n%s\n", buffer + 1);
				(void) strcpy (where, "m");
			} else if (*buffer == 'y') {
			        char   *cp;

				(void) fprintf (stderr,"%s",buffer + 1);
				(void) fgets (where, sizeof where, stdin);
				if (cp = index (where, '\n'))
				    *cp = NULL;
			} else {	/* 'p' */
				(void) sprintf (where,
					        "Enter password for \"%s\": ",
						buffer + 1);
				(void) sprintf (where, "p%s",
						getpassword (where));
			}
			(void) strcat (where, "\n");

			if ((nfd = open (sendfile, O_WRONLY)) == -1) {
				(void) fprintf (stderr, "re-write open error\n");
				(void) close (wfd);
				(void) close (fd);
				(void) unlink (retfile);
				exit (-9);
			}
			if ((res2 = write (nfd, where, strlen (where))) == -1) {
				(void) fprintf (stderr, "Write to DUA failed (%d)... Please retry\n", res2);
				(void) close (fd);
				(void) close (nfd);
				(void) close (wfd);
				(void) unlink (retfile);
				exit (-10);
			}
			(void) close (nfd);
		}
	}
}

void pipe_quit (sig)
int     sig;
{
	(void) unlink (retfile);
	(void) fprintf (stderr,"(signal %d) exiting...\n",sig);
	exit(0);
}
#endif
