/* Client process that communicates with GNU Emacs acting as server.
   Copyright (C) 1986, 1987 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#define NO_SHORTNAMES
#include "../src/config.h"
#undef read
#undef write
#undef open
#ifdef close
#undef close
#endif


#if !defined(BSD) && !defined(HAVE_SYSVIPC)
#include <stdio.h>

main (argc, argv)
     int argc;
     char **argv;
{
  fprintf (stderr, "%s: Sorry, the Emacs server is supported only\n",
	   argv[0]);
  fprintf (stderr, "on systems with Berkeley sockets or System V IPC.\n");
  exit (1);
}

#else /* BSD or HAVE_SYSVIPC */

#if defined(BSD) && ! defined (HAVE_SYSVIPC)
/* BSD code is very different from SYSV IPC code */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdio.h>
#include <errno.h>
#include <sys/stat.h>

#ifndef BSD4_4
extern int sys_nerr;
extern char *sys_errlist[];
extern int errno;
#endif

main (argc, argv)
     int argc;
     char **argv;
{
  int s, i;
  FILE *out;
  struct sockaddr_un server;
  char *homedir, *cwd, *str;
  char string[BUFSIZ];
  struct stat statbfr;

  char *getenv (), *getwd ();

  if (argc < 2)
    {
      fprintf (stderr, "Usage: %s filename\n", argv[0]);
      exit (1);
    }

  /* 
   * Open up an AF_UNIX socket in this person's home directory
   */

  if ((s = socket (AF_UNIX, SOCK_STREAM, 0)) < 0)
    {
      fprintf (stderr, "%s: ", argv[0]);
      perror ("socket");
      exit (1);
    }
  server.sun_family = AF_UNIX;
#ifndef SERVER_HOME_DIR
  (void) sprintf (server.sun_path, "/tmp/esrv%d", geteuid());
  if (stat (server.sun_path, &statbfr) == -1)
    {
      perror ("stat");
      exit (1);
    }
  if (statbfr.st_uid != geteuid())
    {
      fprintf (stderr, "Illegal socket owner\n");
      exit (1);
    }
#else
  if ((homedir = getenv ("HOME")) == NULL)
    {
      fprintf (stderr, "%s: No home directory\n", argv[0]);
      exit (1);
    }
  strcpy (server.sun_path, homedir);
  strcat (server.sun_path, "/.emacs_server");
#endif

  if (connect (s, &server, strlen (server.sun_path) + 2) < 0)
    {
      fprintf (stderr, "%s: ", argv[0]);
      perror ("connect");
      exit (1);
    }
  if ((out = fdopen (s, "r+")) == NULL)
    {
      fprintf (stderr, "%s: ", argv[0]);
      perror ("fdopen");
      exit (1);
    }

  cwd = getwd (string);
  if (cwd == 0)
    {
      /* getwd puts message in STRING if it fails.  */
      fprintf (stderr, "%s: %s (%s)\n", argv[0], string,
	       (errno < sys_nerr) ? sys_errlist[errno] : "unknown error");
      exit (1);
    }

  for (i = 1; i < argc; i++)
    {
      if (*argv[i] == '+')
	{
	  char *p = argv[i] + 1;
	  while (*p >= '0' && *p <= '9') p++;
	  if (*p != 0)
	    fprintf (out, "%s/", cwd);
	}
      else if (*argv[i] != '/')
	fprintf (out, "%s/", cwd);
      fprintf (out, "%s ", argv[i]);
    }
  fprintf (out, "\n");
  fflush (out);

  printf ("Waiting for Emacs...");
  fflush (stdout);

  rewind (out); /* re-read the output */
  str = fgets (string, BUFSIZ, out); 

  /* Now, wait for an answer and print any messages.  */
  
  while (str = fgets (string, BUFSIZ, out))
    printf ("%s", str);
  
  exit (0);
}

#else /* This is the SYSV IPC section */

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <stdio.h>

main (argc, argv)
     int argc;
     char **argv;
{
  int s;
  key_t key;
  struct msgbuf * msgp =
      (struct msgbuf *) malloc (sizeof *msgp + BUFSIZ);
  struct msqid_ds * msg_st;
  char *homedir, buf[BUFSIZ];
  char gwdirb[BUFSIZ];
  char *cwd;
  char *temp;
  char *getwd (), *getcwd (), *getenv ();

  if (argc < 2)
    {
      fprintf (stderr, "Usage: %s filename\n", argv[0]);
      exit (1);
    }

  /*
   * Create a message queue using ~/.emacs_server as the path for ftok
   */
  if ((homedir = getenv ("HOME")) == NULL)
    {
      fprintf (stderr, "%s: No home directory\n", argv[0]);
      exit (1);
    }
  strcpy (buf, homedir);
  strcat (buf, "/.emacs_server");
  creat (buf, 0600);
  key = ftok (buf, 1);	/* unlikely to be anyone else using it */
  s = msgget (key, 0600);
  if (s == -1)
    {
      fprintf (stderr, "%s: ", argv[0]);
      perror ("msgget");
      exit (1);
    }

  /* Determine working dir, so we can prefix it to all the arguments.  */
#ifdef BSD
  temp = getwd (gwdirb);
#else
  temp = getcwd (gwdirb, sizeof gwdirb);
#endif

  cwd = gwdirb;
  if (temp != 0)
    {
      /* On some systems, cwd can look like `@machine/...';
	 ignore everything before the first slash in such a case.  */
      while (*cwd && *cwd != '/')
	cwd++;
      strcat (cwd, "/");
    }
  else
    {
      fprintf (stderr, cwd);
      exit (1);
    }

  msgp->mtext[0] = 0;
  argc--; argv++;
  while (argc)
    {
      if (*argv[0] == '+')
	{
	  char *p = argv[0] + 1;
	  while (*p >= '0' && *p <= '9') p++;
	  if (*p != 0)
	    strcat (msgp->mtext, cwd);
	}
      else if (*argv[0] != '/')
	strcat (msgp->mtext, cwd);

      strcat (msgp->mtext, argv[0]);
      strcat (msgp->mtext, " ");
      argv++; argc--;
    }
  strcat (msgp->mtext, "\n");
  msgp->mtype = 1;
  if (msgsnd (s, msgp, strlen (msgp->mtext)+1, 0) < 0)
    {
      fprintf (stderr, "%s: ", argv[0]);
      perror ("msgsnd");
      exit (1);
    }
  /*
   * Now, wait for an answer
   */
  printf ("Waiting for Emacs...");
  fflush (stdout);

  msgrcv (s, msgp, BUFSIZ, getpid (), 0);	/* wait for anything back */
  strcpy (buf, msgp->mtext);

  printf ("\n%s\n", buf);
  exit (0);
}

#endif /* HAVE_SYSVIPC */

#endif /* BSD or HAVE_SYSVIPC */
