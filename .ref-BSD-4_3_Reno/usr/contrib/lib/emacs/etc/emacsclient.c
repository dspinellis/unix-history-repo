/* Client process that communicates with GNU Emacs acting as server.
   Copyright (C) 1986, 1987 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but without any warranty.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
document "GNU Emacs copying permission notice".   An exact copy
of the document is supposed to have been given to you along with
GNU Emacs so that you can know how you may redistribute it all.
It should be in a file named COPYING.  Among other things, the
copyright notice and this notice must be preserved on all copies.  */


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

main ()
{
  fprintf (stderr, "Sorry, the Emacs server is supported only on Berkeley Unix\n");
  fprintf (stderr, "or System V systems with IPC\n");
  exit (1);
}

#else /* BSD or HAVE_SYSVIPC */

#if defined(BSD) && ! defined (HAVE_SYSVIPC)
/* BSD code is very different from SYSV IPC code */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdio.h>

main (argc, argv)
     int argc;
     char **argv;
{
  int s, n, i;
  FILE *out;
  struct sockaddr_un server;
  char *homedir, *cwd, *str;
  char string[BUFSIZ];

  char *getenv (), *getwd ();

  if (argc < 2)
    {
      printf ("Usage: %s [filename]\n", argv[0]);
      exit (1);
    }

  /* 
   * Open up an AF_UNIX socket in this person's home directory
   */

  if ((s = socket (AF_UNIX, SOCK_STREAM, 0)) < 0)
    {
      perror ("socket");
      exit (1);
    }
  server.sun_family = AF_UNIX;
  if ((homedir = getenv ("HOME")) == NULL)
    {
      fprintf (stderr, "No home directory\n");
      exit (1);
    }
  strcpy (server.sun_path, homedir);
  strcat (server.sun_path, "/.emacs_server");
  if (connect (s, &server, strlen (server.sun_path) + 2) < 0)
    {
      perror ("connect");
      exit (1);
    }
  if ((out = fdopen (s, "r+")) == NULL)
    {
      perror ("fdopen");
      exit (1);
    }

  cwd = getwd (string);
  if (cwd == 0)
    abort ();

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
  char *homedir, *getenv (), buf[BUFSIZ];
  char *getwd (), *getcwd (), gwdirb[BUFSIZ], *cwd;
  if (argc < 2)
    {
      printf ("Usage: %s [filename]\n", argv[0]);
      exit (1);
    }

  /*
   * Create a message queue using ~/.emacs_server as the path for ftok
   */
  if ((homedir = getenv ("HOME")) == NULL)
    {
      fprintf (stderr,"No home directory\n");
      exit (1);
    }
  strcpy (buf, homedir);
  strcat (buf, "/.emacs_server");
  creat (buf, 0600);
  key = ftok (buf, 1);	/* unlikely to be anyone else using it */
  s = msgget (key, 0600);
  if (s == -1)
    {
      perror ("msgget");
      exit (1);
    }

  msgp->mtext[0] = 0;
  argc--; argv++;
  while (argc)
    {
      if (*argv[0] != '/')
	{
	  char *val;
	  cwd = gwdirb; *cwd = '\0';
#ifdef BSD
	  val = getwd (gwdirb);
#else
	  val = getcwd (gwdirb, sizeof gwdirb);
#endif
	  if (val != 0)
	    {
	      strcat (cwd, "/");
	    }
	  else
	    {
	      fprintf (stderr, cwd);
	      *cwd = '\0';
	    }
	  strcat (msgp->mtext, cwd);
	}
	
      strcat (msgp->mtext, argv[0]);
      strcat (msgp->mtext, " ");
      argv++; argc--;
    }
  strcat (msgp->mtext, "\n");
  msgp->mtype = 1;
  if (msgsnd (s, msgp, strlen (msgp->mtext)+1, 1, 0) < 0)
    {
      perror ("msgsnd");
      exit (1);
    }
  /*
   * Now, wait for an answer
   */
  printf ("Waiting\n");

  msgrcv (s, msgp, BUFSIZ, getpid (), 0);	/* wait for anything back */
  strcpy (buf, msgp->mtext);

  printf ("Got back: %s\n", buf);
  exit (0);
}

#endif /* IPC */

#endif /* BSD && IPC */

