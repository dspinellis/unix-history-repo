/* uulog.c
   Display the UUCP log file.

   Copyright (C) 1991, 1992 Ian Lance Taylor

   This file is part of the Taylor UUCP package.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   The author of the program may be contacted at ian@airs.com or
   c/o AIRS, P.O. Box 520, Waltham, MA 02254.

   $Log: uulog.c,v $
   Revision 1.4  1992/03/12  19:54:43  ian
   Debugging based on types rather than number

   Revision 1.3  1992/02/27  05:40:54  ian
   T. William Wells: detach from controlling terminal, handle signals safely

   Revision 1.2  1992/02/23  03:26:51  ian
   Overhaul to use automatic configure shell script

   Revision 1.1  1992/02/14  21:22:51  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char uulog_rcsid[] = "$Id: uulog.c,v 1.4 1992/03/12 19:54:43 ian Rel $";
#endif

#include <errno.h>

#include "system.h"
#include "sysdep.h"
#include "getopt.h"

/* This is a pretty bad implementation of uulog, which I don't think
   is a very useful program anyhow.  It only takes a single -s and/or
   -u switch.  When using HAVE_BNU_LOGGING it requires a system.  It
   does not provide -f.  */

/* The program name.  */
char abProgram[] = "uulog";

/* Local functions.  */

static void ulusage P((void));

/* Long getopt options.  */

static const struct option asLongopts[] = { { NULL, 0, NULL, 0 } };

const struct option *_getopt_long_options = asLongopts;

int
main (argc, argv)
     int argc;
     char **argv;
{
  int iopt;
  /* -s: system name.  */
  const char *zsystem = NULL;
  /* -u: user name.  */
  const char *zuser = NULL;
  /* -I: configuration file name.  */
  const char *zconfig = NULL;
  /* -x: display uuxqt log file.  */
  boolean fuuxqt = FALSE;
  const char *zfile;
  FILE *e;
  char *zline;
  int csystem = 0;
  int cuser = 0;

  while ((iopt = getopt (argc, argv, "I:s:u:xX:")) != EOF)
    {
      switch (iopt)
	{
	case 'I':
	  /* Configuration file name.  */
	  zconfig = optarg;
	  break;

	case 's':
	  /* System name.  */
	  zsystem = optarg;
	  break;

	case 'u':
	  /* User name.  */
	  zuser = optarg;
	  break;

	case 'x':
	  /* Display uuxqt log file.  */
	  fuuxqt = TRUE;
	  break;

	case 'X':
#if DEBUG > 1
	  /* Set debugging level.  */
	  iDebug |= idebug_parse (optarg);
#endif
	  break;

	case 0:
	  /* Long option found and flag set.  */
	  break;

	default:
	  ulusage ();
	  break;
	}
    }

  if (optind != argc)
    ulusage ();

  uread_config (zconfig);

  usysdep_initialize (FALSE, FALSE);

#if ! HAVE_BNU_LOGGING
  zfile = zLogfile;
#else
  {
    const char *zprogram;
    char *zalc;

    /* We need a system to find a BNU log file.  */
    if (zsystem == NULL)
      ulusage ();

    if (fuuxqt)
      zprogram = "uuxqt";
    else
      zprogram = "uucico";

    zalc = (char *) alloca (strlen (zLogfile)
			    + strlen (zprogram)
			    + strlen (zsystem)
			    + 1);
    sprintf (zalc, zLogfile, zprogram, zsystem);
    zfile = zalc;
  }
#endif

  e = fopen (zfile, "r");
  if (e == NULL)
    {
      ulog (LOG_ERROR, "fopen (%s): %s", zfile, strerror (errno));
      usysdep_exit (FALSE);
    }

  /* Read the log file and output the appropriate lines.  */

  if (zsystem != NULL)
    csystem = strlen (zsystem);

  if (zuser != NULL)
    cuser = strlen (zuser);

  while ((zline = zfgets (e, FALSE)) != NULL)
    {
      char *zluser, *zlsys, *znext;
      int cluser, clsys;

      /* Skip any leading whitespace (not that there should be any).  */
      znext = zline + strspn (zline, " \t");

#if ! HAVE_TAYLOR_LOGGING
      /* The user name is the first field on the line.  */
      zluser = znext;
      cluser = strcspn (znext, " \t");
#endif
      
      /* Skip the first field.  */
      znext += strcspn (znext, " \t");
      znext += strspn (znext, " \t");

      /* The system is the second field on the line.  */
      zlsys = znext;
      clsys = strcspn (znext, " \t");

      /* Skip the second field.  */
      znext += clsys;
      znext += strspn (znext, " \t");

#if HAVE_TAYLOR_LOGGING
      /* The user is the third field on the line.  */
      zluser = znext;
      cluser = strcspn (znext, " \t");
#endif

      /* See if we should print this line.  */

      if (zsystem != NULL
	  && (csystem != clsys
	      || strncmp (zsystem, zlsys, clsys) != 0))
	continue;

      if (zuser != NULL
	  && (cuser != cluser
	      || strncmp (zuser, zluser, cluser) != 0))
	continue;

      /* Output the line.  */

      printf ("%s", zline);
    }

  ulog_close ();

  usysdep_exit (TRUE);

  /* Avoid errors about not returning a value.  */
  return 0;
}

/* Print a usage message and die.  */

static void
ulusage ()
{
  fprintf (stderr,
	   "Taylor UUCP version %s, copyright (C) 1991, 1992 Ian Lance Taylor\n",
	   abVersion);
  fprintf (stderr,
	   "Usage: uulog [-s system] [-u user] [-x] [-I file] [-X debug]\n");
  fprintf (stderr,
	   " -s: print entries for named system\n");
  fprintf (stderr,
	   " -u: print entries for name user\n");
#if HAVE_BNU_LOGGING
  fprintf (stderr,
	   " -x: print uuxqt log rather than uucico log\n");
#endif
  fprintf (stderr,
	   " -X debug: Set debugging level (0 for none, 9 is max)\n");
#if HAVE_TAYLOR_CONFIG
  fprintf (stderr,
	   " -I file: Set configuration file to use (default %s%s)\n",
	   NEWCONFIGLIB, CONFIGFILE);
#endif /* HAVE_TAYLOR_CONFIG */
  exit (EXIT_FAILURE);
}
