/* uuname.c
   List the names of known remote UUCP sites.

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

   $Log: uuname.c,v $
   Revision 1.5  1992/03/28  04:42:32  ian
   Franc,ois Pinard: output aliases, added -a switch

   Revision 1.4  1992/03/12  19:54:43  ian
   Debugging based on types rather than number

   Revision 1.3  1992/02/27  05:40:54  ian
   T. William Wells: detach from controlling terminal, handle signals safely

   Revision 1.2  1992/02/23  03:26:51  ian
   Overhaul to use automatic configure shell script

   Revision 1.1  1992/02/14  20:25:55  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char uuname_rcsid[] = "$Id: uuname.c,v 1.5 1992/03/28 04:42:32 ian Rel $";
#endif

#include "system.h"
#include "sysdep.h"
#include "getopt.h"

/* The program name.  */
char abProgram[] = "uuname";

/* Local functions.  */

static void unusage P((void));

/* Long getopt options.  */

static const struct option asLongopts[] = { { NULL, 0, NULL, 0 } };

const struct option *_getopt_long_options = asLongopts;

int
main (argc, argv)
     int argc;
     char **argv;
{
  int iopt;
  /* -a: don't display aliases.  */
  boolean fnoalias = FALSE;
  /* -l: if true, output local node name.  */
  boolean flocal = FALSE;
  /* -I: configuration file name.  */
  const char *zconfig = NULL;

  while ((iopt = getopt (argc, argv, "alI:x:")) != EOF)
    {
      switch (iopt)
	{
	case 'a':
	  /* Don't display aliases.  */
	  fnoalias = TRUE;
	  break;

	case 'l':
	  /* Output local node name.  */
	  flocal = TRUE;
	  break;

	case 'I':
	  /* Configuration file name.  */
	  zconfig = optarg;
	  break;

	case 'x':
#if DEBUG > 1
	  /* Set debugging level.  */
	  iDebug |= idebug_parse (optarg);
#endif
	  break;

	case 0:
	  /* Long option found and flag set.  */
	  break;

	default:
	  unusage ();
	  break;
	}
    }

  if (optind != argc)
    unusage ();

  uread_config (zconfig);

  usysdep_initialize (FALSE, FALSE);

  if (flocal)
    printf ("%s\n", zLocalname);
  else
    {
      int c;
      struct ssysteminfo *pas;
      int i;

      uread_all_system_info (&c, &pas);

      for (i = 0; i < c; i++)
	{
	  printf ("%s\n", pas[i].zname);

	  if (! fnoalias && pas[i].zalias != NULL)
	    {
	      char *zcopy, *ztok;

	      zcopy = (char *) alloca (strlen (pas[i].zalias) + 1);
	      strcpy (zcopy, pas[i].zalias);
	      for (ztok = strtok (zcopy, " ");
		   ztok != NULL;
		   ztok = strtok ((char *) NULL, " "))
		printf ("%s\n", ztok);
	    }
	}
    }

  ulog_close ();

  usysdep_exit (TRUE);

  /* Avoid errors about not returning a value.  */
  return 0;
}

/* Print a usage message and die.  */

static void
unusage ()
{
  fprintf (stderr,
	   "Taylor UUCP version %s, copyright (C) 1991, 1992 Ian Lance Taylor\n",
	   abVersion);
  fprintf (stderr,
	   "Usage: uuname [-a]  [-l] [-I file] [-x debug]\n");
  fprintf (stderr,
	   " -a: don't display aliases\n");
  fprintf (stderr,
	   " -l: print local name\n");
  fprintf (stderr,
	   " -x debug: Set debugging level (0 for none, 9 is max)\n");
#if HAVE_TAYLOR_CONFIG
  fprintf (stderr,
	   " -I file: Set configuration file to use (default %s%s)\n",
	   NEWCONFIGLIB, CONFIGFILE);
#endif /* HAVE_TAYLOR_CONFIG */
  exit (EXIT_FAILURE);
}
