/* uustat.c
   UUCP status program

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

   $Log: uustat.c,v $
   Revision 1.7  1992/03/28  22:54:45  ian
   Allow multiple systems and users or kills and rejuvenates at once

   Revision 1.6  1992/03/12  19:54:43  ian
   Debugging based on types rather than number

   Revision 1.5  1992/03/03  21:34:55  ian
   Handle local execution files

   Revision 1.4  1992/02/27  05:40:54  ian
   T. William Wells: detach from controlling terminal, handle signals safely

   Revision 1.3  1992/02/24  20:36:27  ian
   Roberto Biancardi: skip spaces after strtok (NULL, "")

   Revision 1.2  1992/02/23  03:26:51  ian
   Overhaul to use automatic configure shell script

   Revision 1.1  1992/02/20  04:18:59  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char uustat_rcsid[] = "$Id: uustat.c,v 1.7 1992/03/28 22:54:45 ian Rel $";
#endif

#include <errno.h>

#if HAVE_TIME_H
#include <time.h>
#endif

#include "system.h"
#include "sysdep.h"
#include "getopt.h"

/* The uustat program permits various listings and manipulations of
   files in the spool directory.  This implementation supports the
   following switches:

   -a list all jobs
   -kjobid kill job with specified ID
   -m report status for all remote machines
   -ohour report jobs older than specified number of hours
   -p do "ps -flp" on all processes holding lock files (Unix specific)
   -q list number of jobs for all systems
   -rjobid rejuvenate job with specified ID
   -ssystem report on all jobs for specified system
   -uuser report on all jobs for specified user
   -yhour report jobs younger than specified number of hours
   -Ifile set configuration file name
   -xdebug set debugging level  */

/* The program name.  */
char abProgram[] = "uustat";

/* Local functions.  */

static void ususage P((void));
static boolean fsworkfiles P((int csystems, char **pazsystems,
			      int cusers, char **pazusers,
			      long iold, long iyoung));
static boolean fsworkfiles_system P((const struct ssysteminfo *qsys,
				     int cusers,  char **pazusers,
				     long iold, long iyoung));
static boolean fsworkfile_show P((const struct ssysteminfo *qsys,
				  const struct scmd *qcmd,
				  long itime));
static void usworkfile_header P((const struct ssysteminfo *qsys,
				 const struct scmd *qcmd,
				 const char *zjobid,
				 long itime, boolean ffirst));
static boolean fsquery P((void));
static void usunits_show P((long idiff));
static boolean fsmachines P((void));

/* Long getopt options.  */

static const struct option asLongopts[] = { { NULL, 0, NULL, 0 } };

const struct option *_getopt_long_options = asLongopts;

int
main (argc, argv)
     int argc;
     char **argv;
{
  int iopt;
  /* -a: list all jobs.  */
  boolean fall = FALSE;
  /* -k jobid: kill specified job.  */
  int ckills = 0;
  char **pazkills = NULL;
  /* -m: report machine status.  */
  boolean fmachine = FALSE;
  /* -o hour: report jobs older than given number of hours.  */
  int ioldhours = -1;
  /* -p: report status of jobs holding lock files.  */
  boolean fps = FALSE;
  /* -q: list number of jobs for each system.  */
  boolean fquery = FALSE;
  /* -r jobid: rejuvenate specified job.  */
  int crejuvs = 0;
  char **pazrejuvs = NULL;
  /* -s system: list all jobs for specified system.  */
  int csystems = 0;
  char **pazsystems = NULL;
  /* -u user: list all jobs for specified user.  */
  int cusers = 0;
  char **pazusers = NULL;
  /* -y hour: report jobs younger than given number of hours.  */
  int iyounghours = -1;
  /* -I file: set configuration file.  */
  const char *zconfig = NULL;
  int ccmds;
  long iold;
  long iyoung;
  char *azoneuser[1];
  boolean fret;

  while ((iopt = getopt (argc, argv, "aI:k:mo:pqr:s:u:x:y:")) != EOF)
    {
      switch (iopt)
	{
	case 'a':
	  /* List all jobs.  */
	  fall = TRUE;
	  break;

	case 'I':
	  /* Set configuration file name.  */
	  zconfig = optarg;
	  break;

	case 'k':
	  /* Kill specified job.  */
	  ++ckills;
	  pazkills = (char **) xrealloc ((pointer) pazkills,
					 ckills * sizeof (char *));
	  pazkills[ckills - 1] = optarg;
	  break;

	case 'm':
	  /* Report machine status.  */
	  fmachine = TRUE;
	  break;

	case 'o':
	  /* Report old jobs.  */
	  ioldhours = atoi (optarg);
	  break;

	case 'p':
	  /* Get status of processes holding locks.  */
	  fps = TRUE;
	  break;

	case 'q':
	  /* List number of jobs for each system.  */
	  fquery = TRUE;
	  break;

	case 'r':
	  /* Rejuvenate specified job.  */
	  ++crejuvs;
	  pazrejuvs = (char **) xrealloc ((pointer) pazrejuvs,
					  crejuvs * sizeof (char *));
	  pazrejuvs[crejuvs - 1] = optarg;
	  break;

	case 's':
	  /* List jobs for specified system.  */
	  ++csystems;
	  pazsystems = (char **) xrealloc ((pointer) pazsystems,
					   csystems * sizeof (char *));
	  pazsystems[csystems - 1] = optarg;
	  break;

	case 'u':
	  /* List jobs for specified user.  */
	  ++cusers;
	  pazusers = (char **) xrealloc ((pointer) pazusers,
					 cusers * sizeof (char *));
	  pazusers[cusers - 1] = optarg;
	  break;

	case 'x':
#if DEBUG > 1
	  /* Set debugging level.  */
	  iDebug |= idebug_parse (optarg);
#endif
	  break;

	case 'y':
	  /* List jobs younger than given number of hours.  */
	  iyounghours = atoi (optarg);
	  break;

	case 0:
	  /* Long option found and flag set.  */
	  break;

	default:
	  ususage ();
	  break;
	}
    }

  if (optind != argc)
    ususage ();

  /* To avoid confusion, most options are only permitted by
     themselves.  This restriction might be removed later, but it is
     imposed by most implementations.  We do permit any combination of
     -s, -u, -o and -y, and any combination of -k and -r.  */
  ccmds = 0;
  if (fall)
    ++ccmds;
  if (ckills > 0 || crejuvs > 0)
    ++ccmds;
  if (fmachine)
    ++ccmds;
  if (fps)
    ++ccmds;
  if (fquery)
    ++ccmds;
  if (csystems > 0 || cusers > 0
      || ioldhours != -1 || iyounghours != -1)
    ++ccmds;

  if (ccmds > 1)
    {
      fprintf (stderr, "uustat: Too many options\n");
      ususage ();
    }

  uread_config (zconfig);

  usysdep_initialize (FALSE, FALSE);

  /* If no commands were specified, we list all commands for the given
     user.  */
  if (ccmds == 0)
    {
      cusers = 1;
      azoneuser[0] = xstrdup (zsysdep_login_name ());
      pazusers = azoneuser;
    }

  if (ioldhours == -1)
    iold = (long) -1;
  else
    {
      iold = (isysdep_time ((long *) NULL)
	      - (long) ioldhours * (long) 60 * (long) 60);
      if (iold < 0L)
	iold = 0L;
    }
  if (iyounghours == -1)
    iyoung = (long) -1;
  else
    {
      iyoung = (isysdep_time ((long *) NULL)
		- (long) iyounghours * (long) 60 * (long) 60);
      if (iyoung < 0L)
	iyoung = 0L;
    }

  if (fall
      || ioldhours != -1
      || csystems > 0
      || cusers > 0
      || iyounghours != -1)
    fret = fsworkfiles (csystems, pazsystems, cusers, pazusers, iold,
			iyoung);
  else if (fquery)
    fret = fsquery ();
  else if (fmachine)
    fret = fsmachines ();
  else if (ckills > 0 || crejuvs > 0)
    {
      int i;

      fret = TRUE;
      for (i = 0; i < ckills; i++)
	if (! fsysdep_kill_job (pazkills[i]))
	  fret = FALSE;

      for (i = 0; i < crejuvs; i++)
	if (! fsysdep_rejuvenate_job (pazrejuvs[i]))
	  fret = FALSE;
    }
  else if (fps)
    fret = fsysdep_lock_status ();
  else
    {
#if DEBUG > 0
      ulog (LOG_FATAL, "Can't happen");
#endif
      fret = FALSE;
    }

  ulog_close ();

  usysdep_exit (fret);

  /* Avoid errors about not returning a value.  */
  return 0;
}

/* Print a usage message and die.  */

static void
ususage ()
{
  fprintf (stderr,
	   "Taylor UUCP version %s, copyright (C) 1991, 1992 Ian Lance Taylor\n",
	   abVersion);
  fprintf (stderr,
	   "Usage: uustat [-ampq] [-kr job] [-oy hours] [-s system] [-u user]\n");
  fprintf (stderr,
	   " -a: list all UUCP jobs\n");
  fprintf (stderr,
	   " -k job: kill specified UUCP job\n");
  fprintf (stderr,
	   " -m: report status for all remote machines\n");
  fprintf (stderr,
	   " -o hours: list all jobs older than given number of hours\n");
  fprintf (stderr,
	   " -p: show status of all processes holding UUCP locks\n");
  fprintf (stderr,
	   " -q: list number of jobs for each system\n");
  fprintf (stderr,
	   " -r job: rejuvenate specified UUCP job\n");
  fprintf (stderr,
	   " -s system: list all jobs for specified system\n");
  fprintf (stderr,
	   " -u user: list all jobs for specified user\n");
  fprintf (stderr,
	   " -y hours: list all jobs younger than given number of hours\n");
  fprintf (stderr,
	   " -x debug: Set debugging level (0 for none, 9 is max)\n");
#if HAVE_TAYLOR_CONFIG
  fprintf (stderr,
	   " -I file: Set configuration file to use (default %s%s)\n",
	   NEWCONFIGLIB, CONFIGFILE);
#endif /* HAVE_TAYLOR_CONFIG */
  exit (EXIT_FAILURE);
}

/* Handle various possible requests to look at work files.  */

static boolean
fsworkfiles (csystems, pazsystems, cusers, pazusers, iold, iyoung)
     int csystems;
     char **pazsystems;
     int cusers;
     char **pazusers;
     long iold;
     long iyoung;
{
  boolean fret;
  int i;

  fret = TRUE;

  if (csystems > 0)
    {
      struct ssysteminfo ssys;

      for (i = 0; i < csystems; i++)
	{
	  if (! fread_system_info (pazsystems[i], &ssys))
	    {
	      ulog (LOG_ERROR, "%s: unknown system", pazsystems[i]);
	      fret = FALSE;
	      continue;
	    }

	  if (! fsworkfiles_system (&ssys, cusers, pazusers, iold, iyoung))
	    fret = FALSE;
	}
    }
  else
    {
      int cs;
      struct ssysteminfo *pas;

      uread_all_system_info (&cs, &pas);

      for (i = 0; i < cs; i++)
	if (! fsworkfiles_system (&pas[i], cusers, pazusers, iold, iyoung))
	  fret = FALSE;
    }

  return fret;
}

/* Look at the work files for a particular system.  */

static boolean
fsworkfiles_system (qsys, cusers, pazusers, iold, iyoung)
     const struct ssysteminfo *qsys;
     int cusers;
     char **pazusers;
     long iold;
     long iyoung;
{
  boolean fret;

  if (! fsysdep_get_work_init (qsys, BGRADE_LOW))
    return FALSE;

  while (TRUE)
    {
      struct scmd s;
      long itime;

      if (! fsysdep_get_work (qsys, BGRADE_LOW, &s))
	{
	  usysdep_get_work_free (qsys);
	  return FALSE;
	}
      if (s.bcmd == 'H')
	break;

      if (cusers > 0)
	{
	  int i;

	  for (i = 0; i < cusers; i++)
	    if (strcmp (pazusers[i], s.zuser) == 0)
	      break;
	  if (i >= cusers)
	    continue;
	}

      itime = isysdep_work_time (qsys, s.pseq);

      if (iold != (long) -1 && itime > iold)
	continue;

      if (iyoung != (long) -1 && itime < iyoung)
	continue;

      if (! fsworkfile_show (qsys, &s, itime))
	{
	  usysdep_get_work_free (qsys);
	  return FALSE;
	}
    }

  fret = fsworkfile_show (qsys, (const struct scmd *) NULL, 0L);

  usysdep_get_work_free (qsys);

  return fret;
}

/* Show a single workfile.  This is actually called once for each line
   in the workfile, so we accumulate the lines and show them all at
   once.  This lets us show an execution in a useful fashion.  */

struct scmdlist
{
  struct scmdlist *qnext;
  struct scmd s;
  long itime;
};

static boolean
fsworkfile_show (qsys, qcmd, itime)
     const struct ssysteminfo *qsys;
     const struct scmd *qcmd;
     long itime;
{
  static struct scmdlist *qlist;
  static const char *zlistid;
  const char *zid;

  if (qcmd == NULL)
    zid = NULL;
  else
    {
      zid = zsysdep_jobid (qsys, qcmd->pseq);
      if (zid == NULL)
	return FALSE;
    }

  /* If this is the same jobid as the list, put it on the end.  */

  if (qcmd != NULL
      && qlist != NULL
      && strcmp (zlistid, zid) == 0)
    {
      struct scmdlist *qnew, **pq;

      qnew = (struct scmdlist *) xmalloc (sizeof (struct scmdlist));
      qnew->qnext = NULL;
      qnew->s = *qcmd;
      qnew->itime = itime;
      for (pq = &qlist; *pq != NULL; pq = &(*pq)->qnext)
	;
      *pq = qnew;
      return TRUE;
    }

  if (qcmd != NULL)
    zid = xstrdup (zid);

  /* Here we have found a different job ID, so we print the scmd
     structures that we have accumulated.  We look for the special
     case of an execution (one of the destination files begins with
     X.).  We could be more clever about other situations as well.  */

  if (qlist != NULL)
    {
      struct scmdlist *qlook;

      for (qlook = qlist; qlook != NULL; qlook = qlook->qnext)
	if (qlook->s.bcmd == 'S'
	    && qlook->s.zto[0] == 'X'
	    && qlook->s.zto[1] == '.'
	    && fspool_file (qlook->s.zfrom))
	  break;

      if (qlook == NULL)
	{
	  /* Show all the lines in a regular work file.  */

	  for (qlook = qlist; qlook != NULL; qlook = qlook->qnext)
	    {
	      const char *zfile;

	      usworkfile_header (qsys, &qlook->s, zlistid, qlook->itime,
				 qlook == qlist);

	      switch (qlook->s.bcmd)
		{
		case 'S':
		  if (strchr (qlook->s.zoptions, 'C') != NULL
		      || fspool_file (qlook->s.zfrom))
		    zfile = zsysdep_spool_file_name (qsys, qlook->s.ztemp);
		  else
		    zfile = zsysdep_real_file_name (qsys, qlook->s.zfrom,
						    (const char *) NULL);
		  printf ("Sending %s (%ld bytes) to %s", qlook->s.zfrom,
			  zfile == NULL ? 0L : csysdep_size (zfile),
			  qlook->s.zto);
		  break;
		case 'R':
		  printf ("Requesting %s to %s", qlook->s.zfrom,
			  qlook->s.zto);
		  break;
		case 'X':
		  printf ("Requesting %s to %s", qlook->s.zfrom,
			  qlook->s.zto);
		  break;
#if DEBUG > 0
		default:
		  printf ("Bad line %d", qlook->s.bcmd);
		  break;
#endif
		}

	      printf ("\n");
	    }
	}
      else
	{
	  const char *zxqt;
	  FILE *e;
	  long csize;
	  struct scmdlist *qsize;
	  char *zline;

	  /* Show the command for an execution file.  */
	  zxqt = zsysdep_spool_file_name (qsys, qlook->s.zfrom);
	  if (zxqt == NULL)
	    return FALSE;

	  e = fopen (zxqt, "r");
	  if (e == NULL)
	    {
	      ulog (LOG_ERROR, "fopen (%s): %s", zxqt, strerror (errno));
	      return FALSE;
	    }

	  csize = 0L;
	  for (qsize = qlist; qsize != NULL; qsize = qsize->qnext)
	    {
	      if (qsize->s.bcmd == 'S')
		{
		  const char *zfile;

		  if (strchr (qsize->s.zoptions, 'C') != NULL
		      || fspool_file (qsize->s.zfrom))
		    zfile = zsysdep_spool_file_name (qsys, qsize->s.ztemp);
		  else
		    zfile = zsysdep_real_file_name (qsys, qsize->s.zfrom,
						    (const char *) NULL);
		  if (zfile != NULL)
		    csize += csysdep_size (zfile);
		}
	    }

	  usworkfile_header (qsys, &qlook->s, zlistid, qlook->itime,
			     TRUE);

	  while ((zline = zfgets (e, FALSE)) != NULL)
	    {
	      char *ztok;

	      ztok = strtok (zline, " \t");
	      if (ztok != NULL
		  && strcmp (ztok, "C") == 0)
		{
		  char *zcmd;
		  int clen;

		  zcmd = strtok ((char *) NULL, "");
		  zcmd += strspn (zcmd, " \t");
		  clen = strlen (zcmd);
		  if (zcmd[clen - 1] == '\n')
		    zcmd[clen - 1] = '\0';
		  printf ("Executing %s", zcmd);
		  xfree ((pointer) zline);
		  break;
		}
	      xfree ((pointer) zline);
	    }

	  if (zline == NULL)
	    printf ("Unrecognized execution");

	  printf (" (sending %ld bytes)\n", csize);

	  (void) fclose (e);
	}

      /* Free up the list of entries.  */
      qlook = qlist;
      while (qlook != NULL)
	{
	  struct scmdlist *qnext;

	  qnext = qlook->qnext;
	  xfree ((pointer) qlook);
	  qlook = qnext;
	}

      xfree ((pointer) zlistid);

      qlist = NULL;
      zlistid = NULL;
    }

  /* Start a new list with the entry we just got.  */

  if (qcmd != NULL)
    {
      qlist = (struct scmdlist *) xmalloc (sizeof (struct scmdlist));
      qlist->qnext = NULL;
      qlist->s = *qcmd;
      qlist->itime = itime;
      zlistid = zid;
    }

  return TRUE;
}

/* Show the header of the line describing a workfile.  */

static void
usworkfile_header (qsys, qcmd, zjobid, itime, ffirst)
     const struct ssysteminfo *qsys;
     const struct scmd *qcmd;
     const char *zjobid;
     long itime;
     boolean ffirst;
{
  const char *zshowid;
  struct tm stime;

  if (ffirst)
    zshowid = zjobid;
  else
    zshowid = "-";

  printf ("%.14s %s %s ", zshowid, qsys->zname, qcmd->zuser);

  usysdep_localtime (itime, &stime);
  printf ("%04d-%02d-%02d %02d:%02d:%02d ",
	  stime.tm_year + 1900, stime.tm_mon + 1,
	  stime.tm_mday, stime.tm_hour,
	  stime.tm_min, stime.tm_sec);
}

/* Handle the -q option.  For each remote system this lists the number
   of jobs queued, the number of executions queued, and the current
   call status.  We get the executions all at once, because they are
   not accessed by system.  They could be, but it is possible to have
   executions pending for an unknown system, so special handling would
   still be required.  */

struct sxqtlist
{
  struct sxqtlist *qnext;
  char *zsystem;
  int cxqts;
  long ifirst;
};

/* These local functions need the definition of sxqtlist for the
   prototype.  */

static boolean fsquery_system P((const struct ssysteminfo *qsys,
				 struct sxqtlist **pq,
				 long inow));
static boolean fsquery_show P((const struct ssysteminfo *qsys, int cwork,
			       long ifirstwork,
			       struct sxqtlist *qxqt,
			       long inow));

static boolean
fsquery ()
{
  struct sxqtlist *qlist;
  const char *zfile;
  const char *zsystem;
  boolean ferr;
  long inow;
  int csystems;
  struct ssysteminfo *pas;
  boolean fret;
  int i;

  /* Get a count of all the execution files.  */

  if (! fsysdep_get_xqt_init ())
    return FALSE;

  qlist = NULL;
  while ((zfile = zsysdep_get_xqt (&zsystem, &ferr)) != NULL)
    {
      struct sxqtlist *qlook;

      for (qlook = qlist; qlook != NULL; qlook = qlook->qnext)
	if (strcmp (zsystem, qlook->zsystem) == 0)
	  break;

      if (qlook != NULL)
	{
	  long itime;

	  ++qlook->cxqts;
	  itime = isysdep_file_time (zfile);
	  if (itime < qlook->ifirst)
	    qlook->ifirst = itime;
	}
      else
	{
	  struct sxqtlist *qnew;

	  qnew = (struct sxqtlist *) xmalloc (sizeof (struct sxqtlist));
	  qnew->qnext = qlist;
	  qnew->zsystem = xstrdup (zsystem);
	  qnew->cxqts = 1;
	  qnew->ifirst = isysdep_file_time (zfile);
	  qlist = qnew;
	}
    }

  usysdep_get_xqt_free ();

  if (ferr)
    return FALSE;

  inow = isysdep_time ((long *) NULL);

  /* Get a count of all the work files, and print out the system.  */

  uread_all_system_info (&csystems, &pas);

  fret = TRUE;
  for (i = 0; i < csystems; i++)
    if (! fsquery_system (&pas[i], &qlist, inow))
      fret = FALSE;

  /* Check for the local system in the list of execution files.  */
  if (qlist != NULL)
    {
      struct sxqtlist **pq;

      for (pq = &qlist; *pq != NULL; pq = &(*pq)->qnext)
	{
	  if (strcmp ((*pq)->zsystem, zLocalname) == 0)
	    {
	      struct sxqtlist *qfree;

	      if (! fsquery_show (&sLocalsys, 0, 0L, *pq, inow))
		fret = FALSE;
	      qfree = *pq;
	      *pq = qfree->qnext;
	      xfree ((pointer) qfree->zsystem);
	      xfree ((pointer) qfree);
	      break;
	    }
	}
    }

  /* Print out information for any unknown systems for which we have
     execution files.  */

  if (qlist != NULL && ! fUnknown_ok)
    {
      ulog (LOG_ERROR, "Executions queued up for unknown systems");
      return FALSE;
    }

  while (qlist != NULL)
    {
      struct sxqtlist *qnext;

      sUnknown.zname = qlist->zsystem;
      if (! fsquery_show (&sUnknown, 0, 0L, qlist, inow))
	fret = FALSE;
      qnext = qlist->qnext;
      xfree ((pointer) qlist->zsystem);
      xfree ((pointer) qlist);
      qlist = qnext;
    }

  return fret;
}

/* Query a single known system.  */

static boolean
fsquery_system (qsys, pq, inow)
     const struct ssysteminfo *qsys;
     struct sxqtlist **pq;
     long inow;
{
  int cwork;
  long ifirstwork;
  boolean fret;

  if (! fsysdep_get_work_init (qsys, BGRADE_LOW))
    return FALSE;

  cwork = 0;
  ifirstwork = 0L;
  while (TRUE)
    {
      struct scmd s;
      long itime;

      if (! fsysdep_get_work (qsys, BGRADE_LOW, &s))
	return FALSE;
      if (s.bcmd == 'H')
	break;

      ++cwork;

      itime = isysdep_work_time (qsys, s.pseq);
      if (ifirstwork == 0L || ifirstwork > itime)
	ifirstwork = itime;
    }

  usysdep_get_work_free (qsys);

  /* Find the execution information, if any.  */
  while (*pq != NULL)
    {
      if (strcmp ((*pq)->zsystem, qsys->zname) == 0)
	break;
      pq = &(*pq)->qnext;
    }

  /* If there are no commands and no executions, don't print any
     information for this system.  */
  if (cwork == 0 && *pq == NULL)
    return TRUE;

  fret = fsquery_show (qsys, cwork, ifirstwork, *pq, inow);

  if (*pq != NULL)
    {
      struct sxqtlist *qfree;

      qfree = *pq;
      *pq = qfree->qnext;
      xfree ((pointer) qfree->zsystem);
      xfree ((pointer) qfree);
    }

  return fret;
}

/* Print out the query information for a single system.  We handle the
   local system specially.  */

static boolean
fsquery_show (qsys, cwork, ifirstwork, qxqt, inow)
     const struct ssysteminfo *qsys;
     int cwork;
     long ifirstwork;
     struct sxqtlist *qxqt;
     long inow;
{
  boolean flocal;
  struct sstatus sstat;
  struct tm stime;

  flocal = strcmp (qsys->zname, zLocalname) == 0;

  if (! flocal)
    {
      if (! fsysdep_get_status (qsys, &sstat))
	return FALSE;
    }

  printf ("%s %dC (", qsys->zname, cwork);

  if (cwork == 0)
    printf ("0 secs");
  else
    usunits_show (inow - ifirstwork);

  printf (") ");

  if (qxqt == NULL)
    printf ("0X (0 secs)");
  else
    {
      printf ("%dX (", qxqt->cxqts);
      usunits_show (inow - qxqt->ifirst);
      printf (")");
    }

  if (flocal)
    {
      printf ("\n");
      return TRUE;
    }

  usysdep_localtime (sstat.ilast, &stime);

  printf (" %04d-%02d-%02d %02d:%02d:%02d ", 
	  stime.tm_year + 1900, stime.tm_mon + 1,
	  stime.tm_mday, stime.tm_hour,
	  stime.tm_min, stime.tm_sec);

  printf ("%s\n", azStatus[(int) sstat.ttype]);

  return TRUE;
}

/* Print a time difference in the largest applicable units.  */

static void
usunits_show (idiff)
     long idiff;
{
  const char *zunit;
  long iunits;

  if (idiff > (long) 24 * (long) 60 * (long) 60)
    {
      iunits = idiff / ((long) 24 * (long) 60 * (long) 60);
      zunit = "day";
    }
  else if (idiff > (long) 60 * 60)
    {
      iunits = idiff / (long) (60 * 60);
      zunit = "hour";
    }
  else if (idiff > (long) 60)
    {
      iunits = idiff / (long) 60;
      zunit = "min";
    }
  else
    {
      iunits = idiff;
      zunit = "sec";
    }

  printf ("%ld %s%s", iunits, zunit, iunits == 1 ? "" : "s");
}

/* Give a list of all status entries for all machines that we have
   status entries for.  We need to get a list of status entries in a
   system dependent fashion, since we may have status for unknown
   systems.  */

static boolean
fsmachines ()
{
  pointer phold;
  const char *zsystem;
  boolean ferr;
  struct sstatus sstat;

  if (! fsysdep_all_status_init (&phold))
    return FALSE;

  while ((zsystem = zsysdep_all_status (phold, &ferr, &sstat)) != NULL)
    {
      struct tm stime;

      usysdep_localtime (sstat.ilast, &stime);
      printf ("%-14s %04d-%02d-%02d %02d:%02d:%02d %s", zsystem,
	      stime.tm_year + 1900, stime.tm_mon + 1,
	      stime.tm_mday, stime.tm_hour,
	      stime.tm_min, stime.tm_sec,
	      azStatus[(int) sstat.ttype]);
      if (sstat.ttype != STATUS_TALKING
	  && sstat.cwait > 0)
	{
	  printf (" (%d %s", sstat.cretries,
		  sstat.cretries == 1 ? "try" : "tries");
	  if (sstat.ilast + sstat.cwait > isysdep_time ((long *) NULL))
	    {
	      usysdep_localtime (sstat.ilast + sstat.cwait, &stime);
	      printf (", next %04d-%02d-%02d %02d:%02d:%02d",
		      stime.tm_year + 1900, stime.tm_mon + 1,
		      stime.tm_mday, stime.tm_hour,
		      stime.tm_min, stime.tm_sec);
	    }
	  printf (")");
	}
      printf ("\n");
    }

  usysdep_all_status_free (phold);

  return ! ferr;
}
