/* log.c
   Routines to add entries to the log files.

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

   $Log: log.c,v $
   Revision 1.29  1992/04/01  21:58:35  ian
   Added CLOSE_LOGFILES configuration parameter

   Revision 1.28  1992/03/28  19:40:26  ian
   Close log and statistics file at each master/slave role switch

   Revision 1.27  1992/03/15  04:51:17  ian
   Keep an array of signals we've received rather than a single variable

   Revision 1.26  1992/03/12  19:54:43  ian
   Debugging based on types rather than number

   Revision 1.25  1992/03/08  16:42:41  ian
   Ted Lindgreen: report port and login name in log file

   Revision 1.24  1992/03/04  00:36:44  ian
   Michael Richardson: better chat script debugging

   Revision 1.23  1992/02/27  05:40:54  ian
   T. William Wells: detach from controlling terminal, handle signals safely

   Revision 1.22  1992/02/19  19:36:07  ian
   Rearranged time functions

   Revision 1.21  1992/02/18  04:33:38  ian
   Don't use headers when outputting to terminal

   Revision 1.20  1992/02/14  07:51:49  ian
   Michael Nolan: don't refer to eLdebug if DEBUG is 0

   Revision 1.19  1992/02/08  20:33:57  ian
   Handle all possible signals raised by abort

   Revision 1.18  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.17  1992/02/01  00:51:28  ian
   Michael Nolan: call abort if SIGABRT is not defined

   Revision 1.16  1992/01/28  04:51:34  ian
   Marty Shannon: don't report failed transfers with BNU logging

   Revision 1.15  1992/01/16  18:07:18  ian
   Niels Baggesen: add FAILED to end of xferstats line if appropriate

   Revision 1.14  1992/01/12  19:32:29  ian
   Handle HAVE_BNU_LOGGING with no %s in zLogfile

   Revision 1.13  1992/01/05  04:41:48  ian
   Tweaked HAVE_V2_LOGGING output slightly

   Revision 1.12  1991/12/29  05:00:27  ian
   Was not allocating enough space in zldate_and_time

   Revision 1.11  1991/12/29  04:04:18  ian
   Added a bunch of extern definitions

   Revision 1.10  1991/12/29  02:59:50  ian
   Lele Gaifax: put full year in log file

   Revision 1.9  1991/12/21  23:10:43  ian
   Terry Gardner: record failed file transfers in statistics file

   Revision 1.8  1991/12/18  03:54:14  ian
   Made error messages to terminal appear more normal

   Revision 1.7  1991/12/18  03:14:52  ian
   Use a fixed number of fields in log messages

   Revision 1.6  1991/12/17  07:09:58  ian
   Record statistics in fractions of a second

   Revision 1.5  1991/12/13  04:02:23  ian
   David Nugent: move ERROR: from start of line to after date and time

   Revision 1.4  1991/12/11  04:21:37  ian
   Arne Ludwig: merge in Arne Ludwig's patches for V2 and BNU style logging

   Revision 1.3  1991/11/07  20:32:04  ian
   Chip Salzenberg: allow ANSI_C to be defined in conf.h

   Revision 1.2  1991/09/19  03:23:34  ian
   Chip Salzenberg: append to private debugging file, don't overwrite it

   Revision 1.1  1991/09/10  19:40:31  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char log_rcsid[] = "$Id: log.c,v 1.29 1992/04/01 21:58:35 ian Rel $";
#endif

#include <errno.h>

#if ANSI_C
#include <stdarg.h>
#endif

#if HAVE_TIME_H
#include <time.h>
#endif

#if ! HAVE_TIME_T
#if HAVE_SYS_TIME_T
#include <sys/types.h>
#endif /* HAVE_SYS_TIME_T */
#endif /* ! HAVE_TIME_T */

#include "system.h"

/* External functions.  */
extern int fflush (), fclose ();
#if HAVE_VFPRINTF
extern int vfprintf ();
#endif

/* Static functions.  */

static const char *zldate_and_time P((void));

/* The function to call when a LOG_FATAL error occurs.  */
static void (*fLfatal) P((void));

/* Whether to go to a file.  */
static boolean fLfile;

/* ID number.  */
static int iLid;

/* The current user name.  */
static char *zLuser;

/* The current system name.  */
static char *zLsystem;

/* The current device name.  */
char *zLdevice;

/* The open log file.  */
static FILE *eLlog;

/* Whether we have tried to open the log file.  We need this because
   we don't want to keep trying to open the log file if we failed the
   first time.  It can't be static because under HAVE_BNU_LOGGING we
   may have to write to various different log files.  */
static boolean fLlog_tried;

#if DEBUG > 1
/* The open debugging file.  */
static FILE *eLdebug;

/* Whether we've tried to open the debugging file.  */
static boolean fLdebug_tried;

/* Whether we've written out any debugging information.  */
static boolean fLdebugging;
#endif

/* The open statistics file.  */
static FILE *eLstats;

/* Whether we've tried to open the statistics file.  */
static boolean fLstats_tried;

/* The array of signals.  The elements are only set to TRUE by the
   default signal handler.  They are only set to FALSE if we don't
   care whether we got the signal or not.  */
volatile sig_atomic_t afSignal[INDEXSIG_COUNT];

/* The array of signals to log.  The elements are only set to TRUE by
   the default signal handler.  They are set to FALSE when the signal
   is logged in ulog.  This means that if a signal comes in at just
   the right time we won't log it (or, rather, we'll log it once
   instead of twice), but that is not a catatrophe.  */
volatile sig_atomic_t afLog_signal[INDEXSIG_COUNT];

/* Signal names to use when logging signals.  */
static const char * const azSignal_names[INDEXSIG_COUNT] = INDEXSIG_NAMES;

/* Set the function to call on a LOG_FATAL error.  */

void
ulog_fatal_fn (pfn)
     void (*pfn) P((void));
{
  fLfatal = pfn;
}

/* Decide whether to send log message to the file or not.  */

void
ulog_to_file (ffile)
     boolean ffile;
{
  fLfile = ffile;
}

/* Set the ID number.  This will be called by the usysdep_initialize
   if there is something sensible to set it to.  */

void
ulog_id (i)
     int i;
{
  iLid = i;
}

/* Set the user we are making log entries for.  The arguments will be
   copied into memory.  */

void
ulog_user (zuser)
     const char *zuser;
{
  if (zuser == NULL
      || zLuser == NULL
      || strcmp (zuser, zLuser) != 0)
    {
      xfree ((pointer) zLuser);
      if (zuser == NULL)
	zLuser = NULL;
      else
	zLuser = xstrdup (zuser);
    }
}

/* Set the system name we are making log entries for.  The name is copied
   into memory.  */

void
ulog_system (zsystem)
  const char *zsystem;
{
  if (zsystem == NULL
      || zLsystem == NULL
      || strcmp (zsystem, zLsystem) != 0)
    {
      xfree ((pointer) zLsystem);
      if (zsystem == NULL)
	zLsystem = NULL;
      else
	zLsystem = xstrdup (zsystem);
#if HAVE_BNU_LOGGING      
      /* Under BNU logging we now must write to a different log file.  */
      if (eLlog != NULL)
	{
	  (void) fclose (eLlog);
	  eLlog = NULL;
	  fLlog_tried = FALSE;
	}
#endif /* HAVE_BNU_LOGGING */
    }
}

/* Set the device name.  This is copied into memory.  */

void
ulog_device (zdevice)
     const char *zdevice;
{
  if (zdevice == NULL
      || zLdevice == NULL
      || strcmp (zdevice, zLdevice) != 0)
    {
      xfree ((pointer) zLdevice);
      if (zdevice == NULL)
	zLdevice = NULL;
      else
	zLdevice = xstrdup (zdevice);
    }
}

/* Make a log entry.  We make a token concession to non ANSI_C systems,
   but it clearly won't always work.  */

#if ! ANSI_C
#undef HAVE_VFPRINTF
#endif

/*VARARGS2*/
#if HAVE_VFPRINTF
void
ulog (enum tlog ttype, const char *zmsg, ...)
#else
void
ulog (ttype, zmsg, a, b, c, d, f, g, h, i, j)
     enum tlog ttype;
     const char *zmsg;
#endif
{
#if HAVE_VFPRINTF
  va_list parg;
#endif
  FILE *e, *edebug;
  boolean fstart, fend;
  const char *zhdr, *zstr;

  /* Log any received signal.  We do it this way to avoid calling ulog
     from the signal handler.  A few routines call ulog to get this
     message out with zmsg == NULL.  */
  {
    static boolean fdoing_sigs;

    if (! fdoing_sigs)
      {
	int isig;

	fdoing_sigs = TRUE;
	for (isig = 0; isig < INDEXSIG_COUNT; isig++)
	  {
	    if (afLog_signal[isig])
	      {
		afLog_signal[isig] = FALSE;
		ulog (LOG_ERROR, "Got %s signal", azSignal_names[isig]);
	      }
	  }
	fdoing_sigs = FALSE;
      }
  }

  if (zmsg == NULL)
    return;

#if DEBUG > 1
  /* If we've had a debugging file open in the past, then we want to
     write all log file entries to the debugging file even if it's
     currently closed.  */
  if (fLfile
      && eLdebug == NULL
      && ! fLdebug_tried
      && (fLdebugging || (int) ttype >= (int) LOG_DEBUG))
    {
      fLdebug_tried = TRUE;
      eLdebug = esysdep_fopen (zDebugfile, FALSE, TRUE, TRUE);
      fLdebugging = TRUE;
    }
#endif /* DEBUG > 1 */

  if (! fLfile)
    e = stderr;
#if DEBUG > 1
  else if ((int) ttype >= (int) LOG_DEBUG)
    {
      e = eLdebug;

      /* If we can't open the debugging file, don't output any
	 debugging messages.  */
      if (e == NULL)
	return;
    }
#endif /* DEBUG > 1 */
  else
    {
      if (eLlog == NULL && ! fLlog_tried)
	{
	  fLlog_tried = TRUE;
#if ! HAVE_BNU_LOGGING
	  eLlog = esysdep_fopen (zLogfile, TRUE, TRUE, TRUE);
#else /* HAVE_BNU_LOGGING */
	  {
	    const char *zsys;
	    char *zfile;

	    /* We want to write to .Log/program/system, e.g.  	
	       .Log/uucico/uunet.  The system name may not be set.  */
	    if (zLsystem == NULL)
	      zsys = "ANY";
	    else
	      zsys = zLsystem;

	    zfile = (char *) alloca (strlen (zLogfile)
				     + strlen (abProgram)
				     + strlen (zsys)
				     + 1);
	    sprintf (zfile, zLogfile, abProgram, zsys);
	    eLlog = esysdep_fopen (zfile, TRUE, TRUE, TRUE);
	  }
#endif /* HAVE_BNU_LOGGING */

	  if (eLlog == NULL)
	    {
	      /* We can't open the log file.  We don't even have a
		 safe way to report this problem, since we may not be
		 able to write to stderr (it may, for example, be
		 attached to the incoming call).  */
	      if (fLfatal != NULL)
		(*fLfatal) ();
	      usysdep_exit (FALSE);
	    }
	}

      e = eLlog;

      /* eLlog might be NULL here because we might try to open the log
	 file recursively via esysdep_fopen.  */
      if (e == NULL)
	return;
    }

  edebug = NULL;
#if DEBUG > 1
  if ((int) ttype < (int) LOG_DEBUG)
    edebug = eLdebug;
#endif

  fstart = TRUE;
  fend = TRUE;

  switch (ttype)
    {
    case LOG_NORMAL:
      zhdr = "";
      break;
    case LOG_ERROR:
      zhdr = "ERROR: ";
      break;
    case LOG_FATAL:
      zhdr = "FATAL: ";
      break;
#if DEBUG > 1
    case LOG_DEBUG:
      zhdr = "DEBUG: ";
      break;
    case LOG_DEBUG_START:
      zhdr = "DEBUG: ";
      fend = FALSE;
      break;
    case LOG_DEBUG_CONTINUE:
      zhdr = NULL;
      fstart = FALSE;
      fend = FALSE;
      break;
    case LOG_DEBUG_END:
      zhdr = NULL;
      fstart = FALSE;
      break;
#endif
    default:
      zhdr = "???: ";
      break;
    }

  if (fstart)
    {
      if (! fLfile)
	{
	  fprintf (e, "%s: ", abProgram);
	  if (edebug != NULL)
	    fprintf (edebug, "%s: ", abProgram);
	}
      else
	{
#if HAVE_TAYLOR_LOGGING
	  fprintf (e, "%s ", abProgram);
	  if (edebug != NULL)
	    fprintf (edebug, "%s ", abProgram);
#else /* ! HAVE_TAYLOR_LOGGING */
	  fprintf (e, "%s ", zLuser == NULL ? "uucp" : zLuser);
	  if (edebug != NULL)
	    fprintf (edebug, "%s ", zLuser == NULL ? "uucp" : zLuser);
#endif /* HAVE_TAYLOR_LOGGING */

	  fprintf (e, "%s ", zLsystem == NULL ? "-" : zLsystem);
	  if (edebug != NULL)
	    fprintf (edebug, "%s ", zLsystem == NULL ? "-" : zLsystem);

#if HAVE_TAYLOR_LOGGING
	  fprintf (e, "%s ", zLuser == NULL ? "-" : zLuser);
	  if (edebug != NULL)
	    fprintf (edebug, "%s ", zLuser == NULL ? "-" : zLuser);
#endif /* HAVE_TAYLOR_LOGGING */

	  zstr = zldate_and_time ();
	  fprintf (e, "(%s", zstr);
	  if (edebug != NULL)
	    fprintf (edebug, "(%s", zstr); 

	  if (iLid != 0)
	    {
#if ! HAVE_BNU_LOGGING
#if HAVE_TAYLOR_LOGGING
	      fprintf (e, " %d", iLid);
	      if (edebug != NULL)
		fprintf (edebug, " %d", iLid);
#else /* ! HAVE_TAYLOR_LOGGING */
	      fprintf (e, "-%d", iLid);
	      if (edebug != NULL)
		fprintf (edebug, "-%d", iLid);
#endif /* ! HAVE_TAYLOR_LOGGING */
#else /* HAVE_BNU_LOGGING */

	      /* I assume that the second number here is meant to be
		 some sort of file sequence number, and that it should
		 correspond to the sequence number in the statistics
		 file.  I don't have any really convenient way to do
		 this, so I won't unless somebody thinks it's very
		 important.  */
	      fprintf (e, ",%d,%d", iLid, 0);
	      if (edebug != NULL)
		fprintf (edebug, ",%d,%d", iLid, 0);
#endif /* HAVE_BNU_LOGGING */
	    }

	  fprintf (e, ") ");
	  if (edebug != NULL)
	    fprintf (edebug, ") ");

	  fprintf (e, "%s", zhdr);
	  if (edebug != NULL)
	    fprintf (edebug, "%s", zhdr);
	}
    }

#if HAVE_VFPRINTF
  va_start (parg, zmsg);
  vfprintf (e, zmsg, parg);
  va_end (parg);
  if (edebug != NULL)
    {
      va_start (parg, zmsg);
      vfprintf (edebug, zmsg, parg);
      va_end (parg);
    }
#else /* ! HAVE_VFPRINTF */
  fprintf (e, zmsg, a, b, c, d, f, g, h, i, j);
  if (edebug != NULL)
    fprintf (edebug, zmsg, a, b, c, d, f, g, h, i, j);
#endif /* ! HAVE_VFPRINTF */

  if (fend)
    {
      fprintf (e, "\n");
      if (edebug != NULL)
	fprintf (edebug, "\n");
    }

  (void) fflush (e);
  if (edebug != NULL)
    (void) fflush (edebug);

  if (ttype == LOG_FATAL)
    {
      if (fLfatal != NULL)
	(*fLfatal) ();
      usysdep_exit (FALSE);
    }

#if CLOSE_LOGFILES
  ulog_close ();
#endif
}

/* Close the log file.  There's nothing useful we can do with errors,
   so we don't check for them.  */

void
ulog_close ()
{
  /* Make sure we logged any signal we received.  */
  ulog (LOG_ERROR, (const char *) NULL);

  if (eLlog != NULL)
    {
      (void) fclose (eLlog);
      eLlog = NULL;
      fLlog_tried = FALSE;
    }

#if DEBUG > 1
  if (eLdebug != NULL)
    {
      (void) fclose (eLdebug);
      eLdebug = NULL;
      fLdebug_tried = FALSE;
    }
#endif
}

/* Add an entry to the statistics file.  We may eventually want to put
   failed file transfers in here, but we currently do not.  */

void
ustats (fsucceeded, zuser, zsystem, fsent, cbytes, csecs, cmicros)
     boolean fsucceeded;
     const char *zuser;
     const char *zsystem;
     boolean fsent;
     long cbytes;
     long csecs;
     long cmicros;
{
  long cbps;

  /* On a system which can determine microseconds we might very well
     have both csecs == 0 and cmicros == 0.  */
  if (csecs == 0 && cmicros == 0)
    cbps = 0;
  else
    cbps = (1000 * cbytes) / (csecs * 1000 + cmicros / 1000);

  if (eLstats == NULL)
    {
      if (fLstats_tried)
	return;
      fLstats_tried = TRUE;
      eLstats = esysdep_fopen (zStatfile, TRUE, TRUE, TRUE);
      if (eLstats == NULL)
	return;
    }

#if HAVE_TAYLOR_LOGGING
  fprintf (eLstats,
	   "%s %s (%s) %s%s %ld bytes in %ld.%03ld seconds (%ld bytes/sec)\n",
	   zuser, zsystem, zldate_and_time (),
	   fsucceeded ? "" : "failed after ",
	   fsent ? "sent" : "received",
	   cbytes, csecs, cmicros / 1000, cbps);
#endif /* HAVE_TAYLOR_LOGGING */
#if HAVE_V2_LOGGING
  fprintf (eLstats,
	   "%s %s (%s) (%ld) %s %s %ld bytes %ld seconds\n",
	   zuser, zsystem, zldate_and_time (),
	   (long) time ((time_t *) NULL),
	   fsent ? "sent" : "received",
	   fsucceeded ? "data" : "failed after",
	   cbytes, csecs + cmicros / 500000);
#endif /* HAVE_V2_LOGGING */
#if HAVE_BNU_LOGGING
  {
    static int iseq;

    /* I don't know what the 'M' or the 'C' mean.  This format expects
       us to get the time in fractions of a second; on Unix we could
       use times to do this, and we probably should.  The sequence
       number should probably correspond to the sequence number in the
       log file, but that is currently always 0; using this fake
       sequence number will still at least reveal which transfers are
       from different calls.  We don't report a failed data transfer
       with this format.  */
    if (! fsucceeded)
      return;
    ++iseq;
    fprintf (eLstats,
	     "%s!%s M (%s) (C,%d,%d) [%s] %s %ld / %ld.%03ld secs, %ld %s\n",
	     zsystem, zuser, zldate_and_time (), iLid, iseq,
	     zLdevice == NULL ? "unknown" : zLdevice,
	     fsent ? "->" : "<-",
	     cbytes, csecs, cmicros / 1000, cbps,
	     "bytes/sec");
  }
#endif /* HAVE_BNU_LOGGING */

  (void) fflush (eLstats);

#if CLOSE_LOGFILES
  ustats_close ();
#endif
}

/* Close the statistics file.  */

void
ustats_close ()
{
  if (eLstats != NULL)
    {
      if (fclose (eLstats) != 0)
	ulog (LOG_ERROR, "fclose: %s", strerror (errno));
      eLstats = NULL;
      fLstats_tried = FALSE;
    }
}

/* Return the date and time in a form used for a log entry.  */

static const char *
zldate_and_time ()
{
  long isecs, imicros;
  struct tm s;
#if HAVE_TAYLOR_LOGGING
  static char ab[sizeof "1991-12-31 12:00:00.00"];
#endif
#if HAVE_V2_LOGGING
  static char ab[sizeof "12/31-12:00"];
#endif
#if HAVE_BNU_LOGGING
  static char ab[sizeof "12/31-12:00:00"];
#endif

  isecs = isysdep_time (&imicros);
  usysdep_localtime (isecs, &s);

#if HAVE_TAYLOR_LOGGING
  sprintf (ab, "%04d-%02d-%02d %02d:%02d:%02d.%02d",
	   s.tm_year + 1900, s.tm_mon + 1, s.tm_mday, s.tm_hour,
	   s.tm_min, s.tm_sec, (int) (imicros / 10000));
#endif
#if HAVE_V2_LOGGING
  sprintf (ab, "%d/%d-%02d:%02d", s.tm_mon + 1, s.tm_mday,
	   s.tm_hour, s.tm_min);
#endif
#if HAVE_BNU_LOGGING
  sprintf (ab, "%d/%d-%02d:%02d:%02d", s.tm_mon + 1, s.tm_mday,
	   s.tm_hour, s.tm_min, s.tm_sec);
#endif

  return ab;
}
