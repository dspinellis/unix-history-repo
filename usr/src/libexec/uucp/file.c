/* file.c
   Generic routines to handle files.

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

   $Log: file.c,v $
   Revision 1.16  1992/03/30  04:07:13  ian
   Dirk Musstopf: remove temporary file if receive fails

   Revision 1.15  1992/03/17  00:31:33  ian
   Changed iRecmode to unsigned int

   Revision 1.14  1992/03/11  01:18:15  ian
   Niels Baggesen: drop the connection on a write failure

   Revision 1.13  1992/03/11  00:18:50  ian
   Save temporary file if file send fails

   Revision 1.12  1992/02/29  01:06:59  ian
   Chip Salzenberg: recheck file permissions before sending

   Revision 1.11  1992/02/19  19:36:07  ian
   Rearranged time functions

   Revision 1.10  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.9  1992/01/18  22:48:53  ian
   Reworked sending of mail and general handling of failed transfers

   Revision 1.8  1991/12/21  23:10:43  ian
   Terry Gardner: record failed file transfers in statistics file

   Revision 1.7  1991/12/17  07:09:58  ian
   Record statistics in fractions of a second

   Revision 1.6  1991/12/10  19:29:02  ian
   Move statistics file stuff from file.c to log.c

   Revision 1.5  1991/11/10  19:24:22  ian
   Added pffile protocol entry point for file level control

   Revision 1.4  1991/11/08  04:30:50  ian
   Hannu Strang: flush statistics file after each line

   Revision 1.3  1991/11/07  19:42:16  ian
   Chip Salzenberg: declare inline functions consistently

   Revision 1.2  1991/09/19  03:23:34  ian
   Chip Salzenberg: append to private debugging file, don't overwrite it

   Revision 1.1  1991/09/10  19:40:31  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char file_rcsid[] = "$Id: file.c,v 1.16 1992/03/30 04:07:13 ian Rel $";
#endif

#include <errno.h>

#include "system.h"

/* Current file being sent.  */
openfile_t eSendfile = EFILECLOSED;

/* Current file being received.  */
openfile_t eRecfile = EFILECLOSED;

/* This file has to keep several strings.  I don't want to pay for the
   overhead (small as it is) of calling malloc for each one, and I also
   don't want to truncate the strings.  So I use a structure which holds
   strings up to a certain length, and call malloc for longer strings.
   NULL strings are indistinguishable from empty strings, which turns
   out not to matter.  */

#define CSTRLEN (30)

struct sstring
{
  char ab[CSTRLEN];
  char *zalloc;
};

/* Local functions.  */

__inline__ static boolean ffsetstring P((struct sstring *, const char *));
__inline__ static const char *zfgetstring P((const struct sstring *));
__inline__ static void uffreestring P((struct sstring *));

__inline__
static boolean ffsetstring (q, z)
     struct sstring *q;
     const char *z;
{
  if (z == NULL)
    q->ab[0] = '\0';
  else if (strlen (z) < CSTRLEN)
    strcpy (q->ab, z);
  else
    {
      q->zalloc = strdup (z);
      if (q->zalloc == NULL)
	{
	  ulog (LOG_ERROR, "Not enough memory to store command");
	  return FALSE;
	}
    }
  return TRUE;
}

__inline__
static const char *zfgetstring (q)
     const struct sstring *q;
{
  if (q->zalloc != NULL)
    return q->zalloc;
  else
    return q->ab;
}

__inline__ static void
uffreestring (q)
     struct sstring *q;
{
  xfree ((pointer) q->zalloc);
  q->zalloc = NULL;
}

/* Information we keep for the file being sent.  We have to be able to
   send mail to the user when the transfer is finished, we have to be
   able to report error messages sensibly, and we have to be able to
   clear this send request out of the work queue if we initiated the
   send.  */

/* Work queue sequence number.  */
static pointer pSendseq;
/* File name being transferred from.  */
static struct sstring sSendfrom;
/* File name being transferred to.  */
static struct sstring sSendto;
/* System being transferred to.  */
static struct sstring sSendtosys;
/* Requesting user name.  */
static struct sstring sSenduser;
/* User to send mail to.  */
static struct sstring sSendmail;
/* Start time.  */
static long iSendstart_secs;
static long iSendstart_micros;
/* Indicate whether we had an error during the transfer.  */
static boolean fSenderror;

/* Store information about a file being sent.  */

boolean
fstore_sendfile (e, pseq, zfrom, zto, ztosys, zuser, zmail)
     openfile_t e;
     pointer pseq;
     const char *zfrom;
     const char *zto;
     const char *ztosys;
     const char *zuser;
     const char *zmail;
{
#if DEBUG > 0
  if (ffileisopen (eSendfile))
    ulog (LOG_FATAL, "fstore_sendfile: In progress");
#endif

  if (! ffsetstring (&sSendfrom, zfrom)
      || ! ffsetstring (&sSendto, zto)
      || ! ffsetstring (&sSendtosys, ztosys)
      || ! ffsetstring (&sSenduser, zuser)
      || ! ffsetstring (&sSendmail, zmail))
    {
      (void) ffileclose (e);
      return FALSE;
    }

  eSendfile = e;
  pSendseq = pseq;
  iSendstart_secs = isysdep_process_time (&iSendstart_micros);

  fSenderror = FALSE;

  return TRUE;
}

/* Finish up after sending a file.  The freceived argument indicates
   whether the other file received it successfully.  The cbytes
   argument is the number of bytes that were sent (for the statistics
   entry).  The zwhy argument holds a reason for failure if freceived
   is FALSE.  The fnever argument is TRUE if the file was not received
   correctly and, moreover, will never be received correctly.  */

boolean
fsent_file (freceived, cbytes, zwhy, fnever)
     boolean freceived;
     long cbytes;
     const char *zwhy;
     boolean fnever;
{
  boolean f;
  long isecs, imicros;

  f = TRUE;

  if (fSenderror)
    freceived = FALSE;

  if (ffileisopen (eSendfile))
    (void) ffileclose (eSendfile);

  isecs = isysdep_process_time (&imicros);
  imicros -= iSendstart_micros;
  if (imicros < 0)
    {
      imicros += 1000000;
      isecs--;
    }
  isecs -= iSendstart_secs;

  ustats (freceived, zfgetstring (&sSenduser), zfgetstring (&sSendtosys),
	  TRUE, cbytes, isecs, imicros);

  if (freceived)
    {
      const char *zmail;

      zmail = zfgetstring (&sSendmail);
      if (*zmail != '\0')
	{
	  if (! fmail_transfer (TRUE, zfgetstring (&sSenduser),
				zmail, zwhy,
				zfgetstring (&sSendfrom),
				zLocalname,
				zfgetstring (&sSendto),
				zfgetstring (&sSendtosys),
				(const char *) NULL))
	    f = FALSE;
	}

      if (pSendseq != NULL)
	{
	  if (! fsysdep_did_work (pSendseq))
	    f = FALSE;
	  pSendseq = NULL;
	}
    }
  else
    {
      /* If the transfer failed, we only try to save the file and send
	 mail if it was requested locally (in which case pSendseq !=
	 NULL) and it will never succeed.  We send mail to sSendmail
	 if defined, otherwise to sSenduser.  I hope this is
	 reasonable.  */
      if (pSendseq != NULL && fnever)
	{
	  if (! fmail_transfer (FALSE,
				zfgetstring (&sSenduser),
				zfgetstring (&sSendmail),
				zwhy,
				zfgetstring (&sSendfrom),
				zLocalname,
				zfgetstring (&sSendto),
				zfgetstring (&sSendtosys),
				zsysdep_save_temp_file (pSendseq)))
	    f = FALSE;

	  if (! fsysdep_did_work (pSendseq))
	    f = FALSE;
	  pSendseq = NULL;
	}
    }

  eSendfile = EFILECLOSED;
  uffreestring (&sSendfrom);
  uffreestring (&sSendto);
  uffreestring (&sSendtosys);
  uffreestring (&sSenduser);
  uffreestring (&sSendmail);

  return f;
}

/* Some error occurred while sending a file.  Mark an error and let
   fsent_file handle everything else.  This used to differentiate
   between temporary and permanent errors, but I've decided that all
   transmission errors are temporary.  */

void
usendfile_error ()
{
  fSenderror = TRUE;
}

/* Information we keep for the file being received.  We have to be
   able to move the file into its final location with the correct
   mode, we have to be able to send mail to the user when the transfer
   is finished, we have to be able to report error messages sensibly,
   and we have to be able to clear this receive request out of the
   work queue if we initiated the receive.  */

/* Work queue sequence number.  */
static pointer pRecseq;
/* File name being transferred from.  */
static struct sstring sRecfrom;
/* File name being transferred to.  */
static struct sstring sRecto;
/* System being transferred from.  */
static struct sstring sRecfromsys;
/* Requesting user name.  */
static struct sstring sRecuser;
/* Final file mode.  */
static unsigned int iRecmode;
/* User to send mail to.  */
static struct sstring sRecmail;
/* Temporary file name (as returned by esysdep_open_receive).  */
static struct sstring sRectemp;
/* Start time.  */
static long iRecstart_secs;
static long iRecstart_micros;
/* Indicate whether we had an error during the transfer.  */
static boolean fRecerror;

/* Store information about a file being received.  */

boolean
fstore_recfile (e, pseq, zfrom, zto, zfromsys, zuser, imode, zmail, ztemp)
     openfile_t e;
     pointer pseq;
     const char *zfrom;
     const char *zto;
     const char *zfromsys;
     const char *zuser;
     unsigned int imode;
     const char *zmail;
     const char *ztemp;
{
#if DEBUG > 0
  if (ffileisopen (eRecfile))
    ulog (LOG_FATAL, "fstore_recfile: In progress");
#endif

  if (! ffsetstring (&sRecfrom, zfrom)
      || ! ffsetstring (&sRecto, zto)
      || ! ffsetstring (&sRecfromsys, zfromsys)
      || ! ffsetstring (&sRecuser, zuser)
      || ! ffsetstring (&sRecmail, zmail)
      || ! ffsetstring (&sRectemp, ztemp))
    {
      (void) ffileclose (e);
      (void) remove (ztemp);
      return FALSE;
    }

  eRecfile = e;
  pRecseq = pseq;
  iRecmode = imode;
  iRecstart_secs = isysdep_process_time (&iRecstart_micros);

  fRecerror = FALSE;

  return TRUE;
}

/* Finish up after receiving a file.  The argument indicates whether
   the data was received correctly.  We do not confirm the file
   reception to the other system unless this function returns TRUE.
   This may be called when no file receive is in progress if a fatal
   program error occurs.  The zwhy and fnever arguments are valid if
   fsent is FALSE; fnever is TRUE if the file receive can never
   succeed.  */

boolean
freceived_file (fsent, cbytes, zwhy, fnever)
     boolean fsent;
     long cbytes;
     const char *zwhy;
     boolean fnever;
{
  long isecs, imicros;
  
  if (fRecerror)
    fsent = FALSE;

  if (ffileisopen (eRecfile))
    {
      if (! ffileclose (eRecfile))
	{
	  if (fsent)
	    {
	      zwhy = strerror (errno);
	      fnever = FALSE;
	      ulog (LOG_ERROR, "close: %s", zwhy);
	    }
	  fsent = FALSE;
	}
    }

  if (fsent)
    {
      if (! fsysdep_move_file (zfgetstring (&sRectemp),
			       zfgetstring (&sRecto),
			       iRecmode, TRUE,
			       (pRecseq != NULL
				? zfgetstring (&sRecuser)
				: (const char *) NULL)))
	{
	  zwhy = "could not move to final location";
	  fnever = TRUE;
	  fsent = FALSE;
	}
    }
  else
    (void) remove (zfgetstring (&sRectemp));
    
  isecs = isysdep_process_time (&imicros);
  imicros -= iRecstart_micros;
  if (imicros < 0)
    {
      imicros += 1000000;
      isecs--;
    }
  isecs -= iRecstart_secs;

  ustats (fsent, zfgetstring (&sRecuser), zfgetstring (&sRecfromsys),
	  FALSE, cbytes, isecs, imicros);

  if (fsent)
    {
      const char *zmail;

      zmail = zfgetstring (&sRecmail);
      if (*zmail != '\0')
	(void) fmail_transfer (TRUE, zfgetstring (&sRecuser),
			       zmail, zwhy,
			       zfgetstring (&sRecfrom),
			       zfgetstring (&sRecfromsys),
			       zfgetstring (&sRecto),
			       zLocalname, (const char *) NULL);

      if (pRecseq != NULL)
	{
	  if (! fsysdep_did_work (pRecseq))
	    fsent = FALSE;
	  pRecseq = NULL;
	}
    }
  else
    {
      /* If the transfer failed, we send mail if it was requested
	 locally and if it can never succeed.  */
      if (pRecseq != NULL && fnever)
	{
	  (void) fmail_transfer (FALSE,
				 zfgetstring (&sRecuser),
				 zfgetstring (&sRecmail),
				 zwhy,
				 zfgetstring (&sRecfrom),
				 zfgetstring (&sRecfromsys),
				 zfgetstring (&sRecto),
				 zLocalname, (const char *) NULL);
	  if (! fsysdep_did_work (pRecseq))
	    fsent = FALSE;
	  pRecseq = NULL;
	}
    }

  eRecfile = EFILECLOSED;
  uffreestring (&sRecfrom);
  uffreestring (&sRecto);
  uffreestring (&sRecfromsys);
  uffreestring (&sRecuser);
  uffreestring (&sRecmail);
  uffreestring (&sRectemp);

  return fsent;
}

/* Some error occurred while receiving a file.  Note that we had an
   error, so that when we close up we know that something went wrong.
   We leave the file open because it's easier to handle everything in
   freceived_file.  This used to differentiate between permanent
   errors and temporary errors, but I've decided that all errors that
   occur while the file is being transferred are temporary.  */

void
urecfile_error ()
{
  fRecerror = TRUE;
}

/* There was some sort of protocol error while receiving the file,
   so we want to try it again.  We must truncate the file.  */

boolean
frecfile_rewind ()
{
  eRecfile = esysdep_truncate (eRecfile, zfgetstring (&sRectemp));
  if (! ffileisopen (eRecfile))
    {
      urecfile_error ();
      return FALSE;
    }
  return TRUE;
}

/* Send mail about a file transfer.  We send to the given mailing
   address if there is one, otherwise to the user.  */

boolean
fmail_transfer (fsuccess, zuser, zmail, zwhy, zfromfile, zfromsys,
		ztofile, ztosys, zsaved)
     boolean fsuccess;
     const char *zuser;
     const char *zmail;
     const char *zwhy;
     const char *zfromfile;
     const char *zfromsys;
     const char *ztofile;
     const char *ztosys;
     const char *zsaved;
{
  const char *zsendto;
  const char *az[20];
  int i;

  if (zmail != NULL && *zmail != '\0')
    zsendto = zmail;
  else
    zsendto = zuser;

  i = 0;
  az[i++] = "The file\n\t";
  az[i++] = zfromsys;
  az[i++] = "!";
  az[i++] = zfromfile;
  if (fsuccess)
    az[i++] = "\nwas successfully transferred to\n\t";
  else
    az[i++] = "\ncould not be transferred to\n\t";
  az[i++] = ztosys;
  az[i++] = "!";
  az[i++] = ztofile;
  az[i++] = "\nas requested by\n\t";
  az[i++] = zuser;
  if (! fsuccess)
    {
      az[i++] = "\nfor the following reason:\n\t";
      az[i++] = zwhy;
      az[i++] = "\n";
    }
  if (zsaved != NULL)
    {
      az[i++] = zsaved;
      az[i++] = "\n";
    }

  return fsysdep_mail (zsendto,
		       fsuccess ? "UUCP succeeded" : "UUCP failed",
		       i, az);
}
