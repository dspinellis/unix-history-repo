/* sys3.unx
   The system dependent spool directory subroutines for Unix.

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

   $Log:	sys3.unx,v $
# Revision 1.2  92/05/13  05:42:07  rich
# ported to 386bsd
# 
# Revision 1.1  1992/05/10  18:00:16  rich
# Initial revision
#
   Revision 1.48  1992/04/01  22:36:48  ian
   David J. MacKenzie: some USG_STATFS systems use 512 despite f_bsize

   Revision 1.47  1992/03/30  15:03:07  ian
   Niels Baggesen: USG statfs has an f_bsize field

   Revision 1.46  1992/03/28  22:06:38  ian
   Michael I Bushnell: renamed enum tstatus to avoid header file conflict

   Revision 1.45  1992/03/26  20:20:28  ian
   Reduce race condition in fsdo_lock

   Revision 1.44  1992/03/15  01:54:46  ian
   All execs are now done in isspawn, all waits are done in iswait

   Revision 1.43  1992/03/12  19:54:43  ian
   Debugging based on types rather than number

   Revision 1.42  1992/03/10  20:45:58  ian
   Check size of destination file system as well as temporary system

   Revision 1.41  1992/03/09  19:42:43  ian
   Ted Lindgreen: don't send mail for nonexistent file

   Revision 1.40  1992/03/04  01:40:51  ian
   Thomas Fischer: tweaked a bit for the NeXT

   Revision 1.39  1992/02/29  04:07:08  ian
   Added -j option to uucp and uux

   Revision 1.38  1992/02/29  01:06:59  ian
   Chip Salzenberg: recheck file permissions before sending

   Revision 1.37  1992/02/28  15:57:58  ian
   Give error if esysdep_open_send is given a directory

   Revision 1.36  1992/02/24  22:05:30  ian
   Roberto Biancardi: support F_CHSIZE and F_FREESP in esysdep_truncate

   Revision 1.35  1992/02/24  20:07:43  ian
   John Theus: some systems don't have <fcntl.h>

   Revision 1.34  1992/02/23  03:26:51  ian
   Overhaul to use automatic configure shell script

   Revision 1.33  1992/02/20  04:18:59  ian
   Added uustat

   Revision 1.32  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.31  1992/02/02  20:42:40  ian
   Niels Baggesen: case enum to int before comparison

   Revision 1.30  1992/02/01  00:54:31  ian
   Michael Nolan: cast alloca return value

   Revision 1.29  1992/01/29  04:27:11  ian
   Jay Vassos-Libove: removed some conflicting declarations

   Revision 1.28  1992/01/28  04:34:10  ian
   Marty Shannon: handle trailing '/' to indicate directory

   Revision 1.27  1992/01/14  04:51:48  ian
   David Nugent: don't declare chmod

   Revision 1.26  1992/01/14  04:25:20  ian
   Chip Salzenberg: avoid use before set warning

   Revision 1.25  1992/01/14  03:46:55  ian
   Chip Salzenberg: handle invalid status values in status files

   Revision 1.24  1992/01/13  06:11:39  ian
   David Nugent: can't declare open or fcntl

   Revision 1.23  1992/01/05  03:18:54  ian
   Avoid redefining SEEK_SET

   Revision 1.22  1992/01/04  22:56:22  ian
   Added extern definition

   Revision 1.21  1992/01/03  05:44:35  ian
   Remove temporary file if link fails in fsdo_lock

   Revision 1.20  1991/12/29  04:04:18  ian
   Added a bunch of extern definitions

   Revision 1.19  1991/12/22  22:14:19  ian
   Monty Solomon: added HAVE_UNISTD_H configuration parameter

   Revision 1.18  1991/12/22  20:50:47  ian
   Franc,ois Pinard: fixed bug in fsysdep_get_status

   Revision 1.17  1991/12/12  18:35:47  ian
   Do locking with link to avoid races and to permit running as root

   Revision 1.16  1991/12/12  17:45:34  ian
   fcopy_file now creates the file with IPRIVATE_MODE

   Revision 1.15  1991/12/11  03:59:19  ian
   Create directories when necessary; don't just assume they exist

   Revision 1.14  1991/12/09  19:07:07  ian
   Richard Todd: add HAVE_V2_LOCKFILES--binary number in lock file

   Revision 1.13  1991/12/03  02:59:46  ian
   Using LOCKDIR clobbered a byte on the stack

   Revision 1.12  1991/12/01  02:23:12  ian
   Niels Baggesen: don't multiply include <unistd.h>

   Revision 1.11  1991/12/01  01:12:40  ian
   Marty Shannon: accept truncated status file; also eliminated scanf calls

   Revision 1.10  1991/11/30  23:28:26  ian
   Marty Shannon: some systems need a fake version of the rename system call

   Revision 1.9  1991/11/21  21:43:42  ian
   Eliminate unused MIN_FREE_BYTES

   Revision 1.8  1991/11/21  21:07:46  ian
   Brian Campbell: offer ltrunc as an alternative to ftruncate

   Revision 1.7  1991/11/10  21:32:16  ian
   Fixed ftruncate call

   Revision 1.6  1991/11/10  19:24:22  ian
   Added pffile protocol entry point for file level control

   Revision 1.5  1991/11/07  19:32:28  ian
   Chip Salzenberg: allow LOCKDIR, and check that locking process exists

   Revision 1.4  1991/09/19  17:28:01  ian
   Chip Salzenberg: make sure spool directory files are not world readable

   Revision 1.3  1991/09/19  03:23:34  ian
   Chip Salzenberg: append to private debugging file, don't overwrite it

   Revision 1.2  1991/09/19  03:06:04  ian
   Chip Salzenberg: put BNU temporary files in system's directory

   Revision 1.1  1991/09/10  19:45:50  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char sys3_unx_rcsid[] = "$Id: sys3.unx,v 1.2 92/05/13 05:42:07 rich Exp Locker: root $";
#endif

#include <ctype.h>
#include <errno.h>

#if USE_STDIO && HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "system.h"
#include "sysdep.h"

#include <pwd.h>

#if HAVE_FCNTL_H
#include <fcntl.h>
#else
#if HAVE_SYS_FILE_H
#include <sys/file.h>
#endif
#endif

#ifndef O_RDONLY
#define O_RDONLY 0
#define O_WRONLY 1
#define O_RDWR 2
#endif

/* Get the right header files for statfs and friends.  This stuff is
   from David MacKenzie's df program.  */

#ifdef FS_STATVFS
#include <sys/statvfs.h>
extern int statvfs ();
#endif

#ifdef FS_USG_STATFS
#include <sys/statfs.h>
extern int statfs ();
#endif

#ifdef FS_MNTENT
#include <sys/vfs.h>
extern int statfs ();
#endif

#ifdef FS_GETMNT
#include <sys/param.h>
#include <sys/mount.h>
extern int statfs ();
#endif

#ifdef FS_STATFS
#include <sys/mount.h>
extern int statfs ();
#endif

#ifdef FS_USTAT
#include <ustat.h>
extern int ustat ();
#endif

/* We need a definition for SEEK_SET.  */

#ifndef SEEK_SET
#define SEEK_SET 0
#endif

/* External functions.  */
extern int close (), link (), read (), write ();
#ifndef __386BSD__
extern int kill ();
#endif __386BSD__
extern int fstat (), stat ();
extern int fclose (), fseek (), pclose ();
extern pid_t getpid ();
extern off_t lseek ();
extern char *strrchr ();

#if HAVE_FTRUNCATE
extern int ftruncate ();
#endif

#if HAVE_RENAME
extern int rename ();
#else
static int rename P((const char *zfrom, const char *zto));
#endif

/* There are several types of files that go in the spool directory,
   and they go into various different subdirectories.  When using
   SPOOLDIR_TAYLOR, there is a subdirectory for each system for which
   communication occurs; these system names have been made canonical
   via fread_system_info or ztranslate_name, so they will fit any
   name length restrictions (namely 14 characters on System V).
   Whenever the system name LOCAL appears below, it means whatever
   the local system name is.

   Command files
   These contain instructions for uucico indicating what files to transfer
   to and from what systems.  Each line of a work file is a command
   beginning with S, R or X.
   #if ! SPOOLDIR_TAYLOR
   They are named C.ssssssgqqqq, where ssssss is the system name to
   transfer to or from, g is the grade and qqqq is the sequence number.
   #if SPOOLDIR_V2
   They are put in the spool directory.
   #elif SPOOLDIR_BSD42 | SPOOLDIR_BSD43
   They are put in the directory C.
   #elif SPOOLDIR_BNU
   They are put in a directory named for the system for which they were
   created.
   #elif SPOOLDIR_ULTRIX
   If the directory sys/ssssss exists, they are put in the directory
   sys/ssssss/C; otherwise, they are put in the directory sys/DEFAULT/C.
   #endif
   #else SPOOLDIR_TAYLOR
   They are named C.gqqqq, where g is the grade and qqqq is the sequence
   number, and are placed in the directory ssssss/C. where ssssss is
   the system name to transfer to or from.
   #endif

   Data files
   There are files to be transferred to other systems.  Some files to
   be transferred may not be in the spool directory, depending on how
   uucp was invoked.  Data files are named in work files, so it is
   never necessary to look at them directly (except to remove old ones);
   it is only necessary to create them.  These means that the many
   variations in naming are inconsequential.
   #if ! SPOOLDIR_TAYLOR
   They are named D.ssssssgqqqq where ssssss is a system name (which
   may be LOCAL for locally initiated transfers or a remote system for
   remotely initiated transfers, except that BNU appears to use the
   system the file is being transferred to), g is the grade and qqqq
   is the sequence number.  Some systems use a trailing subjob ID
   number, but we currently do not.  The grade is not important, and
   some systems do not use it.  If the data file is to become an
   execution file on another system the grade (if present) will be
   'X'.  Otherwise Ultrix appears to use 'b'; the uux included with
   gnuucp 1.0 appears to use 'S'; SCO does not appear to use a grade,
   although it does use a subjob ID number.
   #if SPOOLDIR_V2
   They are put in the spool directory.
   #elif SPOOLDIR_BSD42
   If the name begins with D.LOCAL, the file is put in the directory
   D.LOCAL.  Otherwise the file is put in the directory D..
   #elif SPOOLDIR_BSD43
   If the name begins with D.LOCALX, the file is put in the directory
   D.LOCALX.  Otherwise if the name begins with D.LOCAL, the file is
   put in the directory D.LOCAL Otherwise the file is put in the
   directory D..
   #elif SPOOLDIR_BNU
   They are put in a directory named for the system for which they
   were created.
   #elif SPOOLDIR_ULTRIX
   Say the file is being transferred to system REMOTE.  If the
   directory sys/REMOTE exists, then if the file begins with D.LOCALX
   it is put in sys/REMOTE/D.LOCALX, if the file begins with D.LOCAL
   it is put in sys/REMOTE/D.LOCAL, and otherwise it is put in
   sys/REMOTE/D..  If the directory sys/REMOTE does not exist, the
   same applies except that DEFAULT is used instead of REMOTE.
   #endif
   #else SPOOLDIR_TAYLOR
   If the file is to become an executable file on another system it is
   named D.Xqqqq, otherwise it is named D.qqqq where in both cases
   qqqq is a sequence number.  If the corresponding C. file is in
   directory ssssss/C., a D.X file is placed in ssssss/D.X and a D.
   file is placed in ssssss/D..
   #endif

   Execute files
   These are files that specify programs to be executed.  They are
   created by uux, perhaps as run on another system.  These names are
   important, because a file transfer done to an execute file name
   causes an execution to occur.  The name is X.ssssssgqqqq, where
   ssssss is the requesting system, g is the grade, and qqqq is a
   sequence number.
   #if SPOOLDIR_V2 | SPOOLDIR_BSD42
   These files are placed in the spool directory.
   #elif SPOOLDIR_BSD43
   These files are placed in the directory X..
   #elif SPOOLDIR_BNU
   These files are put in a directory named for the system for which
   the files were created.
   #elif SPOOLDIR_ULTRIX
   If there is a spool directory (sys/ssssss) for the requesting
   system, the files are placed in sys/ssssss/X.; otherwise, the files
   are placed in sys/DEFAULT/X..
   #elif SPOOLDIR_TAYLOR
   The system name is automatically truncated to seven characters when
   a file is created.  The files are placed in the subdirectory X. of
   a directory named for the system for which the files were created.
   #endif

   Temporary receive files
   These are used when receiving files from another system.  They are
   later renamed to the final name.  The actual name is unimportant,
   although it generally begins with TM..
   #if SPOOLDIR_V2 | SPOOLDIR_BSD42
   These files are placed in the spool directory.
   #elif SPOOLDIR_BSD43 | SPOOLDIR_ULTRIX | SPOOLDIR_TAYLOR
   These files are placed in the directory .Temp.
   #elif SPOOLDIR_BNU
   These files are placed in a directory named for the system for
   which they were created.
   #endif

   Lock files
   These files are used to lock systems, devices and special files.
   The file LCK..ssssss is used to lock a system, where ssssss is the
   system name.  The file LCK..dev is used to lock a device, where dev
   is the device name.  The file LCK.file is used to lock a file,
   where file is LOG for the log file, SQ for the system sequence
   number file, or SEQF for the work queue sequence number file.  At
   least under Ultrix, the file LCK.XQT is used to lock uuxqt
   execution.  Some systems supposedly use LCK.SEQL for something.  On
   some systems, the contents of the lock file is the ASCII process
   id; on others, it is the process id as four data bytes.  As far as
   I can tell, the only lock file I really have to get right is the
   one locking a device, so that cu won't use it; if somebody tries to
   run old UUCP and this at the same time, they will probably have
   trouble unless they make sure the locking is correct for their
   system.  Not that there is any easy way to make that check,
   unfortunately.  Supposedly all normal systems put the LCK files in
   the spool directory, and this package will do that also.

   System status files
   These are used to record when the last call was made to the system
   and what the status is.  They are used to prevent frequent recalls
   to a system which is not responding.  I will not attempt to
   recreate the format of these exactly, since they are not all that
   important.  They will be put in the directory .Status, as in BNU,
   and they use the system name as the name of the file.

   Log files
   These are used to record what UUCP has done.  I will not attempt to
   recreate the format of these at all.  They will be stored in the
   directory .Log/uucico (or .Log/uucp, .Log/uux, .Log/uuxqt) and
   named for the relevant system.  This is the format used by BNU.

   Statistics files
   I don't really know the format of these.  They are apparently used
   to keep track of what jobs have been run by UUCP, but at least on
   Ultrix they don't seem to be used very consistently.

   Sequence file
   This is used to generate a unique sequence number.  It contains an
   ASCII number.
   #if SPOOLDIR_V2 | SPOOLDIR_BSD42 | SPOOLDIR_BSD43
   The file is named SEQF and is kept in the spool directory.
   #elif SPOOLDIR_BNU
   A separate sequence file is kept for each system in the directory
   .Sequence with the name of the system.
   #elif SPOOLDIR_ULTRIX
   Each system with a file sys/ssssss has a sequence file in
   sys/ssssss/.SEQF.  Other systems use sys/DEFAULT/.SEQF.
   #else SPOOLDIR_TAYLOR
   A sequence file named SEQF is kept in the directory ssssss for each
   system.
   #endif

   Audit files
   Debugging messages are stored in these when running as a slave.  We
   use the file AUDIT in the spool directory.  */

/* Local functions.  */

static char *zsstatic_size P((int c));
#if SPOOLDIR_TAYLOR
static const char *zsappend3 P((const char *zdir1, const char *zdir2,
				const char *zfile));
#endif
#if SPOOLDIR_ULTRIX
static const char *zsappend4 P((const char *zdir1, const char *zdir2,
				const char *zdir3, const char *zfile));
#endif
static const char *zsfind_file P((const char *zsimple,
				  const char *zsystem));
static boolean fscmd_seq P((const char *zsystem, char *zseq));
static const char *zsfile_name P((int btype, const char *zsystem,
				  int bgrade, char *ztname,
				  char *zdname, char *zxname));

/* A few routines to manipulate strings with directories.  */

#define CSTATICLEN (50)
static char abSstatic[CSTATICLEN];
static char *zSstatic_alloc;
static int cSstatic_alloc;

/* Return a pointer to a static buffer of a certain size.  */

static char *
zsstatic_size (c)
     int c;
{
  if (c <= CSTATICLEN)
    return abSstatic;
  if (cSstatic_alloc < c)
    {
      xfree ((pointer) zSstatic_alloc);
      zSstatic_alloc = (char *) xmalloc (c);
      cSstatic_alloc = c;
   }
  return zSstatic_alloc;
}

/* Copy a string into a static buffer.  */

const char *
zscopy (z)
     const char *z;
{
  char *zret;

  zret = zsstatic_size (strlen (z) + 1);
  strcpy (zret, z);
  return (const char *) zret;
}

/* Stick a directory and file name together.  Return a static buffer
   holding the combined name.  This is called by Unix system dependent
   routines outside this file.  */

const char *
zsappend (zdir, zfile)
     const char *zdir;
     const char *zfile;
{
  char *zret;

  zret = zsstatic_size (strlen (zdir) + strlen (zfile) + sizeof "/");
  sprintf (zret, "%s/%s", zdir, zfile);
  return (const char *) zret;
}

#if SPOOLDIR_TAYLOR

/* Stick two directories and a file name together.  Return a static
   buffer holding the combined name.  */

static const char *
zsappend3 (zdir1, zdir2, zfile)
     const char *zdir1;
     const char *zdir2;
     const char *zfile;
{
  char *zret;

  zret = zsstatic_size (strlen (zdir1) + strlen (zdir2)
			+ strlen (zfile) + sizeof "//");
  sprintf (zret, "%s/%s/%s", zdir1, zdir2, zfile);
  return (const char *) zret;
}
    
#endif /* SPOOLDIR_TAYLOR */

#if SPOOLDIR_ULTRIX

/* Stick three directories and a file name together.  Return a static
   buffer holding the combined name.  */

static const char *
zsappend4 (zdir1, zdir2, zdir3, zfile)
     const char *zdir1;
     const char *zdir2;
     const char *zdir3;
     const char *zfile;
{
  char *zret;

  zret = zsstatic_size (strlen (zdir1) + strlen (zdir2) + strlen (zdir3)
			+ strlen (zfile) + sizeof "///");
  sprintf (zret, "%s/%s/%s/%s", zdir1, zdir2, zdir3, zfile);
  return (const char *) zret;
}

/* See whether an ULTRIX spool directory exists for a system.  For system
   ssssss, the spool directory is called sys/ssssss.  */

boolean
fsultrix_has_spool (zsystem)
     const char *zsystem;
{
  char *z;

  z = (char *) alloca (sizeof "sys/" + strlen (zsystem));
  sprintf (z, "sys/%s", zsystem);
  return fsdirectory_exists (z);
}

#endif /* SPOOLDIR_ULTRIX */

/* Create a spool directory for a system.  This is only relevant for
   SPOOLDIR_BNU or SPOOLDIR_TAYLOR.  The system specific directories
   for Ultrix are meant to be created by hand.  */

#if SPOOLDIR_TAYLOR | SPOOLDIR_BNU

static boolean fsmkdir P((const char *zdir));

static boolean
fsmkdir (zdir)
     const char *zdir;
{
  if (mkdir ((char *) zdir, IDIRECTORY_MODE) < 0)
    {
      ulog (LOG_ERROR, "mkdir (%s): %s", zdir, strerror (errno));
      return FALSE;
    }
  return TRUE;
}

#endif /* SPOOLDIR_TAYLOR | SPOOLDIR_BNU */

boolean
fsysdep_make_spool_dir (qsys)
     const struct ssysteminfo *qsys;
{
  const char *zsystem;

  zsystem = qsys->zname;

#if SPOOLDIR_BNU
  if (fsdirectory_exists (zsystem))
    return TRUE;
  if (! fsmkdir (zsystem))
    return FALSE;
#endif /* SPOOLDIR_BNU */

#if SPOOLDIR_TAYLOR
  if (fsdirectory_exists (zsystem))
    return TRUE;
  if (! fsmkdir (zsystem)
      || ! fsmkdir (zsappend (zsystem, "C."))
      || ! fsmkdir (zsappend (zsystem, "D."))
      || ! fsmkdir (zsappend (zsystem, "D.X"))
      || ! fsmkdir (zsappend (zsystem, "X.")))
    return FALSE;
#endif /* SPOOLDIR_TAYLOR */

  return TRUE;
}

/* Given the name of a file as specified in a UUCP command, and the
   system for which this file has been created, return where to find
   it in the spool directory.  The file will begin with C. (a command
   file), D. (a data file) or X. (an execution file).  The return
   value of this function will point to a static buffer.  */

static const char *
zsfind_file (zsimple, zsystem)
     const char *zsimple;
     const char *zsystem;
{
  if (zsimple[1] != '.'
      || (*zsimple != 'C'
	  && *zsimple != 'D'
	  && *zsimple != 'X'))
    {
      ulog (LOG_ERROR, "Unrecognized file name %s", zsimple);
      return NULL;
    }

  switch (*zsimple)
    {
    case 'C':
#if SPOOLDIR_V2
      return zscopy (zsimple);
#endif /* SPOOLDIR_V2 */
#if SPOOLDIR_BSD42 | SPOOLDIR_BSD43
      return zsappend ("C.", zsimple);
#endif /* SPOOLDIR_BSD42 | SPOOLDIR_BSD43 */
#if SPOOLDIR_BNU
      return zsappend (zsystem, zsimple);
#endif /* SPOOLDIR_BNU */
#if SPOOLDIR_ULTRIX
      if (fsultrix_has_spool (zsystem))
	return zsappend4 ("sys", zsystem, "C.", zsimple);
      else
	return zsappend4 ("sys", "DEFAULT", "C.", zsimple);
#endif
#if SPOOLDIR_TAYLOR
      return zsappend3 (zsystem, "C.", zsimple);
#endif

    case 'D':
#if SPOOLDIR_V2
      return zscopy (zsimple);
#endif /* SPOOLDIR_V2 */
#if SPOOLDIR_BSD42 | SPOOLDIR_BSD43
      {
	int c;
	boolean ftruncated;
	char *zalloc;
      
	/* D.LOCAL in D.LOCAL/, others in D./.  If BSD43, D.LOCALX in
	   D.LOCALX/.  */
	ftruncated = TRUE;
	if (strncmp (zsimple + 2, zLocalname, strlen (zLocalname)) == 0)
	  {
	    c = strlen (zLocalname);
	    ftruncated = FALSE;
	  }
	else if (strncmp (zsimple + 2, zLocalname, 7) == 0)
	  c = 7;
	else if (strncmp (zsimple + 2, zLocalname, 6) == 0)
	  c = 6;
	else
	  c = 0;
#if SPOOLDIR_BSD43
	if (c > 0 && zsimple[c + 2] == 'X')
	  c++;
#endif /* SPOOLDIR_BSD43 */
	if (c > 0)
	  {
	    zalloc = (char *) alloca (c + 3);
	    strncpy (zalloc, zsimple, c + 2);
	    zalloc[c + 2] = '\0';

	    /* If we truncated the system name, and there is no existing
	       directory with the truncated name, then just use D..  */
	    if (ftruncated && ! fsdirectory_exists (zalloc))
	      return zsappend ("D.", zsimple);

	    return zsappend (zalloc, zsimple);
	  }
	else
	  return zsappend ("D.", zsimple);
      }
#endif /* SPOOLDIR_BSD42 | SPOOLDIR_BSD43 */
#if SPOOLDIR_BNU
      return zsappend (zsystem, zsimple);
#endif /* SPOOLDIR_BNU */
#if SPOOLDIR_ULTRIX
      {
	int c;
	boolean ftruncated;
	char *zalloc;
	const char *zdir;
      
	/* D.LOCALX in D.LOCALX/, D.LOCAL in D.LOCAL/, others in D./.  */

	ftruncated = TRUE;
	if (strncmp (zsimple + 2, zLocalname, strlen (zLocalname)) == 0)
	  {
	    c = strlen (zLocalname);
	    ftruncated = FALSE;
	  }
	else if (strncmp (zsimple + 2, zLocalname, 7) == 0)
	  c = 7;
	else if (strncmp (zsimple + 2, zLocalname, 6) == 0)
	  c = 6;
	else
	  c = 0;
	if (c > 0 && zsimple[c + 2] == 'X')
	  c++;
	if (c > 0)
	  {
	    zalloc = (char *) alloca (c + 3);
	    strncpy (zalloc, zsimple, c + 2);
	    zalloc[c + 2] = '\0';
	    zdir = zalloc;

	    /* If we truncated the name, and there is no directory for
	       the truncated name, then don't use it.  */
	    if (ftruncated)
	      {
		char *zlook;

		zlook = (char *) alloca (c + 20 + strlen (zsystem));
		if (fsultrix_has_spool (zsystem))
		  sprintf (zlook, "sys/%s/%s", zsystem, zdir);
		else
		  sprintf (zlook, "sys/DEFAULT/%s", zdir);
		if (! fsdirectory_exists (zlook))
		  zdir = "D.";
	      }
	  }
	else
	  zdir = "D.";
      
	if (fsultrix_has_spool (zsystem))
	  return zsappend4 ("sys", zsystem, zdir, zsimple);
	else
	  return zsappend4 ("sys", "DEFAULT", zdir, zsimple);
      }
#endif /* SPOOLDIR_ULTRIX */
#if SPOOLDIR_TAYLOR
      if (zsimple[2] == 'X')
	return zsappend3 (zsystem, "D.X", zsimple);
      else
	return zsappend3 (zsystem, "D.", zsimple);
#endif /* SPOOLDIR_TAYLOR */

      /* Files beginning with X. are execute files.  It is important
	 for security reasons that we know the system which created
	 the X. file.  This is easy under SPOOLDIR_BNU or
	 SPOOLDIR_TAYLOR, because the file will be in a directory
	 named for the system.  Under other schemes, we must get the
	 system name from the X. file name.  To prevent security
	 violations, we set the system name directly here; this will
	 cause problems if the maximum file name length is too short,
	 but hopefully no problem will occur since any System V
	 systems will be using either BNU or TAYLOR.  */

    case 'X':
#if ! SPOOLDIR_BNU && ! SPOOLDIR_TAYLOR
      if (strncmp (zsimple + 2, zsystem, strlen (zsimple) - 7) != 0)
	{
	  char *zcopy;

	  zcopy = (char *) alloca (strlen (zsystem) + 8);
	  sprintf (zcopy, "X.%s%s", zsystem,
		   zsimple + strlen (zsimple) - 5);
	  zsimple = zcopy;
	}
#endif /* ! SPOOLDIR_BNU && ! SPOOLDIR_TAYLOR */

#if SPOOLDIR_V2 | SPOOLDIR_BSD42
      return zscopy (zsimple);
#endif
#if SPOOLDIR_BSD43
      return zsappend ("X.", zsimple);
#endif
#if SPOOLDIR_BNU
      return zsappend (zsystem, zsimple);
#endif
#if SPOOLDIR_ULTRIX
      if (fsultrix_has_spool (zsystem))
	return zsappend4 ("sys", zsystem, "X.", zsimple);
      else
	return zsappend4 ("sys", "DEFAULT", "X.", zsimple);
#endif
#if SPOOLDIR_TAYLOR
      return zsappend3 (zsystem, "X.", zsimple);
#endif

    default:
#if DEBUG > 0
      ulog (LOG_FATAL, "zsfind_file: Can't happen");
#endif /* DEBUG */
      return NULL;
    }
  /*NOTREACHED*/
}

/* Get the status of a system.  */

/*ARGSUSED*/
boolean
fsysdep_get_status (qsys, qret)
     const struct ssysteminfo *qsys;
     struct sstatus *qret;
{
  const char *zname;
  FILE *e;
  char *zline;
  char *zend, *znext;
  boolean fbad;
  int istat;

  zname = zsappend (".Status", qsys->zname);
  e = fopen (zname, "r");
  if (e == NULL)
    {
      if (errno != ENOENT)
	{
	  ulog (LOG_ERROR, "fopen (%s): %s", zname, strerror (errno));
	  return FALSE;
	}
      zline = NULL;
    }
  else
    {
      zline = zfgets (e, FALSE);
      (void) fclose (e);
    }

  if (zline == NULL)
    {
      /* There is either no status file for this system, or it's been
	 truncated, so fake a good status.  */
      qret->ttype = STATUS_COMPLETE;
      qret->cretries = 0;
      qret->ilast = 0;
      qret->cwait = 0;
      return TRUE;
    }

  /* It turns out that scanf is not used much in this program, so for
     the benefit of small computers we avoid linking it in.  This is
     basically

     sscanf (zline, "%d %d %ld %d", &qret->ttype, &qret->cretries,
             &qret->ilast, &qret->cwait);

     except that it's done with strtol.  */

  fbad = FALSE;
  istat = (int) strtol (zline, &zend, 10);
  if (zend == zline)
    fbad = TRUE;

  /* On some systems it may be appropriate to map system dependent status
     values on to our status values.  Perhaps someday.  */

  if (istat < 0 || istat >= (int) STATUS_VALUES)
    istat = (int) STATUS_COMPLETE;
  qret->ttype = (enum tstatus_type) istat;
  znext = zend;
  qret->cretries = (int) strtol (znext, &zend, 10);
  if (zend == znext)
    fbad = TRUE;
  znext = zend;
  qret->ilast = strtol (znext, &zend, 10);
  if (zend == znext)
    fbad = TRUE;
  znext = zend;
  qret->cwait = (int) strtol (znext, &zend, 10);
  if (zend == znext)
    fbad = TRUE;

  xfree ((pointer) zline);

  if (fbad)
    {
      ulog (LOG_ERROR, "Bad format of status file for %s", qsys->zname);
      return FALSE;
    }

  return TRUE;
}

/* Set the status of a remote system.  We assume the system is locked
   when this is called.  */

/*ARGSUSED*/
boolean
fsysdep_set_status (qsys, qset)
     const struct ssysteminfo *qsys;
     const struct sstatus *qset;
{
  const char *zname;
  FILE *e;
  int istat;

  zname = zsappend (".Status", qsys->zname);

  e = esysdep_fopen (zname, TRUE, FALSE, TRUE);
  if (e == NULL)
    return FALSE;
  istat = (int) qset->ttype;

  /* On some systems it may be appropriate to map istat onto a system
     dependent number.  Perhaps someday.  */

  fprintf (e, "%d %d %ld %d %s %s\n", istat, qset->cretries,
	   qset->ilast, qset->cwait, azStatus[(int) qset->ttype],
	   qsys->zname);
  if (fclose (e) != 0)
    {
      ulog (LOG_ERROR, "fclose: %s", strerror (errno));
      return FALSE;
    }

  return TRUE;
}

/* Get the real name of a spool file.  */

const char *
zsysdep_spool_file_name (qsys, zfile)
     const struct ssysteminfo *qsys;
     const char *zfile;
{
  return zsfind_file (zfile, qsys->zname);
}

/* Expand a file name on the local system.  The qsys argument is only
   used to determine which public directory to use.  */

const char *
zsysdep_real_file_name (qsys, zfile, zname)
     const struct ssysteminfo *qsys;
     const char *zfile;
     const char *zname;
{
  const char *ztry;
  char *zlook;

  if (zfile[0] == '/')
    ztry = zfile;
  else if (zfile[0] == '~')
    {
      const char *z;
      char *zcopy;

      z = zstilde_expand (qsys, zfile);
      zcopy = (char *) alloca (strlen (z) + 1);
      strcpy (zcopy, z);
      ztry = zcopy;
    }
  else
    {
      const char *zpub, *z;
      char *zcopy;

      /* Put the file in the public directory.  */
      if (qsys == NULL || qsys->zpubdir == NULL)
	zpub = zPubdir;
      else
	zpub = qsys->zpubdir;
      z = zsappend (zpub, zfile);
      zcopy = (char *) alloca (strlen (z) + 1);
      strcpy (zcopy, z);
      ztry = zcopy;
    }

  /* If we don't have a file name to use within a directory, or we
     haven't named a directory, we use what we've got so far.  If the
     name ends in a '/', it is assumed to name a directory.  */

  if (zname == NULL)
    return zscopy (ztry);

  if (ztry[strlen (ztry) - 1] != '/')
    {
      if (! fsdirectory_exists (ztry))
	return zscopy (ztry);
    }
  else
    {
      char *zcopy;
      int clen;

      clen = strlen (ztry);
      zcopy = (char *) alloca (clen + 1);
      strcpy (zcopy, ztry);
      zcopy[clen - 1] = '\0';
      ztry = zcopy;
    }

  /* Get a name out of zname and tag it on.  */

  zlook = strrchr (zname, '/');
  if (zlook != NULL)
    zname = zlook + 1;

  return zsappend (ztry, zname);
}

/* Return a file name within a directory.  */

const char *
zsysdep_in_dir (zdir, zfile)
     const char *zdir;
     const char *zfile;
{
  if (fsdirectory_exists (zdir))
    return zsappend (zdir, zfile);
  else
    return zdir;
}

/* Open a file to send to another system, and return the mode and
   the size.  */

/*ARGSUSED*/
openfile_t
esysdep_open_send (qsys, zfile, fcheck, zuser, pimode, pcbytes, pfgone)
     const struct ssysteminfo *qsys;
     const char *zfile;
     boolean fcheck;
     const char *zuser;
     unsigned int *pimode;
     long *pcbytes;
     boolean *pfgone;
{
  struct stat s;
  openfile_t e;
  int o;
  
  if (pfgone != NULL)
    *pfgone = FALSE;

  if (fsdirectory_exists (zfile))
    {
      ulog (LOG_ERROR, "%s: is a directory", zfile);
      return EFILECLOSED;
    }
#if USE_STDIO
  e = fopen (zfile, BINREAD);
  if (e == NULL)
    {
      ulog (LOG_ERROR, "fopen (%s): %s", zfile, strerror (errno));
      if (pfgone != NULL && errno == ENOENT)
	*pfgone = TRUE;
      return NULL;
    }
  o = fileno (e);
#else
  e = open (zfile, O_RDONLY, 0);
  if (e == -1)
    {
      ulog (LOG_ERROR, "open (%s): %s", zfile, strerror (errno));
      if (pfgone != NULL && errno == ENOENT)
	*pfgone = TRUE;
      return -1;
    }
  o = e;
#endif

  if (fstat (o, &s) == -1)
    {
      ulog (LOG_ERROR, "fstat: %s", strerror (errno));
      s.st_mode = 0666;
    }

  /* We have to recheck the file permission, although we probably
     checked it already, because otherwise there would be a window in
     which somebody could change the contents of a symbolic link to
     point to some file which was only readable by uucp.  */
  if (fcheck)
    {
      if (! fsuser_access (&s, R_OK, zuser))
	{
	  ulog (LOG_ERROR, "%s: %s", zfile, strerror (EACCES));
	  (void) ffileclose (e);
	  return EFILECLOSED;
	}
    }

  *pimode = s.st_mode & 0777;
  *pcbytes = s.st_size;
  return e;
}

/* Get a temporary file name.  */

/*ARGSUSED*/
const char *
zstemp_file (qsys)
     const struct ssysteminfo *qsys;
{
  static int icount;
  char *zret;

#if SPOOLDIR_V2 | SPOOLDIR_BSD42
  {
    static char ab[sizeof "TM.12345.123"];

    sprintf (ab, "TM.%05d.%03d", getpid (), icount);
    zret = ab;
  }
#endif
#if SPOOLDIR_BSD43 | SPOOLDIR_ULTRIX | SPOOLDIR_TAYLOR
  {
    static char ab[sizeof ".Temp/TM.12345.123"];

    sprintf (ab, ".Temp/TM.%05d.%03d", getpid (), icount);
    zret = ab;
  }
#endif
#if SPOOLDIR_BNU
  {
    static char *z;
    static int calc;
    int cneed;

    cneed = strlen (qsys->zname) + sizeof "/TM.12345.123";
    if (cneed > calc)
      {
	xfree ((pointer) z);
	z = (char *) xmalloc (cneed);
	calc = cneed;
      }
    sprintf (z, "%s/TM.%05d.%03d", qsys->zname, getpid (), icount);
    zret = z;
  }
#endif

  ++icount;

  return zret;
}

/* Open a temporary file to receive into.  This should, perhaps, check
   that we have write permission on the receiving directory, but it
   doesn't.  It is supposed to set *pcbytes to the size of the largest
   file that can be accepted.  */

/*ARGSUSED*/
openfile_t
esysdep_open_receive (qsys, zto, pztemp, pcbytes)
     const struct ssysteminfo *qsys;
     const char *zto;
     const char **pztemp;
     long *pcbytes;
{
  const char *z;
  int o;
  openfile_t e;
  long c1, c2;
  char *zcopy, *zslash;

  z = zstemp_file (qsys);

  o = creat (z, IPRIVATE_FILE_MODE);

  if (o == -1)
    {
      if (errno == ENOENT)
	{
	  if (! fsysdep_make_dirs (z, FALSE))
	    return EFILECLOSED;
	  o = creat (z, IPRIVATE_FILE_MODE);
	}
      if (o == -1)
	{
	  ulog (LOG_ERROR, "creat (%s): %s", z, strerror (errno));
	  return EFILECLOSED;
	}
    }

#if USE_STDIO
  e = fdopen (o, (char *) BINWRITE);

  if (e == NULL)
    {
      ulog (LOG_ERROR, "fdopen (%s): %s", z, strerror (errno));
      (void) close (o);
      (void) remove (z);
      return NULL;
    }
#else
  e = o;
#endif

  *pztemp = z;

  /* Try to determine the amount of free space available for the
     temporary file and for the final destination.  This code is
     mostly from David MacKenzie's df program.  */

  c1 = (long) -1;
  c2 = (long) -1;

  zcopy = (char *) alloca (strlen (zto) + 1);
  strcpy (zcopy, zto);
  zslash = strrchr (zcopy, '/');
  if (zslash != NULL)
    *zslash = '\0';
  else
    {
      zcopy[0] = '.';
      zcopy[1] = '\0';
    }

  {
#ifdef FS_STATVFS
    struct statvfs s;

    if (statvfs (z, &s) >= 0)
      c1 = (long) s.f_bavail * (long) s.f_frsize;
    if (statvfs (zcopy, &s) >= 0)
      c2 = (long) s.f_bavail * (long) s.f_frsize;
#endif
#ifdef FS_USG_STATFS
    struct statfs s;

    /* This structure has an f_bsize field, but on many systems
       f_bfree is measured in 512 byte blocks.  On some systems,
       f_bfree is measured in f_bsize byte blocks.  Rather than
       overestimate the amount of free space, this code assumes that
       f_bfree is measuring 512 byte blocks.  */

    if (statfs (z, &s, sizeof s, 0) >= 0)
      c1 = (long) s.f_bfree * (long) 512;
    if (statfs (zcopy, &s, sizeof s, 0) >= 0)
      c2 = (long) s.f_bfree * (long) 512;
#endif
#ifdef FS_MNTENT
    struct statfs s;

    if (statfs (z, &s) == 0)
      c1 = (long) s.f_bavail * (long) s.f_bsize;
    if (statfs (zcopy, &s) == 0)
      c2 = (long) s.f_bavail * (long) s.f_bsize;
#endif
#ifdef FS_GETMNT
    struct fs_data s;

    if (statfs (z, &s) == 1)
      c1 = (long) s.fd_req.bfreen * (long) 1024;
    if (statfs (zcopy, &s) == 1)
      c2 = (long) s.fd_req.bfreen * (long) 1024;
#endif
#ifdef FS_STATFS
    struct statfs s;

    if (statfs (z, &s) >= 0)
      c1 = (long) s.f_bavail * (long) s.f_fsize;
    if (statfs (zcopy, &s) >= 0)
      c2 = (long) s.f_bavail * (long) s.f_fsize;
#endif
#ifdef FS_USTAT
    struct stat sstat;
    struct ustat s;

    if (fstat (o, &sstat) == 0
	&& ustat (sstat.st_dev, &s) == 0)
      c1 = (long) s.f_tfree * (long) 512;
    if (stat (zcopy, &sstat) == 0
	&& ustat (sstat.st_dev, &s) == 0)
      c2 = (long) s.f_tfree * (long) 512;
#endif
  }

  if (c1 == (long) -1)
    *pcbytes = c2;
  else if (c2 == (long) -1)
    *pcbytes = c1;
  else if (c1 < c2)
    *pcbytes = c1;
  else
    *pcbytes = c2;

  return e;
}

/* After the temporary file has been completely written out, the file
   is closed and this routine is called to move it into its final
   location.  If we fail, we must remove the temporary file.  */

boolean
fsysdep_move_file (zorig, zto, imode, fcheck, zuser)
     const char *zorig;
     const char *zto;
     unsigned int imode;
     boolean fcheck;
     const char *zuser;
{
  DEBUG_MESSAGE2 (DEBUG_SPOOLDIR,
		  "fsysdep_move_file: Moving %s to %s", zorig, zto);

  /* Unless and until we add an option to change the ownership of the
     file, the only information we want from the mode is whether the
     file is executable or not.  It would be dumb to create a file
     with mode 0600, for example, since the owner will be uucp and the
     recipient will not be able to read it.  If we do not have an
     absolute path to the file, which means that it is being moved
     somewhere in the spool directory, we don't change the mode; in
     general, the files in the spool directory should not be
     publically readable.  */

  if (*zto != '/')
    imode = 0;

  if (imode != 0)
    {
      if ((imode & 0111) != 0)
	imode = S_IRWXU | S_IRWXG | S_IRWXO;
      else
	imode = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;
    }

  /* Optionally make sure that zuser has write access on the
     directory.  We only check files that are not in the spool
     directory.  */
  if (fcheck && *zto == '/')
    {
      char *zcopy;
      char *zslash;
      struct stat s;

      zcopy = (char *) alloca (strlen (zto) + 1);
      strcpy (zcopy, zto);
      zslash = strrchr (zcopy, '/');
      if (zslash == zcopy)
	zslash[1] = '\0';
      else
	*zslash = '\0';

      if (stat (zcopy, &s) != 0)
	{
	  ulog (LOG_ERROR, "stat (%s): %s", zcopy, strerror (errno));
	  (void) remove (zorig);
	  return FALSE;
	}
      if (! fsuser_access (&s, W_OK, zuser))
	{
	  ulog (LOG_ERROR, "%s: %s", zcopy, strerror (EACCES));
	  (void) remove (zorig);
	  return FALSE;
	}

      /* A malicious user now has a few milliseconds to change a
	 symbolic link to a directory uucp has write permission on but
	 the user does not (the obvious choice being /usr/lib/uucp).
	 The only certain method I can come up with to close this race
	 is to fork an suid process which takes on the users identity
	 and does the actual copy.  This is sufficiently high overhead
	 that I'm not going to do it.  */
    }

  /* We try to use rename to move the file.  */

  if (rename (zorig, zto) == 0)
    {
      /* We must set the correct file mode, but don't worry if it doesn't
	 work.  There should be an option for setting the owner, as
	 well.  */
      if (imode != 0)
	(void) chmod (zto, imode);
      return TRUE;
    }

  /* If this file is in the spool directory, make sure all directories
     exist.  */
  if (*zto != '/' && errno == ENOENT)
    {
      if (! fsysdep_make_dirs (zto, FALSE))
	{
	  (void) remove (zorig);
	  return FALSE;
	}
      if (rename (zorig, zto) == 0)
	{
	  if (imode != 0)
	    (void) chmod (zto, imode);
	  return TRUE;
	}
    }

  /* If we can't link across devices, we must copy the file by hand.  */
  if (errno != EXDEV)
    {
      ulog (LOG_ERROR, "rename (%s, %s): %s", zorig, zto,
	    strerror (errno));
      (void) remove (zorig);
      return FALSE;
    }

  /* If the destination file is not in the spool directory, any
     necessary directories should already have been made.  */
  if (! fcopy_file (zorig, zto, FALSE, *zto != '/'))
    {
      (void) remove (zorig);
      return FALSE;
    }

  if (remove (zorig) < 0)
    ulog (LOG_ERROR, "remove (%s): %s", zorig, strerror (errno));

  if (imode != 0)
    (void) chmod (zto, imode);

  return TRUE;
}

/* Truncate a file to zero length.  If this fails, it closes and
   removes the file.  We support a number of different means of
   truncation, which is probably a waste of time since this function
   is currently only called when the 'f' protocol resends a file.  */

#if HAVE_FTRUNCATE
#undef HAVE_LTRUNC
#define HAVE_LTRUNC 0
#endif

#if ! HAVE_FTRUNCATE && ! HAVE_LTRUNC
#ifdef F_CHSIZE
#define HAVE_F_CHSIZE 1
#else /* ! defined (F_CHSIZE) */
#ifdef F_FREESP
#define HAVE_F_FREESP 1
#endif /* defined (F_FREESP) */
#endif /* ! defined (F_CHSIZE) */
#endif /* ! HAVE_FTRUNCATE && ! HAVE_LTRUNC */

openfile_t
esysdep_truncate (e, zname)
     openfile_t e;
     const char *zname;
{
  int o;

#if HAVE_FTRUNCATE || HAVE_LTRUNC || HAVE_F_CHSIZE || HAVE_F_FREESP
  int itrunc;

  if (! ffilerewind (e))
    {
      ulog (LOG_ERROR, "rewind: %s", strerror (errno));
      (void) ffileclose (e);
      (void) remove (zname);
      return EFILECLOSED;
    }

#if USE_STDIO
  o = fileno (e);
#else
  o = e;
#endif

#if HAVE_FTRUNCATE
  itrunc = ftruncate (o, 0);
#endif
#if HAVE_LTRUNC
  itrunc = ltrunc (o, (long) 0, SEEK_SET);
#endif
#if HAVE_F_CHSIZE
  itrunc = fcntl (o, F_CHSIZE, (off_t) 0);
#endif
#if HAVE_F_FREESP
  /* This selection is based on an implementation of ftruncate by
     kucharsk@Solbourne.com (William Kucharski).  */
  {
    struct flock fl;

    fl.l_whence = 0;
    fl.l_len = 0;
    fl.l_start = 0;
    fl.l_type = F_WRLCK;

    itrunc = fcntl (o, F_FREESP, &fl);
  }
#endif

  if (itrunc != 0)
    {
#if HAVE_FTRUNCATE
      ulog (LOG_ERROR, "ftruncate: %s", strerror (errno));
#endif
#ifdef HAVE_LTRUNC
      ulog (LOG_ERROR, "ltrunc: %s", strerror (errno));
#endif
#ifdef HAVE_F_CHSIZE
      ulog (LOG_ERROR, "fcntl (F_CHSIZE): %s", strerror (errno));
#endif
#ifdef HAVE_F_FREESP
      ulog (LOG_ERROR, "fcntl (F_FREESP): %s", strerror (errno));
#endif

      (void) ffileclose (e);
      (void) remove (zname);
      return EFILECLOSED;
    }

  return e;
#else /* ! (HAVE_FTRUNCATE || HAVE_LTRUNC) */
  (void) ffileclose (e);
  (void) remove (zname);

  o = creat (zname, IPRIVATE_FILE_MODE);

  if (o == -1)
    {
      ulog (LOG_ERROR, "open (%s): %s", zname, strerror (errno));
      return EFILECLOSED;
    }

#if USE_STDIO
  e = fdopen (o, (char *) BINWRITE);

  if (e == NULL)
    {
      ulog (LOG_ERROR, "fdopen (%s): %s", zname, strerror (errno));
      (void) close (o);
      (void) remove (zname);
      return NULL;
    }
#else /* ! USE_STDIO */
  e = o;
#endif /* ! USE_STDIO */

  return e;
#endif /* ! HAVE_FTRUNCATE */
}

/* Lock something.  If the fspooldir argument is TRUE, the argument is
   a file name relative to the spool directory; otherwise the argument
   is a simple file name which should be created in the system lock
   directory (under BNU this is /etc/locks).  */

/*ARGSUSED*/
boolean
fsdo_lock (zlock, fspooldir)
     const char *zlock;
     boolean fspooldir;
{
  const char *zpath, *zslash;
  int cslash;
  char *ztempfile;
  char abtempfile[20];
  int o;
  pid_t ime;
#if HAVE_V2_LOCKFILES
  int i;
#else
  char ab[12];
#endif
  int cwrote;
  const char *zerr;
  boolean fret;

#ifdef LOCKDIR
  if (fspooldir)
    zpath = zlock;
  else
    {
      char *zalc;

      zalc = (char *) alloca (sizeof LOCKDIR + strlen (zlock) + 1);
      sprintf (zalc, "%s/%s", LOCKDIR, zlock);
      zpath = zalc;
    }
#else /* ! defined (LOCKDIR) */
  zpath = zlock;
#endif

  ime = getpid ();

  /* We do the actual lock by creating a file and then linking it to
     the final file name we want.  This avoids race conditions due to
     one process checking the file before we have finished writing it,
     and also works even if we are somehow running as root.

     First, create the file in the right directory (we must create the
     file in the same directory since otherwise we might attempt a
     cross-device link).  */
  zslash = strrchr (zpath, '/');
  if (zslash == NULL)
    cslash = 0;
  else
    cslash = zslash - zpath + 1;

  ztempfile = (char *) alloca (cslash + sizeof "TMP1234567890");
  strncpy (ztempfile, zpath, cslash);
  sprintf (abtempfile, "TMP%010d", (int) ime);
  ztempfile[cslash] = '\0';
  strcat (ztempfile, abtempfile);

  o = creat (ztempfile, IPUBLIC_FILE_MODE);
  if (o < 0)
    {
      if (errno == ENOENT)
	{
	  if (! fsysdep_make_dirs (ztempfile, FALSE))
	    return FALSE;
	  o = creat (ztempfile, IPUBLIC_FILE_MODE);
	}
      if (o < 0)
	{
	  ulog (LOG_ERROR, "open (%s): %s", ztempfile, strerror (errno));
	  return FALSE;
	}
    }

#if HAVE_V2_LOCKFILES
  i = ime;
  cwrote = write (o, &i, sizeof i);
#else
  sprintf (ab, "%10d\n", (int) ime);
  cwrote = write (o, ab, strlen (ab));
#endif

  zerr = NULL;
  if (cwrote < 0)
    zerr = "write";
  if (close (o) < 0)
    zerr = "close";
  if (zerr != NULL)
    {
      ulog (LOG_ERROR, "%s (%s): %s", zerr, ztempfile, strerror (errno));
      (void) remove (ztempfile);
      return FALSE;
    }

  /* Now try to link the file we just created to the lock file that we
     want.  If it fails, try reading the existing file to make sure
     the process that created it still exists.  We do this in a loop
     to make it easy to retry if the old locking process no longer
     exists.  */

  fret = TRUE;
  o = -1;
  zerr = NULL;

  while (link (ztempfile, zpath) != 0)
    {
      int cgot;
      int ipid;

      fret = FALSE;

      if (errno != EEXIST)
	{
	  ulog (LOG_ERROR, "link (%s, %s): %s", ztempfile, zpath,
		strerror (errno));
	  break;
	}

      o = open (zpath, O_RDWR, 0);
      if (o < 0)
	{
	  zerr = "open";
	  break;
	}

      /* The race starts here.  See below for a discussion.  */

#if HAVE_V2_LOCKFILES
      cgot = read (o, &i, sizeof i);
#else
      cgot = read (o, ab, sizeof ab - 1);
#endif

      if (cgot < 0)
	{
	  zerr = "read";
	  break;
	}

#if HAVE_V2_LOCKFILES
      ipid = i;
#else
      ab[cgot] = '\0';
      ipid = atoi (ab);
#endif

      /* If the process still exists, we will get EPERM rather than
	 ESRCH.  We then return FALSE to indicate that we cannot make
	 the lock.  */
      if (kill (ipid, 0) == 0 || errno == EPERM)
	break;

      /* On NFS, the link might have actually succeeded even though we
	 got a failure return.  This can happen if the original
	 acknowledgement was lost or delayed and the operation was
	 retried.  In this case the pid will be our own.  This
	 introduces a rather improbable race condition: if a stale
	 lock was left with our process ID in it, and another process
	 just did the above kill but has not yet changed the lock file
	 to hold its own process ID, we could start up and make it all
	 the way to here and think we have the lock.  I'm not going to
	 worry about this possibility.  */
      if (ipid == ime)
	{
	  fret = TRUE;
	  break;
	}

      ulog (LOG_ERROR, "Found stale lock %s held by process %d",
	    zpath, ipid);

      /* This is a stale lock, created by a process that no longer
	 exists.

	 Now we could remove the file, but that would be a race
	 condition.  If we were interrupted any time after we did the
	 read until we did the remove, another process could get in,
	 open the file, find that it was a stale lock, remove the file
	 and create a new one.  When we woke up we would remove the
	 file the other process just created.

	 These files are being generated partially for the benefit of
	 cu, and it would be nice to avoid the race however cu avoids
	 it, so that the programs remain compatible.  Unfortunately,
	 nobody seems to know how cu avoids the race, or even if it
	 tries to avoid it at all.

	 There are a few ways to avoid the race.  We could use kernel
	 locking primitives, but they may not be available.  We could
	 link to a special file name, but if that file were left lying
	 around then no stale lock could ever be broken (Henry Spencer
	 would think this was a good thing).

	 Instead I've implemented the following procedure: seek to the
	 start of the file, write our pid into it, sleep for five
	 seconds, and then make sure our pid is still there.  Anybody
	 who checks the file while we're asleep will find our pid
	 there and fail the lock.  The only race will come from
	 another process which has done the read by the time we do our
	 write.  That process will then have five seconds to do its
	 own write.  When we wake up, we'll notice that our pid is no
	 longer in the file, and retry the lock from the beginning.

	 This relies on the atomicity of write(2).  If it possible for
	 the writes of two processes to be interleaved, the two
	 processes could livelock.  POSIX unfortunately leaves this
	 case explicitly undefined; however, given that the write is
	 of less than a disk block, it's difficult to imagine an
	 interleave occurring.

	 Note that this is still a race.  If it takes the second
	 process more than five seconds to do the kill, the lseek, and
	 the write, both processes will think they have the lock.
	 Perhaps the length of time to sleep should be configurable.
	 Even better, perhaps I should add a configuration option to
	 use a permanent lock file, which eliminates any race and
	 forces the installer to be aware of the existence of the
	 permanent lock file.

	 For the benefit of cu, we stat the file after the sleep, to
	 make sure some cu program hasn't deleted it for us.  */

      if (lseek (o, (off_t) 0, SEEK_SET) != 0)
	{
	  zerr = "lseek";
	  break;
	}

#if HAVE_V2_LOCKFILES
      i = ime;
      cwrote = write (o, &i, sizeof i);
#else
      sprintf (ab, "%10d\n", (int) ime);
      cwrote = write (o, ab, strlen (ab));
#endif

      if (cwrote < 0)
	{
	  zerr = "write";
	  break;
	}

      (void) sleep (5);

      if (lseek (o, (off_t) 0, SEEK_SET) != 0)
	{
	  zerr = "lseek";
	  break;
	}

#if HAVE_V2_LOCKFILES
      cgot = read (o, &i, sizeof i);
#else
      cgot = read (o, ab, sizeof ab - 1);
#endif

      if (cgot < 0)
	{
	  zerr = "read";
	  break;
	}

#if HAVE_V2_LOCKFILES
      ipid = i;
#else
      ab[cgot] = '\0';
      ipid = atoi (ab);
#endif

      if (ipid == ime)
	{
	  struct stat sfile, sdescriptor;

	  /* It looks like we have the lock.  Do the final stat
	     check.  */

	  if (stat (zpath, &sfile) != 0)
	    {
	      if (errno != ENOENT)
		{
		  zerr = "stat";
		  break;
		}
	      /* Loop around and try again.  */
	    }
	  else
	    {
	      if (fstat (o, &sdescriptor) < 0)
		{
		  zerr = "fstat";
		  break;
		}

	      if (sfile.st_ino == sdescriptor.st_ino
		  && sfile.st_dev == sdescriptor.st_dev)
		{
		  /* Close the file before assuming we've succeeded to
		     pick up any trailing errors.  */
		  if (close (o) < 0)
		    {
		      zerr = "close";
		      break;
		    }

		  o = -1;

		  /* We have the lock.  */
		  fret = TRUE;
		  break;
		}
	    }
	}

      /* Loop around and try the lock again.  We keep doing this until
	 the lock file holds a pid that exists.  */

      (void) close (o);
      o = -1;
      fret = TRUE;
    }

  if (zerr != NULL)
    ulog (LOG_ERROR, "%s (%s): %s", zerr, zpath, strerror (errno));

  if (o >= 0)
    (void) close (o);

  /* It would be nice if we could leave the temporary file around for
     future calls, but considering that we create lock files in
     various different directories it's probably more trouble than
     it's worth.  */
  if (remove (ztempfile) != 0)
    ulog (LOG_ERROR, "remove (%s): %s", ztempfile, strerror (errno));

  return fret;
}

/* Unlock something.  The fspooldir argument is as in fsdo_lock.  */

boolean
fsdo_unlock (zlock, fspooldir)
     const char *zlock;
     boolean fspooldir;
{
  const char *zpath;

#ifdef LOCKDIR
  if (fspooldir)
    zpath = zlock;
  else
    {
      char *zalc;

      zalc = (char *) alloca (sizeof LOCKDIR + strlen (zlock) + 1);
      sprintf (zalc, "%s/%s", LOCKDIR, zlock);
      zpath = zalc;
    }
#else /* ! defined (LOCKDIR) */
  zpath = zlock;
#endif

  if (remove (zpath) == 0
      || errno == ENOENT)
    return TRUE;
  else
    {
      ulog (LOG_ERROR, "remove (%s): %s", zpath, strerror (errno));
      return FALSE;
    }
}

/* Lock a remote system.  */

/*ARGSUSED*/
boolean
fsysdep_lock_system (qsys)
     const struct ssysteminfo *qsys;
{
  char *z;

  z = (char *) alloca (strlen (qsys->zname) + sizeof "LCK..");
  sprintf (z, "LCK..%.8s", qsys->zname);
  return fsdo_lock (z, FALSE);
}

/* Unlock a remote system.  */

/*ARGSUSED*/
boolean
fsysdep_unlock_system (qsys)
     const struct ssysteminfo *qsys;
{
  char *z;

  z = (char *) alloca (strlen (qsys->zname) + sizeof "LCK..");
  sprintf (z, "LCK..%.8s", qsys->zname);
  return fsdo_unlock (z, FALSE);
}

/* Get a new command sequence number (this is not a sequence number to
   be used for communicating with another system, but a sequence
   number to be used when generating the name of a command file).
   The sequence number is placed into zseq, which should be five
   characters long.  */

#define CSEQLEN (4)

static boolean
fscmd_seq (zsystem, zseq)
     const char *zsystem;
     char *zseq;
{
  int ctries;
  const char *zfile;
  int o;
  int i;

  /* Lock the sequence file.  This may not be correct for all systems,
     but it only matters if the system UUCP and this UUCP are running
     at the same time.  */

  ctries = 0;
  while (! fsdo_lock ("LCK..SEQ", TRUE))
    {
      ++ctries;
      if (ctries > 10)
	{
	  ulog (LOG_ERROR, "Can't lock sequence file");
	  return FALSE;
	}
      sleep (1);
    }

#if SPOOLDIR_V2 | SPOOLDIR_BSD42 | SPOOLDIR_BSD43
  zfile = "SEQF";
#endif
#if SPOOLDIR_BNU
  {
    char *zalc;

    zalc = (char *) alloca (strlen (zsystem) + sizeof ".Sequence/");
    sprintf (zalc, ".Sequence/%s", zsystem);
    zfile = zalc;
  }
#endif
#if SPOOLDIR_ULTRIX
  if (fsultrix_has_spool (zsystem))
    {
      char *zalc;

      zalc = (char *) alloca (strlen (zsystem) + sizeof "sys//.SEQF");
      sprintf (zalc, "sys/%s/.SEQF", zsystem);
      zfile = zalc;
    }
  else
    zfile = "sys/DEFAULT/.SEQF";
#endif /* SPOOLDIR_ULTRIX */
#if SPOOLDIR_TAYLOR
  {
    char *zalc;

    zalc = (char *) alloca (strlen (zsystem) + sizeof "/SEQF");
    sprintf (zalc, "%s/SEQF", zsystem);
    zfile = zalc;
  }
#endif /* SPOOLDIR_TAYLOR */

#ifdef O_CREAT
  o = open (zfile, O_RDWR | O_CREAT, IPUBLIC_FILE_MODE);
#else
  o = open (zfile, O_RDWR);
  if (o < 0 && errno == ENOENT)
    {
      o = creat (zfile, IPUBLIC_FILE_MODE);
      if (o >= 0)
	{
	  (void) close (o);
	  o = open (zfile, O_RDWR);
	}
    }
#endif

  if (o < 0)
    {
      if (errno == ENOENT)
	{
	  if (! fsysdep_make_dirs (zfile, FALSE))
	    {
	      (void) fsdo_unlock ("LCK..SEQ", TRUE);
	      return FALSE;
	    }
#ifdef O_CREAT
	  o = open (zfile, O_RDWR | O_CREAT, IPUBLIC_FILE_MODE);
#else
	  o = creat (zfile, IPUBLIC_FILE_MODE);
	  if (o >= 0)
	    {
	      (void) close (o);
	      o = open (zfile, O_RDWR);
	    }
#endif
	}
      if (o < 0)
	{
	  ulog (LOG_ERROR, "open (%s): %s", zfile, strerror (errno));
	  (void) fsdo_unlock ("LCK..SEQ", TRUE);
	  return FALSE;
	}
    }

  if (read (o, zseq, CSEQLEN) != CSEQLEN)
    strcpy (zseq, "0000");
  zseq[CSEQLEN] = '\0';

  /* We must add one to the sequence number and return the new value.
     On Ultrix, arbitrary characters are allowed in the sequence number.
     On other systems, the sequence number apparently must be in hex.  */

#if SPOOLDIR_V2 | SPOOLDIR_BSD42 | SPOOLDIR_BSD43 | SPOOLDIR_BNU
  i = strtol (zseq, (char **) NULL, 16);
  ++i;
  if (i > 0xffff)
    i = 0;
  /* The sprintf argument has CSEQLEN built into it.  */
  sprintf (zseq, "%04x", (unsigned int) i);
#endif /* SPOOLDIR_V2 | SPOOLDIR_BSD42 | SPOOLDIR_BSD43 | SPOOLDIR_BNU */
#if SPOOLDIR_ULTRIX | SPOOLDIR_TAYLOR
  for (i = CSEQLEN - 1; i >= 0; i--)
    {
      if (zseq[i] == 'z')
	{
	  zseq[i] = '0';
	  continue;
	}

      if (zseq[i] == '9')
	zseq[i] = 'A';
      else if (zseq[i] == 'Z')
	zseq[i] = 'a';
      else if ((zseq[i] >= '0' && zseq[i] < '9')
	       || (zseq[i] >= 'A' && zseq[i] < 'Z')
	       || (zseq[i] >= 'a' && zseq[i] < 'z'))
	++zseq[i];
      else
	{
	  /* A bad character was found; reset the entire sequence
	     number.  */
	  ulog (LOG_ERROR,
		"Bad sequence number %s for system %s",
		zseq, zsystem);
	  strcpy (zseq, "0000");
	}

      break;
    }
#endif /* SPOOLDIR_ULTRIX | SPOOLDIR_TAYLOR */

  if (lseek (o, (off_t) 0, SEEK_SET) < 0
      || write (o, zseq, CSEQLEN) != CSEQLEN
      || close (o) < 0)
    {
      ulog (LOG_ERROR, "lseek or write or close: %s", strerror (errno));
      (void) close (o);
      (void) fsdo_unlock ("LCK..SEQ", TRUE);
      return FALSE;
    }

  (void) fsdo_unlock ("LCK..SEQ", TRUE);

  return TRUE;
}

/* Get the name of a command or data file for a remote system.  The
   btype argument should be C for a command file or D for a data file.
   If the grade of a data file is X, it is assumed that this is going
   to become an execute file on some other system.  The zsystem
   argument is the system that the file will be transferred to.  The
   ztname argument will be set to a file name that could be passed to
   zsysdep_spool_file_name.  The zdname argument, if not NULL, will be
   set to a data file name appropriate for the remote system.  The
   zxname argument, if not NULL, will be set to the name of an execute
   file on the remote system.  None of the names will be more than 14
   characters long.  */

static const char *
zsfile_name (btype, zsystem, bgrade, ztname, zdname, zxname)
     int btype;
     const char *zsystem;
     int bgrade;
     char *ztname;
     char *zdname;
     char *zxname;
{
  char abseq[CSEQLEN + 1];
  char absimple[11 + CSEQLEN];
  const char *zname;
  struct stat s;

  do
    {
      if (! fscmd_seq (zsystem, abseq))
	return NULL;

      if (btype == 'C')
	{
#if ! SPOOLDIR_TAYLOR
	  sprintf (absimple, "C.%.7s%c%s", zsystem, bgrade, abseq);
#else
	  sprintf (absimple, "C.%c%s", bgrade, abseq);
#endif
	}
      else if (btype == 'D')
	{
#if ! SPOOLDIR_TAYLOR
	  /* Note that a data file uses the local system's name.  */
	  sprintf (absimple, "D.%.7s%c%s", zLocalname, bgrade, abseq);
#else
	  if (bgrade == 'X')
	    sprintf (absimple, "D.X%s", abseq);
	  else
	    sprintf (absimple, "D.%s", abseq);
#endif
	}
#if DEBUG > 0
      else
	ulog (LOG_FATAL, "zsfile_name: Can't happen (%d)", btype);
#endif

      zname = zsfind_file (absimple, zsystem);
      if (zname == NULL)
	return NULL;
    }
  while (stat (zname, &s) == 0);

  if (ztname != NULL)
    strcpy (ztname, absimple);

  if (zdname != NULL)
    sprintf (zdname, "D.%.7s%c%s", zLocalname, bgrade, abseq);

  if (zxname != NULL)
    sprintf (zxname, "X.%.7s%c%s", zLocalname, bgrade, abseq);

  return zname;
}

/* Given a set of commands to execute for a remote system, create a
   command file holding them.  This creates a single command file
   holding all the commands passed in.  It returns a jobid.  */

const char *
zsysdep_spool_commands (qsys, bgrade, ccmds, pascmds)
     const struct ssysteminfo *qsys;
     int bgrade;
     int ccmds;
     const struct scmd *pascmds;
{
  const char *z;
  FILE *e;
  int i;
  const struct scmd *q;
  const char *zjobid;

#if DEBUG > 0
  if (! FGRADE_LEGAL (bgrade))
    ulog (LOG_FATAL, "Bad grade %d", bgrade);
#endif

  z = zsfile_name ('C', qsys->zname, bgrade, (char *) NULL, (char *) NULL,
		   (char *) NULL);
  if (z == NULL)
    return NULL;

  e = esysdep_fopen (z, FALSE, FALSE, TRUE);
  if (e == NULL)
    return NULL;

  for (i = 0, q = pascmds; i < ccmds; i++, q++)
    {
      switch (q->bcmd)
	{
	case 'S':
	  fprintf (e, "S %s %s %s -%s %s 0%o %s\n", q->zfrom, q->zto,
		   q->zuser, q->zoptions, q->ztemp, q->imode,
		   q->znotify);
	  break;
	case 'R':
	  fprintf (e, "R %s %s %s -%s\n", q->zfrom, q->zto, q->zuser,
		   q->zoptions);
	  break;
	case 'X':
	  fprintf (e, "X %s %s %s -%s\n", q->zfrom, q->zto, q->zuser,
		   q->zoptions);
	  break;
	default:
	  ulog (LOG_ERROR,
		"zsysdep_spool_commands: Unrecognized type %d",
		q->bcmd);
	  (void) fclose (e);
	  (void) remove (z);
	  return NULL;
	}
    }

  if (fclose (e) != 0)
    {
      ulog (LOG_ERROR, "fclose: %s", strerror (errno));
      (void) remove (z);
      return NULL;
    }

  zjobid = zsfile_to_jobid (qsys, z);
  if (zjobid == NULL)
    (void) remove (z);
  return zjobid;
}

/* Return a name to use for a data file to be copied to another
   system.  The name returned will be for a real file.  The ztname
   argument, if not NULL, will be set to a name that could be passed
   to zsysdep_spool_file_name to get back the return value of this
   function.  The zdname argument, if not NULL, will be set to a name
   that the file could be given on another system.  The zxname
   argument, if not NULL, will be set to a name for an execute file on
   another system.  */

const char *
zsysdep_data_file_name (qsys, bgrade, ztname, zdname, zxname)
     const struct ssysteminfo *qsys;
     int bgrade;
     char *ztname;
     char *zdname;
     char *zxname;
{
  return zsfile_name ('D', qsys->zname, bgrade, ztname, zdname, zxname);
}

/* Return a name for an execute file to be created locally.  This is
   used by uux to execute a command locally with remote files.  */

const char *
zsysdep_xqt_file_name ()
{
  char abseq[CSEQLEN + 1];
  char absx[11 + CSEQLEN];
  const char *zname;
  struct stat s;

  while (TRUE)
    {
      if (! fscmd_seq (zLocalname, abseq))
	return NULL;

      sprintf (absx, "X.%.7sX%s", zLocalname, abseq);

      zname = zsfind_file (absx, zLocalname);
      if (zname == NULL)
	return NULL;
      if (stat (zname, &s) != 0)
	break;
    }

  return zname;
}

/* Start getting a wildcarded file spec.  We use the shell to expand
   the wildcard.  */

static char *zSwildcard_alloc;
static char *zSwildcard;

boolean
fsysdep_wildcard_start (qsys, zfile)
     const struct ssysteminfo *qsys;
     const char *zfile;
{
  char *zcmd;
  const char *azargs[4];
  FILE *e;
  pid_t ipid;

  zSwildcard_alloc = NULL;
  zSwildcard = NULL;

  if (*zfile == '~')
    {
      zfile = zstilde_expand (qsys, zfile);
      if (zfile == NULL)
	return FALSE;
    }

  if (*zfile != '/')
    {
      ulog (LOG_ERROR, "Relative path not permitted in wildcard");
      return FALSE;
    }

  zcmd = (char *) alloca (sizeof ECHO_PROGRAM + sizeof " " + strlen (zfile));
  sprintf (zcmd, "%s %s", ECHO_PROGRAM, zfile);

  azargs[0] = "/bin/sh";
  azargs[1] = "-c";
  azargs[2] = zcmd;
  azargs[3] = NULL;

  e = espopen (azargs, TRUE, &ipid);
  if (e == NULL)
    {
      ulog (LOG_ERROR, "espopen: %s", strerror (errno));
      return FALSE;
    }

  zSwildcard_alloc = zfgets (e, FALSE);

  if (iswait ((unsigned long) ipid, ECHO_PROGRAM) != 0)
    {
      xfree ((pointer) zSwildcard_alloc);
      return FALSE;
    }

  if (zSwildcard_alloc == NULL)
    return FALSE;

  DEBUG_MESSAGE1 (DEBUG_EXECUTE,
		  "fsysdep_wildcard_start: got \"%s\"",
		  zSwildcard_alloc);

  zSwildcard = zSwildcard_alloc;

  return TRUE;
}

/* Get the next wildcard spec.  */

/*ARGSUSED*/
const char *
zsysdep_wildcard (qsys, zfile)
     const struct ssysteminfo *qsys;
     const char *zfile;
{
  char *zret;

  if (zSwildcard_alloc == NULL || zSwildcard == NULL)
    return NULL;

  zret = zSwildcard;

  while (*zSwildcard != '\0' && ! isspace (BUCHAR (*zSwildcard)))
    ++zSwildcard;

  if (*zSwildcard == '\0')
    zSwildcard = NULL;
  else
    {
      *zSwildcard = '\0';
      ++zSwildcard;
      while (*zSwildcard != '\0' && isspace (BUCHAR (*zSwildcard)))
	++zSwildcard;
      if (*zSwildcard == '\0')
	zSwildcard = NULL;
    }

  return zret;
}

/* Finish up getting wildcard specs.  */

boolean
fsysdep_wildcard_end ()
{
  xfree ((pointer) zSwildcard_alloc);
  zSwildcard_alloc = NULL;
  zSwildcard = NULL;
  return TRUE;
}

/* Get the current conversation sequence number for a remote system,
   and increment it for next time.  The conversation sequence number
   is kept in a file named .SQ in the spool directory for that system.
   This is not compatible with other versions of UUCP, but it makes
   more sense to me.  The sequence file is only used if specified in
   the information for that system.  In V2, the file
   /usr/lib/uucp/SQFILE is searched for each system to get a
   conversation sequence number.  */

long
isysdep_get_sequence (qsys)
     const struct ssysteminfo *qsys;
{
  FILE *e;
  const char *zname;
  struct stat s;
  long iseq;

  /* This will only be called when the system is locked anyhow, so there
     is no need to use a separate lock for the conversation sequence
     file.  */

  zname = zsappend (".Sequence", qsys->zname);

  iseq = 0;
  if (stat (zname, &s) == 0)
    {
      boolean fok;
      char *zline;

      /* The file should only be readable and writable by uucp.  */

      if ((s.st_mode & (S_IRWXG | S_IRWXO)) != 0)
	{
	  ulog (LOG_ERROR,
		"Bad file protection for conversation sequence file");
	  return -1;
	}
    
      /* The file already exists, so we don't have to worry about
	 its protection.  */
      e = fopen (zname, "r+");
      if (e == NULL)
	{
	  ulog (LOG_ERROR, "fopen (%s): %s", zname, strerror (errno));
	  return -1;
	}

      zline = zfgets (e, FALSE);

      fok = TRUE;
      if (zline == NULL)
	fok = FALSE;
      else
	{
	  char *zend;

	  iseq = strtol (zline, &zend, 10);
	  if (zend == zline)
	    fok = FALSE;
	  xfree ((pointer) zline);
	}

      if (! fok)
	{
	  ulog (LOG_ERROR, "Bad format for conversation sequence file");
	  return -1;
	}

      rewind (e);
    }
  else
    {
      e = esysdep_fopen (zname, FALSE, FALSE, TRUE);
      if (e == NULL)
	return -1;
    }

  ++iseq;

  fprintf (e, "%ld", iseq);

  if (fclose (e) != 0)
    {
      ulog (LOG_ERROR, "fclose: %s", strerror (errno));
      return -1;
    }

  return iseq;
}

/* Get the Unix file mode of a file.  */

unsigned int
isysdep_file_mode (zfile)
     const char *zfile;
{
  struct stat s;

  if (stat (zfile, &s) != 0)
    {
      ulog (LOG_ERROR, "stat (%s): %s", zfile, strerror (errno));
      return 0;
    }

#if S_IRWXU != 0700
 #error Files modes need to be translated
#endif

  /* We can't return 0, since that indicate an error.  */
  if ((s.st_mode & 0777) == 0)
    return 0400;

  return s.st_mode & 0777;
}

/* Translate a file name and an associated system into a job id.
   These job ids are used by uustat.  We use the system name attached
   to the grade and sequence number.  */

const char *
zsfile_to_jobid (qsys, zfile)
     const struct ssysteminfo *qsys;
     const char *zfile;
{
  char *zid;

  zid = (char *) alloca (strlen (qsys->zname) + CSEQLEN + 2);
  sprintf (zid, "%s%s", qsys->zname,
	   zfile + strlen (zfile) - CSEQLEN - 1);

  return zscopy (zid);
}

/* Turn a job id back into a file name.  */

const char *
zsjobid_to_file (zid, pzsystem)
     const char *zid;
     const char **pzsystem;
{
  int clen;
  char abend[CSEQLEN + 2];
  static char *zsys;
  static int csyslen;
  char abname[CSEQLEN + 11];

  clen = strlen (zid);
  strcpy (abend, zid + clen - CSEQLEN - 1);

  if (clen - CSEQLEN > csyslen)
    {
      zsys = (char *) xrealloc ((pointer) zsys, clen - CSEQLEN);
      csyslen = clen - CSEQLEN;
    }

  strncpy (zsys, zid, clen - CSEQLEN - 1);
  zsys[clen - CSEQLEN - 1] = '\0';
  if (pzsystem != NULL)
    *pzsystem = zsys;

  /* This must correspond to zsfile_name.  */
#if ! SPOOLDIR_TAYLOR
  sprintf (abname, "C.%.7s%s", zsys, abend);
#else
  sprintf (abname, "C.%s", abend);
#endif

  return zsfind_file (abname, zsys);
}

#if ! HAVE_RENAME

/* This is currently the only file which calls rename, so I've put my
   fake rename function in here.  */

static int
rename (zfrom, zto)
     const char *zfrom;
     const char *zto;
{
  /* Try to make the link without removing the old file first.  */
  if (link (zfrom, zto) == -1)
    {
      if (errno != EEXIST)
	return -1;

      /* Assume that this will never be called with zfrom the same as
	 zto.  If it could be, this is wrong.  */
      (void) remove (zto);

      if (link (zfrom, zto) == -1)
	return -1;
    }

  return remove (zfrom);
}

#endif /* ! HAVE_RENAME */

/*
  Local variables:
  mode:c
  End:
  */
