/* config.c
   Read the configuration file.

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

   $Log: config.c,v $
   Revision 1.29  1992/03/28  22:06:38  ian
   Michael I Bushnell: renamed enum tstatus to avoid header file conflict

   Revision 1.28  1992/03/28  21:47:55  ian
   David J. MacKenzie: allow backslash to quote newline in config files

   Revision 1.27  1992/03/28  04:26:12  ian
   David J. MacKenzie: cMaxuuxqts is independent of HAVE_TAYLOR_CONFIG

   Revision 1.26  1992/03/18  23:12:37  ian
   Handle CMDTABTYPE_FULLSTRING correctly if there are no arguments

   Revision 1.25  1992/03/17  01:03:03  ian
   Miscellaneous cleanup

   Revision 1.24  1992/03/12  19:54:43  ian
   Debugging based on types rather than number

   Revision 1.23  1992/03/11  22:06:37  ian
   Marty Shannon: added max-uuxqts command

   Revision 1.22  1992/03/09  05:08:16  ian
   Added status for wrong time to call, not used if system can't be called

   Revision 1.21  1992/03/04  21:39:04  ian
   Local variables in igradecmp have to be integers

   Revision 1.20  1992/03/03  06:06:48  ian
   T. William Wells: don't complain about missing configuration files

   Revision 1.19  1992/02/29  01:06:59  ian
   Chip Salzenberg: recheck file permissions before sending

   Revision 1.18  1992/02/24  04:58:47  ian
   Only permit files to be received into directories that are world-writeable

   Revision 1.17  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.16  1992/01/15  07:06:29  ian
   Set configuration directory in Makefile rather than sysdep.h

   Revision 1.15  1992/01/14  04:04:17  ian
   Chip Salzenberg: strcmp is a macro on AIX

   Revision 1.14  1991/12/29  04:04:18  ian
   Added a bunch of extern definitions

   Revision 1.13  1991/12/29  01:54:46  ian
   Terry Gardner: allow a # character to be quoted in a configuration file

   Revision 1.12  1991/12/22  20:57:57  ian
   Added externs for strcasecmp or strncasecmp

   Revision 1.11  1991/12/18  03:54:14  ian
   Made error messages to terminal appear more normal

   Revision 1.10  1991/12/15  03:42:33  ian
   Added tprocess_chat_cmd for all chat commands, and added CMDTABTYPE_PREFIX

   Revision 1.9  1991/12/13  22:43:06  ian
   Don't continually allocate and free the list of arguments

   Revision 1.8  1991/12/09  18:39:46  ian
   Richard Todd: the pushed back line is specific to a particular multi file

   Revision 1.7  1991/12/09  16:59:48  ian
   Richard Todd: don't warn if special "#" command is unrecognized

   Revision 1.6  1991/12/07  18:05:20  ian
   Franc,ois Pinard: no limit to number of arguments

   Revision 1.5  1991/12/03  02:38:26  ian
   Don't treat numbers with leading zeroes as octal

   Revision 1.4  1991/12/01  19:35:38  ian
   David Nugent: read V2 and BNU files by default even with TAYLOR_CONFIG

   Revision 1.3  1991/11/26  02:04:49  ian
   Bob Denny: add explicit extern for strcmp and strcasecmp

   Revision 1.2  1991/09/19  02:38:21  ian
   Chip Salzenberg: V2 and BNU dialcodes files are relative to CONFIGLIB

   Revision 1.1  1991/09/10  19:38:34  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char config_rcsid[] = "$Id: config.c,v 1.29 1992/03/28 22:06:38 ian Rel $";
#endif

#include <ctype.h>
#include <errno.h>

#include "system.h"
#include "sysdep.h"

/* Some systems make strcmp a macro, which screws us up since we want
   to declare it since we will take its address later.  */
#undef strcmp
#undef strncmp

/* External functions.  */
extern int strcmp (), strncmp (), strcasecmp (), strncasecmp ();
extern int fclose ();

/* Local functions.  */

static void umulti_pushback P((struct smulti_file *, char *));

/* Status strings.  These must match enum tstatus_type.  */

const char *azStatus[] =
{
  "Conversation complete",
  "Port unavailable",
  "Dial failed",
  "Login failed",
  "Handshake failed",
  "Call failed",
  "Talking",
  "Wrong time to call"
};

/* Local node name.  */
const char *zLocalname;

/* Spool directory.  */
const char *zSpooldir = SPOOLDIR;

#if DEBUG > 1
/* Debugging level.  */
int iDebug = 0;

/* Debugging file name.  */
const char *zDebugfile = DEBUGFILE;
#endif

/* Public directory.  */
const char *zPubdir = PUBDIR;

/* Log file name.  */
const char *zLogfile = LOGFILE;

/* Statistics file name.  */
const char *zStatfile = STATFILE;

/* Dialcode file names.  */
char *zDialcodefile;

/* The maximum number of uuxqt processes which may be running at one
   time.  If this is zero, there is no limit.  */
int cMaxuuxqts;

#if HAVE_TAYLOR_CONFIG

/* System file names.  */
char *zSysfile;

/* Port file names.  */
char *zPortfile;

/* Dialer file names.  */
char *zDialfile;

/* Call out login and password file names.  */
char *zCallfile;

/* Call in login and password file names.  */
char *zPwdfile;

/* Command table used to parse the configuration file.  */

static enum tcmdtabret tcadd P((int argc, char **argv, pointer pvar,
				const char *zerr));

const struct scmdtab asCmds[] =
{
  /* System name.  */
  { "nodename", CMDTABTYPE_STRING, (pointer) &zLocalname, NULL },
  { "hostname", CMDTABTYPE_STRING, (pointer) &zLocalname, NULL },
  { "uuname", CMDTABTYPE_STRING, (pointer) &zLocalname, NULL },
  /* Spool directory.  */
  { "spool", CMDTABTYPE_STRING, (pointer) &zSpooldir, NULL },
  /* System information files.  */
  { "sysfile", CMDTABTYPE_FN | 0, (pointer) &zSysfile, tcadd },
  /* Port files.  */
  { "portfile", CMDTABTYPE_FN | 0, (pointer) &zPortfile, tcadd },
  /* Dial files.  */
  { "dialfile", CMDTABTYPE_FN | 0, (pointer) &zDialfile, tcadd },
  /* Dialcode files.  */
  { "dialcodefile", CMDTABTYPE_FN | 0, (pointer) &zDialcodefile, tcadd },
  /* Public directory.  */
  { "pubdir", CMDTABTYPE_STRING, (pointer) &zPubdir, NULL },
  /* Call out login name and password files.  */
  { "callfile", CMDTABTYPE_FN | 0, (pointer) &zCallfile, tcadd },
  /* Call in login name and password files.  */
  { "passwdfile", CMDTABTYPE_FN | 0, (pointer) &zPwdfile, tcadd },
  /* Log file.  */
  { "logfile", CMDTABTYPE_STRING, (pointer) &zLogfile, NULL },
  /* Statistics file.  */
  { "statfile", CMDTABTYPE_STRING, (pointer) &zStatfile, NULL },
#if DEBUG > 1
  /* Debugging file.  */
  { "debugfile", CMDTABTYPE_STRING, (pointer) &zDebugfile, NULL },
  /* Debugging level.  */
  { "debug", CMDTABTYPE_FN | 0, (pointer) &iDebug, tidebug_parse },
#endif
  /* Command for unknown system.  */
  { "unknown", CMDTABTYPE_FN, NULL, tiunknown },
  /* Maximum number of uuxqt processes.  */
  { "max-uuxqts", CMDTABTYPE_INT, (pointer) &cMaxuuxqts, NULL },
#if HAVE_V2_CONFIG
  { "v2-files", CMDTABTYPE_BOOLEAN, (pointer) &fV2, NULL },
#endif
#if HAVE_BNU_CONFIG
  { "bnu-files", CMDTABTYPE_BOOLEAN, (pointer) &fBnu, NULL },
#endif
  /* End marker.  */
  { NULL, 0, NULL, NULL }
};

/* Add strings to a string variable, separating with spaces.  */

/*ARGSUSED*/
static enum tcmdtabret
tcadd (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  int i;

  for (i = 1; i < argc; i++)
    uadd_string ((char **) pvar, argv[i], ' ');
  return CMDTABRET_FREE;
}

#endif /* HAVE_TAYLOR_CONFIG */

/* Process commands from a stdio file according to a command table.
   The character '#' introduces a comment.  Exactly one of e and
   qmulti must be NULL.  If we get the first line of a multi file, we
   attempt to process the command "#"; this command will not arise
   from a normal command file, and lets the caller take special action
   at the start of a new file.  If we are reading with zmulti_gets, we
   ignore the zerr argument and instead use the name of the file.  */

void
uprocesscmds (e, qmulti, qcmds, zerr, iflags)
     FILE *e;
     struct smulti_file *qmulti;
     const struct scmdtab *qcmds;
     const char *zerr;
     int iflags;
{
  while (TRUE)
    {
      static char **pzargs;
      static int calloc_args;
      char *zget, *z;
      int cargs;
      enum tcmdtabret t;
      char **pzhold_args;
      int chold_alloc_args;

      if (e != NULL)
	zget = zfgets (e, (iflags & CMDFLAG_BACKSLASH) != 0);
      else
	{
	  boolean ffirst;

	  zget = zmulti_gets (qmulti, &ffirst, &zerr,
			      (iflags & CMDFLAG_BACKSLASH) != 0);
	  if (zget != NULL && ffirst)
	    {
	      char *zargs;

	      zargs = (char *) "#";
	      t = tprocess_one_cmd (1, &zargs, qcmds, zerr,
				    iflags &~ CMDFLAG_WARNUNRECOG);
	      if (t == CMDTABRET_EXIT || t == CMDTABRET_FREE_AND_EXIT)
		{
		  umulti_pushback (qmulti, zget);
		  return;
		}
	    }
	}

      if (zget == NULL)
	return;

      /* Any # character not preceeded by a backslash starts a
	 comment.  */
      z = zget;
      while ((z = strchr (z, '#')) != NULL)
	{
	  if (z == zget || *(z - 1) != '\\')
	    {
	      *z = '\0';
	      break;
	    }
	  /* Remove the backslash.  */
	  xmemmove (z - 1, z, strlen (z) + 1);
	}

      z = zget;
      cargs = 0;
      while (TRUE)
	{
	  while (*z != '\0' && isspace (BUCHAR (*z)))
	    z++;

	  if (*z == '\0')
	    break;

	  if (cargs >= calloc_args)
	    {
	      calloc_args += 10;
	      pzargs = (char **) xrealloc ((pointer) pzargs,
					   calloc_args * sizeof (char *));
	    }

	  pzargs[cargs] = z;
	  ++cargs;

	  while (*z != '\0' && ! isspace (BUCHAR (*z)))
	    z++;

	  if (*z == '\0')
	    break;

	  *z++ = '\0';
	}

      if (cargs <= 0)
	{
	  xfree ((pointer) zget);
	  continue;
	}

      /* Save off the static variables to allow this function to
	 be called recursively.  */
      pzhold_args = pzargs;
      chold_alloc_args = calloc_args;
      pzargs = NULL;
      calloc_args = 0;

      t = tprocess_one_cmd (cargs, pzhold_args, qcmds, zerr, iflags);

      if (pzargs != NULL)
	xfree ((pointer) pzargs);
      pzargs = pzhold_args;
      calloc_args = chold_alloc_args;

      if (t == CMDTABRET_FREE || t == CMDTABRET_FREE_AND_EXIT)
	xfree ((pointer) zget);
      if (t == CMDTABRET_EXIT || t == CMDTABRET_FREE_AND_EXIT)
	return;
    }
}

/* Process a single command.  */

enum tcmdtabret
tprocess_one_cmd (cargs, azargs, qcmds, zerr, iflags)
     int cargs;
     char **azargs;
     const struct scmdtab *qcmds;
     const char *zerr;
     int iflags;
{
  const struct scmdtab *q;
  int (*pfcmp) P((const char *, const char *));

  if ((iflags & CMDFLAG_CASESIGNIFICANT) != 0)
    pfcmp = strcmp;
  else
    pfcmp = strcasecmp;
  
  for (q = qcmds; q->zcmd != NULL; q++)
    {
      int itype;
      int callowed;

      itype = TTYPE_CMDTABTYPE (q->itype);
      if (itype != CMDTABTYPE_PREFIX)
	{
	  if ((*pfcmp) (q->zcmd, azargs[0]) != 0)
	    continue;
	}
      else
	{
	  int clen;

	  clen = strlen (q->zcmd);
	  if ((iflags & CMDFLAG_CASESIGNIFICANT) != 0)
	    {
	      if (strncmp (q->zcmd, azargs[0], clen) != 0)
		continue;
	    }
	  else
	    {
	      if (strncasecmp (q->zcmd, azargs[0], clen) != 0)
		continue;
	    }
	}

      callowed = CARGS_CMDTABTYPE (q->itype);
      if (callowed != 0 && callowed != cargs)
	{
	  if (zerr != NULL)
	    ulog (LOG_ERROR, "%s: %s: Wrong number of arguments",
		  zerr, q->zcmd);
	  return CMDTABRET_FREE;
	}
      else if (itype == TTYPE_CMDTABTYPE (CMDTABTYPE_STRING))
	{
	  if (cargs != 1 && cargs != 2)
	    {
	      if (zerr != NULL)
		ulog (LOG_ERROR, "%s: %s: Wrong number of arguments",
		      zerr, q->zcmd);
	      return CMDTABRET_FREE;
	    }
	  if (cargs == 1)
	    *(const char **) q->pvar = "";
	  else
	    *(const char **) q->pvar = azargs[1];
	  return CMDTABRET_CONTINUE;
	}
      else if (itype == TTYPE_CMDTABTYPE (CMDTABTYPE_INT)
	       || itype == TTYPE_CMDTABTYPE (CMDTABTYPE_LONG))
	{
	  long i;
	  char *zend;

	  i = strtol (azargs[1], &zend, 10);
	  if (*zend != '\0')
	    {
	      if (zerr != NULL)
		ulog (LOG_ERROR, "%s: %s: Bad number", zerr,
		      q->zcmd);
	      return CMDTABRET_FREE;
	    }
	  if (itype == TTYPE_CMDTABTYPE (CMDTABTYPE_INT))
	    * (int *) q->pvar = (int) i;
	  else
	    * (long *) q->pvar = i;

	  return CMDTABRET_FREE;
	}
      else if (itype == TTYPE_CMDTABTYPE (CMDTABTYPE_BOOLEAN))
	{
	  char b;

	  b = azargs[1][0];
	  if (b == 'y' || b == 'Y' || b == 't' || b == 'T')
	    * (boolean *) q->pvar = TRUE;
	  else if (b == 'n' || b == 'N' || b == 'f' || b == 'F')
	    * (boolean *) q->pvar = FALSE;
	  else if (zerr != NULL)
	    ulog (LOG_ERROR, "%s: %s: Bad boolean", zerr,
		  q->zcmd);
	  return CMDTABRET_FREE;
	}		    
      else if (itype == TTYPE_CMDTABTYPE (CMDTABTYPE_FULLSTRING))
	{
	  int i, clen;
	  char *zset;
		  
	  /* Use all the arguments separated by a ' '
	     character.  */

	  clen = 1;
	  for (i = 1; i < cargs; i++)
	    clen += strlen (azargs[i]) + 1;

	  zset = (char *) xmalloc (clen);
	  *zset = '\0';
	  for (i = 1; i < cargs - 1; i++)
	    {
	      strcat (zset, azargs[i]);
	      strcat (zset, " ");
	    }
	  if (i < cargs)
	    strcat (zset, azargs[i]);

	  * (const char **) q->pvar = zset;

	  return CMDTABRET_FREE;
	}
      else if (itype == TTYPE_CMDTABTYPE (CMDTABTYPE_FN)
	       || itype == TTYPE_CMDTABTYPE (CMDTABTYPE_PREFIX))
	return (*q->ptfn) (cargs, azargs, q->pvar, zerr);
      else
	{
#if DEBUG > 0
	  ulog (LOG_FATAL, "tprocess_one_cmd: Can't happen (0x%x)",
		itype);
#endif
	  return CMDTABRET_FREE;
	}
    }

  if ((iflags & CMDFLAG_WARNUNRECOG) != 0
      && zerr != NULL)
    ulog (LOG_ERROR, "%s: Unrecognized command %s", zerr, azargs[0]);

  return CMDTABRET_FREE;
}

/* Read the configuration file.  If we can't open the configuration file,
   and we're trying to read the default, don't report an error.  This
   permits people to not have a configuration file at all if they are
   satisfied with the compiled in defaults.  */

void
uread_config (zname)
     const char *zname;
{
#if HAVE_TAYLOR_CONFIG

  FILE *e;
  char *zdefault;

  zdefault = (char *) alloca (sizeof NEWCONFIGLIB + sizeof CONFIGFILE);
  sprintf (zdefault, "%s%s", NEWCONFIGLIB, CONFIGFILE);

  /* On UNIX we will be probably be running suid to uucp.  We don't want
     to let somebody run us and specify some arbitrary file as the
     configuration file, since that might let them examine files they
     have no access to.  */
  if (zname != NULL
      && strcmp (zname, zdefault) != 0
      && ! fsysdep_other_config (zname))
    {
      ulog (LOG_ERROR, "Can't read %s; using %s", zname, zdefault);
      zname = zdefault;
    }

  if (zname == NULL)
    zname = zdefault;

  e = fopen (zname, "r");
  if (e == NULL)
    {
      if (strcmp (zname, zdefault) != 0)
	{
	  fprintf (stderr, "%s: %s: %s\n", abProgram, zname,
		   strerror (errno));
	  /* We haven't yet called usysdep_initialize, so it should be
	     safe to just exit.  */
	  exit (EXIT_FAILURE);
	}
      /* Just use defaults.  */
    }
  else
    {
      uiunknown_start ();
      uprocesscmds (e, (struct smulti_file *) NULL, asCmds, zname,
		    CMDFLAG_BACKSLASH);
      (void) fclose (e);
      uiunknown_end ();
    }

#else /* ! HAVE_TAYLOR_CONFIG */

  uiunknown_start ();
  uiunknown_end ();

#endif /* ! HAVE_TAYLOR_CONFIG */

  if (zLocalname == NULL)
    {
      zLocalname = zsysdep_local_name ();
      if (zLocalname == NULL)
	{
	  fprintf (stderr, "%s: Can't get local node name\n", abProgram);
	  exit (EXIT_FAILURE);
	}
    }

  uisetup_localsys ();

#if HAVE_TAYLOR_CONFIG

  /* Get the defaults for the file names.  */

#define SETDEFAULT(z, zfile) \
  if (z == NULL) \
    { \
      z = (char *) xmalloc (sizeof NEWCONFIGLIB + sizeof zfile - 1); \
      strcpy (z, NEWCONFIGLIB); \
      strcat (z, zfile); \
      if (! fsysdep_file_exists (z)) \
	{ \
	  xfree ((pointer) z); \
	  z = NULL; \
	} \
    }

  SETDEFAULT (zSysfile, SYSFILE);
  SETDEFAULT (zPortfile, PORTFILE);
  SETDEFAULT (zDialfile, DIALFILE);
  SETDEFAULT (zDialcodefile, DIALCODEFILE);
  SETDEFAULT (zPwdfile, PASSWDFILE);
  SETDEFAULT (zCallfile, CALLFILE);

#endif /* HAVE_TAYLOR_CONFIG */

#if HAVE_BNU_CONFIG
  /* If we are supposed to read standard BNU files, read Sysfiles to
     get any nonstandard file names.  */
  if (fBnu)
    ubnu_read_sysfiles ();
#endif /* HAVE_BNU_CONFIG */

  /* The format of the dialcodes file is the same for all systems, so
     if additional configuration are being used we add the files in
     here to save having to do it when the dialcodes file is read.  */

#if HAVE_V2_CONFIG
  if (fV2)
    {
      char *z;

      z = (char *) alloca (sizeof OLDCONFIGLIB + sizeof V2_DIALCODES);
      sprintf (z, "%s%s", OLDCONFIGLIB, V2_DIALCODES);
      uadd_string (&zDialcodefile, z, ' ');
    }
#endif /* HAVE_V2_CONFIG */

#if HAVE_BNU_CONFIG
  if (fBnu)
    {
      char *z;

      z = (char *) alloca (sizeof OLDCONFIGLIB + sizeof BNU_DIALCODES);
      sprintf (z, "%s%s", OLDCONFIGLIB, BNU_DIALCODES);
      uadd_string (&zDialcodefile, z, ' ');
    }
#endif /* HAVE_BNU_CONFIG */
}

/* Add a string to a list of strings separated by a separator
   character.  */

void
uadd_string (pz, z, bsep)
     char **pz;
     const char *z;
     int bsep;
{
  if (*pz == NULL)
    *pz = xstrdup (z);
  else
    {
      int clen;

      clen = strlen (*pz);
      *pz = (char *) xrealloc ((pointer) *pz, clen + strlen (z) + 2);
      (*pz)[clen] = (char) bsep;
      strcpy (*pz + clen + 1, z);
    }
}

#if DEBUG > 1

/* Parse a debugging string.  This may be a simple number, which sets
   the given number of bits in iDebug, or it may be a series of single
   letters.  */

static const char * const azDebug_names[] = DEBUG_NAMES;

int
idebug_parse (z)
     const char *z;
{
  char *zend;
  int i, iret;
  char *zcopy, *ztok;

  i = (int) strtol (z, &zend, 0);
  if (*zend == '\0')
    {
      if (i > 15)
	i = 15;
      else if (i < 0)
	i = 0;
      return (1 << i) - 1;
    }

  zcopy = (char *) alloca (strlen (z) + 1);
  strcpy (zcopy, z);

  iret = 0;

  for (ztok = strtok (zcopy, ",");
       ztok != NULL;
       ztok = strtok ((char *) NULL, ","))
    {
      if (strcasecmp (ztok, "all") == 0)
	{
	  iret = DEBUG_MAX;
	  break;
	}
      for (i = 0; azDebug_names[i] != NULL; i++)
	{
	  if (strncasecmp (ztok, azDebug_names[i],
			   strlen (azDebug_names[i])) == 0)
	    {
	      iret |= 1 << i;
	      break;
	    }
	}
      if (azDebug_names[i] == NULL)
	ulog (LOG_ERROR, "Unrecognized debugging option \"%s\"",
	      ztok);
    }

  return iret;
}

/* Parse a debugging string in a configuration file.  The pvar
   arguments points to the field to set.  */

/*ARGSUSED*/
enum tcmdtabret
tidebug_parse (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  int *pidebug = (int *) pvar;
  int i;

  if (argc == 2 &&
      strncasecmp (argv[1], DEBUG_NONE, strlen (DEBUG_NONE)) == 0)
    {
      *pidebug = 0;
      return CMDTABRET_FREE;
    }

  for (i = 1; i < argc; i++)
    *pidebug |= idebug_parse (argv[i]);

  return CMDTABRET_FREE;
}

#endif /* DEBUG > 1 */

/* Given a file name that may actually be several file names each
   separated with a single space character, we want to be able to read
   lines from them as though they were catenated to form a single
   file.  This set of routines allows us to do that easily, although
   only for one set of files at a time.  One line can be pushed back,
   which can be convenient for the first line of a file.  */

struct smulti_file
{
  /* Names of next files to open.  */
  char *z;
  /* String to free up when done.  */
  char *zfree;
  /* Current file.  */
  FILE *e;
  /* Current file name.  */
  char *zname;
  /* Next line to return.  */
  char *znext;
};

struct smulti_file *
qmulti_open (znames)
     const char *znames;
{
  struct smulti_file *qret;

  qret = (struct smulti_file *) xmalloc (sizeof (struct smulti_file));
  qret->z = qret->zfree = xstrdup (znames);
  qret->zname = NULL;
  qret->e = NULL;
  qret->znext = NULL;

  return qret;
}

/* Read the next line from a multiply opened set of files.  */

char *
zmulti_gets (q, pffirst, pzname, fbackslash)
     struct smulti_file *q;
     boolean *pffirst;
     const char **pzname;
     boolean fbackslash;
{
  if (pffirst != NULL)
    *pffirst = FALSE;

  if (q->znext != NULL)
    {
      char *z;

      z = q->znext;
      q->znext = NULL;
      return z;
    }

  while (TRUE)
    {
      char *z;

      if (q->e != NULL)
	{
	  char *zret;

	  zret = zfgets (q->e, fbackslash);
	  if (zret != NULL)
	    {
	      if (pzname != NULL)
		*pzname = q->zname;
	      return zret;
	    }

	  if (fclose (q->e) != 0)
	    {
	      ulog (LOG_ERROR, "fclose: %s", strerror (errno));
	      q->e = NULL;
	      return NULL;
	    }

	  q->e = NULL;
	}

      if (*q->z == '\0')
	return NULL;

      z = q->z;
      q->z += strcspn (z, " ");
      if (*q->z != '\0')
	*q->z++ = '\0';

      q->e = fopen (z, "r");
      if (q->e == NULL)
	{
	  ulog (LOG_ERROR, "fopen (%s): %s", z, strerror (errno));
	  return NULL;
	}

      if (pffirst != NULL)
	*pffirst = TRUE;
      q->zname = z;
    }
}

/* Push back a line so that it is read by the next call to
   zmulti_gets.  This is sometimes used on the first line of a file in
   uprocesscmds.  */

static void
umulti_pushback (q, z)
     struct smulti_file *q;
     char *z;
{
#if DEBUG > 0
  if (q->znext != NULL)
    ulog (LOG_FATAL, "umulti_pushback: Can't happen");
#endif
  q->znext = z;
}

/* Close the multiple file, even though a read is in progress.  If
   zmulti_gets returns NULL, the files will have been closed.  */

boolean
fmulti_close (q)
     struct smulti_file *q;
{
  boolean fret;

  fret = TRUE;

  if (q->e != NULL)
    {
      if (fclose (q->e) != 0)
	{
	  ulog (LOG_ERROR, "fclose: %s", strerror (errno));
	  fret = FALSE;
	}
    }

  xfree ((pointer) q->zfree);
  xfree ((pointer) q);

  return fret;
}

/* See whether a file is in a directory list, and make sure the user
   has appropriate access.  */

boolean
fin_directory_list (qsys, zfile, zdirs, fcheck, freadable, zuser)
     const struct ssysteminfo *qsys;
     const char *zfile;
     const char *zdirs;
     boolean fcheck;
     boolean freadable;
     const char *zuser;
{
  char *zcopy;
  boolean fmatch;

  zcopy = (char *) alloca (strlen (zdirs) + 1);
  strcpy (zcopy, zdirs);

  fmatch = FALSE;

  while (*zcopy != '\0')
    {
      char *z;

      z = zcopy + strcspn (zcopy, " ");
      if (*z == '\0')
	--z;
      else
	*z = '\0';

      if (*zcopy == '!')
	{
	  if (fsysdep_in_directory (qsys, zfile, zcopy + 1, FALSE,
				    FALSE, (const char *) NULL))
	    fmatch = FALSE;
	}
      else
	{
	  if (fsysdep_in_directory (qsys, zfile, zcopy, fcheck,
				    freadable, zuser))
	    fmatch = TRUE;
	}

      zcopy = z + 1;
    }

  return fmatch;
}

/* See whether a file is a spool file.  Spool file names are specially
   crafted to hand around to other UUCP packages.  They always begin
   with 'C', 'D' or 'X', and the second character is always a period.
   The remaining characters are any character that could appear in a
   system name.  */

boolean
fspool_file (zfile)
     const char *zfile;
{
  const char *z;

  if (*zfile != 'C' && *zfile != 'D' && *zfile != 'X')
    return FALSE;
  if (zfile[1] != '.')
    return FALSE;
  for (z = zfile + 2; *z != '\0'; z++)
    if (! isalnum (BUCHAR (*z)) && *z != '_' && *z != '-' && *z != '.')
      return FALSE;
  return TRUE;
}

/* Compare two grades, returning < 0 if the first should be executed
   before the second, == 0 if they are the same, and > 0 if the first
   should be executed after the second.  This code assumes that the
   upper case letters appear in sequence and the lower case letters
   appear in sequence.  */

int
igradecmp (barg1, barg2)
     int barg1;
     int barg2;
{
  int b1, b2;

  /* Make sure the arguments are unsigned.  */
  b1 = (int) BUCHAR (barg1);
  b2 = (int) BUCHAR (barg2);

  if (isdigit (b1))
    {
      if (isdigit (b2))
	return b1 - b2;
      else
	return -1;
    }
  else if (isupper (b1))
    {
      if (isdigit (b2))
	return 1;
      else if (isupper (b2))
	return b1 - b2;
      else
	return -1;
    }
  else
    {
      if (! islower (b2))
	return 1;
      else
	return b1 - b2;
    }
}
