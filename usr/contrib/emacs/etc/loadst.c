/* $Header:   RCS/loadst.v  Revision 1.1  83/02/09  17:16:53  fen  Rel$ */
/*
 * loadst -- print current time and load statistics.
 *				-- James Gosling @ CMU, May 1981
 *  loadst [ -n ] [ interval ]
 *	07/29/81 jag -- also print info on presence of mail.
 *	05/05/82 jag -- add disk drive utilization statistics.
 */

#define NO_SHORTNAMES  /* Do not want config to try to include remap.h */
#include "../src/config.h"
#include <stdio.h>
#include <pwd.h>

/* Define two macros KERNEL_FILE (file to find kernel symtab in)
   and LDAV_SYMBOL (symbol name to look for), based on system type.
   Also define NLIST_STRUCT if the type `nlist' is a structure we
   can get from nlist.h; otherwise must use a.out.h and initialize
   with strcpy.  Note that config.h may define NLIST_STRUCT
   for more modrern USG systems.  */

#ifdef USG
#ifdef HPUX
#define LDAV_SYMBOL "_avenrun"
#define KERNEL_FILE "/hp-ux"
#define NLIST_STRUCT
#else /* not HPUX */
#define LDAV_SYMBOL "avenrun"
#define KERNEL_FILE "/unix"
#endif /* not HPUX */
#else /* not USG */
#define LDAV_SYMBOL "_avenrun"
#define NLIST_STRUCT
#ifndef KERNEL_FILE
#define KERNEL_FILE "/vmunix"
#endif /* no KERNEL_FILE yet */
#endif /* not USG */


#ifdef LOAD_AVE_TYPE
#ifndef NLIST_STRUCT
#include <a.out.h>
#else /* NLIST_STRUCT */
#include <nlist.h>
#endif /* NLIST_STRUCT */
#endif /* LOAD_AVE_TYPE */

#ifdef USG
#include <time.h>
#include <sys/types.h>
#else /* not USG */
#include <sys/time.h>
#include <sys/param.h>
#ifdef LOAD_AVE_TYPE
#include <sys/dk.h>
#endif /* LOAD_AVE_TYPE */
#endif /* USG */

#include <sys/stat.h>

/* We don't want Emacs's macro definitions for these USG primitives. */

#undef open
#undef read

struct tm *localtime ();

#ifdef LOAD_AVE_TYPE
#ifndef NLIST_STRUCT
struct nlist nl[2];
#else /* NLIST_STRUCT */
struct nlist nl[] =
  {
    { LDAV_SYMBOL },
#if defined (CPUSTATES) && defined (DK_NDRIVE)
#define	X_CPTIME	1
    { "_cp_time" },
#define	X_DKXFER	2
    { "_dk_xfer" },
#endif /* have CPUSTATES and DK_NDRIVE */
    { 0 },
  };
#endif /* NLIST_STRUCT */
#endif /* LOAD_AVE_TYPE */

#if defined (CPUSTATES) && defined (DK_NDRIVE)

struct
{
  long	time[CPUSTATES];
  long	xfer[DK_NDRIVE];
} s, s1;

double	etime;

#endif /* have CPUSTATES and DK_NDRIVE */

int nflag;			/* -n flag -- no newline */
int uflag;			/* -u flag -- user current user ID rather
				   than login user ID */
int repetition;			/* repetition interval */


main (argc, argv)
char  **argv;
{
  register int kmem, i;
  char *mail;
  char *user_name;
  struct stat st;

  kmem = open ("/dev/kmem", 0);

#ifdef LOAD_AVE_TYPE
#ifndef NLIST_STRUCT
  strcpy (nl[0].n_name, LDAV_SYMBOL);
  strcpy (nl[1].n_name, "");
#endif /* not NLIST_STRUCT */

  nlist (KERNEL_FILE, nl);
#endif /* LOAD_AVE_TYPE */

  while (--argc > 0)
    {
      argv++;
      if (strcmp (*argv, "-n") == 0)
	nflag++;
      else if (strcmp (*argv, "-u") == 0)
	uflag++;
      else
	if ((repetition = atoi (*argv)) <= 0)
	  {
	    fprintf (stderr, "Bogus argument: %s\n", *argv);
	    exit (1);
	  }
    }

  user_name = uflag ? ((struct passwd *) getpwuid (getuid ())) -> pw_name
#ifdef USG
    : (char *) getenv ("LOGNAME");
#else
    : (char *) getenv ("USER");
#endif

  mail = (char *) malloc (strlen (user_name) + 30);

#ifdef USG
  sprintf (mail, "/usr/mail/%s", user_name);
#else /* not USG */
  sprintf (mail, "/usr/spool/mail/%s", user_name);
#endif /* not USG */

  if (stat (mail, &st) >= 0
      && (st.st_mode & S_IFMT) == S_IFDIR)
    {
      strcat (mail, "/");
      strcat (mail, user_name);
    }

  while (1)
    {
      register struct tm *nowt;
      long now;

      time (&now);
      nowt = localtime (&now);

      printf ("%d:%02d%s ",
	      ((nowt->tm_hour + 11) % 12) + 1,
	      nowt->tm_min,
	      nowt->tm_hour >= 12 ? "pm" : "am");

#ifdef LOAD_AVE_TYPE
      if (kmem >= 0)
	{
	  LOAD_AVE_TYPE avenrun[3];
	  avenrun[0] = 0;
	  lseek (kmem, (long) nl[0].n_value, 0);
	  read (kmem, avenrun, sizeof (avenrun));
	  printf ("%.2f", LOAD_AVE_CVT (avenrun[0]) / 100.0);
	}
#endif /* LOAD_AVE_TYPE */

      printf ("%s", (stat (mail, &st) >= 0 && st.st_size) ? " Mail" : "");

#if defined (CPUSTATES) && defined (DK_NDRIVE)
      if (kmem >= 0)
	{
	  lseek (kmem, (long) nl[X_CPTIME].n_value, 0);
	  read (kmem, s.time, sizeof s.time);
	  lseek (kmem, (long) nl[X_DKXFER].n_value, 0);
	  read (kmem, s.xfer, sizeof s.xfer);
	  etime = 0;
	  for (i = 0; i < DK_NDRIVE; i++)
	    {
	      register t = s.xfer[i];
	      s.xfer[i] -= s1.xfer[i];
	      s1.xfer[i] = t;
	    }
	  for (i = 0; i < CPUSTATES; i++)
	    {
	      register t = s.time[i];
	      s.time[i] -= s1.time[i];
	      s1.time[i] = t;
	      etime += s.time[i];
	    }
	  if (etime == 0.)
	    etime = 1.;
	  etime /= 60.;
	 
	  {   register max = s.xfer[0];
	      for (i = 1; i < DK_NDRIVE; i++)
		if (s.xfer[i] > max)
		  max = s.xfer[i];
	      printf ("[%d]", (int) (max / etime + 0.5));
	    }
	}
#endif /* have CPUSTATES and DK_NDRIVE */
      if (!nflag)
	putchar ('\n');
      fflush (stdout);
      if (repetition <= 0)
	break;
      sleep (repetition);
    }
}
