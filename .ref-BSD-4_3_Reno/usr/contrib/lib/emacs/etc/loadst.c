/*
 * loadst -- print current time and load statistics.
 *				-- James Gosling @ CMU, May 1981
 *  loadst [ -n ] [ interval ]
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
   for more modern USG systems.  */


#ifdef LOAD_AVE_TYPE
#ifndef NLIST_STRUCT
#include <a.out.h>
#else /* NLIST_STRUCT */
#include <nlist.h>
#endif /* NLIST_STRUCT */
#endif /* LOAD_AVE_TYPE */

/* All this serves to #include <param.h> and clean up the consequences.  */
#ifdef BSD
/* It appears param.h defines BSD and BSD4_3 in 4.3
   and is not considerate enough to avoid bombing out
   if they are already defined.  */
#undef BSD
#ifdef BSD4_3
#undef BSD4_3
#define XBSD4_3 /* XBSD4_3 says BSD4_3 is supposed to be defined.  */
#endif
#include <sys/param.h>
/* Now if BSD or BSD4_3 was defined and is no longer,
   define it again.  */
#ifndef BSD
#define BSD
#endif
#ifdef XBSD4_3
#ifndef BSD4_3
#define BSD4_3
#endif
#endif /* XBSD4_3 */
#endif /* BSD */

#ifdef USG
#include <time.h>
#include <sys/types.h>
#else /* not USG */
#include <sys/time.h>
#ifdef LOAD_AVE_TYPE
#ifndef RTU
#ifndef UMAX
#ifdef DKSTAT_HEADER_FILE
#include <sys/dkstat.h>
#else
#include <sys/dk.h>
#endif /* not DKSTAT_HEADER_FILE */
#endif /* UMAX */
#endif /* not RTU */
#endif /* LOAD_AVE_TYPE */
#endif /* USG */

#include <sys/stat.h>

#ifdef BSD
#include <sys/ioctl.h>
#endif /* BSD */

#ifdef UMAX
/*
 *  UMAX 4.2, which runs on the Encore Multimax multiprocessor, does not
 *  have a /dev/kmem.  Information about the workings of the running kernel
 *  can be gathered with inq_stats system calls.
 */
#include <sys/sysdefs.h>
#include <sys/syscall.h>
#include <sys/statistics.h>
#include <sys/procstats.h>
#include <sys/sysstats.h>
#endif /* UMAX */

/* We don't want Emacs's macro definitions for these USG primitives. */

#undef open
#undef read
#undef close

struct tm *localtime ();

#ifndef DKXFER_SYMBOL
#define DKXFER_SYMBOL "_dk_xfer"
#endif
#ifndef CPTIME_SYMBOL
#define CPTIME_SYMBOL "_cp_time"
#endif

#ifdef LOAD_AVE_TYPE
#ifndef NLIST_STRUCT
struct nlist nl[2];
#else /* NLIST_STRUCT */
struct nlist nl[] =
  {
    { LDAV_SYMBOL },
#if defined (CPUSTATES) && defined (DK_NDRIVE)
#define	X_CPTIME	1
    { CPTIME_SYMBOL },
#define	X_DKXFER	2
    { DKXFER_SYMBOL },
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

#ifdef LOAD_AVE_TYPE
LOAD_AVE_TYPE load_average ();
#endif /* LOAD_AVE_TYPE */

main (argc, argv)
     char  **argv;
{
  register int kmem, i;
  char *mail;
  char *user_name;
  struct stat st;
#ifdef LOAD_AVE_TYPE
  LOAD_AVE_TYPE load;
#endif /* LOAD_AVE_TYPE */

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

  mail = (char *) getenv ("MAIL");

  if (mail == 0)
    {
      mail = (char *) malloc (strlen (user_name) + 30);

#if defined (USG) && ! defined (XENIX)
      sprintf (mail, "/usr/mail/%s", user_name);
#else /* Xenix, or not USG */
#ifdef BSD4_4
      sprintf (mail, "/var/mail/%s", user_name);
#else
      sprintf (mail, "/usr/spool/mail/%s", user_name);
#endif
#endif /* Xenix, or not USG */
    }

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
      load = load_average (kmem);
      if (load != (LOAD_AVE_TYPE) -1)
	printf("%.2f", LOAD_AVE_CVT (load) / 100.0);
#endif /* LOAD_AVE_TYPE */

      printf ("%s",
	      ((stat (mail, &st) >= 0 && st.st_size > 0)
	       ? " Mail"
	       : ""));

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
#ifndef BSD4_3
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
	 
#else
	  {
	    static struct timeval tv, tv1;
	    gettimeofday (&tv, 0);
	    etime = (tv.tv_sec - tv1.tv_sec)
		+ (tv.tv_usec - tv1.tv_usec) / 1.0e6;
	    tv1 = tv;
	  }
#endif
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

#ifdef BSD
      /* We are about to loop back and write another unit of output.  */
      /* If previous output has not yet been read by Emacs, flush it
	 so the pty output buffer never gets full and Emacs
	 can always get the latest update right away.  */
      /* ??? Someone should write a USG version of this code!  */
      {
	int zero = 0;

	ioctl (fileno (stdout), TIOCFLUSH, &zero);
      }
#endif
    }
}

#ifdef LOAD_AVE_TYPE

LOAD_AVE_TYPE
load_average (kmem)
     int kmem;
{
#ifdef UMAX

  int i, j;
  double sum;
  struct proc_summary proc_sum_data;
  struct stat_descr proc_info;
  
  proc_info.sd_next = NULL;
  proc_info.sd_subsys = SUBSYS_PROC;
  proc_info.sd_type = PROCTYPE_SUMMARY;
  proc_info.sd_addr = (char *) &proc_sum_data;
  proc_info.sd_size = sizeof (struct proc_summary);
  proc_info.sd_sizeused = 0;
  
  if (inq_stats (1, &proc_info) != 0 )
    {
      perror ("sysline proc summary inq_stats");
      exit (1);
    }
  /*
   * Generate current load average.
   */
  sum = 0;
  for (i = proc_sum_data.ps_nrunidx, j = 0; j < 12; j++)
    {
      sum += proc_sum_data.ps_nrun[i];
      if (--i < 0)
	i = 179;
    }
  return sum / 12;

#else /* not UMAX */

  if (kmem >= 0)
    {
      LOAD_AVE_TYPE avenrun[3];
      avenrun[0] = 0;
      lseek (kmem, (long) nl[0].n_value, 0);
      read (kmem, avenrun, sizeof (avenrun));
      return avenrun[0];
    }
  else
    return (LOAD_AVE_TYPE) -1;

#endif /* UMAX */
}

#endif /* LOAD_AVE_TYPE */
