/* id -- print real and effective UIDs and GIDs
   Copyright (C) 1989, 1991 Free Software Foundation.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by Arnold Robbins, arnold@audiofax.com.
   Major rewrite by David MacKenzie, djm@ai.mit.edu. */

#include <stdio.h>
#include <getopt.h>
#include <sys/types.h>
#include <pwd.h>
#include <grp.h>
#include "system.h"

#ifdef _POSIX_VERSION
#include <limits.h>
#ifndef __386BSD__
#ifdef NGROUPS_MAX
#undef NGROUPS_MAX
#endif /* NGROUPS_MAX */
#define NGROUPS_MAX sysconf (_SC_NGROUPS_MAX)
#endif /* __386BSD__ */
#if !defined(sun) && !defined(ultrix) && !defined(__386BSD__)
#define GETGROUPS_T gid_t
#else /* sun or ultrix */
#define GETGROUPS_T int
#endif /* not sun and not ultrix */
#else /* not _POSIX_VERSION */
struct passwd *getpwuid ();
struct group *getgrgid ();
uid_t getuid ();
gid_t getgid ();
uid_t geteuid ();
gid_t getegid ();
#include <sys/param.h>
#if !defined(NGROUPS_MAX) && defined(NGROUPS)
#define NGROUPS_MAX NGROUPS
#endif /* not NGROUPS_MAX and NGROUPS */
#define GETGROUPS_T int
#endif /* _POSIX_VERSION */

char *xmalloc ();
int getugroups ();
void error ();
void print_user ();
void print_group ();
void print_group_list ();
void print_full_info ();
void usage ();

/* The name this program was run with. */
char *program_name;

/* If nonzero, output only the group ID(s). -g */
int just_group = 0;

/* If nonzero, output user/group name instead of ID number. -n */
int use_name = 0;

/* If nonzero, output real UID/GID instead of default effective UID/GID. -r */
int use_real = 0;

/* If nonzero, output only the user ID(s). -u */
int just_user = 0;

/* If nonzero, output only the supplementary groups. -G */
int just_group_list = 0;

/* The real and effective IDs of the user to print. */
uid_t ruid, euid;
gid_t rgid, egid;

/* The number of errors encountered so far. */
int problems = 0;

struct option longopts[] =
{
  {"group", 0, NULL, 'g'},
  {"name", 0, NULL, 'n'},
  {"real", 0, NULL, 'r'},
  {"user", 0, NULL, 'u'},
  {"groups", 0, NULL, 'G'},
  {NULL, 0, NULL, 0}
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  int optc;

  program_name = argv[0];

  while ((optc = getopt_long (argc, argv, "gnruG", longopts, (int *) 0))
	 != EOF)
    {
      switch (optc)
	{
	case 'g':
	  just_group = 1;
	  break;
	case 'n':
	  use_name = 1;
	  break;
	case 'r':
	  use_real = 1;
	  break;
	case 'u':
	  just_user = 1;
	  break;
	case 'G':
	  just_group_list = 1;
	  break;
	default:
	  usage ();
	}
    }

  if (just_user + just_group + just_group_list > 1)
    error (1, 0, "cannot print only user and only group");

  if (just_user + just_group + just_group_list == 0 && (use_real || use_name))
    error (1, 0, "cannot print only names or real IDs in default format");

  if (argc - optind > 1)
    usage ();

  if (argc - optind == 1)
    {
      struct passwd *pwd = getpwnam (argv[optind]);
      if (pwd == NULL)
	error (1, 0, "%s: No such user", argv[optind]);
      ruid = euid = pwd->pw_uid;
      rgid = egid = pwd->pw_gid;
    }
  else
    {
      euid = geteuid ();
      ruid = getuid ();
      egid = getegid ();
      rgid = getgid ();
    }

  if (just_user)
    print_user (use_real ? ruid : euid);
  else if (just_group)
    print_group (use_real ? rgid : egid);
  else if (just_group_list)
    print_group_list (argv[optind]);
  else
    print_full_info (argv[optind]);
  putchar ('\n');

  exit (problems != 0);
}

/* Print the name or value of user ID UID. */

void
print_user (uid)
     int uid;
{
  struct passwd *pwd = NULL;

  if (use_name)
    {
      pwd = getpwuid (uid);
      if (pwd == NULL)
	problems++;
    }

  if (pwd == NULL)
    printf ("%u", (unsigned) uid);
  else
    printf ("%s", pwd->pw_name);
}

/* Print the name or value of group ID GID. */

void
print_group (gid)
     int gid;
{
  struct group *grp = NULL;

  if (use_name)
    {
      grp = getgrgid (gid);
      if (grp == NULL)
	problems++;
    }

  if (grp == NULL)
    printf ("%u", (unsigned) gid);
  else
    printf ("%s", grp->gr_name);
}

/* Print all of the distinct groups the user is in . */

void
print_group_list (username)
     char *username;
{
  print_group (rgid);
  if (egid != rgid)
    {
      putchar (' ');
      print_group (egid);
    }

#ifdef NGROUPS_MAX
  {
    int ngroups;
    GETGROUPS_T *groups;
    register int i;

    groups = (GETGROUPS_T *) xmalloc (NGROUPS_MAX * sizeof (GETGROUPS_T));
    if (username == 0)
      ngroups = getgroups (NGROUPS_MAX, groups);
    else
      ngroups = getugroups (NGROUPS_MAX, groups, username);
    if (ngroups < 0)
      {
	error (0, errno, "cannot get supplemental group list");
	problems++;
	free (groups);
	return;
      }

    for (i = 0; i < ngroups; i++)
      if (groups[i] != rgid && groups[i] != egid)
	{
	  putchar (' ');
	  print_group (groups[i]);
	}
    free (groups);
  }
#endif
}

/* Print all of the info about the user's user and group IDs. */

void
print_full_info (username)
     char *username;
{
  struct passwd *pwd;
  struct group *grp;

  printf ("uid=%u", (unsigned) ruid);
  pwd = getpwuid (ruid);
  if (pwd == NULL)
    problems++;
  else
    printf ("(%s)", pwd->pw_name);
  
  printf (" gid=%u", (unsigned) rgid);
  grp = getgrgid (rgid);
  if (grp == NULL)
    problems++;
  else
    printf ("(%s)", grp->gr_name);
  
  if (euid != ruid)
    {
      printf (" euid=%u", (unsigned) euid);
      pwd = getpwuid (euid);
      if (pwd == NULL)
	problems++;
      else
	printf ("(%s)", pwd->pw_name);
    }
  
  if (egid != rgid)
    {
      printf (" egid=%u", (unsigned) egid);
      grp = getgrgid (egid);
      if (grp == NULL)
	problems++;
      else
	printf ("(%s)", grp->gr_name);
    }

#ifdef NGROUPS_MAX
  {
    int ngroups;
    GETGROUPS_T *groups;
    register int i;

    groups = (GETGROUPS_T *) xmalloc (NGROUPS_MAX * sizeof (GETGROUPS_T));
    if (username == 0)
      ngroups = getgroups (NGROUPS_MAX, groups);
    else
      ngroups = getugroups (NGROUPS_MAX, groups, username);
    if (ngroups < 0)
      {
	error (0, errno, "cannot get supplemental group list");
	problems++;
	free (groups);
	return;
      }

    if (ngroups > 0)
      fputs (" groups=", stdout);
    for (i = 0; i < ngroups; i++)
      {
	if (i > 0)
	  putchar (',');
	printf ("%u", (unsigned) groups[i]);
	grp = getgrgid (groups[i]);
	if (grp == NULL)
	  problems++;
	else
	  printf ("(%s)", grp->gr_name);
      }
    free (groups);
  }
#endif
}

void
usage ()
{
  fprintf (stderr, "\
Usage: %s [-gnruG] [--group] [--name] [--real] [--user] [--groups] [username]\n",
	   program_name);
  exit (1);
}
