/* idcache.c -- map user and group IDs, cached for speed
   Copyright (C) 1985, 1988, 1989, 1990 Free Software Foundation, Inc.

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

#include <stdio.h>
#include <sys/types.h>
#include <pwd.h>
#include <grp.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifndef _POSIX_VERSION
struct passwd *getpwuid ();
struct group *getgrgid ();
#endif

char *xmalloc ();
char *xstrdup ();

struct userid
{
  unsigned short uid;
  char *name;
  struct userid *next;
};

static struct userid *user_alist;

/* Translate UID to a login name, with cache.  */

char *
getuser (uid)
     unsigned short uid;
{
  register struct userid *tail;
  struct passwd *pwent;
  char usernum_string[20];

  for (tail = user_alist; tail; tail = tail->next)
    if (tail->uid == uid)
      return tail->name;

  pwent = getpwuid (uid);
  tail = (struct userid *) xmalloc (sizeof (struct userid));
  tail->uid = uid;
  tail->next = user_alist;
  if (pwent == 0)
    {
      sprintf (usernum_string, "%u", (unsigned short) uid);
      tail->name = xstrdup (usernum_string);
    }
  else
    tail->name = xstrdup (pwent->pw_name);
  user_alist = tail;
  return tail->name;
}

/* Use the same struct as for userids.  */
static struct userid *group_alist;

/* Translate GID to a group name, with cache.  */

char *
getgroup (gid)
     unsigned short gid;
{
  register struct userid *tail;
  struct group *grent;
  char groupnum_string[20];

  for (tail = group_alist; tail; tail = tail->next)
    if (tail->uid == gid)
      return tail->name;

  grent = getgrgid (gid);
  tail = (struct userid *) xmalloc (sizeof (struct userid));
  tail->uid = gid;
  tail->next = group_alist;
  if (grent == 0)
    {
      sprintf (groupnum_string, "%u", (unsigned int) gid);
      tail->name = xstrdup (groupnum_string);
    }
  else
    tail->name = xstrdup (grent->gr_name);
  group_alist = tail;
  return tail->name;
}
