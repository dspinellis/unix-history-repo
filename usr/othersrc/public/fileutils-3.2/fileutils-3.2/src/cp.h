/* cp.h  -- file copying (data definitions)
   Copyright (C) 1989, 1990, 1991 Free Software Foundation.

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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Written by Torbjorn Granlund, Sweden (tege@sics.se). */

#include <sys/types.h>
#include "system.h"

struct dir_list
{
  struct dir_list *parent;
  ino_t ino;
  dev_t dev;
};

struct entry
{
  ino_t ino;
  dev_t dev;
  char *node;			/* Path name, or &new_file for new inodes.  */
  struct entry *coll_link;	/* 0 = entry not occupied.  */
};

struct htab
{
  unsigned modulus;		/* Size of the `hash' pointer vector.  */
  struct entry *entry_tab;	/* Pointer to dynamically growing vector.  */
  unsigned entry_tab_size;	/* Size of current `entry_tab' allocation.  */
  unsigned first_free_entry;	/* Index in `entry_tab'.  */
  struct entry *hash[1];	/* Vector of pointers in `entry_tab'.  */
};

extern int exit_status;
extern struct htab *htab;

/* For created inodes, a pointer in the search structure to this
   character identifies that the inode as new.  */
extern char new_file;

char *basename ();
char *xmalloc ();
char *xrealloc ();
void forget_copied ();
void forget_all ();
int copy_reg ();
void hash_init ();
char *remember_copied ();
int remember_created ();
void error ();
void usage ();
char *savedir ();
char *stpcpy ();
int yesno ();
int do_copy ();
int copy ();
int copy_dir ();
void strip_trailing_slashes ();
int is_ancestor ();

#ifndef S_IWRITE
#define S_IWRITE S_IWUSR
#define S_IEXEC S_IXUSR
#endif

/* stat and lstat need to be declared because we use their addresses. */
int stat ();
int lstat ();
