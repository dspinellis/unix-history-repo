/* cp.c  -- file copying (main routines)
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

   Written by Torbjorn Granlund, David MacKenzie, and Jim Meyering. */

#ifdef _AIX
 #pragma alloca
#endif
#include <stdio.h>
#include <getopt.h>
#include "cp.h"
#include "backupfile.h"

/* Used by do_copy, make_path, and re_protect
   to keep a list of leading directories whose protections
   need to be fixed after copying. */
struct dir_attr
{
  int is_new_dir;
  int slash_offset;
  struct dir_attr *next;
};

char *dirname ();
enum backup_type get_version ();
int eaccess_stat ();

static int make_path ();
static int re_protect ();

/* Initial number of entries in each hash table entry's table of inodes.  */
#define INITIAL_HASH_MODULE 100

/* Initial number of entries in the inode hash table.  */
#define INITIAL_ENTRY_TAB_SIZE 70

/* A pointer to either lstat or stat, depending on
   whether dereferencing of symlinks is done.  */
int (*xstat) ();

/* The invocation name of this program.  */
char *program_name;

/* If nonzero, copy all files except directories and, if not dereferencing
   them, symbolic links, as if they were regular files. */
int flag_copy_as_regular = 1;

/* If nonzero, dereference symbolic links (copy the files they point to). */
int flag_dereference = 1;

/* If nonzero, remove existing target nondirectories. */
int flag_force = 0;

/* If nonzero, create hard links instead of copying files.
   Create target directories as usual. */
int flag_hard_link = 0;

/* If nonzero, query before overwriting existing targets with regular files. */
int flag_interactive = 0;

/* If nonzero, the command "cp x/e_file e_dir" uses "e_dir/x/e_file"
   as its target instead of the usual "e_dir/e_file." */
int flag_path = 0;

/* If nonzero, give the copies the original files' permissions,
   ownership, and timestamps. */
int flag_preserve = 0;

/* If nonzero, copy directories recursively and copy special files
   as themselves rather than copying their contents. */
int flag_recursive = 0;

/* If nonzero, create symbolic links instead of copying files.
   Create target directories as usual. */
int flag_symbolic_link = 0;

/* If nonzero, when copying recursively, skip any subdirectories that are
   on different filesystems from the one we started on. */
int flag_one_file_system = 0;

/* If nonzero, do not copy a nondirectory that has an existing destination
   with the same or newer modification time. */
int flag_update = 0;

/* If nonzero, display the names of the files before copying them. */
int flag_verbose = 0;

/* The error code to return to the system. */
int exit_status = 0;

/* The bits to preserve in created files' modes. */
int umask_kill;

struct option long_opts[] =
{
  {"archive", 0, NULL, 'a'},
  {"backup", 0, NULL, 'b'},
  {"force", 0, NULL, 'f'},
  {"interactive", 0, NULL, 'i'},
  {"link", 0, NULL, 'l'},
  {"no-dereference", 0, &flag_dereference, 0},
  {"one-file-system", 0, &flag_one_file_system, 1},
  {"path", 0, &flag_path, 1},
  {"preserve", 0, &flag_preserve, 1},
  {"recursive", 0, NULL, 'R'},
  {"suffix", 1, NULL, 'S'},
  {"symbolic-link", 0, NULL, 's'},
  {"update", 0, &flag_update, 1},
  {"verbose", 0, &flag_verbose, 1},
  {"version-control", 1, NULL, 'V'},
  {NULL, 0, NULL, 0}
};

void
main (argc, argv)
     int argc;
     char *argv[];
{
  int c;
  int make_backups = 0;
  char *version;

  program_name = argv[0];

  version = getenv ("SIMPLE_BACKUP_SUFFIX");
  if (version)
    simple_backup_suffix = version;
  version = getenv ("VERSION_CONTROL");

  /* Find out the current file creation mask, to knock the right bits
     when using chmod.  The creation mask is set to to be liberal, so
     that created directories can be written, even if it would not
     have been allowed with the mask this process was started with.  */

  umask_kill = 0777777 ^ umask (0);

  while ((c = getopt_long (argc, argv, "abdfilprsuvxPRS:V:", long_opts,
			   (int *) 0)) != EOF)
    {
      switch (c)
	{
	case 0:
	  break;

	case 'a':		/* Like -dpR. */
	  flag_dereference = 0;
	  flag_preserve = 1;
	  flag_recursive = 1;
	  flag_copy_as_regular = 0;
	  break;

	case 'b':
	  make_backups = 1;
	  break;

	case 'd':
	  flag_dereference = 0;
	  break;

	case 'f':
	  flag_force = 1;
	  flag_interactive = 0;
	  break;

	case 'i':
	  flag_force = 0;
	  flag_interactive = 1;
	  break;

	case 'l':
	  flag_hard_link = 1;
	  break;

	case 'p':
	  flag_preserve = 1;
	  break;

	case 'P':
	  flag_path = 1;
	  break;

	case 'r':
	  flag_recursive = 1;
	  flag_copy_as_regular = 1;
	  break;

	case 'R':
	  flag_recursive = 1;
	  flag_copy_as_regular = 0;
	  break;

	case 's':
#ifdef S_ISLNK
	  flag_symbolic_link = 1;
#else
	  error (0, 0, "symbolic links not supported; making hard links");
	  flag_hard_link = 1;
#endif
	  break;

	case 'u':
	  flag_update = 1;
	  break;

	case 'v':
	  flag_verbose = 1;
	  break;

	case 'x':
	  flag_one_file_system = 1;
	  break;

	case 'S':
	  simple_backup_suffix = optarg;
	  break;

	case 'V':
	  version = optarg;
	  break;

	default:
	  usage ((char *) 0);
	}
    }

  if (flag_hard_link && flag_symbolic_link)
    usage ("cannot make both hard and symbolic links");

  if (make_backups)
    backup_type = get_version (version);

  if (flag_preserve == 1)
    umask_kill = 0777777;

  /* The key difference between -d (+no-dereference) and not is the version
     of `stat' to call.  */

  if (flag_dereference)
    xstat = stat;
  else
    xstat = lstat;

  /* Allocate space for remembering copied and created files.  */

  hash_init (INITIAL_HASH_MODULE, INITIAL_ENTRY_TAB_SIZE);

  exit_status |= do_copy (argc, argv);

  exit (exit_status);
}

/* Scan the arguments, and copy each by calling copy.
   Return 0 if successful, 1 if any errors occur. */

int
do_copy (argc, argv)
     int argc;
     char *argv[];
{
  char *target;
  struct stat sb;
  int new_dst = 0;
  int ret = 0;

  if (optind >= argc)
    usage ("missing file arguments");
  if (optind >= argc - 1)
    usage ("missing file argument");

  target = argv[argc - 1];

  strip_trailing_slashes (target);

  if (lstat (target, &sb))
    {
      if (errno != ENOENT)
	{
	  error (0, errno, "%s", target);
	  return 1;
	}
      else
	new_dst = 1;
    }
  else
    {
      struct stat sbx;

      /* If `target' is not a symlink to a nonexistent file, use
	 the results of stat instead of lstat, so we can copy files
	 into symlinks to directories. */
      if (stat (target, &sbx) == 0)
	sb = sbx;
    }

  if (!new_dst && S_ISDIR (sb.st_mode))
    {
      /* cp file1...filen edir
	 Copy the files `file1' through `filen'
	 to the existing directory `edir'. */

      for (;;)
	{
	  char *arg;
	  char *ap;
	  char *dst_path;
	  int parent_exists = 1; /* True if dirname (dst_path) exists. */
	  struct dir_attr *attr_list;

	  arg = argv[optind];

	  strip_trailing_slashes (arg);

	  if (flag_path)
	    {
	      /* Append all of `arg' to `target'.  */
	      dst_path = xmalloc (strlen (target) + strlen (arg) + 2);
	      stpcpy (stpcpy (stpcpy (dst_path, target), "/"), arg);

	      /* For +path, we have to make sure that the directory
	         dirname (dst_path) exists.  We may have to create a few
	         leading directories. */
	      parent_exists = !make_path (dst_path,
					  strlen (target) + 1, 0700,
					  flag_verbose ? "%s -> %s\n" :
					  (char *) NULL,
					  &attr_list, &new_dst);
	    }
  	  else
  	    {
	      /* Append the last component of `arg' to `target'.  */

	      ap = basename (arg);
	      /* For `cp -R source/.. target', don't copy into `target/..'. */
	      if (!strcmp (ap, ".."))
		dst_path = target;
	      else
		{
		  dst_path = xmalloc (strlen (target) + strlen (ap) + 2);
		  stpcpy (stpcpy (stpcpy (dst_path, target), "/"), ap);
		}
	    }

	  if (!parent_exists)
	    {
	      /* make_path failed, so we shouldn't even attempt the copy. */
	      ret = 1;
  	    }
	  else
	    {
	      ret |= copy (arg, dst_path, new_dst, 0, (struct dir_list *) 0);
	      forget_all ();
  
	      if (flag_path)
		{
		  ret |= re_protect (dst_path, strlen (target) + 1,
					attr_list);
		}
	    }

	  ++optind;
	  if (optind == argc - 1)
	    break;
	}
      return ret;
    }
  else if (argc - optind == 2)
    {
      if (flag_path)
	usage ("when preserving paths, last argument must be a directory");
      return copy (argv[optind], target, new_dst, 0, (struct dir_list *) 0);
    }
  else
    usage ("when copying multiple files, last argument must be a directory");
}

/* Copy the file SRC_PATH to the file DST_PATH.  The files may be of
   any type.  NEW_DST should be non-zero if the file DST_PATH cannot
   exist because its parent directory was just created; NEW_DST should
   be zero if DST_PATH might already exist.  DEVICE is the device
   number of the parent directory, or 0 if the parent of this file is
   not known.  ANCESTORS points to a linked, null terminated list of
   devices and inodes of parent directories of SRC_PATH.
   Return 0 if successful, 1 if an error occurs. */

int
copy (src_path, dst_path, new_dst, device, ancestors)
     char *src_path;
     char *dst_path;
     int new_dst;
     dev_t device;
     struct dir_list *ancestors;
{
  struct stat src_sb;
  struct stat dst_sb;
  int src_mode;
  int src_type;
  char *earlier_file;
  char *dst_backup = NULL;
  int dir_mode_changed = 0;

  if ((*xstat) (src_path, &src_sb))
    {
      error (0, errno, "%s", src_path);
      return 1;
    }

  /* Are we crossing a file system boundary?  */
  if (flag_one_file_system && device != 0 && device != src_sb.st_dev)
    return 0;

  /* We wouldn't insert a node unless nlink > 1, except that we need to
     find created files so as to not copy infinitely if a directory is
     copied into itself.  */

  earlier_file = remember_copied (dst_path, src_sb.st_ino, src_sb.st_dev);

  /* Did we just create this file?  */

  if (earlier_file == &new_file)
    return 0;

  src_mode = src_sb.st_mode;
  src_type = src_sb.st_mode;

  if (S_ISDIR (src_type) && !flag_recursive)
    {
      error (0, 0, "%s: omitting directory", src_path);
      return 1;
    }

  if (!new_dst)
    {
      if ((*xstat) (dst_path, &dst_sb))
	{
	  if (errno != ENOENT)
	    {
	      error (0, errno, "%s", dst_path);
	      return 1;
	    }
	  else
	    new_dst = 1;
	}
      else
	{
	  /* The file exists already.  */

	  if (src_sb.st_ino == dst_sb.st_ino && src_sb.st_dev == dst_sb.st_dev)
	    {
	      if (flag_hard_link)
		return 0;

	      error (0, 0, "`%s' and `%s' are the same file",
		     src_path, dst_path);
	      return 1;
	    }

	  if (!S_ISDIR (src_type))
	    {
	      if (S_ISDIR (dst_sb.st_mode))
		{
		  error (0, 0,
			 "%s: cannot overwrite directory with non-directory",
			 dst_path);
		  return 1;
		}

	      if (flag_update && src_sb.st_mtime <= dst_sb.st_mtime)
		return 0;
	    }

	  if (S_ISREG (src_type) && !flag_force)
	    {
	      if (flag_interactive)
		{
		  if (eaccess_stat (&dst_sb, W_OK) != 0)
		    fprintf (stderr,
			     "%s: overwrite `%s', overriding mode %04o? ",
			     program_name, dst_path, dst_sb.st_mode & 07777);
		  else
		    fprintf (stderr, "%s: overwrite `%s'? ",
			     program_name, dst_path);
		  if (!yesno ())
		    return 0;
		}
	    }

	  if (backup_type != none && !S_ISDIR (dst_sb.st_mode))
	    {
	      char *tmp_backup = find_backup_file_name (dst_path);
	      if (tmp_backup == NULL)
		error (1, 0, "virtual memory exhausted");
	      dst_backup = alloca (strlen (tmp_backup) + 1);
	      strcpy (dst_backup, tmp_backup);
	      free (tmp_backup);
	      if (rename (dst_path, dst_backup))
		{
		  if (errno != ENOENT)
		    {
		      error (0, errno, "cannot backup `%s'", dst_path);
		      return 1;
		    }
		  else
		    dst_backup = NULL;
		}
	      new_dst = 1;
	    }
	  else if (flag_force)
	    {
	      if (S_ISDIR (dst_sb.st_mode))
		{
		  /* Temporarily change mode to allow overwriting. */
		  if (eaccess_stat (&dst_sb, W_OK | X_OK) != 0)
		    {
		      if (chmod (dst_path, 0700))
			{
			  error (0, errno, "%s", dst_path);
			  return 1;
			}
		      else
			dir_mode_changed = 1;
		    }
		}
	      else
		{
		  if (unlink (dst_path) && errno != ENOENT)
		    {
		      error (0, errno, "cannot remove old link to `%s'",
			     dst_path);
		      return 1;
		    }
		  new_dst = 1;
		}
	    }
	}
    }

  /* If the source is a directory, we don't always create the target
     directory.  So +verbose should not announce anything until we're
     sure we'll create a directory. */
  if (flag_verbose && !S_ISDIR (src_type))
    printf ("%s -> %s\n", src_path, dst_path);

  /* Did we copy this inode somewhere else (in this command line argument)
     and therefore this is a second hard link to the inode?  */

  if (!flag_dereference && src_sb.st_nlink > 1 && earlier_file)
    {
      if (link (earlier_file, dst_path))
	{
	  error (0, errno, "%s", dst_path);
	  goto un_backup;
	}
      return 0;
    }

  if (S_ISDIR (src_type))
    {
      struct dir_list *dir;

      /* If this directory has been copied before during the
         recursion, there is a symbolic link to an ancestor
         directory of the symbolic link.  It is impossible to
         continue to copy this, unless we've got an infinite disk.  */

      if (is_ancestor (&src_sb, ancestors))
	{
	  error (0, 0, "%s: cannot copy cyclic symbolic link", src_path);
	  goto un_backup;
	}

      /* Insert the current directory in the list of parents.  */

      dir = (struct dir_list *) alloca (sizeof (struct dir_list));
      dir->parent = ancestors;
      dir->ino = src_sb.st_ino;
      dir->dev = src_sb.st_dev;

      if (new_dst || !S_ISDIR (dst_sb.st_mode))
	{
	  /* Create the new directory writable and searchable, so
             we can create new entries in it.  */

	  if (mkdir (dst_path, (src_mode & umask_kill) | 0700))
	    {
	      error (0, errno, "cannot create directory `%s'", dst_path);
	      goto un_backup;
	    }

	  /* Insert the created directory's inode and device
             numbers into the search structure, so that we can
             avoid copying it again.  */

	  if (remember_created (dst_path))
	    goto un_backup;

	  if (flag_verbose)
	    printf ("%s -> %s\n", src_path, dst_path);
	}

      /* Copy the contents of the directory.  */

      if (copy_dir (src_path, dst_path, new_dst, &src_sb, dir))
	return 1;
    }
#ifdef S_ISLNK
  else if (flag_symbolic_link)
    {
      if (*src_path == '/'
	  || (!strncmp (dst_path, "./", 2) && index (dst_path + 2, '/') == 0)
	  || index (dst_path, '/') == 0)
	{
	  if (symlink (src_path, dst_path))
	    {
	      error (0, errno, "%s", dst_path);
	      goto un_backup;
	    }
	  return 0;
	}
      else
	{
	  error (0, 0,
		 "%s: can only make relative symbolic links in current directory", dst_path);
	  goto un_backup;
	}
    }
#endif
  else if (flag_hard_link)
    {
      if (link (src_path, dst_path))
	{
	  error (0, errno, "cannot create link `%s'", dst_path);
	  goto un_backup;
	}
      return 0;
    }
  else if (S_ISREG (src_type)
	   || (flag_copy_as_regular && !S_ISDIR (src_type)
#ifdef S_ISLNK
	       && !S_ISLNK (src_type)
#endif
	       ))
    {
      if (copy_reg (src_path, dst_path))
	goto un_backup;
    }
  else
#ifdef S_ISFIFO
  if (S_ISFIFO (src_type))
    {
      if (mkfifo (dst_path, src_mode & umask_kill))
	{
	  error (0, errno, "cannot create fifo `%s'", dst_path);
	  goto un_backup;
	}
    }
  else
#endif
    if (S_ISBLK (src_type) || S_ISCHR (src_type)
#ifdef S_ISSOCK
	|| S_ISSOCK (src_type)
#endif
	)
    {
      if (mknod (dst_path, src_mode & umask_kill, src_sb.st_rdev))
	{
	  error (0, errno, "cannot create special file `%s'", dst_path);
	  goto un_backup;
	}
    }
  else
#ifdef S_ISLNK
#ifdef _AIX
#define LINK_BUF PATH_MAX
#else
#define LINK_BUF src_sb.st_size
#endif
  if (S_ISLNK (src_type))
    {
      char *link_val = (char *) alloca (LINK_BUF + 1);
      int link_size;

      link_size = readlink (src_path, link_val, LINK_BUF);
      if (link_size < 0)
	{
	  error (0, errno, "cannot read symbolic link `%s'", src_path);
	  goto un_backup;
	}
      link_val[link_size] = '\0';

      if (symlink (link_val, dst_path))
	{
	  error (0, errno, "cannot create symbolic link `%s'", dst_path);
	  goto un_backup;
	}
      return 0;
    }
  else
#endif
    {
      error (0, 0, "%s: unknown file type", src_path);
      goto un_backup;
    }

  if ((flag_preserve || new_dst)
      && (S_ISREG (src_type) || S_ISDIR (src_type)))
    {
      if (chmod (dst_path, src_mode & umask_kill))
	{
	  error (0, errno, "%s", dst_path);
	  return 1;
	}
    }
  else if (dir_mode_changed)
    {
      /* Reset the temporarily changed mode.  */
      if (chmod (dst_path, dst_sb.st_mode))
	{
	  error (0, errno, "%s", dst_path);
	  return 1;
	}
    }

  /* Adjust the times (and if possible, ownership) for the copy. */

  if (flag_preserve)
    {
      struct utimbuf utb;

      utb.actime = src_sb.st_atime;
      utb.modtime = src_sb.st_mtime;

      if (utime (dst_path, &utb))
	{
	  error (0, errno, "%s", dst_path);
	  return 1;
	}

      if (chown (dst_path, src_sb.st_uid, src_sb.st_gid) && errno != EPERM)
	{
	  error (0, errno, "%s", dst_path);
	  return 1;
	}
    }

  return 0;

un_backup:
  if (dst_backup)
    {
      if (rename (dst_backup, dst_path))
	error (0, errno, "cannot un-backup `%s'", dst_path);
    }
  return 1;
}

/* Ensure that the parent directory of CONST_DIRPATH exists, for
   the +path option.

   SRC_OFFSET is the index in CONST_DIRPATH (which is a destination
   path) of the beginning of the source directory name.
   Create any leading directories that don't already exist,
   giving them permissions MODE.
   If VERBOSE_FMT_STRING is nonzero, use it as a printf format
   string for printing a message after successfully making a directory.
   The format should take two string arguments: the names of the
   source and destination directories.
   Creates a linked list of attributes of intermediate directories,
   *ATTR_LIST, for re_protect to use after calling copy.
   Sets *NEW_DST to 1 if this function creates parent of CONST_DIRPATH.

   Return 0 if parent of CONST_DIRPATH exists as a directory with the proper
   permissions when done, otherwise 1. */

static int
make_path (const_dirpath, src_offset, mode, verbose_fmt_string,
	      attr_list, new_dst)
     char *const_dirpath;
     int src_offset;
     int mode;
     char *verbose_fmt_string;
     struct dir_attr **attr_list;
     int *new_dst;
{
  struct stat stats;
  char *dirpath;		/* A copy of CONST_DIRPATH we can change. */
  char *src;			/* Source name in `dirpath'. */
  char *tmp_dst_dirname;	/* Leading path of `dirpath', malloc. */
  char *dst_dirname;		/* Leading path of `dirpath', alloca. */

  dirpath = alloca (strlen (const_dirpath));
  strcpy (dirpath, const_dirpath);

  src = dirpath + src_offset;

  tmp_dst_dirname = dirname (dirpath); 
  dst_dirname = alloca (strlen (tmp_dst_dirname));
  strcpy (dst_dirname, tmp_dst_dirname);
  free (tmp_dst_dirname);

  *attr_list = NULL;

  if ((*xstat) (dst_dirname, &stats))
    {
      /* Parent of CONST_DIRNAME does not exist.
	 Make all missing intermediate directories. */
      char *slash;

      slash = src;
      while (*slash == '/')
	slash++;
      while (slash = index (slash, '/'))
	{
	  /* Add this directory to the list of directories whose modes need
	     fixing later. */
	  struct dir_attr *new =
	    (struct dir_attr *) xmalloc (sizeof (struct dir_attr));
	  new->slash_offset = slash - dirpath;
	  new->next = *attr_list;
	  *attr_list = new;

	  *slash = '\0';
	  if ((*xstat) (dirpath, &stats))
	    {
	      /* This element of the path does not exist.  We must set
		 *new_dst and new->is_new_dir inside this loop because,
		 for example, in the command `cp +path ../a/../b/c e_dir',
		 make_path creates only e_dir/../a if ./b already exists. */
	      *new_dst = 1;
	      new->is_new_dir = 1;
	      if (mkdir (dirpath, mode))
		{
		  error (0, errno, "cannot make directory `%s'", dirpath);
		  return 1;
		}
	      else
		{
		  if (verbose_fmt_string != NULL)
		    printf (verbose_fmt_string, src, dirpath);
		}
	    }
	  else if (!S_ISDIR (stats.st_mode))
	    {
	      error (0, 0, "`%s' exists but is not a directory", dirpath);
	      return 1;
	    }
	  else
	    {
	      new->is_new_dir = 0;
	      *new_dst = 0;
	    }
	  *slash++ = '/';

	  /* Avoid unnecessary calls to `stat' when given
	     pathnames containing multiple adjacent slashes.  */
	  while (*slash == '/')
	    slash++;
	}
    }

  /* We get here if the parent of `dirpath' already exists. */

  else if (!S_ISDIR (stats.st_mode))
    {
      error (0, 0, "`%s' exists but is not a directory", dst_dirname);
      return 1;
    }
  else if (chmod (dst_dirname, mode))
    {
      error (0, errno, "%s", dst_dirname);
      return 1;
    }
  else
    {
      *new_dst = 0;
    }
  return 0;
}

/* Ensure that the parent directories of CONST_DST_PATH have the
   correct protections, for the +path option.  This is done
   after all copying has been completed, to allow permissions
   that don't include user write/execute.

   SRC_OFFSET is the index in CONST_DST_PATH of the beginning of the
   source directory name.

   ATTR_LIST is a null-terminated linked list of structures that
   indicates the end of the filename of each intermediate directory
   in CONST_DST_PATH that may need to have its attributes changed.
   The command `cp +path +preserve a/b/c d/e_dir' changes the
   attributes of the directories d/e_dir/a and d/e_dir/a/b to match
   the corresponding source directories regardless of whether they
   existed before the `cp' command was given.

   Return 0 if the parent of CONST_DST_PATH and any intermediate
   directories specified by ATTR_LIST have the proper permissions
   when done, otherwise 1. */

static int
re_protect (const_dst_path, src_offset, attr_list)
     char *const_dst_path;
     int src_offset;
     struct dir_attr *attr_list;
{
  struct dir_attr *p;
  char *dst_path;		/* A copy of CONST_DST_PATH we can change. */
  char *src_path;		/* The source name in `dst_path'. */

  dst_path = alloca (strlen (const_dst_path));
  strcpy (dst_path, const_dst_path);
  src_path = dst_path + src_offset; 

  for (p = attr_list; p; p = p->next)
    {
      struct stat src_sb;

      dst_path[p->slash_offset] = '\0';

      if ((*xstat) (src_path, &src_sb))
	{
	  error (0, errno, "%s", src_path);
	  return 1;
	}

      if (flag_preserve || p->is_new_dir)
	{
	  if (chmod (dst_path, src_sb.st_mode & umask_kill))
	    {
	      error (0, errno, "%s", dst_path);
	      return 1;
	    }
	}

      /* Adjust the times (and if possible, ownership) for the copy. */

      if (flag_preserve)
	{
	  struct utimbuf utb;

	  utb.actime = src_sb.st_atime;
	  utb.modtime = src_sb.st_mtime;

	  if (utime (dst_path, &utb))
	    {
	      error (0, errno, "%s", dst_path);
	      return 1;
	    }

	  if (chown (dst_path, src_sb.st_uid, src_sb.st_gid) && errno != EPERM)
	    {
	      error (0, errno, "%s", dst_path);
	      return 1;
	    }
	}

      dst_path[p->slash_offset] = '/';
    }
  return 0;
}

/* Read the contents of the directory SRC_PATH_IN, and recursively
   copy the contents to DST_PATH_IN.  NEW_DST is non-zero if
   DST_PATH_IN is a directory that was created previously in the
   recursion.   SRC_SB and ANCESTORS describe SRC_PATH_IN.
   Return 0 if successful, -1 if an error occurs. */

int
copy_dir (src_path_in, dst_path_in, new_dst, src_sb, ancestors)
     char *src_path_in;
     char *dst_path_in;
     int new_dst;
     struct stat *src_sb;
     struct dir_list *ancestors;
{
  char *name_space;
  char *namep;
  char *src_path;
  char *dst_path;
  int ret = 0;

  errno = 0;
  name_space = savedir (src_path_in, src_sb->st_size);
  if (name_space == 0)
    {
      if (errno)
	{
	  error (0, errno, "%s", src_path_in);
	  return -1;
	}
      else
	error (1, 0, "virtual memory exhausted");
    }

  namep = name_space;
  while (*namep != '\0')
    {
      int fn_length = strlen (namep) + 1;

      dst_path = xmalloc (strlen (dst_path_in) + fn_length + 1);
      src_path = xmalloc (strlen (src_path_in) + fn_length + 1);

      stpcpy (stpcpy (stpcpy (src_path, src_path_in), "/"), namep);
      stpcpy (stpcpy (stpcpy (dst_path, dst_path_in), "/"), namep);

      ret |= copy (src_path, dst_path, new_dst, src_sb->st_dev, ancestors);

      /* Free the memory for `src_path'.  The memory for `dst_path'
	 cannot be deallocated, since it is used to create multiple
	 hard links.  */

      free (src_path);

      namep += fn_length;
    }
  free (name_space);
  return -ret;
}

/* Copy a regular file from SRC_PATH to DST_PATH.
   If the source file contains holes, copies holes and blocks of zeros
   in the source file as holes in the target file.
   (Holes are read as zeroes by the `read' system call.)
   Return 0 if successful, -1 if an error occurred. */

int
copy_reg (src_path, dst_path)
     char *src_path;
     char *dst_path;
{
  char *buf;
  int buf_size;
  int target_desc;
  int source_desc;
  int n_read;
  int n_written;
  struct stat sb;
  char *cp;
  int *ip;
  int return_val = 0;
  long n_read_total = 0;
  int last_write_made_hole = 0;
  int make_holes = 0;

  source_desc = open (src_path, O_RDONLY);
  if (source_desc < 0)
    {
      error (0, errno, "%s", src_path);
      return -1;
    }

  /* Create the new regular file with small permissions initially,
     to not create a security hole.  */

  target_desc = open (dst_path, O_WRONLY | O_CREAT | O_TRUNC, 0600);
  if (target_desc < 0)
    {
      error (0, errno, "cannot create regular file `%s'", dst_path);
      return_val = -1;
      goto ret2;
    }

  /* Find out the optimal buffer size.  */

  if (fstat (target_desc, &sb))
    {
      error (0, errno, "%s", dst_path);
      return_val = -1;
      goto ret;
    }

  buf_size = ST_BLKSIZE (sb);

#ifndef ST_BLOCKS_MISSING
  if (S_ISREG (sb.st_mode))
    {
      /* Find out whether the file contains any sparse blocks. */

      if (fstat (source_desc, &sb))
	{
	  error (0, errno, "%s", src_path);
	  return_val = -1;
	  goto ret;
	}

      /* If the file has fewer blocks than would normally
	 be needed for a file of its size, then
	 at least one of the blocks in the file is a hole. */
      if (S_ISREG (sb.st_mode) &&
	  sb.st_size - (sb.st_blocks * DEV_BSIZE) >= DEV_BSIZE)
	make_holes = 1;
    }
#endif

  /* Make a buffer with space for a sentinel at the end.  */

  buf = (char *) alloca (buf_size + sizeof (int));

  for (;;)
    {
      n_read = read (source_desc, buf, buf_size);
      if (n_read < 0)
	{
	  error (0, errno, "%s", src_path);
	  return_val = -1;
	  goto ret;
	}
      if (n_read == 0)
	break;

      n_read_total += n_read;

      ip = 0;
      if (make_holes)
	{
	  buf[n_read] = 1;	/* Sentinel to stop loop.  */

	  /* Find first non-zero *word*, or the word with the sentinel.  */

	  ip = (int *) buf;
	  while (*ip++ == 0)
	    ;

	  /* Find the first non-zero *byte*, or the sentinel.  */

	  cp = (char *) (ip - 1);
	  while (*cp++ == 0)
	    ;

	  /* If we found the sentinel, the whole input block was zero,
	     and we can make a hole.  */

	  if (cp > buf + n_read)
	    {
	      /* Make a hole.  */
	      if (lseek (target_desc, (off_t) n_read, SEEK_CUR) < 0L)
		{
		  error (0, errno, "%s", dst_path);
		  return_val = -1;
		  goto ret;
		}
	      last_write_made_hole = 1;
	    }
	  else
	    /* Clear to indicate that a normal write is needed. */
	    ip = 0;
	}
      if (ip == 0)
	{
	  n_written = write (target_desc, buf, n_read);
	  if (n_written < n_read)
	    {
	      error (0, errno, "%s", dst_path);
	      return_val = -1;
	      goto ret;
	    }
	  last_write_made_hole = 0;
	}
    }

  /* If the file ends with a `hole', something needs to be written at
     the end.  Otherwise the kernel would truncate the file at the end
     of the last write operation.  */

  if (last_write_made_hole)
    {
#ifndef FTRUNCATE_MISSING
      /* Write a null character and truncate it again.  */
      if (write (target_desc, "", 1) != 1
	  || ftruncate (target_desc, n_read_total) < 0)
#else
      /* Seek backwards one character and write a null.  */
      if (lseek (target_desc, (off_t) -1, SEEK_CUR) < 0L
	  || write (target_desc, "", 1) != 1)
#endif
	{
	  error (0, errno, "%s", dst_path);
	  return_val = -1;
	}
    }

ret:
  if (close (target_desc) < 0)
    {
      error (0, errno, "%s", dst_path);
      return_val = -1;
    }
ret2:
  if (close (source_desc) < 0)
    {
      error (0, errno, "%s", src_path);
      return_val = -1;
    }

  return return_val;
}
