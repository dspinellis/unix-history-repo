/* Synchronous subprocess invocation for GNU Emacs.
   Copyright (C) 1985, 1986, 1987, 1988, 1990 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <signal.h>

#include "config.h"

#include <sys/types.h>
#define PRIO_PROCESS 0
#include <sys/file.h>
#ifdef USG5
#include <fcntl.h>
#endif

#ifndef O_RDONLY
#define O_RDONLY 0
#endif

#ifndef O_WRONLY
#define O_WRONLY 1
#endif

#include "lisp.h"
#include "commands.h"
#include "buffer.h"
#include "paths.h"

#define max(a, b) ((a) > (b) ? (a) : (b))

Lisp_Object Vexec_path, Vexec_directory;

Lisp_Object Vshell_file_name;

#ifndef MAINTAIN_ENVIRONMENT
/* List of strings to append to front of environment of
   all subprocesses when they are started.  */

Lisp_Object Vprocess_environment;
#endif

#ifdef BSD4_1
/* Set nonzero when a synchronous subprocess is made,
   and set to zero again when it is observed to die.
   We wait for this to be zero in order to wait for termination.  */
int synch_process_pid;
#endif /* BSD4_1 */

Lisp_Object
call_process_cleanup (fdpid)
     Lisp_Object fdpid;
{
  register Lisp_Object fd, pid;
  fd = Fcar (fdpid);
  pid = Fcdr (fdpid);
  close (XFASTINT (fd));
  kill (XFASTINT (pid), SIGKILL);
  return Qnil;
}

#ifdef VMS
extern noshare char **environ;
#else
extern char **environ;
#endif

DEFUN ("call-process", Fcall_process, Scall_process, 1, MANY, 0,
  "Call PROGRAM in separate process.\n\
Program's input comes from file INFILE (nil means /dev/null).\n\
Insert output in BUFFER before point; t means current buffer;\n\
 nil for BUFFER means discard it; 0 means discard and don't wait.\n\
Fourth arg DISPLAY non-nil means redisplay buffer as output is inserted.\n\
Remaining arguments are strings passed as command arguments to PROGRAM.\n\
This function waits for PROGRAM to terminate;\n\
if you quit, the process is killed.")
  (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  Lisp_Object display, buffer, path;
  int fd[2];
  int filefd;
  register int pid;
  char buf[1024];
  int count = specpdl_ptr - specpdl;
  register unsigned char **new_argv
    = (unsigned char **) alloca ((max (2, nargs - 2)) * sizeof (char *));
  struct buffer *old = current_buffer;

  CHECK_STRING (args[0], 0);

  if (nargs <= 1 || NULL (args[1]))
#ifdef VMS
    args[1] = build_string ("NLA0:");
#else
    args[1] = build_string ("/dev/null");
#endif /* not VMS */
  else
    args[1] = Fexpand_file_name (args[1], current_buffer->directory);

  CHECK_STRING (args[1], 1);

  {
    register Lisp_Object tem;
    buffer = tem = args[2];
    if (nargs <= 2)
      buffer = Qnil;
    else if (!(EQ (tem, Qnil) || EQ (tem, Qt)
	       || XFASTINT (tem) == 0))
      {
	buffer = Fget_buffer (tem);
	CHECK_BUFFER (buffer, 2);
      }
  }

  display = nargs >= 3 ? args[3] : Qnil;

  {
    register int i;
    for (i = 4; i < nargs; i++)
      {
	CHECK_STRING (args[i], i);
	new_argv[i - 3] = XSTRING (args[i])->data;
      }
    /* Program name is first command arg */
    new_argv[0] = XSTRING (args[0])->data;
    new_argv[i - 3] = 0;
  }

  filefd = open (XSTRING (args[1])->data, O_RDONLY, 0);
  if (filefd < 0)
    {
      report_file_error ("Opening process input file", Fcons (args[1], Qnil));
    }
  /* Search for program; barf if not found.  */
  openp (Vexec_path, args[0], "", &path, 1);
  if (NULL (path))
    {
      close (filefd);
      report_file_error ("Searching for program", Fcons (args[0], Qnil));
    }
  new_argv[0] = XSTRING (path)->data;

  if (XTYPE (buffer) == Lisp_Int)
#ifdef VMS
    fd[1] = open ("NLA0:", 0), fd[0] = -1;
#else
    fd[1] = open ("/dev/null", O_WRONLY), fd[0] = -1;
#endif /* not VMS */
  else
    {
      pipe (fd);
#if 0
      /* Replaced by close_process_descs */
      set_exclusive_use (fd[0]);
#endif
    }

  {
    /* child_setup must clobber environ in systems with true vfork.
       Protect it from permanent change.  */
    register char **save_environ = environ;
    register int fd1 = fd[1];
    char **env;

#ifdef MAINTAIN_ENVIRONMENT
    env = (char **) alloca (size_of_current_environ ());
    get_current_environ (env);
#else
    env = environ;
#endif /* MAINTAIN_ENVIRONMENT */

    pid = vfork ();
#ifdef BSD4_1
    /* cause SIGCHLD interrupts to look for this pid. */
    synch_process_pid = pid;
#endif /* BSD4_1 */

    if (pid == 0)
      {
	if (fd[0] >= 0)
	  close (fd[0]);
#ifdef USG
#ifdef HAVE_PTYS
	setpgrp ();
#endif
#endif
	child_setup (filefd, fd1, fd1, new_argv, env);
      }

    environ = save_environ;

    close (filefd);
    close (fd1);
  }

  if (pid < 0)
    {
      close (fd[0]);
      report_file_error ("Doing vfork", Qnil);
    }

  if (XTYPE (buffer) == Lisp_Int)
    {
#ifndef subprocesses
      wait_without_blocking ();
#endif subprocesses
      return Qnil;
    }

  record_unwind_protect (call_process_cleanup,
			 Fcons (make_number (fd[0]), make_number (pid)));


  if (XTYPE (buffer) == Lisp_Buffer)
    Fset_buffer (buffer);

  immediate_quit = 1;
  QUIT;

  {
    register int nread;

    while ((nread = read (fd[0], buf, sizeof buf)) > 0)
      {
	immediate_quit = 0;
	if (!NULL (buffer))
	  insert (buf, nread);
	if (!NULL (display) && FROM_KBD)
	  redisplay_preserve_echo_area ();
	immediate_quit = 1;
	QUIT;
      }
  }

  /* Wait for it to terminate, unless it already has.  */
  wait_for_termination (pid);

  immediate_quit = 0;

  set_buffer_internal (old);

  unbind_to (count);

  return Qnil;
}

DEFUN ("call-process-region", Fcall_process_region, Scall_process_region,
  3, MANY, 0,
  "Send text from START to END to a process running PROGRAM.\n\
Delete the text if DELETE is non-nil.\n\
Put output in BUFFER, before point.  nil => discard it, t => current buffer.\n\
Sixth arg DISPLAY non-nil means redisplay buffer as output is inserted.\n\
Remaining args are passed to PROGRAM at startup as command args.\n\
This function normally waits for the process to terminate;\n\
if you quit, the process is killed.")
  (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  register Lisp_Object filename_string, start, end;
  char tempfile[20];

  strcpy (tempfile, "/tmp/emacsXXXXXX");
  mktemp (tempfile);

  filename_string = build_string (tempfile);
  start = args[0];
  end = args[1];
  Fwrite_region (start, end, filename_string, Qnil, Qlambda);

  if (!NULL (args[3]))
    Fdelete_region (start, end);

  args[3] = filename_string;
  Fcall_process (nargs - 2, args + 2);
  unlink (tempfile);
  return Qnil;
}

/* This is the last thing run in a newly forked inferior
   either synchronous or asynchronous.
   Copy descriptors IN, OUT and ERR as descriptors 0, 1 and 2.
   Initialize inferior's priority, pgrp, connected dir and environment.
   then exec another program based on new_argv.

   This function may change environ for the superior process.
   Therefore, the superior process must save and restore the value
   of environ around the vfork and the call to this function.

   ENV is the environment */

child_setup (in, out, err, new_argv, env)
     int in, out, err;
     register char **new_argv;
     char **env;
{
  register int pid = getpid();

  setpriority (PRIO_PROCESS, pid, 0);

#ifdef subprocesses
  /* Close Emacs's descriptors that this process should not have.  */
  close_process_descs ();
#endif

  /* Note that use of alloca is always safe here.  It's obvious for systems
     that do not have true vfork or that have true (stack) alloca.
     If using vfork and C_ALLOCA it is safe because that changes
     the superior's static variables as if the superior had done alloca
     and will be cleaned up in the usual way.  */

  if (XTYPE (current_buffer->directory) == Lisp_String)
    {
      register unsigned char *temp;
      register int i;

      i = XSTRING (current_buffer->directory)->size;
      temp = (unsigned char *) alloca (i + 2);
      bcopy (XSTRING (current_buffer->directory)->data, temp, i);
      if (temp[i - 1] != '/') temp[i++] = '/';
      temp[i] = 0;
      chdir (temp);
    }

#ifndef MAINTAIN_ENVIRONMENT
  /* Set `env' to a vector of the strings in Vprocess_environment.  */
  {
    register Lisp_Object tem;
    register char **new_env;
    register int new_length;

    new_length = 0;
    for (tem = Vprocess_environment;
	 (XTYPE (tem) == Lisp_Cons
	  && XTYPE (XCONS (tem)->car) == Lisp_String);
	 tem = XCONS (tem)->cdr)
      new_length++;

    /* new_length + 1 to include terminating 0 */
    env = new_env = (char **) alloca ((new_length + 1) * sizeof (char *));

    /* Copy the env strings into new_env.  */
    for (tem = Vprocess_environment;
	 (XTYPE (tem) == Lisp_Cons
	  && XTYPE (XCONS (tem)->car) == Lisp_String);
	 tem = XCONS (tem)->cdr)
      *new_env++ = (char *) XSTRING (XCONS (tem)->car)->data;
    *new_env = 0;
  }
#endif /* Not MAINTAIN_ENVIRONMENT */

  close (0);
  close (1);
  close (2);

  dup2 (in, 0);
  dup2 (out, 1);
  dup2 (err, 2);
  close (in);
  close (out);
  close (err);

#ifdef USG
#ifndef HAVE_PTYS
  setpgrp ();			/* No arguments but equivalent in this case */
#endif
#else
  setpgrp (pid, pid);
#endif /* USG */
  setpgrp_of_tty (pid);

#ifdef vipc
  something missing here;
#endif vipc

  /* execvp does not accept an environment arg so the only way
     to pass this environment is to set environ.  Our caller
     is responsible for restoring the ambient value of environ.  */
  environ = env;
  execvp (new_argv[0], new_argv);

  write (1, "Couldn't exec the program ", 26);
  write (1, new_argv[0], strlen (new_argv[0]));
  _exit (1);
}

init_callproc ()
{
  register char * sh;
  extern char **environ;
  register char **envp;
  Lisp_Object execdir;

  /* Turn PATH_EXEC into a path.  `==' is just a string which we know
     will not be the name of an environment variable.  */
  Vexec_path = decode_env_path ("==", PATH_EXEC);
  Vexec_directory = Ffile_name_as_directory (Fcar (Vexec_path));
  Vexec_path = nconc2 (decode_env_path ("PATH", ""), Vexec_path);

  execdir = Fdirectory_file_name (Vexec_directory);
  if (access (XSTRING (execdir)->data, 0) < 0)
    {
      printf ("Warning: executable/documentation dir (%s) does not exist.\n",
	      XSTRING (Vexec_directory)->data);
      sleep (2);
    }

  sh = (char *) egetenv ("SHELL");
  Vshell_file_name = build_string (sh ? sh : "/bin/sh");

#ifndef MAINTAIN_ENVIRONMENT
  /* The equivalent of this operation was done
     in init_environ in environ.c if MAINTAIN_ENVIRONMENT */
  Vprocess_environment = Qnil;
#ifndef CANNOT_DUMP
  if (initialized)
#endif
    for (envp = environ; *envp; envp++)
      Vprocess_environment = Fcons (build_string (*envp),
				    Vprocess_environment);
#endif /* MAINTAIN_ENVIRONMENT */
}

syms_of_callproc ()
{
  DEFVAR_LISP ("shell-file-name", &Vshell_file_name,
    "*File name to load inferior shells from.\n\
Initialized from the SHELL environment variable.");

  DEFVAR_LISP ("exec-path", &Vexec_path,
    "*List of directories to search programs to run in subprocesses.\n\
Each element is a string (directory name) or nil (try default directory).");

  DEFVAR_LISP ("exec-directory", &Vexec_directory,
    "Directory that holds programs that come with GNU Emacs,\n\
intended for Emacs to invoke.");

#ifndef MAINTAIN_ENVIRONMENT
  DEFVAR_LISP ("process-environment", &Vprocess_environment,
    "List of strings to append to environment of subprocesses that are started.\n\
Each string should have the format ENVVARNAME=VALUE.");
#endif

  defsubr (&Scall_process);
  defsubr (&Scall_process_region);
}
