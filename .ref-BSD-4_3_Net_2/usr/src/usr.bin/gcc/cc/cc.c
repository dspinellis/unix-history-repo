/*-
 * This code is derived from software copyrighted by the Free Software
 * Foundation.
 *
 * Modified 1991 by Donn Seeley at UUNET Technologies, Inc.
 */

#ifndef lint
static char sccsid[] = "@(#)cc.c	6.7 (Berkeley) 5/8/91";
#endif /* not lint */

/*-
   Copyright (C) 1987,1989 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

This paragraph is here to try to keep Sun CC from dying.
The number of chars here seems crucial!!!!  */

void record_temp_file ();

/* This program is the user interface to the C compiler and possibly to
other compilers.  It is used because compilation is a complicated procedure
which involves running several programs and passing temporary files between
them, forwarding the users switches to those programs selectively,
and deleting the temporary files at the end.

CC recognizes how to compile each input file by suffixes in the file names.
Once it knows which kind of compilation to perform, the procedure for
compilation is specified by a string called a "spec".

Specs are strings containing lines, each of which (if not blank)
is made up of a program name, and arguments separated by spaces.
The program name must be exact and start from root, since no path
is searched and it is unreliable to depend on the current working directory.
Redirection of input or output is not supported; the subprograms must
accept filenames saying what files to read and write.

In addition, the specs can contain %-sequences to substitute variable text
or for conditional text.  Here is a table of all defined %-sequences.
Note that spaces are not generated automatically around the results of
expanding these sequences; therefore, you can concatenate them together
or with constant text in a single argument.

 %%	substitute one % into the program name or argument.
 %i     substitute the name of the input file being processed.
 %b     substitute the basename of the input file being processed.
	This is the substring up to (and not including) the last period.
 %g     substitute the temporary-file-name-base.  This is a string chosen
	once per compilation.  Different temporary file names are made by
	concatenation of constant strings on the end, as in `%g.s'.
	%g also has the same effect of %d.
 %d	marks the argument containing or following the %d as a
	temporary file name, so that that file will be deleted if CC exits
	successfully.  Unlike %g, this contributes no text to the argument.
 %w	marks the argument containing or following the %w as the
	"output file" of this compilation.  This puts the argument
	into the sequence of arguments that %o will substitute later.
 %W{...}
	like %{...} but mark last argument supplied within
	as a file to be deleted on failure.
 %o	substitutes the names of all the output files, with spaces
	automatically placed around them.  You should write spaces
	around the %o as well or the results are undefined.
	%o is for use in the specs for running the linker.
	Input files whose names have no recognized suffix are not compiled
	at all, but they are included among the output files, so they will
	be linked.
 %p	substitutes the standard macro predefinitions for the
	current target machine.  Use this when running cpp.
 %P	like %p, but puts `__' before and after the name of each macro.
	This is for ANSI C.
 %s     current argument is the name of a library or startup file of some sort.
        Search for that file in a standard list of directories
	and substitute the full pathname found.
 %eSTR  Print STR as an error message.  STR is terminated by a newline.
        Use this when inconsistent options are detected.
 %a     process ASM_SPEC as a spec.
        This allows config.h to specify part of the spec for running as.
 %l     process LINK_SPEC as a spec.
 %L     process LIB_SPEC as a spec.
 %S     process STARTFILE_SPEC as a spec.  A capital S is actually used here.
 %E     process ENDFILE_SPEC as a spec.  A capital E is actually used here.
 %c	process SIGNED_CHAR_SPEC as a spec.
 %C     process CPP_SPEC as a spec.  A capital C is actually used here.
 %1	process CC1_SPEC as a spec.
 %{S}   substitutes the -S switch, if that switch was given to CC.
	If that switch was not specified, this substitutes nothing.
	Here S is a metasyntactic variable.
 %{S*}  substitutes all the switches specified to CC whose names start
	with -S.  This is used for -o, -D, -I, etc; switches that take
	arguments.  CC considers `-o foo' as being one switch whose
	name starts with `o'.  %{o*} would substitute this text,
	including the space; thus, two arguments would be generated.
 %{S:X} substitutes X, but only if the -S switch was given to CC.
 %{!S:X} substitutes X, but only if the -S switch was NOT given to CC.
 %{|S:X} like %{S:X}, but if no S switch, substitute `-'.
 %{|!S:X} like %{!S:X}, but if there is an S switch, substitute `-'.

The conditional text X in a %{S:X} or %{!S:X} construct may contain
other nested % constructs or spaces, or even newlines.
They are processed as usual, as described above.

The character | is used to indicate that a command should be piped to
the following command, but only if -pipe is specified.

Note that it is built into CC which switches take arguments and which
do not.  You might think it would be useful to generalize this to
allow each compiler's spec to say which switches take arguments.  But
this cannot be done in a consistent fashion.  CC cannot even decide
which input files have been specified without knowing which switches
take arguments, and it must know which input files to compile in order
to tell which compilers to run.

CC also knows implicitly that arguments starting in `-l' are to
be treated as compiler output files, and passed to the linker in their proper
position among the other output files.

*/

#include <stdio.h>
#include <sys/types.h>
#include <signal.h>
#include <sys/file.h>

#include "config.h"
#include "obstack.h"
#include "gvarargs.h"

#ifdef USG
#ifndef R_OK
#define R_OK 4
#define W_OK 2
#define X_OK 1
#endif

#define vfork fork
#endif /* USG */

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free
extern int xmalloc ();
extern void free ();

/* If a stage of compilation returns an exit status >= 1,
   compilation of that file ceases.  */

#define MIN_FATAL_STATUS 1

/* This is the obstack which we use to allocate many strings.  */

struct obstack obstack;

char *handle_braces ();
char *save_string ();
char *concat ();
int do_spec ();
int do_spec_1 ();
char *find_file ();
static char *find_exec_file ();
void validate_switches ();
void validate_all_switches ();
void fancy_abort ();

/* config.h can define ASM_SPEC to provide extra args to the assembler
   or extra switch-translations.  */
#ifndef ASM_SPEC
#define ASM_SPEC ""
#endif

/* config.h can define CPP_SPEC to provide extra args to the C preprocessor
   or extra switch-translations.  */
#ifndef CPP_SPEC
#define CPP_SPEC ""
#endif

/* config.h can define CC1_SPEC to provide extra args to cc1
   or extra switch-translations.  */
#ifndef CC1_SPEC
#define CC1_SPEC ""
#endif

/* config.h can define LINK_SPEC to provide extra args to the linker
   or extra switch-translations.  */
#ifndef LINK_SPEC
#define LINK_SPEC ""
#endif

/* config.h can define LIB_SPEC to override the default libraries.  */
#ifndef LIB_SPEC
#define LIB_SPEC "%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}"
#endif

/* config.h can define ENDFILE_SPEC to override the default crtn files.  */
#ifndef ENDFILE_SPEC
#define ENDFILE_SPEC ""
#endif

/* config.h can define STARTFILE_SPEC to override the default crt0 files.  */
#ifndef STARTFILE_SPEC
#define STARTFILE_SPEC  \
  "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}"
#endif

/* This spec is used for telling cpp whether char is signed or not.  */
#define SIGNED_CHAR_SPEC  \
  (DEFAULT_SIGNED_CHAR ? "%{funsigned-char:-D__CHAR_UNSIGNED__}"	\
   : "%{!fsigned-char:-D__CHAR_UNSIGNED__}")

/* This defines which switch letters take arguments.  */

#ifndef SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR)      \
  ((CHAR) == 'D' || (CHAR) == 'U' || (CHAR) == 'o' \
   || (CHAR) == 'e' || (CHAR) == 'T' || (CHAR) == 'u' \
   || (CHAR) == 'I' || (CHAR) == 'Y' || (CHAR) == 'm' \
   || (CHAR) == 'L' || (CHAR) == 'i' || (CHAR) == 'A')
#endif

/* This defines which multi-letter switches take arguments.  */

#ifndef WORD_SWITCH_TAKES_ARG
#define WORD_SWITCH_TAKES_ARG(STR) (!strcmp (STR, "Tdata"))
#endif

/* This structure says how to run one compiler, and when to do so.  */

struct compiler
{
  char *suffix;			/* Use this compiler for input files
				   whose names end in this suffix.  */
  char *spec;			/* To use this compiler, pass this spec
				   to do_spec.  */
};

/* Here are the specs for compiling files with various known suffixes.
   A file that does not end in any of these suffixes will be passed
   unchanged to the loader and nothing else will be done to it.  */

struct compiler compilers[] =
{
  {".c",
   "cpp %{nostdinc} %{C} %{v} %{D*} %{U*} %{I*} %{M*} %{i*} %{trigraphs} -undef \
        -D__GNUC__ %{ansi:-trigraphs -$ -D__STRICT_ANSI__} %{!ansi:%p} %P\
        %c %{O:-D__OPTIMIZE__} %{traditional} %{pedantic} %{P}\
	%{Wcomment*} %{Wtrigraphs} %{Wall} %{w} %C\
        %i %{!M*:%{!E:%{!pipe:%g.cpp}}}%{E:%W{o*}}%{M*:%W{o*}} |\n\
    %{!M*:%{!E:cc1 %{!pipe:%g.cpp} %1 \
		   %{!Q:-quiet} -dumpbase %i %{Y*} %{d*} %{m*} %{f*} %{a}\
		   %{g} %{O} %{W*} %{w} %{pedantic} %{ansi} %{traditional}\
		   %{v:-version} %{gg:-symout %g.sym} %{pg:-p} %{p}\
		   %{pg:%{fomit-frame-pointer:%e-pg and -fomit-frame-pointer are incompatible}}\
		   %{S:%W{o*}%{!o*:-o %b.s}}%{!S:-o %{|!pipe:%g.s}} |\n\
              %{!S:as %{R} %{j} %{J} %{h} %{d2} %a %{gg:-G %g.sym}\
		      %{c:%W{o*}%{!o*:-o %w%b.o}}%{!c:-o %d%w%b.o}\
                      %{!pipe:%g.s}\n }}}"},
  {".cc",
   "cpp -+ %{nostdinc} %{C} %{v} %{D*} %{U*} %{I*} %{M*} %{i*} \
        -undef -D__GNUC__ -D__GNUG__ %p %P\
        %c %{O:-D__OPTIMIZE__} %{traditional} %{pedantic} %{P}\
	%{Wcomment*} %{Wtrigraphs} %{Wall} %{w} %C\
        %i %{!M*:%{!E:%{!pipe:%g.cpp}}}%{E:%W{o*}}%{M*:%W{o*}} |\n\
    %{!M*:%{!E:cc1plus %{!pipe:%g.cpp} %1\
		   %{!Q:-quiet} -dumpbase %i %{Y*} %{d*} %{m*} %{f*} %{a}\
		   %{g} %{O} %{W*} %{w} %{pedantic} %{traditional}\
		   %{v:-version} %{gg:-symout %g.sym} %{pg:-p} %{p}\
		   %{pg:%{fomit-frame-pointer:%e-pg and -fomit-frame-pointer are incompatible}}\
		   %{S:%W{o*}%{!o*:-o %b.s}}%{!S:-o %{|!pipe:%g.s}} |\n\
              %{!S:as %{R} %{j} %{J} %{h} %{d2} %a %{gg:-G %g.sym}\
		      %{c:%W{o*}%{!o*:-o %w%b.o}}%{!c:-o %d%w%b.o}\
                      %{!pipe:%g.s}\n }}}"},
  {".i",
   "cc1 %i %1 %{!Q:-quiet} %{Y*} %{d*} %{m*} %{f*} %{a}\
	%{g} %{O} %{W*} %{w} %{pedantic} %{ansi} %{traditional}\
	%{v:-version} %{gg:-symout %g.sym} %{pg:-p} %{p}\
	%{S:%W{o*}%{!o*:-o %b.s}}%{!S:-o %{|!pipe:%g.s}} |\n\
    %{!S:as %{R} %{j} %{J} %{h} %{d2} %a %{gg:-G %g.sym}\
            %{c:%W{o*}%{!o*:-o %w%b.o}}%{!c:-o %d%w%b.o} %{!pipe:%g.s}\n }"},
  {".s",
   "%{!S:as %{R} %{j} %{J} %{h} %{d2} %a \
            %{c:%W{o*}%{!o*:-o %w%b.o}}%{!c:-o %d%w%b.o} %i\n }"},
  {".S",
   "cpp %{nostdinc} %{C} %{v} %{D*} %{U*} %{I*} %{M*} %{trigraphs} \
        -undef -D__GNUC__ -$ %p %P\
        %c %{O:-D__OPTIMIZE__} %{traditional} %{pedantic} %{P}\
	%{Wcomment*} %{Wtrigraphs} %{Wall} %{w} %C\
        %i %{!M*:%{!E:%{!pipe:%g.s}}}%{E:%W{o*}}%{M*:%W{o*}} |\n\
    %{!M*:%{!E:%{!S:as %{R} %{j} %{J} %{h} %{d2} %a \
                    %{c:%W{o*}%{!o*:-o %w%b.o}}%{!c:-o %d%w%b.o}\
		    %{!pipe:%g.s}\n }}}"},
  /* Mark end of table */
  {0, 0}
};

/* Here is the spec for running the linker, after compiling all files.  */
char *link_spec = "%{!c:%{!M*:%{!E:%{!S:ld %{o*} %l\
 %{A} %{d} %{e*} %{N} %{n} %{r} %{s} %{S} %{T*} %{t} %{u*} %{X} %{x} %{z}\
 %{y*} %{!A:%{!nostdlib:%S}} \
 %{L*} %o %{!nostdlib:%L -lgnulib %{!A:%E}}\n }}}}";

/* Accumulate a command (program name and args), and run it.  */

/* Vector of pointers to arguments in the current line of specifications.  */

char **argbuf;

/* Number of elements allocated in argbuf.  */

int argbuf_length;

/* Number of elements in argbuf currently in use (containing args).  */

int argbuf_index;

/* Number of commands executed so far.  */

int execution_count;

/* Flag indicating whether we should print the command and arguments */

unsigned char vflag;

/* Name with which this program was invoked.  */

char *programname;

/* User-specified -B prefix to attach to command names,
   or 0 if none specified.  */

char *user_exec_prefix = 0;

/* Environment-specified prefix to attach to command names,
   or 0 if none specified.  */

char *env_exec_prefix = 0;

/* Suffix to attach to directories searched for commands.  */

char *machine_suffix = 0;

/* Default prefixes to attach to command names.  */

#ifndef STANDARD_EXEC_PREFIX
#define STANDARD_EXEC_PREFIX "/usr/libexec/"
#endif /* !defined STANDARD_EXEC_PREFIX */

char *standard_exec_prefix = STANDARD_EXEC_PREFIX;
char *standard_exec_prefix_1 = "/usr/libexec/gcc-";

#ifndef STANDARD_STARTFILE_PREFIX
#define STANDARD_STARTFILE_PREFIX "/usr/lib/"
#endif /* !defined STANDARD_STARTFILE_PREFIX */

char *standard_startfile_prefix = STANDARD_STARTFILE_PREFIX;
char *standard_startfile_prefix_1 = "/usr/local/lib/";
char *standard_startfile_prefix_2 = "/usr/lib/gcc-";

/* Clear out the vector of arguments (after a command is executed).  */

void
clear_args ()
{
  argbuf_index = 0;
}

/* Add one argument to the vector at the end.
   This is done when a space is seen or at the end of the line.
   If DELETE_ALWAYS is nonzero, the arg is a filename
    and the file should be deleted eventually.
   If DELETE_FAILURE is nonzero, the arg is a filename
    and the file should be deleted if this compilation fails.  */

void
store_arg (arg, delete_always, delete_failure)
     char *arg;
     int delete_always, delete_failure;
{
  if (argbuf_index + 1 == argbuf_length)
    {
      argbuf = (char **) realloc (argbuf, (argbuf_length *= 2) * sizeof (char *));
    }

  argbuf[argbuf_index++] = arg;
  argbuf[argbuf_index] = 0;

  if (delete_always || delete_failure)
    record_temp_file (arg, delete_always, delete_failure);
}

/* Record the names of temporary files we tell compilers to write,
   and delete them at the end of the run.  */

/* This is the common prefix we use to make temp file names.
   It is chosen once for each run of this program.
   It is substituted into a spec by %g.
   Thus, all temp file names contain this prefix.
   In practice, all temp file names start with this prefix.

   This prefix comes from the envvar TMPDIR if it is defined;
   otherwise, in /tmp.  */

char *temp_filename;

/* Length of the prefix.  */

int temp_filename_length;

/* Define the list of temporary files to delete.  */

struct temp_file
{
  char *name;
  struct temp_file *next;
};

/* Queue of files to delete on success or failure of compilation.  */
struct temp_file *always_delete_queue;
/* Queue of files to delete on failure of compilation.  */
struct temp_file *failure_delete_queue;

/* Record FILENAME as a file to be deleted automatically.
   ALWAYS_DELETE nonzero means delete it if all compilation succeeds;
   otherwise delete it in any case.
   FAIL_DELETE nonzero means delete it if a compilation step fails;
   otherwise delete it in any case.  */

void
record_temp_file (filename, always_delete, fail_delete)
     char *filename;
     int always_delete;
     int fail_delete;
{
  register char *name;
  name = (char *) xmalloc (strlen (filename) + 1);
  strcpy (name, filename);

  if (always_delete)
    {
      register struct temp_file *temp;
      for (temp = always_delete_queue; temp; temp = temp->next)
	if (! strcmp (name, temp->name))
	  goto already1;
      temp = (struct temp_file *) xmalloc (sizeof (struct temp_file));
      temp->next = always_delete_queue;
      temp->name = name;
      always_delete_queue = temp;
    already1:;
    }

  if (fail_delete)
    {
      register struct temp_file *temp;
      for (temp = failure_delete_queue; temp; temp = temp->next)
	if (! strcmp (name, temp->name))
	  goto already2;
      temp = (struct temp_file *) xmalloc (sizeof (struct temp_file));
      temp->next = failure_delete_queue;
      temp->name = name;
      failure_delete_queue = temp;
    already2:;
    }
}

/* Delete all the temporary files whose names we previously recorded.  */

void
delete_temp_files ()
{
  register struct temp_file *temp;

  for (temp = always_delete_queue; temp; temp = temp->next)
    {
#ifdef DEBUG
      int i;
      printf ("Delete %s? (y or n) ", temp->name);
      fflush (stdout);
      i = getchar ();
      if (i != '\n')
	while (getchar () != '\n') ;
      if (i == 'y' || i == 'Y')
#endif /* DEBUG */
	{
	  if (unlink (temp->name) < 0)
	    if (vflag)
	      perror_with_name (temp->name);
	}
    }

  always_delete_queue = 0;
}

/* Delete all the files to be deleted on error.  */

void
delete_failure_queue ()
{
  register struct temp_file *temp;

  for (temp = failure_delete_queue; temp; temp = temp->next)
    {
#ifdef DEBUG
      int i;
      printf ("Delete %s? (y or n) ", temp->name);
      fflush (stdout);
      i = getchar ();
      if (i != '\n')
	while (getchar () != '\n') ;
      if (i == 'y' || i == 'Y')
#endif /* DEBUG */
	{
	  if (unlink (temp->name) < 0)
	    if (vflag)
	      perror_with_name (temp->name);
	}
    }
}

void
clear_failure_queue ()
{
  failure_delete_queue = 0;
}

/* Compute a string to use as the base of all temporary file names.
   It is substituted for %g.  */

void
choose_temp_base ()
{
  extern char *getenv ();
  char *base = getenv ("TMPDIR");
  int len;

  if (base == (char *)0)
    base = "/tmp/";

  len = strlen (base);
  temp_filename = (char *) xmalloc (len + sizeof("/ccXXXXXX"));
  strcpy (temp_filename, base);
  if (len > 0 && temp_filename[len-1] != '/')
    temp_filename[len++] = '/';
  strcpy (temp_filename + len, "ccXXXXXX");

  mktemp (temp_filename);
  temp_filename_length = strlen (temp_filename);
}

/* Search for an execute file through our search path.
   Return 0 if not found, otherwise return its name, allocated with malloc.  */

static char *
find_exec_file (prog)
     char *prog;
{
  int win = 0;
  char *temp;
  int size;

  size = strlen (standard_exec_prefix);
  if (user_exec_prefix != 0 && strlen (user_exec_prefix) > size)
    size = strlen (user_exec_prefix);
  if (env_exec_prefix != 0 && strlen (env_exec_prefix) > size)
    size = strlen (env_exec_prefix);
  if (strlen (standard_exec_prefix_1) > size)
    size = strlen (standard_exec_prefix_1);
  size += strlen (prog) + 1;
  if (machine_suffix)
    size += strlen (machine_suffix) + 1;
  temp = (char *) xmalloc (size);

  /* Determine the filename to execute.  */

  if (user_exec_prefix)
    {
      if (machine_suffix)
	{
	  strcpy (temp, user_exec_prefix);
	  strcat (temp, machine_suffix);
	  strcat (temp, prog);
	  win = (access (temp, X_OK) == 0);
	}
      if (!win)
	{
	  strcpy (temp, user_exec_prefix);
	  strcat (temp, prog);
	  win = (access (temp, X_OK) == 0);
	}
    }

  if (!win && env_exec_prefix)
    {
      if (machine_suffix)
	{
	  strcpy (temp, env_exec_prefix);
	  strcat (temp, machine_suffix);
	  strcat (temp, prog);
	  win = (access (temp, X_OK) == 0);
	}
      if (!win)
	{
	  strcpy (temp, env_exec_prefix);
	  strcat (temp, prog);
	  win = (access (temp, X_OK) == 0);
	}
    }

  if (!win)
    {
      if (machine_suffix)
	{
	  strcpy (temp, standard_exec_prefix);
	  strcat (temp, machine_suffix);
	  strcat (temp, prog);
	  win = (access (temp, X_OK) == 0);
	}
      if (!win)
	{
	  strcpy (temp, standard_exec_prefix);
	  strcat (temp, prog);
	  win = (access (temp, X_OK) == 0);
	}
    }

  if (!win)
    {
      if (machine_suffix)
	{
	  strcpy (temp, standard_exec_prefix_1);
	  strcat (temp, machine_suffix);
	  strcat (temp, prog);
	  win = (access (temp, X_OK) == 0);
	}
      if (!win)
	{
	  strcpy (temp, standard_exec_prefix_1);
	  strcat (temp, prog);
	  win = (access (temp, X_OK) == 0);
	}
    }

  if (win)
    return temp;
  else
    return 0;
}

/* stdin file number.  */
#define STDIN_FILE_NO 0

/* stdout file number.  */
#define STDOUT_FILE_NO 1

/* value of `pipe': port index for reading.  */
#define READ_PORT 0

/* value of `pipe': port index for writing.  */
#define WRITE_PORT 1

/* Pipe waiting from last process, to be used as input for the next one.
   Value is STDIN_FILE_NO if no pipe is waiting
   (i.e. the next command is the first of a group).  */

int last_pipe_input;

/* Fork one piped subcommand.  FUNC is the system call to use
   (either execv or execvp).  ARGV is the arg vector to use.
   NOT_LAST is nonzero if this is not the last subcommand
   (i.e. its output should be piped to the next one.)  */

static int
pexecute (func, program, argv, not_last)
     char *program;
     int (*func)();
     char *argv[];
     int not_last;
{
  int pid;
  int pdes[2];
  int input_desc = last_pipe_input;
  int output_desc = STDOUT_FILE_NO;

  /* If this isn't the last process, make a pipe for its output,
     and record it as waiting to be the input to the next process.  */

  if (not_last)
    {
      if (pipe (pdes) < 0)
	pfatal_with_name ("pipe");
      output_desc = pdes[WRITE_PORT];
      last_pipe_input = pdes[READ_PORT];
    }
  else
    last_pipe_input = STDIN_FILE_NO;

  pid = vfork ();

  switch (pid)
    {
    case -1:
      pfatal_with_name ("vfork");
      break;

    case 0: /* child */
      /* Move the input and output pipes into place, if nec.  */
      if (input_desc != STDIN_FILE_NO)
	{
	  close (STDIN_FILE_NO);
	  dup (input_desc);
	  close (input_desc);
	}
      if (output_desc != STDOUT_FILE_NO)
	{
	  close (STDOUT_FILE_NO);
	  dup (output_desc);
	  close (output_desc);
	}

      /* Close the parent's descs that aren't wanted here.  */
      if (last_pipe_input != STDIN_FILE_NO)
	close (last_pipe_input);

      /* Exec the program.  */
      (*func) (program, argv);
      perror_exec (program);
      exit (-1);
      /* NOTREACHED */

    default:
      /* In the parent, after forking.
	 Close the descriptors that we made for this child.  */
      if (input_desc != STDIN_FILE_NO)
	close (input_desc);
      if (output_desc != STDOUT_FILE_NO)
	close (output_desc);

      /* Return child's process number.  */
      return pid;
    }
}

/* Execute the command specified by the arguments on the current line of spec.
   When using pipes, this includes several piped-together commands
   with `|' between them.

   Return 0 if successful, -1 if failed.  */

int
execute ()
{
  int i, j;
  int n_commands;		/* # of command.  */
  char *string;
  struct command
    {
      char *prog;		/* program name.  */
      char **argv;		/* vector of args.  */
      int pid;			/* pid of process for this command.  */
    };

  struct command *commands;	/* each command buffer with above info.  */

  /* Count # of piped commands.  */
  for (n_commands = 1, i = 0; i < argbuf_index; i++)
    if (strcmp (argbuf[i], "|") == 0)
      n_commands++;

  /* Get storage for each command.  */
  commands
    = (struct command *) alloca (n_commands * sizeof (struct command));

  /* Split argbuf into its separate piped processes,
     and record info about each one.
     Also search for the programs that are to be run.  */

  commands[0].prog = argbuf[0]; /* first command.  */
  commands[0].argv = &argbuf[0];
  string = find_exec_file (commands[0].prog);
  if (string)
    commands[0].argv[0] = string;

  for (n_commands = 1, i = 0; i < argbuf_index; i++)
    if (strcmp (argbuf[i], "|") == 0)
      {				/* each command.  */
	argbuf[i] = 0;	/* termination of command args.  */
	commands[n_commands].prog = argbuf[i + 1];
	commands[n_commands].argv = &argbuf[i + 1];
	string = find_exec_file (commands[n_commands].prog);
	if (string)
	  commands[n_commands].argv[0] = string;
	n_commands++;
      }

  argbuf[argbuf_index] = 0;

  /* If -v, print what we are about to do, and maybe query.  */

  if (vflag)
    {
      /* Print each piped command as a separate line.  */
      for (i = 0; i < n_commands ; i++)
	{
	  char **j;

	  for (j = commands[i].argv; *j; j++)
	    fprintf (stderr, " %s", *j);

	  /* Print a pipe symbol after all but the last command.  */
	  if (i + 1 != n_commands)
	    fprintf (stderr, " |");
	  fprintf (stderr, "\n");
	}
      fflush (stderr);
#ifdef DEBUG
      fprintf (stderr, "\nGo ahead? (y or n) ");
      fflush (stderr);
      j = getchar ();
      if (j != '\n')
	while (getchar () != '\n') ;
      if (j != 'y' && j != 'Y')
	return 0;
#endif /* DEBUG */
    }

  /* Run each piped subprocess.  */

  last_pipe_input = STDIN_FILE_NO;
  for (i = 0; i < n_commands; i++)
    {
      extern int execv(), execvp();
      char *string = commands[i].argv[0];

      commands[i].pid = pexecute ((string != commands[i].prog ? execv : execvp),
				  string, commands[i].argv,
				  i + 1 < n_commands);

      if (string != commands[i].prog)
	free (string);
    }

  execution_count++;

  /* Wait for all the subprocesses to finish.
     We don't care what order they finish in;
     we know that N_COMMANDS waits will get them all.  */

  {
    int ret_code = 0;

    for (i = 0; i < n_commands; i++)
      {
	int status;
	int pid;
	char *prog;

	pid = wait (&status);
	if (pid < 0)
	  abort ();

	if (status != 0)
	  {
	    int j;
	    for (j = 0; j < n_commands; j++)
	      if (commands[j].pid == pid)
		prog = commands[j].prog;

	    if ((status & 0x7F) != 0)
	      fatal ("Program %s got fatal signal %d.", prog, (status & 0x7F));
	    if (((status & 0xFF00) >> 8) >= MIN_FATAL_STATUS)
	      ret_code = -1;
	  }
      }
    return ret_code;
  }
}

/* Find all the switches given to us
   and make a vector describing them.
   The elements of the vector a strings, one per switch given.
   If a switch uses the following argument, then the `part1' field
   is the switch itself and the `part2' field is the following argument.
   The `valid' field is nonzero if any spec has looked at this switch;
   if it remains zero at the end of the run, it must be meaningless.  */

struct switchstr
{
  char *part1;
  char *part2;
  int valid;
};

struct switchstr *switches;

int n_switches;

/* Also a vector of input files specified.  */

char **infiles;

int n_infiles;

/* And a vector of corresponding output files is made up later.  */

char **outfiles;

/* Create the vector `switches' and its contents.
   Store its length in `n_switches'.  */

void
process_command (argc, argv)
     int argc;
     char **argv;
{
  extern char *getenv ();
  register int i;
  n_switches = 0;
  n_infiles = 0;

  env_exec_prefix = getenv ("GCC_EXEC_PREFIX");

  /* Scan argv twice.  Here, the first time, just count how many switches
     there will be in their vector, and how many input files in theirs.
     Here we also parse the switches that cc itself uses (e.g. -v).  */

  for (i = 1; i < argc; i++)
    {
      if (argv[i][0] == '-' && argv[i][1] != 'l')
	{
	  register char *p = &argv[i][1];
	  register int c = *p;

	  switch (c)
	    {
	    case 'b':
	      machine_suffix = p + 1;
	      break;

	    case 'B':
	      user_exec_prefix = p + 1;
	      break;

	    case 'v':	/* Print our subcommands and print versions.  */
	      vflag++;
	      n_switches++;
	      break;

	    default:
	      n_switches++;

	      if (SWITCH_TAKES_ARG (c) && p[1] == 0)
		i++;
	      else if (WORD_SWITCH_TAKES_ARG (p))
		i++;
	    }
	}
      else
	n_infiles++;
    }

  /* Then create the space for the vectors and scan again.  */

  switches = ((struct switchstr *)
	      xmalloc ((n_switches + 1) * sizeof (struct switchstr)));
  infiles = (char **) xmalloc ((n_infiles + 1) * sizeof (char *));
  n_switches = 0;
  n_infiles = 0;

  /* This, time, copy the text of each switch and store a pointer
     to the copy in the vector of switches.
     Store all the infiles in their vector.  */

  for (i = 1; i < argc; i++)
    {
      if (argv[i][0] == '-' && argv[i][1] != 'l')
	{
	  register char *p = &argv[i][1];
	  register int c = *p;

	  if (c == 'B' || c == 'b')
	    continue;
	  switches[n_switches].part1 = p;
	  if ((SWITCH_TAKES_ARG (c) && p[1] == 0)
	      || WORD_SWITCH_TAKES_ARG (p))
	    switches[n_switches].part2 = argv[++i];
	  else if (c == 'o') {
	    /* On some systems, ld cannot handle -o without space.
	       So split the -o and its argument.  */
	    switches[n_switches].part2 = (char *) xmalloc (strlen (p));
	    strcpy (switches[n_switches].part2, &p[1]);
	    p[1] = 0;
	  } else
	    switches[n_switches].part2 = 0;
	  switches[n_switches].valid = 0;
	  n_switches++;
	}
      else
	infiles[n_infiles++] = argv[i];
    }

  switches[n_switches].part1 = 0;
  infiles[n_infiles] = 0;
}

/* Process a spec string, accumulating and running commands.  */

/* These variables describe the input file name.
   input_file_number is the index on outfiles of this file,
   so that the output file name can be stored for later use by %o.
   input_basename is the start of the part of the input file
   sans all directory names, and basename_length is the number
   of characters starting there excluding the suffix .c or whatever.  */

char *input_filename;
int input_file_number;
int input_filename_length;
int basename_length;
char *input_basename;

/* These are variables used within do_spec and do_spec_1.  */

/* Nonzero if an arg has been started and not yet terminated
   (with space, tab or newline).  */
int arg_going;

/* Nonzero means %d or %g has been seen; the next arg to be terminated
   is a temporary file name.  */
int delete_this_arg;

/* Nonzero means %w has been seen; the next arg to be terminated
   is the output file name of this compilation.  */
int this_is_output_file;

/* Nonzero means %s has been seen; the next arg to be terminated
   is the name of a library file and we should try the standard
   search dirs for it.  */
int this_is_library_file;

/* Process the spec SPEC and run the commands specified therein.
   Returns 0 if the spec is successfully processed; -1 if failed.  */

int
do_spec (spec)
     char *spec;
{
  int value;

  clear_args ();
  arg_going = 0;
  delete_this_arg = 0;
  this_is_output_file = 0;
  this_is_library_file = 0;

  value = do_spec_1 (spec, 0);

  /* Force out any unfinished command.
     If -pipe, this forces out the last command if it ended in `|'.  */
  if (value == 0)
    {
      if (argbuf_index > 0 && !strcmp (argbuf[argbuf_index - 1], "|"))
	argbuf_index--;

      if (argbuf_index > 0)
	value = execute ();
    }

  return value;
}

/* Process the sub-spec SPEC as a portion of a larger spec.
   This is like processing a whole spec except that we do
   not initialize at the beginning and we do not supply a
   newline by default at the end.
   INSWITCH nonzero means don't process %-sequences in SPEC;
   in this case, % is treated as an ordinary character.
   This is used while substituting switches.
   INSWITCH nonzero also causes SPC not to terminate an argument.

   Value is zero unless a line was finished
   and the command on that line reported an error.  */

int
do_spec_1 (spec, inswitch)
     char *spec;
     int inswitch;
{
  register char *p = spec;
  register int c;
  char *string;

  while (c = *p++)
    /* If substituting a switch, treat all chars like letters.
       Otherwise, NL, SPC, TAB and % are special.  */
    switch (inswitch ? 'a' : c)
      {
      case '\n':
	/* End of line: finish any pending argument,
	   then run the pending command if one has been started.  */
	if (arg_going)
	  {
	    obstack_1grow (&obstack, 0);
	    string = obstack_finish (&obstack);
	    if (this_is_library_file)
	      string = find_file (string);
	    store_arg (string, delete_this_arg, this_is_output_file);
	    if (this_is_output_file)
	      outfiles[input_file_number] = string;
	  }
	arg_going = 0;

	if (argbuf_index > 0 && !strcmp (argbuf[argbuf_index - 1], "|"))
	  {
	    int i;
	    for (i = 0; i < n_switches; i++)
	      if (!strcmp (switches[i].part1, "pipe"))
		break;

	    /* A `|' before the newline means use a pipe here,
	       but only if -pipe was specified.
	       Otherwise, execute now and don't pass the `|' as an arg.  */
	    if (i < n_switches)
	      {
		switches[i].valid = 1;
		break;
	      }
	    else
	      argbuf_index--;
	  }

	if (argbuf_index > 0)
	  {
	    int value = execute ();
	    if (value)
	      return value;
	  }
	/* Reinitialize for a new command, and for a new argument.  */
	clear_args ();
	arg_going = 0;
	delete_this_arg = 0;
	this_is_output_file = 0;
	this_is_library_file = 0;
	break;

      case '|':
	/* End any pending argument.  */
	if (arg_going)
	  {
	    obstack_1grow (&obstack, 0);
	    string = obstack_finish (&obstack);
	    if (this_is_library_file)
	      string = find_file (string);
	    store_arg (string, delete_this_arg, this_is_output_file);
	    if (this_is_output_file)
	      outfiles[input_file_number] = string;
	  }

	/* Use pipe */
	obstack_1grow (&obstack, c);
	arg_going = 1;
	break;

      case '\t':
      case ' ':
	/* Space or tab ends an argument if one is pending.  */
	if (arg_going)
	  {
	    obstack_1grow (&obstack, 0);
	    string = obstack_finish (&obstack);
	    if (this_is_library_file)
	      string = find_file (string);
	    store_arg (string, delete_this_arg, this_is_output_file);
	    if (this_is_output_file)
	      outfiles[input_file_number] = string;
	  }
	/* Reinitialize for a new argument.  */
	arg_going = 0;
	delete_this_arg = 0;
	this_is_output_file = 0;
	this_is_library_file = 0;
	break;

      case '%':
	switch (c = *p++)
	  {
	  case 0:
	    fatal ("Invalid specification!  Bug in cc.");

	  case 'b':
	    obstack_grow (&obstack, input_basename, basename_length);
	    arg_going = 1;
	    break;

	  case 'd':
	    delete_this_arg = 2;
	    break;

	  case 'e':
	    /* {...:%efoo} means report an error with `foo' as error message
	       and don't execute any more commands for this file.  */
	    {
	      char *q = p;
	      char *buf;
	      while (*p != 0 && *p != '\n') p++;
	      buf = (char *) alloca (p - q + 1);
	      strncpy (buf, q, p - q);
	      buf[p - q] = 0;
	      error ("%s", buf);
	      return -1;
	    }
	    break;

	  case 'g':
	    obstack_grow (&obstack, temp_filename, temp_filename_length);
	    delete_this_arg = 1;
	    arg_going = 1;
	    break;

	  case 'i':
	    obstack_grow (&obstack, input_filename, input_filename_length);
	    arg_going = 1;
	    break;

	  case 'o':
	    {
	      register int f;
	      for (f = 0; f < n_infiles; f++)
		store_arg (outfiles[f], 0, 0);
	    }
	    break;

	  case 's':
	    this_is_library_file = 1;
	    break;

	  case 'W':
	    {
	      int index = argbuf_index;
	      /* Handle the {...} following the %W.  */
	      if (*p != '{')
		abort ();
	      p = handle_braces (p + 1);
	      if (p == 0)
		return -1;
	      /* If any args were output, mark the last one for deletion
		 on failure.  */
	      if (argbuf_index != index)
		record_temp_file (argbuf[argbuf_index - 1], 0, 1);
	      break;
	    }

	  case 'w':
	    this_is_output_file = 1;
	    break;

	  case '{':
	    p = handle_braces (p);
	    if (p == 0)
	      return -1;
	    break;

	  case '%':
	    obstack_1grow (&obstack, '%');
	    break;

/*** The rest just process a certain constant string as a spec.  */

	  case '1':
	    do_spec_1 (CC1_SPEC, 0);
	    break;

	  case 'a':
	    do_spec_1 (ASM_SPEC, 0);
	    break;

	  case 'c':
	    do_spec_1 (SIGNED_CHAR_SPEC, 0);
	    break;

	  case 'C':
	    do_spec_1 (CPP_SPEC, 0);
	    break;

	  case 'l':
	    do_spec_1 (LINK_SPEC, 0);
	    break;

	  case 'L':
	    do_spec_1 (LIB_SPEC, 0);
	    break;

	  case 'p':
	    do_spec_1 (CPP_PREDEFINES, 0);
	    break;

	  case 'P':
	    {
	      char *x = (char *) alloca (strlen (CPP_PREDEFINES) * 2 + 1);
	      char *buf = x;
	      char *y = CPP_PREDEFINES;

	      /* Copy all of CPP_PREDEFINES into BUF,
		 but put __ after every -D and at the end of each arg,  */
	      while (1)
		{
		  if (! strncmp (y, "-D", 2))
		    {
		      *x++ = '-';
		      *x++ = 'D';
		      *x++ = '_';
		      *x++ = '_';
		      y += 2;
		    }
		  else if (*y == ' ' || *y == 0)
		    {
		      *x++ = '_';
		      *x++ = '_';
		      if (*y == 0)
			break;
		      else
			*x++ = *y++;
		    }
		  else
		    *x++ = *y++;
		}
	      *x = 0;

	      do_spec_1 (buf, 0);
	    }
	    break;

	  case 'S':
	    do_spec_1 (STARTFILE_SPEC, 0);
	    break;

	  case 'E':
	    do_spec_1 (ENDFILE_SPEC, 0);
	    break;

	  default:
	    abort ();
	  }
	break;

      default:
	/* Ordinary character: put it into the current argument.  */
	obstack_1grow (&obstack, c);
	arg_going = 1;
      }

  return 0;		/* End of string */
}

/* Return 0 if we call do_spec_1 and that returns -1.  */

char *
handle_braces (p)
     register char *p;
{
  register char *q;
  char *filter;
  int pipe = 0;
  int negate = 0;

  if (*p == '|')
    /* A `|' after the open-brace means,
       if the test fails, output a single minus sign rather than nothing.
       This is used in %{|!pipe:...}.  */
    pipe = 1, ++p;

  if (*p == '!')
    /* A `!' after the open-brace negates the condition:
       succeed if the specified switch is not present.  */
    negate = 1, ++p;

  filter = p;
  while (*p != ':' && *p != '}') p++;
  if (*p != '}')
    {
      register int count = 1;
      q = p + 1;
      while (count > 0)
	{
	  if (*q == '{')
	    count++;
	  else if (*q == '}')
	    count--;
	  else if (*q == 0)
	    abort ();
	  q++;
	}
    }
  else
    q = p + 1;

  if (p[-1] == '*' && p[0] == '}')
    {
      /* Substitute all matching switches as separate args.  */
      register int i;
      --p;
      for (i = 0; i < n_switches; i++)
	if (!strncmp (switches[i].part1, filter, p - filter))
	  give_switch (i);
    }
  else
    {
      /* Test for presence of the specified switch.  */
      register int i;
      int present = 0;

      /* If name specified ends in *, as in {x*:...},
	 check for presence of any switch name starting with x.  */
      if (p[-1] == '*')
	{
	  for (i = 0; i < n_switches; i++)
	    {
	      if (!strncmp (switches[i].part1, filter, p - filter - 1))
		{
		  switches[i].valid = 1;
		  present = 1;
		}
	    }
	}
      /* Otherwise, check for presence of exact name specified.  */
      else
	{
	  for (i = 0; i < n_switches; i++)
	    {
	      if (!strncmp (switches[i].part1, filter, p - filter)
		  && switches[i].part1[p - filter] == 0)
		{
		  switches[i].valid = 1;
		  present = 1;
		  break;
		}
	    }
	}

      /* If it is as desired (present for %{s...}, absent for %{-s...})
	 then substitute either the switch or the specified
	 conditional text.  */
      if (present != negate)
	{
	  if (*p == '}')
	    {
	      give_switch (i);
	    }
	  else
	    {
	      if (do_spec_1 (save_string (p + 1, q - p - 2), 0) < 0)
		return 0;
	    }
	}
      else if (pipe)
	{
	  /* Here if a %{|...} conditional fails: output a minus sign,
	     which means "standard output" or "standard input".  */
	  do_spec_1 ("-", 0);
	}
    }

  return q;
}

/* Pass a switch to the current accumulating command
   in the same form that we received it.
   SWITCHNUM identifies the switch; it is an index into
   the vector of switches gcc received, which is `switches'.
   This cannot fail since it never finishes a command line.  */

give_switch (switchnum)
     int switchnum;
{
  do_spec_1 ("-", 0);
  do_spec_1 (switches[switchnum].part1, 1);
  do_spec_1 (" ", 0);
  if (switches[switchnum].part2 != 0)
    {
      do_spec_1 (switches[switchnum].part2, 1);
      do_spec_1 (" ", 0);
    }
  switches[switchnum].valid = 1;
}

/* Search for a file named NAME trying various prefixes including the
   user's -B prefix and some standard ones.
   Return the absolute pathname found.  If nothing is found, return NAME.  */

char *
find_file (name)
     char *name;
{
  int size;
  char *temp;
  int win = 0;

  /* Compute maximum size of NAME plus any prefix we will try.  */

  size = strlen (standard_exec_prefix);
  if (user_exec_prefix != 0 && strlen (user_exec_prefix) > size)
    size = strlen (user_exec_prefix);
  if (env_exec_prefix != 0 && strlen (env_exec_prefix) > size)
    size = strlen (env_exec_prefix);
  if (strlen (standard_exec_prefix) > size)
    size = strlen (standard_exec_prefix);
  if (strlen (standard_exec_prefix_1) > size)
    size = strlen (standard_exec_prefix_1);
  if (strlen (standard_startfile_prefix) > size)
    size = strlen (standard_startfile_prefix);
  if (strlen (standard_startfile_prefix_1) > size)
    size = strlen (standard_startfile_prefix_1);
  if (strlen (standard_startfile_prefix_2) > size)
    size = strlen (standard_startfile_prefix_2);
  if (machine_suffix)
    size += strlen (machine_suffix) + 1;
  size += strlen (name) + 1;

  temp = (char *) alloca (size);

  if (user_exec_prefix)
    {
      if (machine_suffix)
	{
	  strcpy (temp, user_exec_prefix);
	  strcat (temp, machine_suffix);
	  strcat (temp, name);
	  win = (access (temp, R_OK) == 0);
	}
      if (!win)
	{
	  strcpy (temp, user_exec_prefix);
	  strcat (temp, name);
	  win = (access (temp, R_OK) == 0);
	}
    }

  if (!win && env_exec_prefix)
    {
      if (machine_suffix)
	{
	  strcpy (temp, env_exec_prefix);
	  strcat (temp, machine_suffix);
	  strcat (temp, name);
	  win = (access (temp, R_OK) == 0);
	}
      if (!win)
	{
	  strcpy (temp, env_exec_prefix);
	  strcat (temp, name);
	  win = (access (temp, R_OK) == 0);
	}
    }

  if (!win)
    {
      if (machine_suffix)
	{
	  strcpy (temp, standard_exec_prefix);
	  strcat (temp, machine_suffix);
	  strcat (temp, name);
	  win = (access (temp, R_OK) == 0);
	}
      if (!win)
	{
	  strcpy (temp, standard_exec_prefix);
	  strcat (temp, name);
	  win = (access (temp, R_OK) == 0);
	}
    }

  if (!win)
    {
      if (machine_suffix)
	{
	  strcpy (temp, standard_exec_prefix_1);
	  strcat (temp, machine_suffix);
	  strcat (temp, name);
	  win = (access (temp, R_OK) == 0);
	}
      if (!win)
	{
	  strcpy (temp, standard_exec_prefix_1);
	  strcat (temp, name);
	  win = (access (temp, R_OK) == 0);
	}
    }

  if (!win)
    {
      if (machine_suffix)
	{
	  strcpy (temp, standard_startfile_prefix);
	  strcat (temp, machine_suffix);
	  strcat (temp, name);
	  win = (access (temp, R_OK) == 0);
	}
      if (!win)
	{
	  strcpy (temp, standard_startfile_prefix);
	  strcat (temp, name);
	  win = (access (temp, R_OK) == 0);
	}
    }

  if (!win)
    {
      if (machine_suffix)
	{
	  strcpy (temp, standard_startfile_prefix_1);
	  strcat (temp, machine_suffix);
	  strcat (temp, name);
	  win = (access (temp, R_OK) == 0);
	}
      if (!win)
	{
	  strcpy (temp, standard_startfile_prefix_1);
	  strcat (temp, name);
	  win = (access (temp, R_OK) == 0);
	}
    }

  if (!win)
    {
      if (machine_suffix)
	{
	  strcpy (temp, standard_startfile_prefix_2);
	  strcat (temp, machine_suffix);
	  strcat (temp, name);
	  win = (access (temp, R_OK) == 0);
	}
      if (!win)
	{
	  strcpy (temp, standard_startfile_prefix_2);
	  strcat (temp, name);
	  win = (access (temp, R_OK) == 0);
	}
    }

  if (!win)
    {
      if (machine_suffix)
	{
	  strcpy (temp, "./");
	  strcat (temp, machine_suffix);
	  strcat (temp, name);
	  win = (access (temp, R_OK) == 0);
	}
      if (!win)
	{
	  strcpy (temp, "./");
	  strcat (temp, name);
	  win = (access (temp, R_OK) == 0);
	}
    }

  if (win)
    return save_string (temp, strlen (temp));
  return name;
}

/* On fatal signals, delete all the temporary files.  */

void
fatal_error (signum)
     int signum;
{
  signal (signum, SIG_DFL);
  delete_failure_queue ();
  delete_temp_files ();
  /* Get the same signal again, this time not handled,
     so its normal effect occurs.  */
  kill (getpid (), signum);
}

int
main (argc, argv)
     int argc;
     char **argv;
{
  register int i;
  int value;
  int error_count = 0;
  int linker_was_run = 0;
  char *explicit_link_files;

  programname = argv[0];

  if (signal (SIGINT, SIG_IGN) != SIG_IGN)
    signal (SIGINT, fatal_error);
  if (signal (SIGHUP, SIG_IGN) != SIG_IGN)
    signal (SIGHUP, fatal_error);
  if (signal (SIGTERM, SIG_IGN) != SIG_IGN)
    signal (SIGTERM, fatal_error);
  if (signal (SIGPIPE, SIG_IGN) != SIG_IGN)
    signal (SIGPIPE, fatal_error);

  argbuf_length = 10;
  argbuf = (char **) xmalloc (argbuf_length * sizeof (char *));

  obstack_init (&obstack);

  choose_temp_base ();

  /* Make a table of what switches there are (switches, n_switches).
     Make a table of specified input files (infiles, n_infiles).  */

  process_command (argc, argv);

  if (vflag)
    {
      extern char *version_string;
      fprintf (stderr, "gcc version %s\n", version_string);
      if (n_infiles == 0)
	exit (0);
    }

  if (n_infiles == 0)
    fatal ("No input files specified.");

  /* Make a place to record the compiler output file names
     that correspond to the input files.  */

  outfiles = (char **) xmalloc (n_infiles * sizeof (char *));
  bzero (outfiles, n_infiles * sizeof (char *));

  /* Record which files were specified explicitly as link input.  */

  explicit_link_files = (char *) xmalloc (n_infiles);
  bzero (explicit_link_files, n_infiles);

  for (i = 0; i < n_infiles; i++)
    {
      register struct compiler *cp;
      int this_file_error = 0;

      /* Tell do_spec what to substitute for %i.  */

      input_filename = infiles[i];
      input_filename_length = strlen (input_filename);
      input_file_number = i;

      /* Use the same thing in %o, unless cp->spec says otherwise.  */

      outfiles[i] = input_filename;

      /* Figure out which compiler from the file's suffix.  */

      for (cp = compilers; cp->spec; cp++)
	{
	  if (strlen (cp->suffix) < input_filename_length
	      && !strcmp (cp->suffix,
			  infiles[i] + input_filename_length
			  - strlen (cp->suffix)))
	    {
	      /* Ok, we found an applicable compiler.  Run its spec.  */
	      /* First say how much of input_filename to substitute for %b  */
	      register char *p;

	      input_basename = input_filename;
	      for (p = input_filename; *p; p++)
		if (*p == '/')
		  input_basename = p + 1;
	      basename_length = (input_filename_length - strlen (cp->suffix)
				 - (input_basename - input_filename));
	      value = do_spec (cp->spec);
	      if (value < 0)
		this_file_error = 1;
	      break;
	    }
	}

      /* If this file's name does not contain a recognized suffix,
	 record it as explicit linker input.  */

      if (! cp->spec)
	explicit_link_files[i] = 1;

      /* Clear the delete-on-failure queue, deleting the files in it
	 if this compilation failed.  */

      if (this_file_error)
	{
	  delete_failure_queue ();
	  error_count++;
	}
      /* If this compilation succeeded, don't delete those files later.  */
      clear_failure_queue ();
    }

  /* Run ld to link all the compiler output files.  */

  if (error_count == 0)
    {
      int tmp = execution_count;
      value = do_spec (link_spec);
      if (value < 0)
	error_count = 1;
      linker_was_run = (tmp != execution_count);
    }

  /* If options said don't run linker,
     complain about input files to be given to the linker.  */

  if (! linker_was_run && error_count == 0)
    for (i = 0; i < n_infiles; i++)
      if (explicit_link_files[i])
	error ("%s: linker input file unused since linking not done",
	       outfiles[i]);

  /* Set the `valid' bits for switches that match anything in any spec.  */

  validate_all_switches ();

  /* Warn about any switches that no pass was interested in.  */
  
  for (i = 0; i < n_switches; i++)
    if (! switches[i].valid)
      error ("unrecognized option `-%s'", switches[i].part1);

  /* Delete some or all of the temporary files we made.  */

  if (error_count)
    delete_failure_queue ();
  delete_temp_files ();

  exit (error_count);
}

xmalloc (size)
     int size;
{
  register int value = malloc (size);
  if (value == 0)
    fatal ("Virtual memory full.");
  return value;
}

xrealloc (ptr, size)
     int ptr, size;
{
  register int value = realloc (ptr, size);
  if (value == 0)
    fatal ("Virtual memory full.");
  return value;
}

/* Return a newly-allocated string whose contents concatenate those of s1, s2, s3.  */

char *
concat (s1, s2, s3)
     char *s1, *s2, *s3;
{
  int len1 = strlen (s1), len2 = strlen (s2), len3 = strlen (s3);
  char *result = (char *) xmalloc (len1 + len2 + len3 + 1);

  strcpy (result, s1);
  strcpy (result + len1, s2);
  strcpy (result + len1 + len2, s3);
  *(result + len1 + len2 + len3) = 0;

  return result;
}

char *
save_string (s, len)
     char *s;
     int len;
{
  register char *result = (char *) xmalloc (len + 1);

  bcopy (s, result, len);
  result[len] = 0;
  return result;
}

pfatal_with_name (name)
     char *name;
{
  extern int errno, sys_nerr;
  extern char *sys_errlist[];
  char *s;

  if (errno < sys_nerr)
    s = concat ("%s: ", sys_errlist[errno], "");
  else
    s = "cannot open %s";
  fatal (s, name);
}

perror_with_name (name)
     char *name;
{
  extern int errno, sys_nerr;
  extern char *sys_errlist[];
  char *s;

  if (errno < sys_nerr)
    s = concat ("%s: ", sys_errlist[errno], "");
  else
    s = "cannot open %s";
  error (s, name);
}

perror_exec (name)
     char *name;
{
  extern int errno, sys_nerr;
  extern char *sys_errlist[];
  char *s;

  if (errno < sys_nerr)
    s = concat ("installation problem, cannot exec %s: ",
		sys_errlist[errno], "");
  else
    s = "installation problem, cannot exec %s";
  error (s, name);
}

/* More 'friendly' abort that prints the line and file.
   config.h can #define abort fancy_abort if you like that sort of thing.  */

void
fancy_abort ()
{
  fatal ("Internal gcc abort.");
}

#ifdef HAVE_VPRINTF

/* Output an error message and exit */

int 
fatal (va_alist)
     va_dcl
{
  va_list ap;
  char *format;
  
  va_start(ap);
  format = va_arg (ap, char *);
  vfprintf (stderr, format, ap);
  va_end (ap);
  fprintf (stderr, "\n");
  delete_temp_files ();
  exit (1);
}  

error (va_alist) 
     va_dcl
{
  va_list ap;
  char *format;

  va_start(ap);
  format = va_arg (ap, char *);
  fprintf (stderr, "%s: ", programname);
  vfprintf (stderr, format, ap);
  va_end (ap);

  fprintf (stderr, "\n");
}

#else /* not HAVE_VPRINTF */

fatal (msg, arg1, arg2)
     char *msg, *arg1, *arg2;
{
  error (msg, arg1, arg2);
  delete_temp_files (0);
  exit (1);
}

error (msg, arg1, arg2)
     char *msg, *arg1, *arg2;
{
  fprintf (stderr, "%s: ", programname);
  fprintf (stderr, msg, arg1, arg2);
  fprintf (stderr, "\n");
}

#endif /* not HAVE_VPRINTF */


void
validate_all_switches ()
{
  struct compiler *comp;
  register char *p;
  register char c;

  for (comp = compilers; comp->spec; comp++)
    {
      p = comp->spec;
      while (c = *p++)
	if (c == '%' && *p == '{')
	  /* We have a switch spec.  */
	  validate_switches (p + 1);
    }

  p = link_spec;
  while (c = *p++)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);

  /* Now notice switches mentioned in the machine-specific specs.  */

#ifdef ASM_SPEC
  p = ASM_SPEC;
  while (c = *p++)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);
#endif

#ifdef CPP_SPEC
  p = CPP_SPEC;
  while (c = *p++)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);
#endif

#ifdef SIGNED_CHAR_SPEC
  p = SIGNED_CHAR_SPEC;
  while (c = *p++)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);
#endif

#ifdef CC1_SPEC
  p = CC1_SPEC;
  while (c = *p++)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);
#endif

#ifdef LINK_SPEC
  p = LINK_SPEC;
  while (c = *p++)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);
#endif

#ifdef LIB_SPEC
  p = LIB_SPEC;
  while (c = *p++)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);
#endif

#ifdef STARTFILE_SPEC
  p = STARTFILE_SPEC;
  while (c = *p++)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);
#endif
}

/* Look at the switch-name that comes after START
   and mark as valid all supplied switches that match it.  */

void
validate_switches (start)
     char *start;
{
  register char *p = start;
  char *filter;
  register int i;

  if (*p == '|')
    ++p;

  if (*p == '!')
    ++p;

  filter = p;
  while (*p != ':' && *p != '}') p++;

  if (p[-1] == '*')
    {
      /* Mark all matching switches as valid.  */
      --p;
      for (i = 0; i < n_switches; i++)
	if (!strncmp (switches[i].part1, filter, p - filter))
	  switches[i].valid = 1;
    }
  else
    {
      /* Mark an exact matching switch as valid.  */
      for (i = 0; i < n_switches; i++)
	{
	  if (!strncmp (switches[i].part1, filter, p - filter)
	      && switches[i].part1[p - filter] == 0)
	    switches[i].valid = 1;
	}
    }
}
