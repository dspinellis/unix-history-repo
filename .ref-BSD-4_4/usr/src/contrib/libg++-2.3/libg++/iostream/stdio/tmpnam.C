/*
 * tmpname.c
 * Original Author:	G. Haley
 */

/*
FUNCTION
<<tmpnam, tempnam>>---generate name for a temporary file

INDEX
	tmpnam
INDEX
	tempnam

ANSI_SYNOPSIS
	#include <stdio.h>
	char *tmpnam(char *<[s]>);
	char *tempnam(char *<[dir]>, char *<[pfx]>);

TRAD_SYNOPSIS
	#include <stdio.h>
	char *tmpnam(<[s]>)
	char *<[s]>;

	char *tempnam(<[dir]>, <[pfx]>)
	char *<[dir]>;
	char *<[pfx]>;

DESCRIPTION
Use either of these functions to generate a name for a temporary file.
The generated name is guaranteed to avoid collision with other files
(for up to <<TMP_MAX>> calls of either function).

<<tmpnam>> generates file names with the value of <<P_tmpdir>>
(defined in `<<stdio.h>>') as the leading directory component of the path.

You can use the <<tmpnam>> argument <[s]> to specify a suitable area
of memory for the generated filename; otherwise, you can call
<<tmpnam(NULL)>> to use an internal static buffer.

<<tempnam>> allows you more control over the generated filename: you
can use the argument <[dir]> to specify the path to a directory for
temporary files, and you can use the argument <[pfx]> to specify a
prefix for the base filename.

If <[dir]> is <<NULL>>, <<tempnam>> will attempt to use the value of
environment variable <<TMPDIR>> instead; if there is no such value,
<<tempnam>> uses the value of <<P_tmpdir>> (defined in `<<stdio.h>>').

If you don't need any particular prefix to the basename of temporary
files, you can pass <<NULL>> as the <[pfx]> argument to <<tempnam>>.

WARNINGS
The generated filenames are suitable for temporary files, but do not
in themselves make files temporary.  Files with these names must still
be explicitly removed when you no longer want them.

If you supply your own data area <[s]> for <<tmpnam>>, you must ensure
that it has room for at least <<L_tmpnam>> elements of type <<char>>.

RETURNS
Both <<tmpnam>> and <<tempnam>> return a pointer to the newly
generated filename. 

PORTABILITY
ANSI C requires <<tmpnam>>, but does not specify the use of
<<P_tmpdir>>.  The System V Interface Definition (Issue 2) requires
both <<tmpnam>> and <<tmpname>>.
*/

#include <stdioprivate.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
extern "C" _G_pid_t getpid(void);
/* Try to open the file specified, if it can be opened then try
   another one */

static void __temp_name(char *result,
			const char *part1,
			const char *part2,
			int part3)
{
    static int counter = 0;
  /*  Generate the filename and make sure that there isn't one called
      it already */
    while (1) {
	sprintf (result, "%s/%s%x.%x", part1, part2, part3, counter);
	int t = open(result, O_RDONLY);
	if (t == 0) {
	    break;
	}
	counter++;
	close(t);
    }
}
	     
char *tmpnam(char *s)
{
  static char *filename;
  static char tmp_buffer[L_tmpnam];

  char *result;
  int pid;

  if (s == NULL) {
      result = tmp_buffer;
  }
  else
    result = s;
  pid = getpid ();

  __temp_name(result, P_tmpdir, "t", pid);

  return result;  
}

// FIXME: Not ANSI - should be in a separate file!
extern "C" char *tempnam(const char *dir, const char *pfx)
{
  char *filename;
  int current;
  int length;
  if (dir == NULL && (dir = getenv ("TMPDIR")) == NULL)
   dir = P_tmpdir;

  length = strlen(dir) + strlen(pfx) + 10 + 1; /* two 8 digit
						  numbers + . / */

  filename = (char *)malloc(length);
  if (filename) 
    __temp_name(filename, dir, pfx, getpid());
  return filename;
}
