#include <stdio.h>
#include <ctype.h>
#include "../src/paths.h"

/* zippy.c
 * 
 * Print a quotation from Zippy the Pinhead.
 * Qux <Kaufman-David@Yale> March 6, 1986
 * 
 */

#define BUFSIZE  2000
#define SEP      '\0'
#define YOW_FILE "yow.lines"

main (argc, argv)
     int argc;
     char *argv[];
{
  FILE *fp;
  char file[BUFSIZ];
  void yow();

  if (argc > 2 && !strcmp (argv[1], "-f"))
    strcpy (file, argv[2]);
  else
#ifdef vms
    sprintf (file, "%s%s", PATH_EXEC, YOW_FILE);
#else
    sprintf (file, "%s/%s", PATH_EXEC, YOW_FILE);
#endif

  if ((fp = fopen(file, "r")) == NULL) {
    perror(file);
    exit(1);
  }

  /* initialize random seed */
  srand((int) (getpid() + time((long *) 0)));

  yow(fp);
  fclose(fp);
  exit(0);
}

void
yow (fp)
     FILE *fp;
{
  static long len = -1;
  long offset;
  int c, i = 0;
  char buf[BUFSIZE];

  /* Get length of file, go to a random place in it */
  if (len == -1) {
    if (fseek(fp, 0, 2) == -1) {
      perror("fseek 1");
      exit(1);
    }
    len = ftell(fp);
  }
  offset = rand() % len;
  if (fseek(fp, offset, 0) == -1) {
    perror("fseek 2");
    exit(1);
  }

  /* Read until SEP, read next line, print it.
     (Note that we will never print anything before the first seperator.)
     If we hit EOF looking for the first SEP, just recurse. */
  while ((c = getc(fp)) != SEP)
    if (c == EOF) {
      yow(fp);
      return;
    }

  /* Skip leading whitespace, then read in a quotation.
     If we hit EOF before we find a non-whitespace char, recurse. */
  while (isspace(c = getc(fp)))
    ;
  if (c == EOF) {
    yow(fp);
    return;
  }
  buf[i++] = c;
  while ((c = getc(fp)) != SEP && c != EOF) {
    buf[i++] = c;

    if (i == BUFSIZ-1)
      /* Yow! Is this quotation too long yet? */
      break;
  }
  buf[i++] = 0;
  printf("%s\n", buf);
}

