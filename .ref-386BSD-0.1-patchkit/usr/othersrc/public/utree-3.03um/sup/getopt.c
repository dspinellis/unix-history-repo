/*
 *      GETOPT.C
 *      System V like command line option parser
 */

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
/* !  This function is a modified version of getopt() `stolen' from   ! */
/* !  the public domain electronic mail system ELM version 2.3        ! */
/* !    (C) Copyright 1986, 1987, by Dave Taylor                      ! */
/* !    (C) Copyright 1988, 1989, 1990, USENET Community Trust        ! */
/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

#include <stdio.h>

#ifdef  SYSV
#define index(s, c)  strchr(s, c)
extern char *strchr();
#else
extern char *index();
#endif

int  opterr = 1;                /* Error handling flag  */
int  optind = 1;                /* Index in argv        */
int  optopt;                    /* Current option       */
char *optarg;                   /* Option argument      */

getopt(argc, argv, opts)
  register int argc;
  register char **argv, *opts;
{
  static int sp = 1;
  register char *cp;
  register int c;

  if(sp == 1)
    if(optind >= argc || argv[optind][0] != '-' || argv[optind][1] == '\0')
      return(EOF);
    else if( !strcmp(argv[optind], "--")) {
      optind++;
      return(EOF);
    }
  optopt = c = argv[optind][sp];
  if(c == ':' || (cp = index(opts, c)) == NULL) {
    if(opterr)
      (void) fprintf(stderr, "%s: illegal option -- %c\n", argv[0], c);
    else if(argv[optind][++sp] == '\0') {
      optind++;
      sp = 1;
    }
    return('?');
  }
  if(*++cp == ':') {
    if(argv[optind][sp+1] != '\0')
      optarg = &argv[optind++][sp+1];
    else if(++optind >= argc) {
      if(opterr)
	(void) fprintf(stderr, "%s: option requires an argument -- %c\n",
		       argv[0], c);
      sp = 1;
      return('?');
    }
    else
      optarg = argv[optind++];
    sp = 1;
  }
  else {
    if(argv[optind][++sp] == '\0') {
      sp = 1;
      optind++;
    }
    optarg = NULL;
  }
  return(c);

} /* getopt() */
