/* main.c - widget */

#ifndef lint
static char *rcsid = "$Header: /f/osi/others/quipu/uips/sd/RCS/main.c,v 7.4 91/02/22 09:32:17 mrose Interim $";
#endif

/*
 * $Header: /f/osi/others/quipu/uips/sd/RCS/main.c,v 7.4 91/02/22 09:32:17 mrose Interim $
 */


#include <signal.h>
#include <stdio.h>
#define IP _IP
#include <curses.h>
#undef OK
#include <varargs.h>

#include "widget.h"
#include "quipu/util.h"

extern  char    goto_path[], namestr[], passwd[];
extern  char   *myname;
extern  WINDOW *Text;
extern  WIDGET  mainwdgts[];
extern  WIDGET  cnnctwdgts[];
extern  int     print_parse_errors;
extern  char   *oidtable, *tailfile, *myname;
extern  char    testing;

void quit(), die(), setsignals(), int_quit(), sd_quit(), read_args();
void user_tailor(), main_bind(), cnnct_bind(), interact(), help_init();

void exit();

main(argc, argv)
     unsigned int     argc;
     char    *argv[];
{
    print_parse_errors = FALSE;
    quipu_syntaxes();

    namestr[0] = '\0';
    passwd[0] = '\0';

    read_args(argc, &argv);
    dsap_init((int *) 0, &argv);

    initwidgets();
    setsignals();

    user_tailor(); 
    main_bind();
    cnnct_bind();
    help_init();
    interact();
    return(0);
}

void read_args(argc, avptr)
     unsigned int argc;
     char ***avptr;
{
  register char **av;
  register char *cp;

  if (argc <= 1) return;

  av = *avptr;
  av++;

  while ((cp = *av) && (*cp == '-')) {
    switch (*++cp) {
    case 'u':
      if (*++av != NULLCP) (void) strcpy(namestr, *av);
      break;
    case 'p':
      if (*++av != NULLCP) (void) strcpy(passwd, *av);
      break;
    case 'T':
      if (*++av != NULLCP) oidtable = *av;
      break;
    case 'c':
      if (*++av != NULLCP) myname = *av;
      break;
    case 't':
      if (lexequ(*av, "-test") != 0) {
        if (*++av != NULLCP) tailfile = *av;
      } else {
        testing = TRUE;
      }
      break;
    }
    av++;
  }
}

void setsignals()
{
  int     i;

  for (i=0; i<18; i++)
    (void) signal(i, SIG_DFL);
}

void eprint(str)
     char    *str;
{
  tprint(str);
}

void sd_quit()
{
  quit("\n", 0);
}

void quit(error, sig)
     char    *error;
     int     sig;
{
  endwidgets();
  (void) ds_unbind();
  hide_picture();
  (void) printf(error);
  exit(sig);
}

void int_quit(sig)
     int sig;
{
  quit("\n", sig);
}


advise (va_alist)
     va_dcl
{
  int     code;
  va_list ap;
  extern LLog * log_dsap;
  
  va_start (ap);
  
  code = va_arg (ap, int);
  
  (void) _ll_log (log_dsap, code, ap);
  
  va_end (ap);
}

