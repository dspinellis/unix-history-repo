
#ifndef lint
static char *rcsid = "$Header: /f/osi/others/quipu/uips/pod/RCS/main.c,v 7.2 91/02/22 09:31:34 mrose Interim $";
#endif

/*
 * $Header: /f/osi/others/quipu/uips/pod/RCS/main.c,v 7.2 91/02/22 09:31:34 mrose Interim $
 */

#include "quipu/util.h"
#include "quipu/photo.h"
#include "quipu/common.h"
#include "quipu/entry.h"
#include "quipu/dua.h"
#include "pod.h"

#include <varargs.h>

extern int print_parse_errors;
extern char *oidtable, *tailfile, *myname;
extern char namestr[], passwd[];
extern Widget toplevel;
extern bool testing;

int podphoto(), quipu_pe_cmp();
void CreateWidgets(), PodLoop(), displayStartupError();
char *cnnct_bind();
void kill_message(), message();

void user_tailor();
void read_args(), quit();

main (argc, argv)
     unsigned int argc;
     char **argv;
{
  char *mess;

  print_parse_errors = FALSE;
  quipu_syntaxes();
  want_oc_hierarchy();

  namestr[0] = '\0';
  passwd[0] = '\0';

  toplevel = XtInitialize("X-Directory", "Pod", NULL, 0,
                               &argc, argv);

  read_args(&argc, &argv);
  dsap_init((int *) 0, &argv);

  user_tailor();

  CreateWidgets();
  message("Connecting to Directory. Please Wait...");

  if ((mess = cnnct_bind()) != NULLCP) {
    kill_message();
    displayStartupError(mess);
    XtMainLoop();
    quit(1);
  } 

  set_attribute_syntax (str2syntax("photo"),
			(IFP)pe_cpy,    NULLIFP,
			NULLIFP,        podphoto,
			(IFP)pe_cpy,    quipu_pe_cmp,
			pe_free,        NULLCP,
			NULLIFP,        TRUE );

  kill_message();
  PodLoop();

  return 0;
}

void read_args(acptr, avptr)
     unsigned int *acptr;
     char ***avptr;
{
  register char *cp;
  char **av;
  int count;

  if (acptr == (unsigned int *) NULL) return;
  if (*acptr <= 1) return;

  av = *avptr;
  av++, count = 1;

  while ((cp = *av) && (*cp == '-')) {
    switch (*++cp) {
    case 'u':
      if (*++av != NULLCP) (void) strcpy(namestr, *av);
      count++;
      break;
    case 'p':
      if (*++av != NULLCP) (void) strcpy(passwd, *av);
      count++;
      break;
    case 'T':
      if (*++av != NULLCP) oidtable = *av;
      count++;
      break;
    case 'c':
      if (*++av != NULLCP) myname = *av;
      count++;
      break;
    case 't':
      if (lexequ(*av, "-test") != 0) {
	if (*++av != NULLCP) tailfile = *av;
	count++;
      } else {
	testing = TRUE;
      }
      break;
    }
    av++;
    count++;
  }
  *acptr -= count;
  *avptr = av;
}

void quit(sig)
     int     sig;
{
  (void) ds_unbind();
  exit(sig);
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
