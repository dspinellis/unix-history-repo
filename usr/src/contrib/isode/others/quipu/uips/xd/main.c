/* $Header: /f/osi/others/quipu/uips/xd/RCS/main.c,v 7.1 91/02/22 09:33:02 mrose Interim $ */
#ifndef lint
	static char *rcsid = "$Id: main.c,v 7.1 91/02/22 09:33:02 mrose Interim $";
#endif
/*
 $Log:	main.c,v $
 * Revision 7.1  91/02/22  09:33:02  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/06/12  13:10:51  mrose
 * *** empty log message ***
 * 
 * Revision 1.5  90/04/26  10:22:42  emsrssn
 * Installation fixed
 * 
 * 
 * Revision 1.4  90/04/25  17:28:12  emsrssn
 * Lint tidy up
 * 
 * 
 * Revision 1.3  90/04/19  13:54:14  emsrssn
 * keyboard accelerator now activates button highlight.
 * 
 * search types available is dependent on current position
 * to prevent unreasonable searches.
 * 
 * the help popup changes automatically depending on the 
 * position of the cursor
 * 
 * buttons remain a fixed size when the application is
 * resized
 * 
 * command line options are now handled properly
 * 
 * logging added
 * 
 * "reads" are now sorted to show mail address at top etc.
 * 
 * 
 * Revision 1.2  90/03/09  15:57:32  emsrssn
 * First public distribution
 * 
 * 
 * Revision 1.1  90/03/08  13:18:40  emsrssn
 * Initial revision
 * 
 * 
*/

#include "quipu/util.h"
#include <varargs.h>
#include <X11/Intrinsic.h>

extern void Loop(), xprint(), init_widgets();
extern int print_parse_errors;
extern XtAppContext app_con;
extern Widget toplevel;

char *local_dit;

void
main (argc, argv)
int argc;
char **argv;
{
  print_parse_errors = FALSE;
  
  toplevel = XtAppInitialize(&app_con, "Xd", NULL, 0,
                             &argc, argv, NULL , NULL, 0);

  quipu_syntaxes();
  dsap_init(&argc, &argv);
  user_tailor();
  init_widgets();
  cnnct_bind();
  Set_Search_Area(local_dit);
  Loop();
  free_memory();     /* frees up memory used for result_list & lookback_list */
}


die(sig, str)
int     sig;
char    *str;
{
  xprint(str);
  quit (sig);
}

quit(sig)
int     sig;
{
  free_all();
  (void) ds_unbind();
  exit(sig);
}


void    advise (va_alist)
va_dcl
{
  va_list ap;
  extern LLog * log_dsap;

  va_start (ap);
  (void) va_arg (ap, int);
  va_end (ap);
}
