/* next.c:  Functions for NeXT as target machine for GNU C compiler.  */

/* Note that the include below means that we can't debug routines in
   m68k.c when running on a COFF system.  */

#include "m68k.c"

/* Make everything that used to go in the text section really go there.  */

int flag_no_mach_text_sections = 0;

#define OPT_STRCMP(opt) (!strncmp (opt, p, sizeof (opt)-1))

/* 1 if handle_pragma has been called yet.  */

static int pragma_initialized;

/* Initial setting of `optimize'.  */

static int initial_optimize_flag;

extern char *get_directive_line ();

/* Called from check_newline via the macro HANDLE_PRAGMA.
   FINPUT is the source file input stream.  */

void
handle_pragma (finput)
     FILE *finput;
{
  register char *p = get_directive_line (finput);

  /* Record initial setting of optimize flag, so we can restore it.  */
  if (!pragma_initialized)
    {
      pragma_initialized = 1;
      initial_optimize_flag = optimize;
    }

  if (OPT_STRCMP ("CC_OPT_ON"))
    optimize = 1, obey_regdecls = 0;
  else if (OPT_STRCMP ("CC_OPT_OFF"))
    optimize = 0, obey_regdecls = 1;
  else if (OPT_STRCMP ("CC_OPT_RESTORE"))
    {
      extern int initial_optimize_flag;

      if (optimize != initial_optimize_flag)
	{
	  if (initial_optimize_flag)
	    obey_regdecls = 0;
	  else
	    obey_regdecls = 1;
	  optimize = initial_optimize_flag;
	}
    }
  else if (OPT_STRCMP ("CC_WRITABLE_STRINGS"))
    flag_writable_strings = 1;
  else if (OPT_STRCMP ("CC_NON_WRITABLE_STRINGS"))
    flag_writable_strings = 0;
  else if (OPT_STRCMP("CC_NO_MACH_TEXT_SECTIONS"))
    flag_no_mach_text_sections = 1;
}
