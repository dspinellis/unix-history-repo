#include <X/mit-copyright.h>

/* Copyright Massachusetts Institute of Technology 1985 */

/* $Header: lncmd.h,v 10.4 86/02/01 16:03:32 tony Rel $ */
/* lncmd.h - Command sequences DEC printers, in particular LN0x laser
   printers */

#define LN_RIS             "\033c"
#define LN_SSU             "\033[%d I"
#define LN_PUM_SET         "\033[11h"
#define LN_PFS             "\033[%s J"
#define LN_DECSLRM         "\033[%d;%ds"
#define LN_HPA             "\033[%d`"
#define LN_VPA             "\033[%dd"
#define LN_SIXEL_GRAPHICS  "\033P%d;%d;%dq"
#define LN_ST              "\033\\"
#define LN_DECOPM_SET      "\033[?52h"
#define LN_SGR             "\033[1%dm"
