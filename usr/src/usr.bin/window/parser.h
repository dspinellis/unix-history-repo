/*
 *	@(#)parser.h	3.1 84/01/12
 */

#include <stdio.h>
#include "value.h"
#include "context.h"
#include "token.h"
#include "string.h"
#include "lcmd.h"
#include "var.h"

#define p_erred()	(cx.x_erred)
#define p_synerred()	(cx.x_synerred)
#define p_clearerr()	(cx.x_erred = cx.x_synerred = 0)
#define p_abort()	(cx.x_abort)
