/*
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)lconv.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <limits.h>
#include <locale.h>

static char empty[] = "";

/*
 * Default (C) locale conversion.
 */
static struct lconv C_lconv = {
	".",			/* decimal_point */
	empty,			/* thousands_sep */
	empty,			/* grouping */
	empty,			/* int_curr_symbol */
	empty,			/* currency_symbol */
	empty,			/* mon_decimal_point */
	empty,			/* mon_thousands_sep */
	empty,			/* mon_grouping */
	empty,			/* positive_sign */
	empty,			/* negative_sign */
	CHAR_MAX,		/* int_frac_digits */
	CHAR_MAX,		/* frac_digits */
	CHAR_MAX,		/* p_cs_precedes */
	CHAR_MAX,		/* p_sep_by_space */
	CHAR_MAX,		/* n_cs_precedes */
	CHAR_MAX,		/* n_sep_by_space */
	CHAR_MAX,		/* p_sign_posn */
	CHAR_MAX,		/* n_sign_posn */
};

/*
 * Current locale conversion.
 */
struct lconv *__lconv = &C_lconv;
