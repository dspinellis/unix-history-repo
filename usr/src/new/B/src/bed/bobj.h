/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: bobj.h,v 2.0 84/06/18 15:46:31 guido Exp $ */

/*
 * B editor -- Interface to "B machine".
 */

/*
 * General values.
 */

value grab_com();
value copy();

/*
 * Operations on texts.
 */

value mk_text();
value trim();
value concat();
