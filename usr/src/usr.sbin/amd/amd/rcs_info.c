/*
 * $Id: rcs_info.c,v 5.2 90/06/23 22:19:54 jsp Rel $
 *
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)rcs_info.c	5.1 (Berkeley) 6/29/90
 */

/*
 * Pretty-print some RCS information
 */

#include "am.h"

void show_rcs_info(msg, buf)
const char *msg;
char *buf;
{
	/*
	 * Trivial fsm to print an RCS header without
	 * printing the RCS variables themselves.
	 */
	const char *p = msg;

	enum rstate { Text, Header, Body, FQuit } st = Text;

	while (st != FQuit) {
		if (st == Text || st == Body) {
			/*
			 * Find next $
			 */
			int len;
			char *q = strchr(p, '$');
			if (q) {
				/*
				 * Write out upto the '$'
				 */
				len = q-p-(st==Body?1:0);
				bcopy(p, buf, len);
				/*
				 * Advance p
				 */
				p = q+1;
				/*
				 * Switch state
				 */
				if (st == Body)
					st = Text;
				else
					st = Header;
			} else {
				/*
				 * Nothing more to do, write
				 * out rest of line and quit
				 */
				len = strlen(p);
				bcopy(p, buf, len);
				st = FQuit;
			}
			buf += len;
		} else if (st == Header) {
			/*
			 * Skip past $blah: part
			 */
			char *q = strchr(p, ':');
			if (q) {
				p = q+2;
				st = Body;
			} else {
				st = FQuit;
			}
		}
	}
	*buf = '\0';
}
