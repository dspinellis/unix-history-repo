/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)cmdtab.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include "tip.h"

extern	int shell(), getfl(), sendfile(), chdirectory();
extern	int finish(), help(), pipefile(), pipeout(), consh(), variable();
extern	int cu_take(), cu_put(), dollar(), genbrk(), suspend();

esctable_t etable[] = {
	{ '!',	NORM,	"shell",			 shell },
	{ '<',	NORM,	"receive file from remote host", getfl },
	{ '>',	NORM,	"send file to remote host",	 sendfile },
	{ 't',	NORM,	"take file from remote UNIX",	 cu_take },
	{ 'p',	NORM,	"put file to remote UNIX",	 cu_put },
	{ '|',	NORM,	"pipe remote file",		 pipefile },
	{ '$',	NORM,	"pipe local command to remote host", pipeout },
#ifdef CONNECT
	{ 'C',  NORM,	"connect program to remote host",consh },
#endif
	{ 'c',	NORM,	"change directory",		 chdirectory },
	{ '.',	NORM,	"exit from tip",		 finish },
	{CTRL('d'),NORM,"exit from tip",		 finish },
	{CTRL('y'),NORM,"suspend tip (local+remote)",	 suspend },
	{CTRL('z'),NORM,"suspend tip (local only)",	 suspend },
	{ 's',	NORM,	"set variable",			 variable },
	{ '?',	NORM,	"get this summary",		 help },
	{ '#',	NORM,	"send break",			 genbrk },
	{ 0, 0, 0 }
};
