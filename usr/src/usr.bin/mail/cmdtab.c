/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)cmdtab.c	5.11 (Berkeley) 6/1/90";
#endif /* not lint */

#include "def.h"

/*
 * Mail -- a mail program
 *
 * Define all of the command names and bindings.
 */

extern int type(), preserve(), delete(), undelete(), next(), shell(), schdir();
extern int save(), help(), headers(), pdot(), respond(), editor();
extern int quitcmd(), rexit(), pcmdlist(), sendmail(), from(), copycmd();
extern int messize(), psalloc(), deltype(), unset(), set(), source();
extern int pversion(), group(), top(), core(), null(), stouch(), visual();
extern int swrite(), dosh(), file(), echo(), Respond(), scroll(), ifcmd();
extern int elsecmd(), endifcmd(), mboxit(), clobber(), alternates();
extern int folders(), igfield(), Type(), retfield(), more(), More();
extern int saveigfield(), saveretfield();
extern int unread();	/* , Header(); */

struct cmd cmdtab[] = {
	"next",		next,		NDMLIST,	0,	MMNDEL,
	"alias",	group,		M|RAWLIST,	0,	1000,
	"print",	type,		MSGLIST,	0,	MMNDEL,
	"type",		type,		MSGLIST,	0,	MMNDEL,
	"Type",		Type,		MSGLIST,	0,	MMNDEL,
	"Print",	Type,		MSGLIST,	0,	MMNDEL,
	"visual",	visual,		I|MSGLIST,	0,	MMNORM,
	"top",		top,		MSGLIST,	0,	MMNDEL,
	"touch",	stouch,		W|MSGLIST,	0,	MMNDEL,
	"preserve",	preserve,	W|MSGLIST,	0,	MMNDEL,
	"delete",	delete,		W|P|MSGLIST,	0,	MMNDEL,
	"dp",		deltype,	W|MSGLIST,	0,	MMNDEL,
	"dt",		deltype,	W|MSGLIST,	0,	MMNDEL,
	"undelete",	undelete,	P|MSGLIST,	MDELETED,MMNDEL,
	"unset",	unset,		M|RAWLIST,	1,	1000,
	"mail",		sendmail,	R|M|I|STRLIST,	0,	0,
	"mbox",		mboxit,		W|MSGLIST,	0,	0,
	"more",		more,		MSGLIST,	0,	MMNDEL,
	"page",		more,		MSGLIST,	0,	MMNDEL,
	"More",		More,		MSGLIST,	0,	MMNDEL,
	"Page",		More,		MSGLIST,	0,	MMNDEL,
	"unread",	unread,		MSGLIST,	0,	MMNDEL,
	"!",		shell,		I|STRLIST,	0,	0,
	"copy",		copycmd,	M|STRLIST,	0,	0,
	"chdir",	schdir,		M|RAWLIST,	0,	1,
	"cd",		schdir,		M|RAWLIST,	0,	1,
	"save",		save,		STRLIST,	0,	0,
	"source",	source,		M|RAWLIST,	1,	1,
	"set",		set,		M|RAWLIST,	0,	1000,
	"shell",	dosh,		I|NOLIST,	0,	0,
	"version",	pversion,	M|NOLIST,	0,	0,
	"group",	group,		M|RAWLIST,	0,	1000,
	"write",	swrite,		STRLIST,	0,	0,
	"from",		from,		MSGLIST,	0,	MMNORM,
	"file",		file,		T|M|RAWLIST,	0,	1,
	"folder",	file,		T|M|RAWLIST,	0,	1,
	"folders",	folders,	T|M|NOLIST,	0,	0,
	"?",		help,		M|NOLIST,	0,	0,
	"z",		scroll,		M|STRLIST,	0,	0,
	"headers",	headers,	MSGLIST,	0,	MMNDEL,
	"help",		help,		M|NOLIST,	0,	0,
	"=",		pdot,		NOLIST,		0,	0,
	"Reply",	Respond,	R|I|MSGLIST,	0,	MMNDEL,
	"Respond",	Respond,	R|I|MSGLIST,	0,	MMNDEL,
	"reply",	respond,	R|I|MSGLIST,	0,	MMNDEL,
	"respond",	respond,	R|I|MSGLIST,	0,	MMNDEL,
	"edit",		editor,		I|MSGLIST,	0,	MMNORM,
	"echo",		echo,		M|RAWLIST,	0,	1000,
	"quit",		quitcmd,	NOLIST,		0,	0,
	"list",		pcmdlist,	M|NOLIST,	0,	0,
	"xit",		rexit,		M|NOLIST,	0,	0,
	"exit",		rexit,		M|NOLIST,	0,	0,
	"size",		messize,	MSGLIST,	0,	MMNDEL,
	"hold",		preserve,	W|MSGLIST,	0,	MMNDEL,
	"if",		ifcmd,		F|M|RAWLIST,	1,	1,
	"else",		elsecmd,	F|M|RAWLIST,	0,	0,
	"endif",	endifcmd,	F|M|RAWLIST,	0,	0,
	"alternates",	alternates,	M|RAWLIST,	0,	1000,
	"ignore",	igfield,	M|RAWLIST,	0,	1000,
	"discard",	igfield,	M|RAWLIST,	0,	1000,
	"retain",	retfield,	M|RAWLIST,	0,	1000,
	"saveignore",	saveigfield,	M|RAWLIST,	0,	1000,
	"savediscard",	saveigfield,	M|RAWLIST,	0,	1000,
	"saveretain",	saveretfield,	M|RAWLIST,	0,	1000,
/*	"Header",	Header,		STRLIST,	0,	1000,	*/
	"core",		core,		M|NOLIST,	0,	0,
	"#",		null,		M|NOLIST,	0,	0,
	"clobber",	clobber,	M|RAWLIST,	0,	1,
	0,		0,		0,		0,	0
};
