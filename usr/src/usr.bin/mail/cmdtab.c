#

#include "def.h"

/*
 * Mail -- a mail program
 *
 * Define all of the command names and bindings.
 */

static char *SccsId = "@(#)cmdtab.c	2.5 %G%";

extern int type(), preserve(), delete(), undelete(), next(), shell(), schdir();
extern int save(), help(), headers(), pdot(), strace(), respond(), editor();
extern int edstop(), rexit(), pcmdlist(), sendmail(), from(), copycmd();
extern int messize(), psalloc(), deltype(), unset(), set(), source();
extern int pversion(), group(), top(), core(), null(), stouch(), visual();
extern int swrite(), dosh(), file(), echo(), Respond(), scroll(), ifcmd();
extern int elsecmd(), endifcmd(), mboxit(), clobber(), alternates();

struct cmd cmdtab[] = {
	"next",		next,		NDMLIST,	0,	MMNDEL,
	"alias",	group,		M|RAWLIST,	0,	1000,
	"print",	type,		MSGLIST,	0,	MMNDEL,
	"type",		type,		MSGLIST,	0,	MMNDEL,
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
	"!",		shell,		I|STRLIST,	0,	0,
	"copy",		copycmd,	M|STRLIST,	0,	0,
	"chdir",	schdir,		M|STRLIST,	0,	0,
	"cd",		schdir,		M|STRLIST,	0,	0,
	"save",		save,		STRLIST,	0,	0,
	"source",	source,		M|STRLIST,	0,	0,
	"set",		set,		M|RAWLIST,	0,	1000,
	"shell",	dosh,		I|NOLIST,	0,	0,
	"version",	pversion,	M|NOLIST,	0,	0,
	"group",	group,		M|RAWLIST,	0,	1000,
	"write",	swrite,		STRLIST,	0,	0,
	"from",		from,		MSGLIST,	0,	MMNORM,
	"file",		file,		T|M|RAWLIST,	0,	1,
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
	"quit",		edstop,		NOLIST, 	0,	0,
	"list",		pcmdlist,	M|NOLIST,	0,	0,
	"xit",		rexit,		M|NOLIST,	0,	0,
	"exit",		rexit,		M|NOLIST,	0,	0,
	"size",		messize,	MSGLIST,	0,	MMNDEL,
	"hold",		preserve,	W|MSGLIST,	0,	MMNDEL,
	"if",		ifcmd,		F|M|RAWLIST,	1,	1,
	"else",		elsecmd,	F|M|RAWLIST,	0,	0,
	"endif",	endifcmd,	F|M|RAWLIST,	0,	0,
	"alternates",	alternates,	M|RAWLIST,	0,	1000,
	"core",		core,		M|NOLIST,	0,	0,
	"#",		null,		M|NOLIST,	0,	0,
	"clobber",	clobber,	M|RAWLIST,	0,	1,
	0,		0,		0,		0,	0
};
