#

#include "def.h"

/*
 * Mail -- a mail program
 *
 * Define all of the command names and bindings.
 */

extern int type(), preserve(), delete(), undelete(), next(), shell(), schdir();
extern int save(), help(), headers(), pdot(), strace(), respond(), editor();
extern int edstop(), exit(), pcmdlist(), previous(), sendmail(), from();
extern int messize(), psalloc(), deltype(), unset(), set(), source();
extern int pversion(), group(), top(), core(), null(), stouch(), visual();
extern int swrite(), dosh(), file(), echo(), Respond();

struct cmd cmdtab[] = {
	"next",		next,		NDMLIST,	0,	MMNORM,
	"alias",	group,		M|RAWLIST,	0,	1000,
	"print",	type,		MSGLIST,	0,	MMNDEL,
	"type",		type,		MSGLIST,	0,	MMNDEL,
	"visual",	visual,		I|MSGLIST,	0,	MMNORM,
	"top",		top,		MSGLIST,	0,	MMNDEL,
	"touch",	stouch,		MSGLIST,	0,	MMNDEL,
	"preserve",	preserve,	MSGLIST,	0,	MMNDEL,
	"delete",	delete,		P|MSGLIST,	0,	MMNDEL,
	"dp",		deltype,	MSGLIST,	0,	MMNDEL,
	"dt",		deltype,	MSGLIST,	0,	MMNDEL,
	"undelete",	undelete,	P|MSGLIST,	MDELETED,MMNDEL,
	"unset",	unset,		M|RAWLIST,	1,	1000,
	"mail",		sendmail,	M|I|STRLIST,	0,	0,
	"mbox",		stouch,		MSGLIST,	0,	0,
	"!",		shell,		I|STRLIST,	0,	0,
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
	"file",		file,		M|NOLIST,	0,	0,
	"?",		help,		M|NOLIST,	0,	0,
	"headers",	headers,	RAWLIST,	0,	1,
	"help",		help,		M|NOLIST,	0,	0,
	"-",		previous,	RAWLIST,	0,	1,
	"=",		pdot,		NOLIST,		0,	0,
	"Reply",	Respond,	I|MSGLIST,	0,	MMNDEL,
	"Respond",	Respond,	I|MSGLIST,	0,	MMNDEL,
	"reply",	respond,	I|MSGLIST,	0,	MMNDEL,
	"respond",	respond,	I|MSGLIST,	0,	MMNDEL,
	"edit",		editor,		I|MSGLIST,	0,	MMNORM,
	"echo",		echo,		RAWLIST,	0,	1000,
	"quit",		edstop,		NOLIST, 	0,	0,
	"list",		pcmdlist,	M|NOLIST,	0,	0,
	"xit",		exit,		M|NOLIST,	0,	0,
	"exit",		exit,		M|NOLIST,	0,	0,
	"size",		messize,	MSGLIST,	0,	MMNDEL,
	"hold",		preserve,	MSGLIST,	0,	MMNDEL,
	"core",		core,		M|NOLIST,	0,	0,
	"#",		null,		M|NOLIST,	0,	0,
	0,		0,		0,		0,	0
};
