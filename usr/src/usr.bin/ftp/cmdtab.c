#ifndef lint
static char sccsid[] = "@(#)cmdtab.c	4.1 (Berkeley) %G%";
#endif
#include "ftp_var.h"

/*
 * User FTP -- Command Tables.
 */
int	setascii(), setbell(), setbinary(), setdebug(), setform();
int	setmode(), setpeer(), setprompt(), setstruct(), settenex();
int	settrace(), settype(), setverbose();
int	disconnect();
int	cd(), lcd(), delete(), user();
int	ls(), get(), help(), put();
int	quit(), renamefile(), status();
int	quote(), rmthelp(), shell();
int	pwd(), makedir(), removedir();

char	asciihelp[] =	"set ascii transfer type";
char	beephelp[] =	"beep when command completed";
char	binaryhelp[] =	"set binary transfer type";
char	cdhelp[] =	"change remote working directory";
char	connecthelp[] =	"connect to remote tftp";
char	deletehelp[] =	"delete remote file";
char	debughelp[] =	"toggle/set debugging mode";
char	dirhelp[] =	"list contents of remote directory";
char	disconhelp[] =	"terminate ftp session";
char	formhelp[] =	"set file transfer format";
char	helphelp[] =	"print local help information";
char	lcdhelp[] =	"change local working directory";
char	lshelp[] =	"nlist contents of remote directory";
char	mkdirhelp[] =	"make directory on the remote machine";
char	modehelp[] =	"set file transfer mode";
char	prompthelp[] =	"force interactive prompting on multiple commands";
char	pwdhelp[] =	"print working directory on remote machine";
char	quithelp[] =	"terminate ftp session and exit";
char	quotehelp[] =	"send arbitrary ftp command";
char	receivehelp[] =	"receive file";
char	remotehelp[] =	"get help from remote server";
char	renamehelp[] =	"rename file";
char	rmdirhelp[] =	"remove directory on the remote machine";
char	sendhelp[] =	"send file";
char	shellhelp[] =	"escape to the shell";
char	statushelp[] =	"show current status";
char	structhelp[] =	"set file transfer structure";
char	tenexhelp[] =	"set tenex file transfer type";
char	tracehelp[] =	"toggle packet tracing";
char	typehelp[] =	"set file transfer type";
char	userhelp[] =	"send new user information";
char	verbosehelp[] =	"toggle verbose mode";

struct cmd cmdtab[] = {
	{ "!",		shellhelp,	0,	shell },
	{ "ascii",	asciihelp,	0,	setascii },
	{ "bell",	beephelp,	0,	setbell },
	{ "binary",	binaryhelp,	0,	setbinary },
	{ "bye",	quithelp,	0,	quit },
	{ "cd",		cdhelp,		0,	cd },
	{ "close",	disconhelp,	0,	disconnect },
	{ "connect",	connecthelp,	0,	setpeer },
	{ "delete",	deletehelp,	0,	delete },
	{ "debug",	debughelp,	0,	setdebug },
	{ "dir",	dirhelp,	1,	ls },
	{ "form",	formhelp,	0,	setform },
	{ "get",	receivehelp,	1,	get },
	{ "help",	helphelp,	0,	help },
	{ "lcd",	lcdhelp,	0,	lcd },
	{ "ls",		lshelp,		1,	ls },
	{ "mode",	modehelp,	0,	setmode },
	{ "mkdir",	mkdirhelp,	0,	makedir },
	{ "prompt",	prompthelp,	0,	setprompt },
	{ "put",	sendhelp,	1,	put },
	{ "pwd",	pwdhelp,	0,	pwd },
	{ "quit",	quithelp,	0,	quit },
	{ "quote",	quotehelp,	1,	quote },
	{ "recv",	receivehelp,	1,	get },
	{ "remotehelp",	remotehelp,	0,	rmthelp },
	{ "rename",	renamehelp,	0,	renamefile },
	{ "rmdir",	rmdirhelp,	0,	removedir },
	{ "send",	sendhelp,	1,	put },
	{ "status",	statushelp,	0,	status },
	{ "struct",	structhelp,	0,	setstruct },
	{ "tenex",	tenexhelp,	0,	settenex },
	{ "trace",	tracehelp,	0,	settrace },
	{ "type",	typehelp,	0,	settype },
	{ "user",	userhelp,	0,	user },
	{ "verbose",	verbosehelp,	0,	setverbose },
	{ "?",		helphelp,	0,	help },
	0
};

int	NCMDS = sizeof (cmdtab) / sizeof (cmdtab[0]);
