/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)cmdtab.c	5.1 (Berkeley) %G%";
#endif not lint

#include "ftp_var.h"

/*
 * User FTP -- Command Tables.
 */
int	setascii(), setbell(), setbinary(), setdebug(), setform();
int	setglob(), sethash(), setmode(), setpeer(), setport();
int	setprompt(), setstruct();
int	settenex(), settrace(), settype(), setverbose();
int	disconnect();
int	cd(), lcd(), delete(), mdelete(), user();
int	ls(), mls(), get(), mget(), help(), append(), put(), mput();
int	quit(), renamefile(), status();
int	quote(), rmthelp(), shell();
int	pwd(), makedir(), removedir();

char	appendhelp[] =	"append to a file";
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
char	globhelp[] =	"toggle metacharacter expansion of local file names";
char	hashhelp[] =	"toggle printing `#' for each buffer transferred";
char	helphelp[] =	"print local help information";
char	lcdhelp[] =	"change local working directory";
char	lshelp[] =	"nlist contents of remote directory";
char	mdeletehelp[] =	"delete multiple files";
char	mdirhelp[] =	"list contents of multiple remote directories";
char	mgethelp[] =	"get multiple files";
char	mkdirhelp[] =	"make directory on the remote machine";
char	mlshelp[] =	"nlist contents of multiple remote directories";
char	modehelp[] =	"set file transfer mode";
char	mputhelp[] =	"send multiple files";
char	porthelp[] =	"toggle use of PORT cmd for each data connection";
char	prompthelp[] =	"force interactive prompting on multiple commands";
char	pwdhelp[] =	"print working directory on remote machine";
char	quithelp[] =	"terminate ftp session and exit";
char	quotehelp[] =	"send arbitrary ftp command";
char	receivehelp[] =	"receive file";
char	remotehelp[] =	"get help from remote server";
char	renamehelp[] =	"rename file";
char	rmdirhelp[] =	"remove directory on the remote machine";
char	sendhelp[] =	"send one file";
char	shellhelp[] =	"escape to the shell";
char	statushelp[] =	"show current status";
char	structhelp[] =	"set file transfer structure";
char	tenexhelp[] =	"set tenex file transfer type";
char	tracehelp[] =	"toggle packet tracing";
char	typehelp[] =	"set file transfer type";
char	userhelp[] =	"send new user information";
char	verbosehelp[] =	"toggle verbose mode";

struct cmd cmdtab[] = {
	{ "!",		shellhelp,	0,	0,	shell },
	{ "append",	appendhelp,	1,	1,	put },
	{ "ascii",	asciihelp,	0,	1,	setascii },
	{ "bell",	beephelp,	0,	0,	setbell },
	{ "binary",	binaryhelp,	0,	1,	setbinary },
	{ "bye",	quithelp,	0,	0,	quit },
	{ "cd",		cdhelp,		0,	1,	cd },
	{ "close",	disconhelp,	0,	1,	disconnect },
	{ "delete",	deletehelp,	0,	1,	delete },
	{ "debug",	debughelp,	0,	0,	setdebug },
	{ "dir",	dirhelp,	1,	1,	ls },
	{ "form",	formhelp,	0,	1,	setform },
	{ "get",	receivehelp,	1,	1,	get },
	{ "glob",	globhelp,	0,	0,	setglob },
	{ "hash",	hashhelp,	0,	0,	sethash },
	{ "help",	helphelp,	0,	0,	help },
	{ "lcd",	lcdhelp,	0,	0,	lcd },
	{ "ls",		lshelp,		1,	1,	ls },
	{ "mdelete",	mdeletehelp,	1,	1,	mdelete },
	{ "mdir",	mdirhelp,	1,	1,	mls },
	{ "mget",	mgethelp,	1,	1,	mget },
	{ "mkdir",	mkdirhelp,	0,	1,	makedir },
	{ "mls",	mlshelp,	1,	1,	mls },
	{ "mode",	modehelp,	0,	1,	setmode },
	{ "mput",	mputhelp,	1,	1,	mput },
	{ "open",	connecthelp,	0,	0,	setpeer },
	{ "prompt",	prompthelp,	0,	0,	setprompt },
	{ "sendport",	porthelp,	0,	0,	setport },
	{ "put",	sendhelp,	1,	1,	put },
	{ "pwd",	pwdhelp,	0,	1,	pwd },
	{ "quit",	quithelp,	0,	0,	quit },
	{ "quote",	quotehelp,	1,	1,	quote },
	{ "recv",	receivehelp,	1,	1,	get },
	{ "remotehelp",	remotehelp,	0,	1,	rmthelp },
	{ "rename",	renamehelp,	0,	1,	renamefile },
	{ "rmdir",	rmdirhelp,	0,	1,	removedir },
	{ "send",	sendhelp,	1,	1,	put },
	{ "status",	statushelp,	0,	0,	status },
	{ "struct",	structhelp,	0,	1,	setstruct },
	{ "tenex",	tenexhelp,	0,	1,	settenex },
	{ "trace",	tracehelp,	0,	0,	settrace },
	{ "type",	typehelp,	0,	1,	settype },
	{ "user",	userhelp,	0,	1,	user },
	{ "verbose",	verbosehelp,	0,	0,	setverbose },
	{ "?",		helphelp,	0,	0,	help },
	{ 0 },
};

int	NCMDS = (sizeof (cmdtab) / sizeof (cmdtab[0])) - 1;
