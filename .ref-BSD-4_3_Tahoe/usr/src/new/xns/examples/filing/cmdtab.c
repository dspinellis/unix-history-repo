/* $Header: cmdtab.c,v 2.2 86/12/11 06:12:07 jqj Exp $ */
/* $Log:	cmdtab.c,v $
 * Revision 2.2  86/12/11  06:12:07  jqj
 * Eliminated form, mode, and struct commands.  Started adding support for
 * more file types.
 * 
 * Revision 2.1  86/06/02  07:10:04  jqj
 * bugfix in NCMDS
 * 
 * Revision 2.0  85/11/21  07:22:43  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.1  85/11/20  14:18:58  jqj
 * Initial revision
 * 
 */
#ifndef lint
static char sccsid[] = "@(#)cmdtab.c	4.7 (Berkeley) 7/26/83";
#endif

#include "ftp_var.h"

/*
 * User FTP -- Command Tables.
 */
int	setascii(), setbell(), setbinary(), setdebug();
int	setglob(), sethash(), setpeer();
int	setprompt();
int	settrace(), settype(), setverbose();
int	disconnect();
int	cd(), lcd(), delete(), mdelete(), user();
int	ls(), mls(), get(), mget(), help(), append(), put(), mput();
int	quit(), renamefile(), status();
int	shell();
int	pwd(), makedir(), removedir();

char	appendhelp[] =	"append to a file";
char	asciihelp[] =	"set ascii (tText) transfer type";
char	beephelp[] =	"beep when command completed";
char	binaryhelp[] =	"set binary (tUnspecified) transfer type";
char	cdhelp[] =	"change remote working directory";
char	connecthelp[] =	"connect to remote tftp";
char	deletehelp[] =	"delete remote file";
char	debughelp[] =	"toggle/set debugging mode";
char	dirhelp[] =	"list contents of remote directory";
char	disconhelp[] =	"terminate ftp session";
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
char	mputhelp[] =	"send multiple files";
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
	{ "mput",	mputhelp,	1,	1,	mput },
	{ "open",	connecthelp,	0,	0,	setpeer },
	{ "prompt",	prompthelp,	0,	0,	setprompt },
	{ "put",	sendhelp,	1,	1,	put },
	{ "pwd",	pwdhelp,	0,	1,	pwd },
	{ "quit",	quithelp,	0,	0,	quit },
	{ "recv",	receivehelp,	1,	1,	get },
	{ "rename",	renamehelp,	0,	1,	renamefile },
	{ "rmdir",	rmdirhelp,	0,	1,	removedir },
	{ "send",	sendhelp,	1,	1,	put },
	{ "status",	statushelp,	0,	0,	status },
	{ "trace",	tracehelp,	0,	0,	settrace },
	{ "type",	typehelp,	0,	1,	settype },
	{ "user",	userhelp,	0,	1,	user },
	{ "verbose",	verbosehelp,	0,	0,	setverbose },
	{ "?",		helphelp,	0,	0,	help },
	{ 0 },
};

int	NCMDS = (sizeof (cmdtab) / sizeof (cmdtab[0]) - 1);

