/*	cmdtab.c	4.3	81/05/31	*/
#include "tip.h"

extern int shell(), getfl(), sendfile(), chdirectory(),
	finish(), help(), pipefile(), consh(), variable(),
	cu_take(), cu_put(), dollar(), genbrk();

esctable_t etable[] =
{
	{ '!',	NORM,	"shell",			 shell },
	{ '<',	NORM,	"receive file from remote host", getfl },
	{ '>',	NORM,	"send file to remote host",	 sendfile },
	{ 't',	NORM,	"take file from remote UNIX",	 cu_take },
	{ 'p',	NORM,	"put file to remote UNIX",	 cu_put },
	{ '|',	NORM,	"pipe remote file",		 pipefile },
#ifdef CONNECT
	{ '%',  NORM,	"connect program to remote host",consh },
#endif
	{ 'c',	NORM,	"change directory",		 chdirectory },
	{ '.',	NORM,	"exit from tip",		 finish },
	{CTRL(d),NORM,	"exit from tip",		 finish },
	{ 's',	NORM,	"set variable",			 variable },
	{ '?',	NORM,	"get this summary",		 help },
	{ '#',	NORM,	"send break",			 genbrk },
	{ 0, 0, 0 }
};
