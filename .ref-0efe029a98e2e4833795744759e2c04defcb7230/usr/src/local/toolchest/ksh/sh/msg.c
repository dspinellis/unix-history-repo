/* @(#)msg.c	1.1 */
/*
 *	UNIX shell
 *	S. R. Bourne
 *	Rewritten by David Korn
 *
 *	AT&T Bell Laboratories
 *
 */


#include	<errno.h>
#include	"flags.h"
#include	"name.h"
#include	"defs.h"
#include	"sym.h"
#include	"builtins.h"
#include	"brkincr.h"
#include	"test.h"
#include	"timeout.h"
#include	"history.h"

#ifdef MULTIBYTE
#include	"national.h"
const MSG version 	= "@(#)Version M-06/03/86a";

#else
const MSG version 	= "@(#)Version 06/03/86a";
#endif /* MULTIBYTE */

extern struct Bfunction randnum;
extern struct Bfunction seconds;

/* error messages */
const MSG	time_warn	= "\r\n\007shell time out in 60 seconds";
const MSG	timed_out	= "timed out waiting for input";
const MSG	recursive	= "recursive call";
const MSG	noquery		= "no query process";
const MSG	nohistory	= "no history file";
const MSG	badopt		= "bad option(s)";
const MSG	mailmsg		= "you have mail in $_";
const MSG	nospace		= "no space";
const MSG	synmsg		= "syntax error";

const MSG	parexp		= ") expected";
const MSG	argexp		= "argument expected";
const MSG	endmatch	= "] missing";
const MSG	badnum		= "bad number";
const MSG	badcooked	= "cannot reset tty to cooked mode";
const MSG	badparam	= "parameter null or not set";
const MSG	unset		= "parameter not set";
const MSG	badsub		= "bad substitution";
const MSG	badcreate	= "cannot create";
const MSG	restricted	= "restricted";
const MSG	nofork		= "cannot fork: too many processes";
const MSG	pexists		= "process already exists";
const MSG	noswap		= "cannot fork: no swap space";
const MSG	piperr		= "cannot make pipe";
const MSG	logout		= "Use 'exit' to logout";
const MSG	badopen		= "cannot open";
const MSG	coredump	= " - core dumped";
const MSG	ptrace		= "ptrace: ";
const MSG	arglist		= "arg list too long";
const MSG	txtbsy		= "text busy";
const MSG	toobig		= "too big";
const MSG	badexec		= "cannot execute";
const MSG	pwderr		= "cannot access parent directories";
const MSG	notfound	= " not found";
const MSG	nomorefiles	= "too many open files";
#ifdef ELIBACC
/* shared library error messages */
const MSG	libacc 		= "can't access a needed shared library";
const MSG	libbad		= "accessing a corrupted shared library";
const MSG	libscn		= ".lib section in a.out corrupted";
const MSG	libmax		= "attempting to link in too many libs";
#endif	/* ELIBACC */
const MSG	badfile		= "bad file unit number";
const MSG	baddir		= "bad directory";
const MSG	badtrap		= "bad trap";
const MSG	wtfailed	= "is read only";
const MSG	notid		= "invalid identifier";
const MSG	badop		= "unknown test operator";
const MSG	noalias		= " alias not found";
const MSG	is_reserved	= " is a reserved word";
const MSG	is_builtin	= " is a shell builtin";
const MSG	is_alias	= " is an alias for ";
const MSG	is_function	= " is a function";
const MSG	is_xalias	= " is an exported alias for ";
const MSG	is_talias	= " is a tracked alias for ";
const MSG	is_xfunction	= " is an exported function";
const MSG	is_		= " is ";
const MSG	on_		= "on";
const MSG	off_		= "off";
const MSG	divzero		= "division by zero";
const MSG	subscript	= "subscript out of range";
const MSG	argcount	= "bad argument count";
const MSG	fn_hdr		= "\n{\n";
#ifdef VSH
const MSG	big_vi		= "fc -e \"${VISUAL:-${EDITOR:-vi}}\" ";
#endif
#ifdef JOBS
#ifdef BSD
const MSG	j_not_tty	= "Warning: no access to tty; thus no job control in this shell...\n";
const MSG	j_newtty	= "Switching to new tty driver...\n";
const MSG	j_oldtty	= "Reverting to old tty driver...\n";
const MSG	j_no_start	= "Cannot start job control\n";
#endif
const MSG	j_Done		= " Done";
const MSG	j_amp		= " &\n";
const MSG	j_cpid		= "|&\n";
const MSG	j_space		= "|\n      ";
const MSG	j_Running	= " Running";
const MSG	j_coredump	= "(coredump)";
const MSG	j_terminate	= "You have stopped jobs\n";
const MSG	j_running	= "You have running jobs\n";
const MSG	j_no_job	= "No such job";
const MSG	j_no_proc	= "No such process";
const MSG	j_perm		= "Permission denied";
const MSG	j_kill		= "kill: ";
const MSG	kill_usage	= "Arguments should be jobs or process ids";
const MSG	j_no_jctl	= "No job control";
#endif
#ifdef DEVFD
const MSG	devfd		= "/dev/fd/";
#endif

/* string constants */
const MSG	test_opts	= "rwxdcbfugkLpsnzt";
const MSG	opt_heading	= "Current option settings";
const MSG	nullstr		= "";
const MSG	sptbnl		= " \t\n";
const MSG	defpath		= "/bin:/usr/bin:";
const MSG	defedit		= "/bin/ed";
const MSG	colon		= ": ";
const MSG	minus		= "-";
const MSG	endoffile	= "end of file";
const MSG	unexpected 	= " unexpected";
const MSG	unmatched 	= " unmatched";
const MSG	unknown 	= "<job name unknown>";
const MSG	let_syntax	= "arithmetic expression ending in single ')' ";
const MSG	atline		= " at line ";
const MSG	devnull		= "/dev/null";
const MSG	execpmsg	= "+ ";
const MSG	supprompt	= "# ";
const MSG	stdprompt	= "$ ";
const MSG	profile		= "${HOME:-.}/.profile";
const MSG	sysprofile	= "/etc/profile";
const MSG	suid_profile	= "/etc/suid_profile";
#ifdef BSD_4_2
const MSG	prohibited	= "login setuid/setgid shells prohibited";
#endif /* BSD_4_2 */
#ifdef SUID_EXEC
const MSG	suid_exec	= "/etc/suid_exec";
const MSG	devfdNN		= "/dev/fd/??";
#endif /* SUID_EXEC */
const MSG	histfname	= "/.sh_history";
const MSG	unlimited	= "unlimited";
#ifdef ECHO_N
const MSG	echo_bin	= "/bin/echo";
const MSG	echo_opt	= "-R";
#endif	/* ECHO_N */
const MSG	btest		= "test";
const MSG	bkill		= "kill";
const MSG	bset		= "set";
const MSG	blet		= "let";
const MSG	bread		= "read";
const MSG	dot		= ".";
const MSG	bltfn		= "function ";
const MSG	intbase		= "base";
const MSG	setpwd		= "PWD=`/bin/pwd 2>/dev/null`";
const MSG	t_real		= "\nreal";
const MSG	t_user		= "user";
const MSG	t_sys		= "sys";

#ifdef apollo
#undef NULL
#define NULL ""
#define nullstr	""
#endif	/* apollo */

/* built in names */ struct name_value node_names[] =
{
	"PATH",		NULL,
	"PS1",		NULL,
	"PS2",		"> ",
#ifdef apollo
	"IFS",		" \t\n",
#else
	"IFS",		sptbnl,
#endif	/* apollo */
	"PWD",		NULL,
	"HOME",		NULL,
	"MAIL",		NULL,
	"REPLY",	NULL,
	"SHELL",	"/bin/sh",
	"EDITOR",	"/bin/ed",
#ifdef apollo
	"MAILCHECK",	NULL,
	"RANDOM",	NULL,
#else
	"MAILCHECK",	(char*)(&mailchk),
	"RANDOM",	(char*)(&randnum),
#endif	/* apollo */
	"ENV",		NULL,
	"HISTFILE",	NULL,
	"HISTSIZE",	NULL,
	"FCEDIT",	"/bin/ed",
	"CDPATH",	NULL,
	"MAILPATH",	NULL,
	"PS3",		"#? ",
	"OLDPWD",	NULL,
	"VISUAL",	NULL,
	"COLUMNS",	NULL,
	"LINES",	NULL,
#ifdef apollo
	"PPID",		NULL,
	"_",		NULL,
	"TMOUT",	NULL,
	"SECONDS",	NULL,
#else
	"PPID",		(char*)(&ppid),
	"_",		(char*)(&lastarg),
	"TMOUT",	(char*)(&timeout),
	"SECONDS",	(char*)(&seconds),
#endif	/* apollo */
#ifdef ACCT
	"SHACCT",	NULL,
#endif	/* ACCT */
	nullstr,	NULL
};


/* built in aliases - automatically exported */
struct name_value alias_names[] =
{
	"cat",		"/bin/cat",
	"chmod",	"/bin/chmod",
	"cc",		"/bin/cc",
	"cp",		"/bin/cp",
	"date",		"/bin/date",
	"ed",		"/bin/ed",
	"false",	"let 0",
	"functions",	"typeset -f",
#ifdef BSD
	"grep",		"/usr/ucb/grep",
#else
	"grep",		"/bin/grep",
#endif	/* BSD */
	"hash",		"alias -t",
	"history",	"fc -l",
	"integer",	"typeset -i",
	"lpr",		"/usr/bin/lpr",
	"ls",		"/bin/ls",
	"make",		"/bin/make",
	"mail",		"/bin/mail",
	"mv",		"/bin/mv",
	"nohup",	"nohup ",
	"pr",		"/bin/pr",
	"r",		"fc -e -",
	"rm",		"/bin/rm",
	"sed",		"/bin/sed",
	"sh",		"/bin/sh",
	"true",		":",
	"type",		"whence -v",
#ifdef BSD
	"vi",		"/usr/ucb/vi",
#else
	"vi",		"/usr/bin/vi",
#endif	/* BSD */
	"who",		"/bin/who",
#ifdef JOBS
#ifdef BSD
	"suspend",	"kill -STOP $$",
#endif	/* BSD */
#endif	/* JOBS */
	nullstr,	0
};

/* tables */
SYSTAB reserved=
{
		{"case",	CASYM},
		{"do",		DOSYM},
		{"done",	ODSYM},
		{"elif",	EFSYM},
		{"else",	ELSYM},
		{"esac",	ESSYM},
		{"fi",		FISYM},
		{"for",		FORSYM},
		{"function",	PROCSYM},
		{"if",		IFSYM},
		{"in",		INSYM},
		{"select",	SELSYM},
		{"then",	THSYM},
		{"time",	TIMSYM},
		{"until",	UNSYM},
		{"while",	WHSYM},
		{"{",		BRSYM},
		{"}",		KTSYM},
		{nullstr,	0},
};

/*
 * The signal numbers go in the low bits and the attributes go in the high bits
 */

SYSTAB	signal_names =
{
		{"ALRM",	(SIGALRM+1)|(SIGCAUGHT<<SIGBITS)},
		{"BUS",		(SIGBUS+1)},
#ifdef SIGCHLD
		{"CHLD",	(SIGCHLD+1)|(SIGNOSET<<SIGBITS)},
#endif	/* SIGCHLD */
#ifdef SIGCLD
		{"CLD",		(SIGCLD+1)|(SIGNOSET<<SIGBITS)},
#endif	/* SIGCLD */
#ifdef SIGCONT
		{"CONT",	(SIGCONT+1)|(SIGNOSET<<SIGBITS)},
#endif	/* SIGCONT */
		{"EMT",		(SIGEMT+1)},
		{"ERR",		(MAXTRAP+1)|(SIGIGNORE<<SIGBITS)},
		{"EXIT",	1|(SIGIGNORE<<SIGBITS)},
		{"FPE",		(SIGFPE+1)},
		{"HUP",		(SIGHUP+1)},
		{"ILL",		(SIGILL+1)|(SIGNOSET<<SIGBITS)},
		{"INT",		(SIGINT+1)|(SIGCAUGHT<<SIGBITS)},
#ifdef SIGIO
		{"IO",		(SIGIO+1)|(SIGNOSET<<SIGBITS)},
#endif	/* SIGIO */
		{"IOT",		(SIGIOT+1)},
		{"KILL",	(SIGKILL+1)|(SIGIGNORE<<SIGBITS)},
#ifdef SIGPHONE
		{"PHONE",	(SIGPHONE+1)|((SIGNOSET|SIGCAUGHT)<<SIGBITS)},
#endif	/* SIGPHONE */
		{"PIPE",	(SIGPIPE+1)},
#ifdef SIGPOLL
		{"POLL",	(SIGPOLL+1)|((SIGNOSET|SIGCAUGHT)<<SIGBITS)},
#endif	/* SIGPOLL */
#ifdef SIGPROF
		{"PROF",	(SIGPROF+1)|(SIGNOSET<<SIGBITS)},
#endif	/* SIGPROF */
#ifdef SIGPWR
# if SIGPWR>0
		{"PWR",		(SIGPWR+1)|(SIGNOSET<<SIGBITS)},
# endif
#endif	/* SIGPWR */
		{"QUIT",	(SIGQUIT+1)|((SIGNOSET|SIGCAUGHT)<<SIGBITS)},
		{"SEGV",	(SIGSEGV+1)},
#ifdef SIGSTOP
		{"STOP",	(SIGSTOP+1)|(SIGNOSET<<SIGBITS)},
#endif	/* SIGSTOP */
		{"SYS",		(SIGSYS+1)|(SIGNOSET<<SIGBITS)},
		{"TERM",	(SIGTERM+1)|(SIGCAUGHT<<SIGBITS)},
#ifdef SIGTINT
		{"TINT",	(SIGTINT+1)|(SIGNOSET<<SIGBITS)},
#endif	/* SIGTINT */
		{"TRAP",	(SIGTRAP+1)|(SIGNOSET<<SIGBITS)},
#ifdef SIGTSTP
		{"TSTP",	(SIGTSTP+1)|(SIGNOSET<<SIGBITS)},
#endif	/* SIGTSTP */
#ifdef SIGTTIN
		{"TTIN",	(SIGTTIN+1)|(SIGNOSET<<SIGBITS)},
#endif	/* SIGTTIN */
#ifdef SIGTTOU
		{"TTOU",	(SIGTTOU+1)|(SIGNOSET<<SIGBITS)},
#endif	/* SIGTTOU */
#ifdef SIGURG
		{"URG",		(SIGURG+1)|(SIGNOSET<<SIGBITS)},
#endif	/* SIGURG */
#ifdef SIGUSR1
		{"USR1",	(SIGUSR1+1)},
#endif	/* SIGUSR1 */
#ifdef SIGUSR2
		{"USR2",	(SIGUSR2+1)},
#endif	/* SIGUSR2 */
#ifdef SIGVTALRM
		{"VTALRM",	(SIGVTALRM+1)|(SIGNOSET<<SIGBITS)},
#endif	/* SIGVTALRM */
#ifdef SIGWINCH
		{"WINCH",	(SIGWINCH+1)|((SIGNOSET|SIGCAUGHT)<<SIGBITS)},
#endif	/* SIGWINCH */
#ifdef SIGWIND
		{"WIND",	(SIGWIND+1)|((SIGNOSET|SIGCAUGHT)<<SIGBITS)},
#endif	/* SIGWIND */
#ifdef SIGXCPU
		{"XCPU",	(SIGXCPU+1)|(SIGNOSET<<SIGBITS)},
#endif	/* SIGXCPU */
#ifdef SIGXFSZ
		{"XFSZ",	(SIGXFSZ+1)|(SIGNOSET<<SIGBITS)},
#endif	/* SIGXFSZ */
		{nullstr,	0}
};

SYSTAB	sig_messages =
{
		{"Alarm call",			(SIGALRM+1)},
		{"Bus error",			(SIGBUS+1)},
#ifdef SIGCHLD
		{"Child stopped or terminated",	(SIGCHLD+1)},
#endif	/* SIGCHLD */
#ifdef SIGCLD
		{"Death of Child", 		(SIGCLD+1)},
#endif	/* SIGCLD */
#ifdef SIGCONT
		{"Stopped process continued",	(SIGCONT+1)},
#endif	/* SIGCONT */
		{"EMT trap",			(SIGEMT+1)},
		{"Floating exception",		(SIGFPE+1)},
		{"Hangup",			(SIGHUP+1)},
		{"Illegal instruction",		(SIGILL+1)},
#ifdef JOBS
		{"Interrupt",			(SIGINT+1)},
#else
		{nullstr,			(SIGINT+1)},
#endif	/* JOBS */
#ifdef SIGIO
		{"IO signal",			(SIGIO+1)},
#endif	/* SIGIO */
		{"abort",			(SIGIOT+1)},
		{"Killed",			(SIGKILL+1)},
		{"Quit",			(SIGQUIT+1)},
#ifdef JOBS
		{"Broken Pipe",			(SIGPIPE+1)},
#else
		{nullstr,			(SIGPIPE+1)},
#endif	/* JOBS */
#ifdef SIGPROF
		{"Profiling time alarm",	(SIGPROF+1)},
#endif	/* SIGPROF */
#ifdef SIGPWR
# if SIGPWR>0
		{"Power fail",			(SIGPWR+1)},
# endif
#endif	/* SIGPWR */
		{"Memory fault",		(SIGSEGV+1)},
#ifdef SIGSTOP
		{"Stopped (signal)",		(SIGSTOP+1)},
#endif	/* SIGSTOP */
		{"Bad system call", 		(SIGSYS+1)},
		{"Terminated",			(SIGTERM+1)},
#ifdef SIGTINT
#ifdef JOBS
		{"Interrupt",			(SIGTINT+1)},
#else
		{nullstr,			(SIGTINT+1)},
#endif	/* JOBS */
#endif	/* SIGTINT */
		{"Trace/BPT trap",		(SIGTRAP+1)},
#ifdef SIGTSTP
		{"Stopped",			(SIGTSTP+1)},
#endif	/* SIGTSTP */
#ifdef SIGTTIN
		{"Stopped (tty input)",		(SIGTTIN+1)},
#endif	/* SIGTTIN */
#ifdef SIGTTOU
		{"Stopped(tty output)",		(SIGTTOU+1)},
#endif	/* SIGTTOU */
#ifdef SIGURG
		{"Socket interrupt",		(SIGURG+1)},
#endif	/* SIGURG */
#ifdef SIGUSR1
		{"User signal 1",		(SIGUSR1+1)},
#endif	/* SIGUSR1 */
#ifdef SIGUSR2
		{"User signal 2",		(SIGUSR2+1)},
#endif	/* SIGUSR2 */
#ifdef SIGVTALRM
		{"Virtual time alarm",		(SIGVTALRM+1)},
#endif	/* SIGVTALRM */
#ifdef SIGWINCH
		{"Window size change", 		(SIGWINCH+1)},
#endif	/* SIGWINCH */
#ifdef SIGXCPU
		{"Exceeded CPU time limit",	(SIGXCPU+1)},
#endif	/* SIGXCPU */
#ifdef SIGXFSZ
		{"Exceeded file size limit",	(SIGXFSZ+1)},
#endif	/* SIGXFSZ */
		{nullstr,	0}
};

SYSTAB option_flags=
{
	{"allexport",		Allexp},
	{"bgnice",		Bgnice},
	{"emacs",		Emacs},
	{"errexit",		Errflg},
	{"gmacs",		Gmacs},
	{"ignoreeof",		Noeof},
#ifdef apollo
	{"inprocess",		Inproc},
#endif	/* apollo */
	{"interactive",		Intflg},
	{"keyword",		Keyflg},
	{"markdirs",		Markdir},
	{"monitor",		Monitor},
	{"noexec",		Noexec},
	{"noglob",		Noglob},
	{"nounset",		Noset},
	{"protected",		Privmod},
	{"restricted",		Rshflg},
	{"trackall",		Hashall},
	{"verbose",		Readpr},
	{"vi",			Editvi},
	{"viraw",		Viraw},
	{"xtrace",		Execpr},
	{nullstr,		0}
};

#ifdef BSD
# ifndef apollo
char  *limit_names[] =
{
	"time(seconds)   ",
	"memory(kbytes)  ",
	"data(kbytes)    ",
	"stack(kbytes)   ",
	"file(blocks)    ",
	"coredump(blocks)"
};
# endif /* apollo */
#endif	/* BSD */

SYSTAB	commands=
{
		{".",		SYSDOT},
		{":",		SYSNULL},
		{"[",		SYSTEST},
		{ "alias",	SYSALIAS},
#ifdef JOBS
#if BSD || SXT
		{ "bg",		SYSBG},
#endif	/* BSD */
#endif	/* JOBS */
		{"break",	SYSBREAK},
		{"cd",		SYSCD},
		{"continue",	SYSCONT},
		{"echo",	SYSECHO},
		{"exec",	SYSEXEC},
		{"exit",	SYSEXIT},
		{"export",	SYSXPORT},
		{"eval",	SYSEVAL},
		{"fc",		SYSFC},
#ifdef JOBS
# if BSD || SXT
		{"fg",		SYSFG},
# endif /* BSD */
#endif	/* JOBS */
#ifdef apollo
		{"inlib",	SYSINLIB},
		{"inprocess",	SYSINPROCESS},
#endif	/* apollo */
#ifdef JOBS
		{"jobs",	SYSJOBS},
		{"kill",	SYSKILL},
#endif	/* JOBS */
		{"let",		SYSLET},
		{"login",	SYSLOGIN},
#ifndef BSD
		{"newgrp",	SYSLOGIN},
#endif	/* BSD */
		{"print",	SYSPRINT},
		{"pwd",		SYSPWD},
		{"read",	SYSREAD},
		{"readonly",	SYSRDONLY},
		{"return",	SYSRETURN},
		{"set",		SYSSET},
		{"shift",	SYSSHFT},
		{"test",	SYSTEST},
		{"times",	SYSTIMES},
		{"trap",	SYSTRAP},
		{"typeset",	SYSTYPESET},
		{"ulimit",	SYSULIMIT},
		{"umask",	SYSUMASK},
		{"unalias",	SYSUNALIAS},
		{"unset",	SYSUNSET},
		{"wait",	SYSWAIT},
		{"whence",	SYSWHENCE},
		{nullstr,		0}
};

SYSTAB	testops =
{
		{"!=",		TEST_SNE},
		{"-a",		TEST_AND},
		{"-ef",		TEST_EF},
		{"-eq",		TEST_EQ},
		{"-ge",		TEST_GE},
		{"-gt",		TEST_GT},
		{"-le",		TEST_LE},
		{"-lt",		TEST_LT},
		{"-ne",		TEST_NE},
		{"-nt",		TEST_NT},
		{"-o",		TEST_OR},
		{"-ot",		TEST_OT},
		{"=",		TEST_SEQ},
		{nullstr,	0}
};

SYSTAB	attributes =
{
		{"export",	N_EXPORT},
		{"readonly",	N_RDONLY},
		{"tagged",	T_FORM},
#ifdef apollo
		{" filename",	A_FLAG},
#endif	/* apollo */
		{"long",	(L_FLAG|INT_GER)},
		{"unsigned",	(UN_SIGN|INT_GER)},
		{"function",	(BLT_NOD|INT_GER)},
		{"integer",	INT_GER},
		{"lowercase",	U_TO_L},
		{"zerofill",	Z_FILL},
		{"leftjust",	L_JUST},
		{"rightjust",	R_JUST},
		{"uppercase",	L_TO_U},
		{nullstr,	0}
};


#ifdef BSD
int tty_speeds[] = {0, 50, 75, 110, 134, 150, 200, 300,
			600,1200,1800,2400,9600,19200,0};
#endif /* BSD */

#ifdef MULTIBYTE
char int_charsize[] =
{
	1, CCS1_IN_SIZE, CCS2_IN_SIZE, CCS3_IN_SIZE,	/* input sizes */
	1, CCS1_OUT_SIZE, CCS2_OUT_SIZE, CCS3_OUT_SIZE	/* output widths */
};
#endif /* MULTIBYTE */

