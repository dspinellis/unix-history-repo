/*
 * NIM daemon command parser
 *
 * Frank Pronk
 * Copyright (c) 1984
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netccitt/x25.h>
#include <netdb.h>

#include "../h/x29.h"

#include "nim.h"

#define	NEXTC	(*sym.s_next ? *sym.s_next++ : (char)0)

#define SSET	0200+0
#define SPAR	0200+1
#define SPROF	0200+2
#define SSTATUS	0200+3
#define SRESET	0200+4
#define SINT	0200+5
#define SINTD	0200+6
#define SCLEAR	0200+7
#define SHELP	0200+8
#define SCALL	0200+9
#define SPRI	0200+10
#define SREV	0200+11
#define SNUM	0200+12
#define SSTR	0200+13
#define SNUI	0200+14

struct	NIMCommand {
	char	*c_name;
	short	c_value;
} Commands[] = {
	"set",		SSET,
	"par",		SPAR,
	"prof",		SPROF,
	"profile",	SPROF,
	"stat",		SSTATUS,
	"status",	SSTATUS,
	"reset",	SRESET,
	"int",		SINT,
	"intd",		SINTD,
	"clear",	SCLEAR,
	"help",		SHELP,
	"call",		SCALL,
	"p",		SPRI,
	"rev",		SREV,
	"nui",		SNUI,
	0,		0,
};

struct	sockaddr_x25 RemoteHostAddr;
char	*RemoteHostName;
char	*Nui;		/* network user identification */

/*
 * Structure used by insymbol() keep track of its
 * current position in the command string being
 * parsed and to hold the results of the last
 * call to insymbol()
 */

struct	symbol {
	char	*s_start;	/* start of line to be parsed */
	char	*s_next;	/* current position in line */
	short	s_type;		/* see defines above */
	int	s_num;		/* value if current symbol is a number */
	char	*s_str;		/* address of symbol if a string */
} sym;

/*
 * List of symbolic names that can be
 * used as replacements for numeric codes
 * in specific "set parameter" requests
 */

struct	NameList {
	char	*n_name;
	short	n_value;
} ForwardList[]	= { "cr", 2, "control", 126, "off", 0, 0, 0 },
  OnOffList[]	= { "on", 1, "off", 0, 0, 0},
  LfList[]	= { "on", 4, "off", 0, "none", 0, "local", 4, "remote", 1, "both", 5, 0, 0};

/*
 * list of x.29 symbolic keywords that can be substituted
 * for numeric codes.  Note that this list contains only
 * the x.29 parameters that an average user would ever
 * want to change.
 */

struct	X29Keyword {
	char	*x_word;	/* parameter name */
	short	x_param;	/* parameter code */
	struct	NameList *x_list;
} X29Keywords[]	= {
	"escape",	X29_ESCAPE_TO_CMD_CODE,		OnOffList,
	"echo",		X29_ECHO_CODE,			OnOffList,
	"forward",	X29_FORWARDING_SIGNAL_CODE,	ForwardList,
	"timer",	X29_IDLE_TIMER_CODE,		OnOffList,
	"break",	X29_BREAK_PROCEDURE_CODE,	0,
	"lf",		X29_LF_AFTER_CR,		LfList,
	"lf-insertion",	X29_LF_AFTER_CR,		LfList,
	"editing",	X29_EDITING,			OnOffList,
	"erase",	X29_CHARACTER_DELETE,		0,
	"kill",		X29_LINE_DELETE,		0,
	"replay",	X29_LINE_DISPLAY,		0,
	"display",	X29_LINE_DISPLAY,		0,
	0,		0,				0
};

/*
 * Attempt to parse string pointed to by 'cp'.
 */

NimCommand(cp)
register char *cp;
{

	sym.s_start = sym.s_next = cp;
	/*
	 * strip parity bit from command
	 */
	while ((*cp&0177) != '\0')
		*cp++ &= 0177;
	switch(insymbol(0)) {
	case SCLEAR:
		ClearCommand();
		return;

	case SHELP:
	case '?':
		HelpCommand();
		return;

	case SINT:
		InterruptCommand(0);
		break;

	case SINTD:
		InterruptCommand(1);
		return;

	case SPAR:
		ParCommand();
		return;

	case SPROF:
		ProfileCommand();
		return;

	case SRESET:
		ResetCommand();
		return;

	case SSET:
		SetCommand();
		return;

	case SSTATUS:
		StatusCommand();
		return;

	case SPRI:
	case SREV:
	case SNUM:
		X121CallCommand();
		return;

	case SCALL:
		CallCommand();
		return;

	case SNUI:
		NuiCommand ();
		return;

	case '\0':
		NullCommand();
		return;

	case '.':
		DotCommand();
		return;

	default:
		message ("Unknown NIM command\r");
		return;
	}
}

GetNumber(np)
register struct NameList *np;
{

	(void) insymbol(0);
	if (sym.s_type == SNUM)
		return (1);
	if (sym.s_type == '(') {
		register int value;

		switch (insymbol(1)) {
		case SSTR:
			if (strlen (sym.s_str) != 1)
				return (0);
			value = *sym.s_str;
			break;

		case SNUM:
			value = sym.s_num;
			break;

		default:
			if (sym.s_type == '\r')
				return (0);
			value = sym.s_type;
		}
		(void) insymbol(0);
		if(sym.s_type == ')') {
			sym.s_num = value;
			return(1);
		}
		return(0);
	}
	if (sym.s_type == SSTR && np)
		for (; np->n_name; np++)
			if (strcmp(np->n_name, sym.s_str) == 0) {
				sym.s_num = np->n_value;
				return (1);
			}
	return (0);
}

/*
 * Save the string pointed to by 's'
 */

char *
saves(s)
char *s;
{
	register char *p;
	char *malloc();

	p = malloc(strlen(s) + 1);
	strcpy(p, s);
	return (p);
}

char
GetEscaped ()
{
	register char c, value;

	c = NEXTC;
	if (c < '0' || c > '7')
		return (c);
	value = c - '0';
	c = NEXTC;
	if (c >= '0' && c <= '7') {
		register int n;

		value = value*8 + c - '0';
		c = NEXTC;
		if (c >= '0' && c <= '7' && (n = value*8 + c-'0') < 256) {
			value = n;
			c = NEXTC;
		}
	}
	return (value);
}

insymbol(special)
{
	register char c, *cp;
	char buf[128];

	if (sym.s_str) {
		free(sym.s_str);
		sym.s_str = 0;
	}

	cp = buf;
	while ((c = NEXTC) == ' ' || c == '\t');

	if (c == '\'' || c == '"') {
		register char quote = c;

		while (1) {
			c = NEXTC;
			if (c == '\0')
				return (sym.s_type = 0);
			if (c == quote)
				break;
			if (c == '\\')
				c = GetEscaped ();
			if(cp < buf + sizeof (buf))
				*cp++ = c;
		}
		*cp = '\0';
		sym.s_type = SSTR;
		sym.s_str = saves (buf);
		return (sym.s_type);
	}

	if (c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '\\') {
		do {
			if (c == '\\')
				c = GetEscaped ();
			if(cp < buf + sizeof (buf)) {
				if (!special && c >= 'A' && c <= 'Z')
					c -= 'A' - 'a';
				*cp++ = c;
			}
			c = NEXTC;
		} while (c >= '0' && c <= '9' || c >= 'a' && c <= 'z' ||
			 c >= 'A' && c <= 'Z' || c == '_' || c == '-');
		*cp = '\0';
		if (c)
			sym.s_next--;

		/*
		 * set symbol type to SSTR and
		 * then look for keywords
		 */

		sym.s_type = SSTR;
		if (!special) {
			register struct NIMCommand *np;

			cp = &buf[0];
			for (np = Commands; np->c_name; np++)
				if(strcmp(cp, np->c_name) == 0) {
					sym.s_type = np->c_value;
					break;
				}
		}
		if (sym.s_type == SSTR)
			sym.s_str = saves(buf);
		return (sym.s_type);
	}

	if (c >= '0' && c <= '9') {
		do {
			if(cp < buf + sizeof (buf))
				*cp++ = c;
			c = NEXTC;
		} while(c >= '0' && c <= '9');
		*cp = '\0';
		if (c)
			sym.s_next--;
		sym.s_num = atoi(buf);
		sym.s_type = SNUM;
		sym.s_str = saves(buf);
		return (SNUM);
	}
	return (sym.s_type = c);
}

/*
 * Does nothing useful - retained for
 * compatibility with Datapac.
 */

DotCommand()
{
	message (Banner);
}

X121CallCommand()
{
	register int len, havenet = 0;

	if (State & ST_DATA) {
		message ("You are connected to %s, please clear this call before attempting another\r",
			RemoteHostName?RemoteHostName:(char *)RemoteHostAddr.x25_addr);
		return;
	}
	bzero ((char *)&RemoteHostAddr, sizeof (RemoteHostAddr));
	if (RemoteHostName) {
		free (RemoteHostName);
		RemoteHostName = (char *)0;
	}

	while (sym.s_type == SPRI || sym.s_type == SREV) {
		if (sym.s_type == SPRI)		/* Datapac specific */
			RemoteHostAddr.x25_opts.op_psize = X25_PS128;
		else if (sym.s_type == SREV)
			RemoteHostAddr.x25_opts.op_flags |= X25_REVERSE_CHARGE;
		(void) insymbol(0);
	}
getaddr:
	if (sym.s_type != SNUM) {
		message("Non-numeric destination address\r");
		return;
	}
	if (strlen(sym.s_str) > sizeof (RemoteHostAddr.x25_addr) - 1) {
		message ("Destination address too long\r");
		return;
	}
	strcpy(RemoteHostAddr.x25_addr, sym.s_str);

	(void) insymbol(1);
	if (sym.s_type == ':' || sym.s_type == '.') {
		if (havenet) {
			message("Destination address error\r");
			return;
		}
		havenet++;
		RemoteHostAddr.x25_net = atoi(RemoteHostAddr.x25_addr);
		(void) insymbol(1);
		goto getaddr;
	}

	if (sym.s_type == ',')		/* skip optional comma */
		(void) insymbol(1);

	RemoteHostAddr.x25_udata[0] = ITI_CALL;
	RemoteHostAddr.x25_udlen = 4;
	if (sym.s_type == SSTR || sym.s_type == SNUM) {
		if ((len = strlen(sym.s_str)) > sizeof (RemoteHostAddr.x25_udata) - 4) {
			message ("Userdata field too long\r");
			RemoteHostAddr.x25_addr[0] = '\0';
			return;
		}
		strcpy(RemoteHostAddr.x25_udata + 4, sym.s_str);
		RemoteHostAddr.x25_udlen += len;
		(void) insymbol(1);
	}

	if (sym.s_type == ',')		/* skip optional comma */
		(void) insymbol(1);
	if (sym.s_type == SSTR || sym.s_type == SNUM) {	/* protocol id */
		if (strlen(sym.s_str) > 4) {
			message ("Protocol field too long\r");
			RemoteHostAddr.x25_addr[0] = '\0';
			return;
		}
		strcpy(RemoteHostAddr.x25_udata, sym.s_str);
		(void) insymbol(0);
	}

	if (sym.s_type != '\0') {	/* still more to come? */
		message ("usage: [options] address [userdata] [protocol id]\r");
		RemoteHostAddr.x25_addr[0] = '\0';
		return;
	}

	InitiateSession ();
}

CallCommand ()
{
	register struct hostent *hp;
	struct hostent *getx25hostbyname ();

	if (insymbol(1) == '\0') {
		if (RemoteHostAddr.x25_addr[0] == '\0') {
			message ("Call who?\r");
			return;
		}
	} else {
		if (sym.s_type != SSTR) {
			message("nimd: usage: call [hostname]\r");
			return;
		}
		if ((hp = getx25hostbyname (sym.s_str)) == 0) {
			message ("nimd: can't find \"%s\" in host table\r",
				sym.s_str);
			return;
		}
		if (RemoteHostName)
			free (RemoteHostName);
		RemoteHostName = saves (sym.s_str);
		bcopy (hp->h_addr, (char *)&RemoteHostAddr, sizeof (RemoteHostAddr));
		RemoteHostAddr.x25_udata[0] = ITI_CALL;
	}
	InitiateSession ();
}
	
InitiateSession ()
{
	register char *cp;
	struct sockaddr_x25 peer;
	int slen = sizeof (struct sockaddr_x25);
	char buf[256];
	int on = 1;

#ifdef waterloo
	extern char user_name[];

	if ((RemoteHostAddr.x25_opts.op_flags & X25_REVERSE_CHARGE) == 0 &&
	     !(user_name[0] ? x25_can_callin(user_name) : 1)) {
		message("You may not place locally charged calls\r");
		return;
	}
#endif
	sprint (buf, "calling %s", RemoteHostAddr.x25_addr);
	cp = buf + strlen (buf);
	if (RemoteHostName) {
		sprint (cp, " (%s)", RemoteHostName);
		cp = cp + strlen (cp);
	}
	if (RemoteHostAddr.x25_opts.op_flags & X25_REVERSE_CHARGE)
		strcat (cp, " collect");
	log (buf);
	if ((NetFd = socket (AF_CCITT, SOCK_STREAM, 0)) < 0) {
		error ();
		return;
	}
	RemoteHostAddr.x25_family = AF_CCITT;
	RemoteHostAddr.x25_opts.op_flags |= X25_MQBIT;
	if (connect (NetFd, (char *)&RemoteHostAddr, sizeof (RemoteHostAddr)) < 0) {
		error ();
		close (NetFd);
		NetFd = -1;
		return;
	}
	if (getpeername (NetFd, (struct sockaddr *)&peer, &slen))
		error ();
	NetInfo.n_psize = (1 << peer.x25_opts.op_psize);
	log ("call succeeded: packet size=%d", NetInfo.n_psize);
	if (CurrentX29Parms[X29_RECEIVE_NET_MSGS_CODE]) {
		message ("nimd:\tCall connected to %s", RemoteHostAddr.x25_addr);
		if (RemoteHostName)
			message (" (%s)", RemoteHostName);
		message ("\r\t(%s charging, packet size: %d)\r\r",
			RemoteHostAddr.x25_opts.op_flags & X25_REVERSE_CHARGE ?
			"remote" : "local", NetInfo.n_psize);
	}

	ioctl (NetFd, FIONBIO, (char *)&on);
	State |= ST_DATA;
	State &= ~ST_COMMAND;
}

struct X29Keyword *
LookupX29Keyword(word)
char *word;
{
	register struct X29Keyword *xp;

	for (xp = X29Keywords; xp->x_word; xp++)
		if (strcmp(word, xp->x_word) == 0)
			return (xp);
	return ((struct X29Keyword *)0);
}

ParCommand()
{
	register int param;
	register struct X29Keyword *xp;

	if (insymbol(0) == '\0') {
		DisplayCurrentParms();
		return;
	}
	while (sym.s_type != '\0') {
		switch (sym.s_type) {
		case SNUM:
			param = sym.s_num;
			xp = 0;
			break;

		case SSTR:
			if ((xp = LookupX29Keyword(sym.s_str)) == 0) {
				message("%s: unknown X.29 parameter\r", sym.s_str);
				return;
			}
			param = xp->x_param;
			break;

		default:
			message("number or X.29 parameter name expected\r");
			return;
		}
		if (xp)
			message ("%s:%d\r", xp->x_word, CurrentX29Parms[param]);
		else
			message ("%d:%d\r", param, CurrentX29Parms[param]);
		if (insymbol(0) == ',')		/* skip optional comma */
			(void) insymbol(0);
	}
}

DisplayCurrentParms()
{
	register int nparams, pnum, i;

	nparams = NetInfo.n_nparms;
	for (i=0; i<nparams; i++) {
		pnum = pnums[i];
		message (i%6 ? ", %d:%d" : "\t%d:%d", pnum,
			CurrentX29Parms[pnum]);
		if (i % 6 == 5)
			message ("\r");
	}
	if (nparams % 6)
		message ("\r");
}

SetCommand()
{
	register int setandread = 0, param;
	register struct X29Keyword *xp;

	(void) insymbol(0);
	if (sym.s_type == '?') {
		setandread++;
		insymbol(0);
	}
	while (sym.s_type != '\0') {
		switch (sym.s_type) {
		case SNUM:
			param = sym.s_num;
			xp = 0;
			break;

		case SSTR:
			if ((xp = LookupX29Keyword(sym.s_str)) == 0) {
				message("%s: unknown X.29 parameter\r", sym.s_str);
				return;
			}
			param = xp->x_param;
			break;

		default:
			message("number or X.29 parameter name expected\r");
			return;
		}
		(void) insymbol(0);
		if (sym.s_type != ':' && sym.s_type != '=') {
			message("':' or '=' expected between parameter and value\r");
			return;
		}
		if (GetNumber(xp ? xp->x_list : 0)) {
			if (SetX29Parm(param, sym.s_num))
				if (xp)
					message ("par %s:invalid\r", xp->x_word);
				else
					message("par %d:invalid\r", param);
			else
				if (setandread)
					if (xp)
						message ("par %s:%d\r",
							xp->x_word, sym.s_num);
					else
						message("PAR %d:%d\r", param, sym.s_num);
		} else {
			message("number expected\r");
			return;
		}
		(void) insymbol(0);
		if (sym.s_type == ',')		/* skip optional comma */
			(void) insymbol(0);
	}
}

ProfileCommand()
{
	register int displayonly = 0, profile;

	if (insymbol(0) == '?') {
		displayonly++;
		(void) insymbol(0);
	}
	if (sym.s_type == SNUM) {
		profile = sym.s_num;
		if (profile >= 0 && profile <= 6)
			if (insymbol(0) == '\0') {
				if (displayonly)
					DisplayProfile(profile);
				else
					InitProfile(profile);
				return;
			}
	}
	message("usage: prof n or prof?n where n is a number between 1 and 6\r");
}

DisplayProfile (profile)
{
	register int i, nparms, pnum, value;
	extern char profiles[NPROFILES+1][NX29_PARMS];

	nparms = NetInfo.n_nparms;
	for (i=1; i<=nparms; i++) {
		pnum = pnums[i];
		value = profiles[profile][pnum];
		if (i % 6 == 0)
			if (i == 0)
				message ("prof %d\t%d:%d", profile, pnum, value);
			else
				message ("\t%d:%d", pnum, value);
		else
			message (", %d:%d", pnum, value);
		if (i % 6 == 5)
			message ("\r");
	}
	if (nparms % 6)
		message ("\r");
}

StatusCommand ()
{
	if (State & ST_DATA)
		message ("Connected to %s\r", RemoteHostName);
	else
		message ("idle\r");
}

InterruptCommand(discard)
{
	if (discard)
		Break (21);
	else
		Break (1);
}

ClearCommand ()
{
	if (State & ST_DATA)
		ExitDataState ("local directive");
	else
		message ("you are not connected to anybody\r");
}

NullCommand ()
{
	if (State & ST_ESCCOMM)
		State &= ~(ST_COMMAND|ST_ESCCOMM);
	else
		message (Banner);
}

ResetCommand()
{
	ResetBufs ();
}

HelpCommand()
{
	GetHelp ("general-info");
}

GetHelp (topic)
char *topic;
{
	register int fd;
	char HelpFile[128];

	strcpy (HelpFile, HELPFILE);
	strcat (HelpFile, topic);
	if ((fd = open (HelpFile, 0)) < 0) {
		message ("No help with %s\r", topic);
		return;
	}
	/* not yet implemented */
	close (fd);
}

NuiCommand ()		/* net completely implemented */
{
	if (insymbol (1) == '\0') {
		if (Nui)
			message ("NUI = %s\r", Nui);
		else
			message ("No valid NUI\r");
		return;
	}
}
