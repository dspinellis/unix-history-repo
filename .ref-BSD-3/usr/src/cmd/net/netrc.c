/*
	netrc.c

	procedures to read and parse the .netrc file

	You may call:
		commandfile() 		to read the file.
		rdnetfile(cfile)	to read the file.

Note:
	commandfile()
		will read the passwd file
		if getenv(HOME) searches the passwd file

Table of netrc options
	option			default
	------			-------
	default			default machine
	login string		current login
	password string		-
	notify yes/no		yes
	write yes/no		yes
	command string		-
	force yes/no		no

Fabry has suggested that machine names be more general:
that you be able to say:

	cory:	fabry on Cory
	caf:	caf on Cory
	c:	fabry on C

so the formulation would look like:

	default key
	key: machine login passwd ...
	key: ....

and so on

Gould has suggested the format be:

	pseudo cory 	real Cory 	login fabry
	pseudo caf 	real Cory 	login caf
	pseudo c 	real C 		login fabry

Init file example:
format local C remote A

	default A
	machine A    local C link /dev/net-A    speed 9
	machine Cory local C link /dev/net-Cory speed 9
	
if remote == 0, default is A
also options:
	vaxtovax, length, debug

speed: speeds baud setting
	300	7
	1200	9
	9600	13
onlyuid:
	should be a decimal uid, as returned by getuid()
	if 0 is considered ignored.

passwords work as follows:
   passwd = "\n" means no password

*/
# include "defs.h"

/* tokens, returned by parser */
# define MACHINE 1
# define LOGIN 2
# define PASSWORD 3
# define ONLYUID 4
# define NOTIFY 5
# define COMMAND 7
# define ID 8
# define YES 9
# define DEFAULT 10
# define WRITE 11
# define NO 12
# define FORCE 13
# define LOCALTOK 14
# define LINK 15
# define SPEED 16
# define VAXTOVAX 17
# define LENGTH 18
# define DEBUGTOK 19
# define ALTIME 20
# define ALCOUNT 21

/* global */
char vaxtovax;
int linkspeed;
char local;
int maxbread;
int atime;
char device[20];
int datasize;
int onlyuid = 0;

/* local */
static char tokval[BUFSIZ];

static struct tokstruct {
	char *tokstr;
	int tval;
}	toktab[]= {
	"machine",	MACHINE,
	"login",	LOGIN,
	"password",	PASSWORD,
	"onlyuid",	ONLYUID,
	"notify",	NOTIFY,
	"command",	COMMAND,
	"yes",		YES,
	"y",		YES,
	"no",		NO,
	"n",		NO,
	"default",	DEFAULT,
	"write",	WRITE,
	"force",	FORCE,
	"local",	LOCALTOK,
	"speed",	SPEED,
	"link",		LINK,
	"vaxtovax",	VAXTOVAX,
	"length",	LENGTH,
	"debug",	DEBUGTOK,
	"time",		ALTIME,
	"count",	ALCOUNT,
	0,		0
	};

static struct stat statbuf;

/*
	commandfile()

	this procedure reads in and parses the .netrc file.
	when you call this, if the remote machine is to be explicitely
	set, the global variable "remote" must have a value.
	on return, if it is non-zero, "remote" will have the
	remote machine the data was collected for.
	status.localname need not have a value.
*/
commandfile(){
	char *hdir, buf[BUFSIZ];
	FILE *cfile;
	hdir = getenv("HOME");
	if(hdir == NULL)hdir = ".";
	sprintf(buf,"%s/.netrc",hdir);
/*
	debug("file %s",buf);
*/
	cfile = fopen(buf,"r");
	if(cfile == NULL)return;
	rdnetfile(cfile);
	fclose(cfile);
	}
/*
	read the file cfile and parse
*/
rdnetfile(cfile)
	FILE *cfile;
{
	int t;
	if(cfile == NULL)return;
	if(fstat(fileno(cfile),&statbuf) < 0 || (statbuf.st_mode & 0444) == 0)
		return;
	while((t = token(cfile))){
		switch(t){
		case DEFAULT:
			if(token(cfile) == ID && remote == 0)remote = lookup(tokval);
			/*
			debug("rem %c\n",remote);
			*/
			break;
		case MACHINE:
			if(remote == 0)remote = getremote(local);
			if(token(cfile) != ID)continue;
			if(remote != lookup(tokval))continue;
			/* this is the entry for the remote mach we want */
			getnetline(cfile);
			return;
			break;
		}
		}
	return;
	}
/*
	read a line of the file
*/
static getnetline(cfile)
	FILE *cfile;
{
	int t;
	while((t = token(cfile))){
		switch(t){
		/* these options are usually in the .netrc file */
		case MACHINE: return;
		case LOGIN:
			if(token(cfile) && status.login[0] == 0)
				strcpy(status.login,tokval);
			break;
		case PASSWORD:
			if(fstat(fileno(cfile),&statbuf) >= 0
			&& (statbuf.st_mode & 077) != 0){
				err("Error - .netrc file not correct mode.\n");
				err("Remove password or correct mode.\n");
				exit(1);
				}
			if(token(cfile) && status.mpasswd[0] == 0)
				strcpy(status.mpasswd,tokval);
			/*
			debug("mp:%s:%s\n",status.mpasswd,tokval);
			*/
			break;
		case NOTIFY:
			status.nonotify = token(cfile) == NO;
			break;
		case WRITE:
			status.nowrite = token(cfile) == NO;
			break;
		case COMMAND:
			if(token(cfile) && status.defcmd[0] == 0)
				strcpy(status.defcmd,tokval);
			break;
		case FORCE:
			status.force = token(cfile) == YES;
			break;

		/* these options are usually in /usr/net/initfile */
		case LOCALTOK:
			if(token(cfile))local = lookup(tokval);
			break;
		case LINK:
			if(token(cfile))strcpy(device,tokval);
			break;
		case SPEED:
			if(token(cfile))linkspeed = atoi(tokval);
			break;
		case VAXTOVAX:
			vaxtovax++;
			break;
		case LENGTH:
			if(token(cfile))datasize = atoi(tokval);
			break;
		case DEBUGTOK:
			debugflg++;
			break;
		case ALTIME:
			if(token(cfile))atime = atoi(tokval);
			break;
		case ALCOUNT:
			if(token(cfile))maxbread = atoi(tokval);
			break;
		case ONLYUID:
			if(token(cfile))onlyuid = atoi(tokval);
			break;
		default:
			err("Unknown .netrc option %s\n",tokval);	
			break;
		}
		}
	}
static token(cfile)
	FILE *cfile;
{	/* returns next token in cfile, 0 on EOF */
	char *p;
	int c;
	if(feof(cfile))return(0);
	while((c = getc(cfile)) != EOF && (c == '\n' || c == '\t'
		|| c == ' ' || c == ','));
	/* next char begins token */
	if(c == EOF)return(0);
	p = tokval;
	if(c == '"'){	/* process quoted string */
		while((c = getc(cfile)) != EOF && c != '"'){
			if(c == '\\')c = getc(cfile);
			*p++ = c;
			}
		}
	else {
		*p++ = c;
		while((c = getc(cfile)) != EOF && c != '\n' && c != '\t' 
			&& c != ' ' && c != ','){
			if(c == '\\')c = getc(cfile);
			*p++ = c;
			}
		}
	*p = 0;
	if(tokval[0] == 0)return(0);
/*
	debug("tok %s",tokval);
*/
	return(tlookup(tokval));
	}
static tlookup(str)
  char *str; {
	struct tokstruct *p;
	for(p = toktab; p->tokstr; p++)
		if(streql(p->tokstr,str) == 0){
			return(p->tval);
			}
	return(ID);
	}
