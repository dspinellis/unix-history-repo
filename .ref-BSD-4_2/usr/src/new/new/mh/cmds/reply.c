#include "mh.h"
#include <stdio.h>
#include <signal.h>
#include <strings.h>

/*#define NEWS 1*/

#define NOUSE 0

/* #define TEST 1 */

char    *anyl[] = {
	"no",   0,
	"yes",  0,
	"list", 0,
	0
};

char *aleqs[] = {
	"list",              0,         /* 0 */
	"edit [<editor>]",   0,         /* 1 */
	"quit [delete]",     0,         /* 2 */
	"send [verbose]",    0,         /* 3 */
	0
};

int  *vec[MAXARGS], anot;
int ccme = 1;
struct msgs *mp;
char *ed;
int inplace;            /* preserve links in anno */

struct swit switches[] = {
	"annotate",           0,      /* 0 */
	"noannotate",         0,      /* 1 */
	"ccme",              -1,      /* 2 */
	"noccme",            -1,      /* 3 */
	"editor editor",      0,      /* 4 */
	"inplace",            0,      /* 5 */
	"noinplace",          0,      /* 6 */
	"help",               4,      /* 7 */
	0,                    0
};

char *ltrim();
char *rtrim();
char *niceadd();
char *fix();
char *addr();

main(argc, argv)
char *argv[];
{
	char *folder, *nfolder, *msg, *maildir;
	register char *cp, **ap;
	register int cur;
	char *arguments[50], **argp;

#ifdef NEWS
	m_news();
#endif
	msg = 0; anot = 0; folder = 0;

	cp = r1bindex(argv[0], '/');
	if((cp = m_find(cp)) != NULL) {
		ap = brkstring(cp = getcpy(cp), " ", "\n");
		ap = copyip(ap, arguments);
	} else
		ap = arguments;
	copyip(argv+1, ap);
	argp = arguments;
	while(cp = *argp++) {
		if(*cp == '-')
			switch(smatch(++cp, switches)) {
			case -2:ambigsw(cp, switches);       /* ambiguous */
				goto leave;
							     /* unknown */
			case -1:fprintf(stderr, "repl: -%s unknown\n", cp);
				goto leave;
			case 0: anot = 1;  continue;         /* -annotate */
			case 1: anot = 0;  continue;         /* -noannotate */
			case 2: ccme = 1;  continue;         /* -ccme */
			case 3: ccme = 0;  continue;         /* -noccme */
			case 4: if(!(ed = *argp++)) {        /* -editor */
		fprintf(stderr, "repl: Missing argument for %s switch\n", argp[-2]);
					goto leave;
				}
				continue;
			case 5: inplace = 1;  continue;      /* -inplace */
			case 6: inplace = 0;  continue;      /* -noinplace */
							     /* -help */
			case 7: help("repl   [+folder] [msg] [switches]",
				     switches);
				goto leave;
			}
		if(*cp == '+') {
			if(folder) {
				fprintf(stderr, "Only one folder at a time.\n");
				goto leave;
			} else
				folder = cp + 1;
		} else if(msg) {
			fprintf(stderr, "Only one message per reply.\n");
			goto leave;
		} else
			msg = cp;
	}
	if(!msg)
		msg = "cur";
	if(!folder)
		folder = m_getfolder();
	maildir = m_maildir(folder);
	if(chdir(maildir) < 0) {
		fprintf(stderr, "Can't chdir to: ");
		perror(maildir);
		goto leave;
	}
	if(!(mp = m_gmsg(folder))) {
		fprintf(stderr, "Can't read folder!?\n");
		goto leave;
	}
	if(mp->hghmsg == 0) {
		fprintf(stderr, "No messages in \"%s\".\n", folder);
		goto leave;
	}
	if(!m_convert(msg))
		goto leave;
	if(mp->numsel == 0) {
		fprintf(stderr, "repl: pepperoni pizza\n");/* never get here */
		goto leave;
	}
	if(mp->numsel > 1) {
		fprintf(stderr, "Only one message at a time.\n");
		goto leave;
	}
	m_replace(pfolder, folder);
	if(mp->lowsel != mp->curmsg)
		m_setcur(mp->lowsel);
	repl(getcpy(m_name(mp->lowsel)));
 leave:
	m_update();
	done(0);
}


repl(msg)
{
	register char *cp;
	register int i,j;
	register FILE *in;
	char name[NAMESZ], field[BUFSIZ];
	char *drft, *msgid, *replto, *from, *cc, *sub, *date, *to;
	int state, out, status, intr;
	int pid, wpid;
	char **argp, *address;

	if((in = fopen(msg, "r")) == NULL) {
		fprintf(stderr, "Can't open "); perror(msg);
		return;
	}
	drft = m_maildir(draft);
	if((out = open(drft, 0)) >= 0) {
		cp = concat("\"", drft, "\" exists; delete? ", 0);
		while((i = gans(cp, anyl)) == 2)
			showfile(drft);
		if(!i)
			return;
		free(cp);
		close(out);
	}
	if((out = creat(drft, m_gmprot())) < 0) {
		fprintf(stderr, "Can't create \"%s\".\n", drft);
		return;
	}

	state = FLD;
	replto = msgid = to = from = cc = sub = date = 0;

    for(;;) {

	switch(state = m_getfld(state, name, field, sizeof field, in)) {

	case FLD:
	case FLDEOF:
	case FLDPLUS:
		if(uleq(name, "from"))
			from = niceadd(field, from);
		if(uleq(name, "cc"))
			cc = niceadd(field, cc);
		if(uleq(name, "subject"))
			sub = niceadd(field, sub);
		if(uleq(name, "date"))
			date = niceadd(field, date);
		if(uleq(name, "to"))
			to = niceadd(field, to);
		if(uleq(name, "message-id"))
			msgid = niceadd(field, msgid);
		if(uleq(name, "reply-to"))
			replto = niceadd(field, replto);
	/*      if(uleq(name, "sender"))
			sender = niceadd(field, sender);        */
		if(state == FLDEOF)
			goto done;
		break;

	case BODY:
	case BODYEOF:
	case FILEEOF:
		goto done;

	default:
		fprintf(stderr, "getfld returned %d\n", state);
		return;
	}

    }

done:

    /*  if(!(address = addr(sender)))
		if(!(address = addr(from)))
			address = addr(replto);
    */
   /*   if(!(address = addr(replto)))
		address = addr(from);
    */
	address = replto ? addr(replto) : addr(from);
	if(!ccme)
		to = 0;
	if(!(from || replto)) {
		fprintf(stderr, "No one to reply to!!!\n");
		return;
	}
	fclose(in);
	type(out, "To: ");                      /* To: */
	type(out, replto ? replto : from);
	if(cc || to )                           /* cc: */
		type(out, "cc: ");
	if(cc) {
		if(address)
			cc = fix(cc, address);
		if(to)
			rtrim(cc);
		type(out, cc);
	}
	if(to) {
		if(cc)
			type(out, ",\n    ");
		if(address)
			to = fix(to, address);
		type(out, to);
	}
	if(sub) {                               /* Subject: Re: */
		type(out, "Subject: ");
		if(*sub == ' ') sub++;
		if((sub[0] != 'R' && sub[0] != 'r') ||
		   (sub[1] != 'E' && sub[1] != 'e') ||
		   sub[2] != ':')
			type(out, "Re: ");
		type(out, sub);
	}                                       /* In-reply-to: */
	if(date) {
		type(out, "In-reply-to: Your message of ");
		date[strlen(date)-1] = '.';
		if(*date == ' ') date++;
		type(out, date);
		type(out, "\n");
		if(msgid) {
			type(out, "             ");
			if(*msgid == ' ') msgid++;
			type(out, msgid);
		}
	}
	type(out, "----------\n");
	close(out);
	if(m_edit(&ed, drft, NOUSE, msg) < 0)
		return;
#ifdef TEST
	fprintf(stderr, "!! Test Version of SEND Being Run !!\n");
	fprintf(stderr, "   Send verbose !\n\n");
#endif

    for(;;) {
	if(!(argp = getans("\nWhat now? ", aleqs))) {
		unlink("@");
		return;
	}
	switch(smatch(*argp, aleqs)) {
		case 0: showfile(drft);                         /* list */
			break;
		case 1: if(*++argp)                             /* edit */
				ed = *argp;
			if(m_edit(&ed, drft, NOUSE, msg) == -1)
				return;
			break;
		case 2: if(*++argp && *argp[0] == 'd')          /* quit */
				if(unlink(drft) == -1)  {
					fprintf(stderr, "Can't unlink %s ", drft);
					perror("");
				}
			return;
		case 3: if(*++argp) cp = *argp;  else cp = "";  /* send */

			if(!mp->msgflags&READONLY) {    /* annotate first */
			    if(anot > 0) {
				while((pid = fork()) == -1) sleep(5);
				if(pid) {
					while(wpid=wait((int *)NULL)!= -1 && wpid!= pid);
					if(stat(drft, field) == -1)
						annotate(msg, "Replied", "", inplace);
					return;
				}
			    }
			}
			if(!m_send(cp, drft))
				return;
		default:fprintf(stderr, "repl: illegal option\n");       /*##*/
			break;
	}
    }
}
