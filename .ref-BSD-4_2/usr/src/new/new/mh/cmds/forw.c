#ifndef lint
static char sccsid[] = "@(#)forw.c	4.1 2/23/83";
#endif

#include "mh.h"
#include <stdio.h>
#include <signal.h>
#include <strings.h>

#define NONE 0
#define NOUSE 0

/* #define TEST 1 */

int *vec[MAXARGS];
struct msgs *mp;
char drft[128];
int inplace;            /* preserve links in anno */

char    *anyl[] = {
	"no",   0,
	"yes",  0,
	"list", 0,
	0,
};

char    *anyv[] = {
	"no",           0,
	"yes",          0,
	"verbose",      0,
	0,
};

struct swit switches[] = {
	"all",               -3,      /* 0 */
	"annotate",           0,      /* 1 */
	"noannotate",         0,      /* 2 */
	"editor editor",      0,      /* 3 */
	"form formfile",      0,      /* 4 */
	"inplace",            0,      /* 5 */
	"noinplace",          0,      /* 6 */
	"help",               4,      /* 7 */
	0,                    0
};

char *aleqs[] = {
	"list",              0,         /* 0 */
	"edit [<editor>]",   0,         /* 1 */
	"quit [delete]",     0,         /* 2 */
	"send [verbose]",    0,         /* 3 */
	0
};


main(argc, argv)
char *argv[];
{
	char *folder, *maildir, *msgs[100], *ed, *form;
	register int msgnum;
	register char *cp, **ap;
	int msgp, status, anot;
	int in, out, intr;
	int pid, wpid, msgcnt;
	char *arguments[50], **argp;
	char numbuf[5];

#ifdef NEWS
	m_news();
#endif
	form = 0; anot = 0; folder = 0; msgp = 0; ed = 0;
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
			case -1:fprintf(stderr, "forw: -%s unknown\n", cp);
				goto leave;
							     /* -all */
			case 0: fprintf(stderr, "\"-all\" changed to \"all\"\n");
				goto leave;
			case 1: anot = 1;  continue;         /* -annotate */
			case 2: anot = 0;  continue;         /* -noannotate */
			case 3: if(!(ed = *argp++)) {        /* -editor */
      missing:  fprintf(stderr, "forw: Missing argument for %s switch\n", argp[-2]);
					goto leave;
				}
				continue;
			case 4: if(!(form = *argp++))        /* -form */
					goto missing;
				continue;
			case 5: inplace = 1;  continue;      /* -inplace */
			case 6: inplace = 0;  continue;      /* -noinplace */
							     /* -help */
			case 7: help("forw   [+folder] [msgs] [switches]",
				     switches);
				goto leave;
			}
		if(*cp == '+') {
			if(folder) {
				fprintf(stderr, "Only one folder at a time.\n");
				goto leave;
			} else
				folder = cp + 1;
		} else
			msgs[msgp++] = cp;
	}
	if(!msgp)
		msgs[msgp++] = "cur";
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
	for(msgnum = 0; msgnum < msgp; msgnum++)
		if(!m_convert(msgs[msgnum]))
			goto leave;
	if(mp->numsel == 0) {
		fprintf(stderr, "forw: italian salami.\n");      /* never get here */
		goto leave;
	}
	if(form) {
		if((in = open(m_maildir(form), 0)) < 0) {
			fprintf(stderr, "forw: Can't open form file: %s\n", form);
			goto leave;
		}
	} else if((in = open(m_maildir(components), 0)) < 0 &&
		   (in = open(stdcomps, 0)) < 0) {
			fprintf(stderr, "forw: Can't open default components file!!\n");
			goto leave;
	}
	copy(m_maildir(draft), drft);
	if((out = open(drft, 0)) >= 0) {
		if(!fdcompare(in, out)) {
			cp = concat("\"", drft, "\" exists; Delete? ", 0);
			while((msgnum = gans(cp, anyl)) == 2)
				showfile(drft);
			if(!msgnum)
				return;
		}
		close(out);
	}
	if((out = creat(drft, m_gmprot())) < 0) {
		fprintf(stderr, "Can't create \"%s\"\n", drft);
		goto leave;
	}
	cpydata(in, out);
	close(in);
	for(msgcnt = 1, msgnum = mp->lowsel; msgnum <= mp->hghsel; msgnum++)
		if(mp->msgstats[msgnum]&SELECTED)  {
			if((in = open(cp = m_name(msgnum), 0)) < 0) {
				fprintf(stderr, "Can't open message \"%s\"\n", cp);
				unlink(drft);
				goto leave;
			}
			type(out, "\n\n-------");
			if(msgnum == mp->lowsel) {
				type(out, " Forwarded Message");
				if(mp->numsel > 1)
					type(out, "s");
			} else {
				type(out, " Message ");
				sprintf(numbuf, "%d", msgcnt);
				type(out, numbuf);
			}
			type(out, "\n\n");
			cpydata(in, out);
			close(in);
			msgcnt++;
		}
	type(out, "\n\n------- End of Forwarded Message");
	if(mp->numsel > 1)
		type(out, "s");
	type(out, "\n");
	close(out);
	m_replace(pfolder, folder);
	if(mp->lowsel != mp->curmsg)
		m_setcur(mp->lowsel);
	if(m_edit(&ed, drft, NOUSE, NONE) < 0)
		goto leave;

#ifdef TEST
	fprintf(stderr, "!! Test Version of SEND Being Run !!\n");
	fprintf(stderr, "   Send verbose !\n\n");
#endif
    for(;;) {
	if(!(argp = getans("\nWhat now? ", aleqs)))
		goto leave;
	switch(smatch(*argp, aleqs)) {
		case 0: showfile(drft);                         /* list */
			break;
		case 1: if(*++argp)                             /* edit */
				ed = *argp;
			if(m_edit(&ed, drft, NOUSE, NONE) == -1)
				goto leave;
			break;
		case 2: if(*++argp && *argp[0] == 'd')          /* quit */
				if(unlink(drft) == -1)  {
					fprintf(stderr, "Can't unlink %s ", drft);
					perror("");
				}
			goto leave;
		case 3: if(*++argp) cp = *argp;  else cp = "";  /* send */

			if(!mp->msgflags&READONLY) {    /* annotate first */
			    if(anot > 0) {
				while((pid = fork()) == -1) sleep(5);
				if(pid) {
					while(wpid=wait((int *)NULL)!= -1 && wpid!= pid);
					doano();
					goto leave;
				}
			    }
			}
			if(!m_send(cp, drft))
				goto leave;
		default:fprintf(stderr, "forw: illegal option\n");       /*##*/
			break;
	}
    }

 leave:
	m_update();
	done(0);
}


cpydata(in, out)
{
	char buf[BUFSIZ];
	register int i;

	do
		if((i = read(in, buf, sizeof buf)) > 0)
			write(out, buf, i);
	while(i == sizeof buf);
}


doano()
{
	FILE *in;
	char name[NAMESZ], field[256];
	register int ind, state;
	register char *text;

	if(stat(drft, field) != -1) {
		fprintf(stderr, "%s not sent-- no annotations made.\n", drft);
		return;
	}
	text = copy(drft, field);
	text[1] = 0;
	do
		*text = text[-1];
	while(--text >= field && *text != '/');
	*++text =
#ifdef UCB
		'#';
#else
		',';                  /* New backup convention */
#endif
	if((in = fopen(field, "r")) == NULL) {
		fprintf(stderr, "Can't open "); perror(field);
		return;
	}
	state = FLD;
	text = 0;
   for(;;) switch(state = m_getfld(state, name, field, sizeof field, in)) {

	case FLD:
	case FLDEOF:
	case FLDPLUS:
		if(uleq(name, "to") || uleq(name, "cc") == 0) {
			if(state == FLD) {
				text = add(name, text);
				text = add(":", text);
			}
			text = add(field, text);
		}
		if(state == FLDEOF)
			goto out;
		continue;
	case BODY:
	case BODYEOF:
		goto out;
	default:
		fprintf(stderr, "Getfld returned %d\n", state);
		return;
	}

out:
	fclose(in);

	for(ind = mp->lowsel; ind <= mp->hghsel; ind++)
		if(mp->msgstats[ind] & SELECTED)
			annotate(m_name(ind), "Forwarded", text, inplace);
}
