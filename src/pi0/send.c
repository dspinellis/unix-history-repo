/* Copyright (c) 1979 Regents of the University of California */
#include "0.h"
#include "tree.h"
/*
 * pi - Pascal interpreter code translator
 * Bill Joy UCB
 * February 5, 1978
 */

int	pipbuf[259];
int	pv[2], pv2[2];
int	pid -1;

char	*PI1		"/usr/lib/pi1";

#define	ETXTBSY	26

char	printed, hadsome;
#ifdef PC0
char	fileout;
#endif

send(i, a1, a2, a3, a4)
	register int i;
{
	register int *ap;
	register char *cp;
	int x;
	extern errno;
	extern char *lastname, *obj;

	switch (i) {

	case RINIT:
#ifdef PC0
		if (opt('f')) {
			fileout++;
			if (fcreat(pipbuf, "tree") < 0) {
				perror( "tree");
				pexit(NOSTART);
			}
		} else {
#endif
			if (pipe(pv) || pipe(pv2)) {
				perror( "pipe");
				pexit(NOSTART);
			}
			pid = fork();
			if (pid == -1) {
				perror(0);
				pexit(NOSTART);
			}
			if (pid == 0) {
				close(0);
				dup(pv[0]);
				close(pv[0]);
				close(pv[1]);
				close(pv2[0]);
				for (;;) {
#ifdef DEBUG
					execl(PI1, "pi1", hp21mx ? "" : 0, 0);
#else
					execl(PI1, "pi1", 0);
#endif
					if (errno != ETXTBSY)
						break;
					sleep(2);
				}
				perror(PI1);
				exit(1);
			}
			pipbuf[0] = pv[1];
			close(pv[0]);
			pv[0] = pv2[0];
			close(pv2[1]);
#ifdef PC0
		}
#endif
		filename = lastname = savestr(filename);
		obj = savestr(obj);
		errfile = savestr(errfile);
		putac(i);
		putaw(soffset(lastname));
		putaw(soffset(obj));
		putac(pv2[1]);
		for (i = 0; i < 26; i++)
			putac(opts[i]);
		putac(efil);
		putaw(soffset(errfile));
		putac(ofil);
		break;

	case RENQ:
ackit:
		if (opt('l'))
			yyoutline();
		putac(RENQ);
		ack();
		break;

	case RTREE:
		ap = a1;
		putac(i);
		i = *ap++;
		if (i < 0 || i > T_LAST)
			panic("send RTREE");
		putac(i);
		cp = trdesc[i];
		while (*cp) switch (*cp++) {

		case 's':
			cp = ap;
			while (*cp)
				putac(*cp++);
			putac(0);
			ap = (((unsigned) cp) + 2) &~ 1;
			return (ap);

		case 'd':
			putac(*ap++);
			continue;

		case 'n':
			putaw(*ap++);
			continue;

		case '"':
			putaw(soffset(*ap++));
			continue;

		case 'p':
			putaw(toffset(*ap++));
			continue;

		default:
			panic("send RTREE case");
		}
#ifdef DEBUG
		if (*ap < 0 || *ap > T_LAST)
			printf("trdesc[%d] flunks\n", i);
#endif
		return (ap);

#ifdef DEBUG
	case RTRCHK:
		putac(i);
		putaw(a1);
		break;
#endif

	case RTRFREE:
		tsend();
	case REVTBEG:
	case REVVBEG:
	case REVTEND:
	case REVVEND:
	case REVENIT:
		putac(i);
		break;

	case RSTRING:
		putac(RSTRING);
		for (cp = a1; *cp; cp)
			putac(*cp++);
		putac(0);
		break;

	case REVLAB:
		tsend();
		putac(i);
		putaw(toffset(a1));
		break;

	case REVCNST:
		tsend();
		putac(i);
		putaw(a1);
		putaw(soffset(a2));
		putaw(toffset(a3));
		break;

	case REVTYPE:
		tsend();
		putac(i);
		putaw(a1);
		putaw(soffset(a2));
		putaw(toffset(a3));
		break;

	case REVVAR:
		tsend();
		putac(i);
		putaw(a1);
		putaw(toffset(a2));
		putaw(toffset(a3));
		break;

	case REVFHDR:
		tsend();
		putac(i);
		putaw(toffset(a1));
		break;

	case REVFBDY:
		putac(i);
		break;

	case REVFEND:
		tsend();
		putac(i);
		putaw(toffset(a1));
		putaw(a2);
		putaw(a3);
		putaw(soffset(lastname));
		putaw(soffset(filename));
		putac(printed);
		putac(hadsome);
		goto ackit;

	case ROPUSH:
	case ROPOP:
		putac(i);
		putac(a1);
		break;

	case ROSET:
		putac(i);
		putac(a1);
		putaw(a2);
		break;

	case RKILL:
		kill(pid, 1);
	     /* wait(&status); */
		break;

	case RFINISH:
		putac(i);
		fflush(pipbuf);
#ifdef PC0
		if (!fileout) {
#endif
			if (read(pv[0], &x, 2) != 2)
				panic("RFINISH");
			eflg =| x;
#ifdef PC0
		} else
			pexit(NOSTART);
#endif
		return;

	default:
		panic("send");
	}
}

putaw(i)
	int i;
{

	putw(i, pipbuf);
}

putac(i)
	int i;
{

	putc(i, pipbuf);
}

extern	struct nl *Fp;

ack()
{
	int i[3], j;

#ifdef PC0
	if (!fileout) {
#endif
		fflush(pipbuf);
		j = read(pv[0], &i, 6);
		if (j != 6) {
			error("Fatal error in pass 2");
			pexit(DIED);
		}
		if (soffset(lastname) != i[0])
			lastname = filename;
		Fp = i[1];
		printed = i[2] & 0377;
		hadsome = (i[2] >> 8) & 0377;
#ifdef PC0
	} else
		Fp = NIL;
#endif
}
