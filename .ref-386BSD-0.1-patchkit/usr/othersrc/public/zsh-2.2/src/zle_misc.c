/*
 *
 * zle_misc.c - miscellaneous editor routines
 *
 * This file is part of zsh, the Z shell.
 *
 * This software is Copyright 1992 by Paul Falstad
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 *
 */

#define ZLE
#include "zsh.h"


void selfinsert() /**/
{
int ncs = cs+mult;

	if (mult < 0) { mult = -mult; ncs = cs; }
	if (insmode || ll == cs)
		spaceinline(mult);
	else if (mult+cs > ll)
		spaceinline(ll-(mult+cs));
	while (mult--)
		line[cs++] = c;
	cs = ncs;
}

void selfinsertunmeta() /**/
{
	c &= 0x7f;
	if (c == '\r') c = '\n';
	selfinsert();
}

void deletechar() /**/
{
	if (mult < 0) { mult = -mult; backwarddeletechar(); return; }
	if (c == 4 && !ll)
		{
		eofsent = 1;
		return;
		}
	if (!(cs+mult > ll || line[cs] == '\n'))
		{
		cs += mult;
		backdel(mult);
		}
	else
		feep();
}

void backwarddeletechar() /**/
{
	if (mult < 0) { mult = -mult; deletechar(); return; }
	if (mult > cs)
		mult = cs;
	backdel(mult);
}

void videletechar() /**/
{
	if (mult < 0) { mult = -mult; vibackwarddeletechar(); return; }
	if (c == 4 && !ll) {
		eofsent = 1;
		return;
	}
	if (!(cs+mult > ll || line[cs] == '\n')) {
		cs += mult;
		backkill(mult,0);
		if (cs && (cs == ll || line[cs] == '\n')) cs--;
	} else
		feep();
}

void vibackwarddeletechar() /**/
{
	if (mult < 0) { mult = -mult; videletechar(); return; }
	if (mult > cs)
		mult = cs;
	if (cs-mult < viinsbegin) { feep(); return; }
	backkill(mult,1);
}

void vikillline() /**/
{
	if (viinsbegin > cs) { feep(); return; }
	backdel(cs-viinsbegin);
}

void killwholeline() /**/
{
int i,fg;

	if (mult < 0) return;
	while (mult--)
		{
		if (fg = (cs && cs == ll))
			cs--;
		while (cs && line[cs-1] != '\n') cs--;
		for (i = cs; i != ll && line[i] != '\n'; i++);
		forekill(i-cs+(i != ll),fg);
		}
}

void killbuffer() /**/
{
	cs = 0;
	forekill(ll,0);
}

void backwardkillline() /**/
{
int i = 0;

	if (mult < 0) { mult = -mult; killline(); return; }
	while (mult--)
		{
		while (cs && line[cs-1] != '\n') cs--,i++;
		if (mult && cs && line[cs-1] == '\n')
			cs--,i++;
		}
	forekill(i,1);
}

void gosmacstransposechars() /**/
{
int cc;

	if (cs < 2 || line[cs-1] == '\n' || line[cs-2] == '\n')
		{
		if (line[cs] == '\n' || line[cs+1] == '\n')
			{
			feep();
			return;
			}
		cs += (cs == 0 || line[cs-1] == '\n') ? 2 : 1;
		}
	cc = line[cs-2];
	line[cs-2] = line[cs-1];
	line[cs-1] = cc;
}

void transposechars() /**/
{
int cc;
int neg = mult < 0;

	if (neg) mult = -mult;
	while (mult--) {
		if (cs == 0 || line[cs-1] == '\n') {
			if (ll == cs || line[cs] == '\n' || line[cs+1] == '\n') {
				feep();
				return;
			}
			cs++;
		}
		if (!neg) {
			if (cs != ll && line[cs] != '\n') cs++;
		} else {
			if (cs != 0 && line[cs-1] != '\n') cs--;
		}
		cc = line[cs-2];
		line[cs-2] = line[cs-1];
		line[cs-1] = cc;
	}
}

void poundinsert() /**/
{
	if (*line != '#') {
		cs = 0;
		spaceinline(1);
		*line = '#';
	} else {
		cs = 0;
		foredel(1);
	}
	done = 1;
}

void acceptline() /**/
{
	done = 1;
}

void acceptandhold() /**/
{
	pushnode(bufstack,ztrdup((char *) line));
	stackcs = cs;
	done = 1;
}

void killline() /**/
{
int i = 0;

	if (mult < 0) { mult = -mult; backwardkillline(); return; }
	while (mult--) {
		if (line[cs] == '\n')
			cs++,i++;
		while (cs != ll && line[cs] != '\n') cs++,i++;
	}
	backkill(i,0);
}

void killregion() /**/
{
	if (mark > ll)
		mark = ll;
	if (mark > cs)
		forekill(mark-cs,0);
	else
		backkill(cs-mark,1);
}

void copyregionaskill() /**/
{
	if (mark > ll)
		mark = ll;
	if (mark > cs)
		cut(cs,mark-cs,0);
	else
		cut(mark,cs-mark,1);
}

static int kct,yankb,yanke;

void yank() /**/
{
int cc;
char *buf = cutbuf;

	if (!cutbuf) {
		feep();
		return;
	}
	if (mult < 0) return;
	if (vibufspec) {
		vibufspec = tolower(vibufspec);
		vibufspec += (idigit(vibufspec)) ? -'1'+26 : -'a';
		if (!(buf = vibuf[vibufspec])) {
			feep();
			vibufspec = 0;
			return;
		}
		vibufspec = 0;
	}
	yankb = cs;
	while (mult--) {
		kct = kringnum;
		cc = strlen(buf);
		spaceinline(cc);
		strncpy((char *) line+cs,buf,cc);
		cs += cc;
		yanke = cs;
	}
}

void viputafter() /**/
{
int cc;
char *buf = cutbuf;

	if (!cutbuf) {
		feep();
		return;
	}
	if (mult < 0) return;
	if (vibufspec) {
		vibufspec = tolower(vibufspec);
		vibufspec += (idigit(vibufspec)) ? -'1'+26 : -'a';
		if (!(buf = vibuf[vibufspec])) {
			feep();
			vibufspec = 0;
			return;
		}
		vibufspec = 0;
	}
	if (strchr(buf,'\n')) {
		cs = findeol();
		if (cs == ll) { spaceinline(1); line[cs] = '\n'; }
	}
	if (cs != ll) cs++;
	yankb = cs;
	while (mult--) {
		kct = kringnum;
		cc = strlen(buf);
		spaceinline(cc);
		strncpy((char *) line+cs,buf,cc);
		cs += cc;
		yanke = cs;
	}
	cs = yankb;
}

void yankpop() /**/
{
int cc;

	if (!(lastcmd & ZLE_YANK) || !kring[kct]) {
		feep();
		return;
	}
	cs = yankb;
	foredel(yanke-yankb);
	cc = strlen(kring[kct]);
	spaceinline(cc);
	strncpy((char *) line+cs,kring[kct],cc);
	cs += cc;
	yanke = cs;
	kct = (kct-1) & (KRINGCT-1);
}

void overwritemode() /**/
{
	insmode ^= 1;
}

void undefinedkey() /**/
{
	feep();
}

void quotedinsert() /**/
{
#ifndef TIO
struct sgttyb sob;
	sob = shttyinfo.sgttyb;
	sob.sg_flags = (sob.sg_flags|RAW) & ~ECHO;
	ioctl(SHTTY,TIOCSETN,&sob);
#endif
	c = getkey(0);
#ifndef TIO
	setterm();
#endif
	if (c) selfinsert(); else feep();
}

void digitargument() /**/
{
	if (!(lastcmd & ZLE_ARG))
		mult = 0;
	mult = mult*10+(c&0xf);
	if (lastcmd & ZLE_NEGARG) mult = -mult;
}

void negargument() /**/
{
	if (lastcmd & ZLE_ARG) feep();
}

void universalargument() /**/
{
	if (!(lastcmd & ZLE_ARG))
		mult = 4;
	else
		mult *= 4;
}

void copyprevword() /**/
{
int len,t0;

	for (t0 = cs-1; t0 >= 0; t0--)
		if (iword(line[t0]))
			break;
	for (; t0 >= 0; t0--)
		if (!iword(line[t0]))
			break;
	if (t0)
		t0++;
	len = cs-t0;
	spaceinline(len);
	strncpy((char *) line+cs,(char *) line+t0,len);
	cs += len;
}

void sendbreak() /**/
{
	errflag = done = 1;
}

void undo() /**/
{
char *s;
struct undoent *ue;

	ue = undos+undoct;
	if (!ue->change)
		{
		feep();
		return;
		}
	line[ll] = '\0';
	s = ztrdup((char *) line+ll-ue->suff);
	sizeline((ll = ue->pref+ue->suff+ue->len)+1);
	strncpy((char *) line+ue->pref,ue->change,ue->len);
	strcpy((char *) line+ue->pref+ue->len,s);
	free(s);
	ue->change = NULL;
	undoct = (undoct-1) & (UNDOCT-1);
	cs = ue->cs;
}

void quoteregion() /**/
{
char *s,*t;
int x,y;

	if (mark > ll)
		mark = ll;
	if (mark < cs)
		{
		x = mark;
		mark = cs;
		cs = x;
		}
	s = hcalloc((y = mark-cs)+1);
	strncpy(s,(char *) line+cs,y);
	s[y] = '\0';
	foredel(mark-cs);
	t = makequote(s);
	spaceinline(x = strlen(t));
	strncpy((char *) line+cs,t,x);
	mark = cs;
	cs += x;
}

void quoteline() /**/
{
char *s;

	line[ll] = '\0';
	s = makequote((char *) line);
	setline(s);
}

char *makequote(s) /**/
char *s;
{
int qtct = 0;
char *l,*ol;

	for (l = s; *l; l++)
		if (*l == '\'')
			qtct++;
	l = ol = halloc((qtct*3)+3+strlen(s));
	*l++ = '\'';
	for (; *s; s++)
		if (*s == '\'')
			{
			*l++ = '\'';
			*l++ = '\\';
			*l++ = '\'';
			*l++ = '\'';
			}
		else
			*l++ = *s;
	*l++ = '\'';
	*l = '\0';
	return ol;
}

#define NAMLEN 70

int executenamedcommand() /**/
{
char buf[NAMLEN],*ptr;
int len,ch,t0;

	strcpy(buf,"execute: ");
	ptr = buf+9;
	len = 0;
	statusline = buf;
	refresh();
	for (;ch = getkey(1);refresh())
		{
		switch (ch)
			{
			case 8: case 127:
				if (len)
					{
					len--;
					*--ptr = '\0';
					}
				break;
			case 23:
				while (len && (len--, *--ptr != '-'))
					*ptr = '\0';
				break;
			case 21:
				len = 0;
				ptr = buf+9;
				*ptr = '\0';
				break;
			case 10: case 13: goto brk;
			case 7: case -1: statusline = NULL; return z_undefinedkey;
			case 9: case 32:
				{
				Lklist ll;
				int ambig = 100;

				heapalloc();
				ll = newlist();
				for (t0 = 0; t0 != ZLECMDCOUNT; t0++)
					if (strpfx(buf+9,zlecmds[t0].name))
						{
						int xx;

						addnode(ll,zlecmds[t0].name);
						xx = pfxlen(peekfirst(ll),zlecmds[t0].name);
						if (xx < ambig)
							ambig = xx;
						}
				permalloc();
				if (!full(ll))
					feep();
				else if (!nextnode(firstnode(ll)))
					{
					strcpy(buf+9,peekfirst(ll));
					ptr = buf+(len = strlen(buf));
					}
				else
					{
					strcpy(buf+9,peekfirst(ll));
					len = ambig;
					ptr = buf+9+len;
					*ptr = '\0';
					feep();
					listmatches(ll,NULL);
					}
				break;
				}
			default:
				if (len == NAMLEN-10 || icntrl(ch))
					feep();
				else
					*ptr++ = ch, *ptr = '\0', len++;
				break;
			}
		}
brk:
	statusline = NULL;
	ptr = buf+9;
	for (t0 = 0; t0 != ZLECMDCOUNT; t0++)
		if (!strcmp(ptr,zlecmds[t0].name))
			break;
	if (t0 != ZLECMDCOUNT)
		return lastnamed = t0;
	else
		return z_undefinedkey;
}

void vijoin() /**/
{
int x;

	if ((x = findeol()) == ll)
		{
		feep();
		return;
		}
	cs = x+1;
	for (x = 1; cs != ll && iblank(line[cs]); cs++,x++);
	backdel(x);
	spaceinline(1);
	line[cs] = ' ';
}

void viswapcase() /**/
{
	if (cs < ll)
		{
		int ch = line[cs];

		if (islower(ch))
			ch = tuupper(ch);
		else if (isupper(ch))
			ch = tulower(ch);
		line[cs++] = ch;
		}
}

void vicapslockpanic() /**/
{
char ch;

	statusline = "press a lowercase key to continue";
	refresh();
	do
		ch = getkey(0);
	while (!islower(ch));
}

void visetbuffer() /**/
{
int ch;

	ch = getkey(1);
	if (!ialnum(ch)) {
		feep();
		return;
	}
	vibufspec = ch;
}

static char *bp;
static int lensb,countp;

void stradd(d) /**/
char *d;
{
	while (*bp++ = *d++);
	bp--;
}

int putstr(d) /**/
int d;
{
	*bp++ = d;
	if (countp)
		lensb++;
	return 0;
}

#define tstradd(X) \
	if (termok && unset(SINGLELINEZLE)) { \
		char tbuf[2048],*tptr = tbuf; \
		if (tgetstr(X,&tptr)) \
			tputs(tbuf,1,putstr); \
	} \
	break

/* get a prompt string */

char *putprompt(fm,lenp) /**/
char *fm;int *lenp;
{
char *ss,*bl0;
static char buf1[256],buf2[256],*buf;
char buf3[MAXPATHLEN];
int t0,bracepos = 0;
struct tm *tm = NULL;
time_t timet;

	lensb = 0; countp = 1;
	if (!fm) { *lenp = 0; return ""; }
	/* kludge alert! */
	buf = (buf == buf1) ? buf2 : buf1;
	bp = bl0 = buf;
	if (!columns) columns = 80;
	clearerr(stdin);
	for(;*fm;fm++) {
		if (bp-buf >= 220)
			break;
		if (*fm == '%')
			switch (*++fm) {
				case '~':
					t0 = finddir(pwd);
					if (t0 != -1) {
						*bp++ = '~';
						stradd(usernames[t0]);
						stradd(pwd+strlen(userdirs[t0]));
						break;
					}
					if (!strncmp(pwd,home,t0 = strlen(home)) && t0 > 1) {
						*bp++ = '~';
						stradd(pwd+t0);
						break;
					}
				case 'd': case '/': stradd(pwd); break;
				case 'c': case '.':
					t0 = finddir(pwd);
					if (t0 != -1) {
						sprintf(buf3,"~%s%s",usernames[t0],
							pwd+strlen(userdirs[t0]));
					} else if (!strncmp(pwd,home,t0 = strlen(home)) && t0 > 1) {
						sprintf(buf3,"~%s",pwd+t0);
					} else {
						strcpy(buf3,pwd);
					}
					t0 = 1;
					if (idigit(fm[1])) { t0 = fm[1]-'0'; fm++; }
					for (ss = buf3+strlen(buf3); ss > buf3; ss--)
						if (*ss == '/' && !--t0) {
							ss++;
							break;
						}
					if (*ss == '/' && ss[1] && (ss != buf3)) ss++;
					stradd(ss);
					break;
				case 'C':
					strcpy(buf3,pwd);
					t0 = 1;
					if (idigit(fm[1])) { t0 = fm[1]-'0'; fm++; }
					for (ss = buf3+strlen(buf3); ss > buf3; ss--)
						if (*ss == '/' && !--t0) {
							ss++;
							break;
						}
					if (*ss == '/' && ss[1] && (ss != buf3)) ss++;
					stradd(ss);
					break;
				case 'h': case '!':
					sprintf(bp,"%d",curhist);
					bp += strlen(bp);
					break;
				case 'M': stradd(hostnam); break;
				case 'm':
					if (idigit(fm[1]))
						t0 = (*++fm)-'0';
					else
						t0 = 1;
					for (ss = hostnam; *ss; ss++)
						if (*ss == '.' && !--t0)
							break;
					t0 = *ss;
					*ss = '\0';
					stradd(hostnam);
					*ss = t0;
					break;
				case 'S': tstradd("so"); /* <- this is a macro */
				case 's': tstradd("se");
				case 'B': tstradd("md");
				case 'b': tstradd("me");
				case 'U': tstradd("us");
				case 'u': tstradd("ue");
				case '{': bracepos = bp-buf; countp = 0; break;
				case '}': lensb += (bp-buf)-bracepos; countp = 1; break;
				case 't': case '@':
					timet = time(NULL);
					tm = localtime(&timet);
					ztrftime(bp,16,"%l:%M%p",tm);
					if (*bp == ' ')
						chuck(bp);
					bp += strlen(bp);
					break;
				case 'T':
					timet = time(NULL);
					tm = localtime(&timet);
					ztrftime(bp,16,"%k:%M",tm);
					bp += strlen(bp);
					break;
				case '*':
					timet = time(NULL);
					tm = localtime(&timet);
					ztrftime(bp,16,"%k:%M:%S",tm);
					bp += strlen(bp);
					break;
				case 'n': stradd(username); break;
				case 'w':
					timet = time(NULL);
					tm = localtime(&timet);
					ztrftime(bp,16,"%a %e",tm);
					bp += strlen(bp);
					break;
				case 'W':
					timet = time(NULL);
					tm = localtime(&timet);
					ztrftime(bp,16,"%m/%d/%y",tm);
					bp += strlen(bp);
					break;
				case 'D':
 					strcpy(buf3, "%y-%m-%d");
 					if (fm[1] == '{') {
 						for (ss = fm + 1, t0 = 0; *ss; ++ss)
 							if (*ss == '{')
 								++t0;
 							else if (*ss == '}')
 								if (--t0 == 0)
 									break;
 						if (*ss == '}' && t0 == 0) {
 							t0 = (ss - 1) - (fm + 1);
 							strncpy(buf3, fm + 2, t0);
 							buf3[t0] = 0;
 							fm = ss;
 						}
 					}
  					timet = time(NULL);
  					tm = localtime(&timet);
 					ztrftime(bp,16,buf3,tm);
  					bp += strlen(bp);
  					break;
				case 'l':
					if (*ttystrname) stradd((strncmp(ttystrname,"/dev/tty",8) ? 
						ttystrname+5 : ttystrname+8));
					else stradd("()");
					break;
				case '?':
					sprintf(bp,"%d",lastval);
					bp += strlen(bp);
					break;
				case '%': *bp++ = '%'; break;
				case '#': *bp++ = (geteuid()) ? '%' : '#'; break;
				case 'r': stradd(rstring); break;
				case 'R': stradd(Rstring); break;
				default: *bp++ = '%'; *bp++ = *fm; break;
			}
		else if (*fm == '!') {
			sprintf(bp,"%d",curhist);
			bp += strlen(bp);
		} else {
			if (fm[0] == '\\' && fm[1])
				fm++;
			if ((*bp++ = *fm) == '\n')
				bl0 = bp, lensb = 0;
		}
	}
	*lenp = (bp-bl0)-lensb;
	*lenp %= columns;
	if (*lenp == columns-1) {
		*lenp = 0;
		*bp++ = ' ';
	}
	*bp = '\0';
	return buf;
}

