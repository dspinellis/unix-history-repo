/*
 *
 * zle_refresh.c - screen update
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

char **obuf = NULL,**nbuf = NULL;
int olnct,nlnct;
int winw,winh,winpos;

int vcs,vln,vmaxln;

void resetvideo() /**/
{
int ln;
static int lwinw = -1,lwinh = -1;

	setterm();
	winw = columns-1;
	if (isset(SINGLELINEZLE) || !termok)
		winh = 1;
	else
		winh = (lines < 2) ? 24 : lines;
	winpos = vln = vmaxln = 0;
	if (lwinw != winw || lwinh != winh)
		{
		if (nbuf)
			{
			for (ln = 0; ln != lwinh; ln++)
				{
				free(nbuf[ln]);
				free(obuf[ln]);
				}
			free(nbuf);
			free(obuf);
			}
		nbuf = (char **) zalloc((winh+1)*sizeof(char *));
		obuf = (char **) zalloc((winh+1)*sizeof(char *));
		for (ln = 0; ln != winh+1; ln++)
			{
			nbuf[ln] = zalloc(winw+1);
			obuf[ln] = zalloc(winw+1);
			}
		lwinw = winw;
		lwinh = winh;
		}
	for (ln = 0; ln != winh+1; ln++)
		{
		*nbuf[ln] = '\0';
		*obuf[ln] = '\0';
		}
	if (!pptlen)
		nbuf[0][0] = obuf[0][0] = '\0';
	else
		{
		for (ln = 0; ln != pptlen-1; ln++)
			nbuf[0][ln] = obuf[0][ln] = ' ';
		nbuf[0][ln] = obuf[0][ln] = '>';
		nbuf[0][pptlen] = obuf[0][pptlen] = '\0';
		}
	vcs = pptlen;
	olnct = nlnct = 1;
}

int scrollwindow() /**/
{
int t0,hwinh = winh/2;

	for (t0 = 0; t0 != winh-hwinh; t0++)
		{
		char *s;

		s = nbuf[t0];
		nbuf[t0] = nbuf[t0+hwinh];
		nbuf[t0+hwinh] = s;
		}
	for (t0 = 0; t0 != pptlen-1; t0++)
		nbuf[0][t0] = ' ';
	strcpy(nbuf[0]+t0,"> ...");
	return winh-hwinh;
}

/* this is the messy part. */
/* this define belongs where it's used!!! */

#define nextline { *s = (unsigned char)'\0'; \
	if (winh == ln+1) if (nvln != -1) break; else ln = scrollwindow()-1; \
	s = (unsigned char *)nbuf[++ln]; sen = s+winw; \
	}

void refresh() /**/
{
unsigned char *s,*t,*sen,*scs = line+cs; char **qbuf;
int ln = 0,nvcs,nvln = -1,t0;

	cost = 0;
	if (resetneeded)
		{
		resetvideo();
		resetneeded = 0;
		if (isset(SINGLELINEZLE) || !termok)
			vcs = 0;
		else
			printf("%s",pmpt);
		}
	zleactive = 1;
	if (isset(SINGLELINEZLE) || !termok)
		{
		singlerefresh();
		return;
		}

/* first, we generate the video line buffers so we know what to
	put on the screen. 

	s = ptr into the video buffer.
	t = ptr into the real buffer.
	sen = end of the video buffer (eol)
*/

	s = (unsigned char *)(nbuf[ln = 0]+pptlen);
	t = line;
	sen = (unsigned char *)(*nbuf+winw);
	for (; *t; t++)
		{
		if (icntrl((char)*t))
			if (*t == '\n')
				{
				if (t == scs)
					{
					nvcs = (char *)s-nbuf[nvln = ln];
					scs = (unsigned char *)NULL;
					}
				nextline
				}
			else if ((char)*t == '\t')
				{
				int t1 = (char *)s-nbuf[ln];

				if ((t1|7)+1 >= winw) nextline
				else
					do
						*s++ = ' ';
					while ((++t1) & 7);
				}
			else
				{
				if (s == sen) nextline
				*s++ = '^';
				if (s == sen) nextline
				*s++ = (*t == 127) ? '?' : (*t | '@');
				}
		else
			{
			if (s == sen) nextline
			*s++ = *t;
			}
/* if the cursor is here, remember it */

		if (t == scs)
			nvcs = s-(unsigned char *)nbuf[nvln = ln]-1;
		}
	if (scs == t)
		nvcs = s-(unsigned char *)nbuf[nvln = ln];
	*s = '\0';
	nlnct = ln+1;
	if (statusline)
		strcpy(nbuf[(nlnct == winh) ? winh-1 : nlnct++],statusline);

/* do RPROMPT */

	if (pmpt2 && ln == 0 && strlen(nbuf[0])+strlen(pmpt2) < winw)
		{
		for (t0 = strlen(nbuf[0]); t0 != winw; t0++)
			nbuf[0][t0] = ' ';
		strcpy(nbuf[0]+winw-strlen(pmpt2),pmpt2);
		}
	for (ln = 0; ln < nlnct; ln++)
		{

/* if old line and new line are different,
	see if we can insert/delete a line */

		if (ln < olnct && strncmp(nbuf[ln],obuf[ln],16))
			{
			if (tccan(TCDELLINE) && !strncmp(nbuf[ln],obuf[ln+1],16)
					&& obuf[ln+1][0] && ln != olnct)
				{
				int t0;

				moveto(ln,0);
				tcout(TCDELLINE);
				for (t0 = ln; t0 != olnct; t0++)
					strcpy(obuf[t0],obuf[t0+1]);
				olnct--;
				}

/* don't try to insert a line if olnct < vmaxln (vmaxln is the number
	of lines that have been displayed by this routine) so that we don't
	go off the end of the screen. */

			else if (tccan(TCINSLINE) && !strncmp(nbuf[ln+1],obuf[ln],16) &&
					olnct < vmaxln && nbuf[ln+1][0] && ln != olnct)
				{
				int t0;

				moveto(ln,0);
				tcout(TCINSLINE);
				for (t0 = olnct; t0 != ln; t0--)
					strcpy(obuf[t0],obuf[t0-1]);
				*obuf[ln] = '\0';
				olnct++;
				}
			}
		refreshline(ln);
		}

/* if old buffer had extra lines, do a clear-end-of-display if we can,
	otherwise, just fill new buffer with blank lines and refresh them */

	if (olnct > nlnct)
		{
		for (ln = nlnct; ln < olnct; ln++)
			nbuf[ln][0] = '\0';
		if (tccan(TCCLEAREOD))
			{
			moveto(nlnct,0);
			tcout(TCCLEAREOD);
			}
		else
			for (ln = nlnct; ln < olnct; ln++)
				refreshline(ln);
		}

/* move to the new cursor position */

	moveto(nvln,nvcs);
	qbuf = nbuf;
	nbuf = obuf;
	obuf = qbuf;
	olnct = nlnct;
	if (nlnct > vmaxln)
		vmaxln = nlnct;
	fflush(stdout);
}

#define tcinscost(X) (tccan(TCMULTINS) ? tclen[TCMULTINS] : (X)*tclen[TCINS])
#define tcdelcost(X) (tccan(TCMULTDEL) ? tclen[TCMULTDEL] : (X)*tclen[TCDEL])
#define tc_delchars(X) tcmultout(TCDEL,TCMULTDEL,(X))
#define tc_inschars(X) tcmultout(TCINS,TCMULTINS,(X))
#define tc_upcurs(X) tcmultout(TCUP,TCMULTUP,(X))
#define tc_leftcurs(X) tcmultout(TCLEFT,TCMULTLEFT,(X))

void refreshline(ln) /**/
int ln;
{
char *nl = nbuf[ln],*ol = obuf[ln];
char *p1;
char junk,*truncptr = &junk;
int ccs = 0;

	if (ln >= olnct)
		*ol = '\0';
	for (;;)
		{
		while (*nl && *nl == *ol)
			{
			nl++,ol++,ccs++;
			}
		if (!*nl && !*ol)
			{ *truncptr = '\0'; return; }

/* if this is the end of the new buffer but the old buffer has stuff
	here, clear to end of line if we can, otherwise fill the new buffer
	with blanks and continue. */

		if (!*nl)
			{
			if (tccan(TCCLEAREOL) && strlen(ol) > tclen[TCCLEAREOL])
				{
				moveto(ln,ccs);
				tcout(TCCLEAREOL);
				*ol = '\0';
				*truncptr = '\0';
				return;
				}
			else
				{
				int x = strlen(ol);
				char *p = nl;

				truncptr = p;
				while (x--)
					*p++ = ' ';
				*p = '\0';
				continue;
				}
			}

/* if this is the end of the old buffer, just dump the rest of the
	new buffer. */

		if (!*ol)
			{
			while (*nl == ' ')
				nl++,ccs++;
			if (*nl)
				{
				moveto(ln,ccs);
				fwrite(nl,strlen(nl),1,stdout);
				cost += strlen(nl);
				ccs = (vcs += strlen(nl));
				}
			*truncptr = 0;
			return;
			}
		moveto(ln,ccs);

/* try to insert/delete characters */

		if (ol[1] != nl[1] && tccan(TCDEL))
			{
			int ct = 0;

			for (p1 = ol; *p1; p1++,ct++)
				if (tcdelcost(ct) < streqct(p1,nl))
					{
					tc_delchars(ct);
					ol = p1;
					break;
					}
			if (*p1)
				continue;
			}

		if (ol[1] != nl[1] && tccan(TCINS))
			{
			int ct = 0;

			for (p1 = nl; *p1; p1++,ct++)
				if (tcinscost(ct) < streqct(p1,ol)+ct)
					{
#if 0
/* make sure we aren't inserting characters off the end of the screen;
	if we are, jump to the end and truncate the line, if we can do
	it quickly (gee, clever idea, Paul!) */
					if (ct+ccs+strlen(ol) >= winw-1)
						{
						if (!tccan(TCMULTRIGHT) || ccs > winw-tclen[TCMULTRIGHT])
							continue;
						moveto(ln,winw-1-ct);
						if (!tccan(TCCLEAREOL) || ct < tclen[TCCLEAREOL])
							{
							int x = ct;

							while (vcs++,x--)
								putchar(' ');
							}
						else
							tcout(TCCLEAREOL);
						moveto(ln,ccs);
						}
#endif
					if (ct+ccs+strlen(ol) < winw-1)
						{
						tc_inschars(ct = p1-nl);
						ccs = (vcs += p1-nl);
						cost += ct;
						fwrite(nl,ct,1,stdout);
						nl += ct;
						break;
						}
					}
			if (*p1)
				continue;
			}

/* if we can't do anything fancy, just write the new character and
	keep going. */

		putchar(*nl);
		cost++;
		nl++,ol++,ccs = ++vcs;
		}
}

void moveto(ln,cl) /**/
int ln;int cl;
{

/* move up */

	if (ln < vln)
		{
		tc_upcurs(vln-ln);
		vln = ln;
		}

/* move down; if we might go off the end of the screen, use newlines
	instead of TCDOWN */

	while (ln > vln)
		if (cl < (vcs/2) || ln >= vmaxln || !tccan(TCLEFT))
			{
			putchar('\r');
			putchar('\n');
			cost+=2;
			vln++;
			vcs = 0;
			}
		else
			{
			tc_downcurs(ln-vln);
			vln = ln;
			}
	if (cl < (vcs/2) || !tccan(TCLEFT))
		{
		putchar('\r');
		cost++;
		vcs = 0;
		}
	if (vcs < cl)
		tc_rightcurs(cl-vcs);
	else if (vcs > cl)
		tc_leftcurs(vcs-cl);
	vcs = cl;
}

void tcmultout(cap,multcap,ct) /**/
int cap;int multcap;int ct;
{
	if (tccan(multcap) && (!tccan(cap) || tclen[multcap] < tclen[cap]*ct))
		tcoutarg(multcap,ct);
	else while (ct--)
		tcout(cap);
}

void tc_rightcurs(ct) /**/
int ct;
{

/* do a multright if it's cheaper or if we're walking over the prompt.  */

	if (tccan(TCMULTRIGHT) &&
			(ct > tclen[TCMULTRIGHT] || vln == 0 && vcs < pptlen))
		tcoutarg(TCMULTRIGHT,ct);

/* if we're walking over the prompt and we can do a bunch of cursor rights,
	do them, even though they're more expensive.  (We can't redraw the
	prompt very easily in general.)  */

	else if (vln == 0 && vcs < pptlen && tccan(TCRIGHT))
		while (ct--)
			tcout(TCRIGHT);

/* otherwise write the contents of the video buffer. */

	else
		fwrite(nbuf[vln]+vcs,ct,1,stdout);
}

void tc_downcurs(ct) /**/
int ct;
{
	if (tccan(TCMULTDOWN) &&
			(!tccan(TCDOWN) || tclen[TCMULTDOWN] < tclen[TCDOWN]*ct))
		tcoutarg(TCMULTDOWN,ct);
	else if (tccan(TCDOWN))
		while (ct--)
			tcout(TCDOWN);
	else
		{
		while (ct--)
			putchar('\n');
		vcs = 0;
		}
}

/* I'm NOT going to worry about padding unless anyone complains. */

void tcout(cap) /**/
int cap;
{
	tputs(tcstr[cap],1,putraw);
}

void tcoutarg(cap,arg) /**/
int cap;int arg;
{
	tputs(tgoto(tcstr[cap],arg,arg),1,putraw);
}

void clearscreen() /**/
{
	tcout(TCCLEARSCREEN);
	resetneeded = 1;
}

void redisplay() /**/
{
	trashzle();
}

void trashzle() /**/
{
	if (zleactive)
		{
		refresh();
		moveto(nlnct,0);
		printf("%s",postedit);
		fflush(stdout);
		unsetterm();
		resetneeded = 1;
		}
}

void singlerefresh() /**/
{
char *vbuf,*vp,**qbuf,*op;
int t0,vsiz,nvcs;

	for (vsiz = 1+pptlen, t0 = 0; t0 != ll; t0++,vsiz++)
		if (line[t0] == '\t')
			vsiz += 7;
		else if (icntrl(line[t0]))
			vsiz++;
	vbuf = zalloc(vsiz);
	strcpy(vbuf,pmpt);
	vp = vbuf+pptlen;
	for (t0 = 0; t0 != ll; t0++)
		{
		if (line[t0] == '\t')
			do
				*vp++ = ' ';
			while ((vp-vbuf) & 7);
		else if (line[t0] == '\n')
			{
			*vp++ = '\\';
			*vp++ = 'n';
			}
		else if (line[t0] == 0x7f)
			{
			*vp++ = '^';
			*vp++ = '?';
			}
		else if (icntrl(line[t0]))
			{
			*vp++ = '^';
			*vp++ = line[t0] | '@';
			}
		else
			*vp++ = line[t0];
		if (t0 == cs)
			nvcs = vp-vbuf-1;
		}
	if (t0 == cs)
		nvcs = vp-vbuf;
	*vp = '\0';
	if ((winpos && nvcs < winpos+1) || (nvcs > winpos+winw-1))
		{
		if ((winpos = nvcs-(winw/2)) < 0)
			winpos = 0;
		}
	if (winpos)
		vbuf[winpos] = '<';
	if (strlen(vbuf+winpos) > winw)
		{
		vbuf[winpos+winw-1] = '>';
		vbuf[winpos+winw] = '\0';
		}
	strcpy(nbuf[0],vbuf+winpos);
	free(vbuf);
	nvcs -= winpos;
	for (t0 = 0,vp = *nbuf,op = *obuf; *vp; t0++,vp++)
		{
		if (*vp != *op && !(*vp == ' ' && !*op))
			{
			singmoveto(t0);
			putchar(*vp);
			vcs++;
			}
		if (*op)
			op++;
		}
	if (*op)
		{
		singmoveto(t0);
		for (; *op; op++)
			{
			putchar(' ');
			vcs++;
			}
		}
	singmoveto(nvcs);
	qbuf = nbuf;
	nbuf = obuf;
	obuf = qbuf;
	fflush(stdout);
}

void singmoveto(pos) /**/
int pos;
{
	while (pos < vcs)
		{
		vcs--;
		putchar('\b');
		}
	while (pos > vcs)
		{
		putchar(nbuf[0][vcs]);
		vcs++;
		}
}

int streqct(s,t) /**/
char *s;char *t;
{
int ct = 0;

	while (*s && *s == *t) s++,t++,ct++;
	return ct;
}

