#include "ex.h"
#ifdef VISUAL
#include "ex_tty.h"
#include "ex_vis.h"
#include "ex_re.h"
/*
 * Ex - a text editor
 * Bill Joy UCB September 1977
 */

vmain(ic)
	char *ic;
{
	register int i;

	i = Vmain(ic);
	flusho();
	return (i);
}

int	delete(), join(), vshift();

char	vscandir[3]	"/\n";

static	char op;

Vmain(ic)
	char *ic;
{
	register int c, cnt, i;
	char hadcnt, first, *oglobp;
	extern char Peekkey, *genindent();
	int (*OP)(), ind, vmunddot, *addr;
	extern char *loc1;

	if (nextic) {
		ic = nextic;
		nextic = 0;
	}
	first = 1;
vnline:
	if (vundkind != VMANY)
		vundkind = VNONE;
	getDOT();
	vmunddot = *dot;
vmark:
	if (vmoving)
		wcursor = vfindcol(vmovcol);
	else if (first) {
		wcursor = ic;
		first = 0;
	} else
		wcursor = vskipwh(ic);
	ic = linebuf;
	vmove();
	for (;;) {
		TSYNC();
		killglob();
		if (digit(peekkey()) && peekkey() != '0') {
			hadcnt = 1;
			cnt = vgetcnt();
			if (cnt <= 0) {
				beep();
				continue;
			}
		} else {
			Xhadcnt = hadcnt = 0;
			Xcnt = cnt = 1;
		}
vagain:
		op = c = getkey();
		if (HSTR && HSTR[0] == c && HSTR[1] == 0)
			goto home;
		if (UPLINE && UPLINE[0] == c && UPLINE[1] == 0)
			goto upline;
		switch (c) {
			case CTRL(z):	/* sync */
				if (!visual)
					vsync(LINES - 1);
				else
					vredraw(ZERO);
				vfixcurs();
				continue;
			case CTRL(s):
				vsave();
				synctmp();
				continue;
			case ESCAPE:	/* cancel command */
				beep();
				continue;
			case '.':	/* repeat change */
				if (lastcmd[0] == 0) {
					beep();
					continue;
				}
				if (hadcnt)
					lastcnt = cnt;
				cnt = lastcnt;
				hadcnt = lasthad;
				vglobp = lastcmd;
				goto vagain;
#ifdef UNIMP
			case 'N':
				vglobp = vscandir[0] == '/' ? "?" : "/";
				goto vagain;
#endif
			case 'n':	/* next line matching */
				vglobp = vscandir;
				goto vagain;
			case 'H':	/* home cursor */
				if (hadcnt) {
					beep();
					continue;
				}
				markDOT();
				if (vcline == 0) {
					c = '^';
					break;
				}
home:
				cnt = vcline;
			case '-':	/* back up (to first white) */
				vmoving = 0;
				goto vup;
			case 'k':	/* up line same column */
upline:
				if (vmoving == 0) {
					vmoving = 1;
					vmovcol = column(cursor);
				}
vup:
				vsave();
				if (cnt > dot - one)
					cnt = dot - one;
				if (cnt == 0) {
					beep();
					continue;
				}
				dot =- cnt;
				if (cnt > vcline || !visual) {
					vch = '.';
					return (1);
				}
				vcline =- cnt;
				goto vnline;
			case '\'':
				c = getesc();
				if (c == 0)
					continue;
				if (c == '\'')
					c = 'a' + 26;
				else if (c != letter(c)) {
					beep();
					continue;
				}
				for (addr = one; addr <= dol; addr++)
					if (names[c-'a'] == (*addr &~ 01))
						break;
				if (addr > dol) {
					beep();
					continue;
				}
				markDOT();
gogo:
				vsave();
				vch = -1;
				getline(*addr);
				loc1 = vskipwh(linebuf);
				goto it;
			case 'G':
				if (!hadcnt)
					cnt = dol - zero;
				if (cnt < 1 || cnt > dol - zero) {
					beep();
					continue;
				}
				addr = zero + cnt;
				markDOT();
				goto gogo;
			case 'K':
				c = getesc();
				if (c == 0)
					continue;
				if (c != letter(c)) {
					beep();
					continue;
				}
				vsave();
				names[c-'a'] = (*dot &~ 01);
				continue;
			case 'L':
				cnt = vcnt - vcline - 1;
				if (cnt == 0) {
					c = '^';
					break;
				}
				markDOT();
				goto vdown;
			case CTRL(d):	/* scroll half-screen */
				if (hadcnt)
					vSCROLL = cnt;
				cnt = vSCROLL;
				if (visual) {
					ind = vcnt - vcline - 1;
					cnt =+ ind;
				}
			case '+':	/* next line, first non-white */
			case CR:
				vmoving = 0;
				goto vdown;
			case 'j':	/* down line respecting position */
			case NL:
				if (vmoving == 0) {
					vmoving = 1;
					vmovcol = column(cursor);
				}
vdown:
				if (dot == dol) {
					beep();
					continue;
				}
				vsave();
				if (cnt > dol - dot) {
					ind =- cnt - (dol - dot);
					if (ind < 0)
						ind = 0;
					cnt = dol - dot;
				}
				i = vcnt - vcline - 1;
				if (c != CTRL(d) && cnt <= i) {
					vcline =+ cnt;
					dot =+ cnt;
					goto vnline;
				}
				cnt =- i;
				dot =+ i;
				vcline =+ i;
				if (c != CTRL(d)) {
					if (!visual || vfit(cnt, dot) > LINES) {
						vch = '.';
						dot =+ cnt;
						return (1);
					}
				}
				vroll(cnt);
				if (c == CTRL(d) && VLINES != LINES) {
					vcline =- ind;
					dot =- ind;
					if (vcline < 0) {
						dot =- vcline;
						vcline = 0;
					}
				}
				goto vnline;
			case 'A':
				if (hadcnt) {
					beep();
					continue;
				}
				ungetkey('a');
				xtrey(c);
				c = '$';
				break;
			case 'I':
				if (hadcnt) {
					beep();
					continue;
				}
				ungetkey('i');
				xtrey(c);
				c = '^';
				break;
			case 'C':
				if (hadcnt) {
					beep();
					continue;
				}
				xtrey(c);
				setwork();
				if (*cursor == 0) {
					ungetkey('a');
					continue;
				}
				ungetkey('$');
				c = 'c';
				break;
			case 'z':	/* context */
			case 'v':
				if (!visual) {
					if (hadcnt) {
						beep();
						continue;
					}
					vsave();
					ostop();
					setoutt();
					inopen = 0;
					addr2 = dot;
					zeq(vlast - 1);
					inopen++;
					termreset();
					Outchar = &vputchar;
					ostart();
					vholdmove = 1;
					vch = '.';
					return (1);
				}
				vsave();
				c = getesc();
				if (c == 0)
					continue;
				if (hadcnt) {
					markDOT();
					addr = zero + cnt;
					if (addr < one)
						addr = one;
					if (addr > dol)
						addr = dol;
				} else
					addr = dot;
				switch (c) {
					case '.':
					case '-':
						break;
					case '^':
						if (addr == one) {
							beep();
							continue;
						}
						break;
					case '+':
						if (addr == dol) {
							beep();
							continue;
						}
						c = 0;
						break;
					case CR:
					case NL:
						vch = 0;
						break;
					default:
						beep();
						continue;
				}
				vmoving = 0;
				vch = c;
				dot = addr;
				return (1);
			case 'Y':
				if (!visual && cnt <= TUBELINES)
					goto yok;
				if (cnt > vcnt - vcline) {
					beep();
					continue;
				}
yok:
				vsave();
				vyank(cnt);
				continue;
		}
		vmoving = 0;
		setwork(c);
		switch (c) {
			case '\\':	/* delete line */
				if (vglobp == 0) {
					c = getesc();
					if (c == 0)
						continue;
					if (c != '\\') {
						beep();
						continue;
					}
				}
				if (cnt > vcnt - vcline) {
					beep();
					continue;
				}
				vsave();
				setLAST();
				c = vliny[vcline];
				addr = dot;
				for (i = 0; i < cnt; i++)
					vulines[i] = dot[i];
				vyank(cnt);
				vresaddr = dot;
				vrescnt = cnt;
				vdelcnt = 0;
				vundkind = VMANY;
				vrescurs = cursor;
				vremote(cnt, delete);
				velide(cnt, vcline);
				if (addr > dol)
					vcline--;
				if (vcnt == 0) {
					if (dol == zero)
						error("No lines in buffer");
					if (!visual) {
						vgoto(c, 0);
						vputchar('@');
					}
					vch = '.';
					return (1);
				}
				getDOT();
				vscrap();
				c = vcline ? vliny[vcline - 1] : ZERO;
				if (vcline == vcnt)
					vredraw(c);
				else
					vsync(c);
				goto vmark;
			case 'J':	/* join lines */
				if (cnt > TUBELINES || dot == dol ||
				    hadcnt && cnt == 1) {
					beep();
					continue;
				}
				vsave();
				setLAST();
				if (cnt == 1)
					cnt = 2;
				if (cnt > dol - dot + 1)
					cnt = dol - dot + 1;
				vrescnt = cnt;
				copy(vulines, dot, cnt * sizeof vulines[0]);
				vdelcnt = 1;
				vresaddr = dot;
				vrescurs = cursor;
				vundkind = VMANY;
				cursor = strend(linebuf);
				vremote(cnt, join);
				vsave();
				velide(cnt - 1, vcline + 1);
				vsyncCL();
				if (!*cursor && cursor > linebuf)
					cursor--;
				vfixcurs();
				continue;
#ifdef UNIMP
			case '<':
			case '>':
				if (cnt > vcnt - vcline) {
					beep();
					continue;
				}
				vsave();
				setLAST();
				vrescnt = cnt;
				copy(vulines, dot, cnt * sizeof vulines[0]);
				vdelcnt = cnt;
				vresaddr = dot;
				vrescurs = cursor;
				vundkind = VMANY;
				vremote(cnt, vshift);
				dot = vresaddr;
				getDOT();
				vsyncCL();
				vmoving = 0;
				goto vmark;
#endif
			case 'S':	/* replace lines */
				if (cnt > vcnt - vcline) {
					beep();
					continue;
				}
				vsave();
				setLAST();
				ind = whitecnt(linebuf);
				c = vliny[vcline];
				addr = dot;
				for (i = 0; i < cnt; i++)
					vulines[i] = dot[i];
				vyank(cnt);
				vresaddr = dot;
				vrescnt = cnt;
				vundkind = VMANY;
				vremote(cnt, delete);
				velide(cnt, vcline);
				vcline--;
				if (addr <= dol)
					dot--;
				*genindent(ind) = 0;
				vdoappend(genbuf);
				if (!visual) {
					holdat = 1;
					vclrlin(c);
					holdat = 0;
					voinit();
				} else {
					vcline++;
					vopen(dot, c);
					vsyncCL();
				}
				vrescurs = cursor;
				vdelcnt = 1;
				cursor = linebuf;
				linebuf[0] = 0;
				vfixcurs();
				vappend('o', 1, ind);
				continue;
			case 'O':	/* open new lines above */
			case 'o':	/* open new lines below */
				vsave();
				setLAST();
				ind = whitecnt(linebuf);
				if (c == 'O') {
					vcline--;
					dot--;
					if (dot > zero)
						getDOT();
				}
				if (!visual) {
					c = LINES - CA;
					vup1();
				} else {
					c = vcline < 0 ? ZERO : vliny[vcline] + vdepth();
					i = vliny[vcline + 1] - c;
					if (i < cnt)
						vopenup(cnt - i);
				}
				*genindent(ind) = 0;
				vdoappend(genbuf);
				if (!visual)
					voinit();
				else {
					vcline++;
					vopen(dot, c);
					vsyncCL();
				}
				vrescurs = cursor;
				vresaddr = dot;
				vrescnt = 0;
				vdelcnt = 1;
				vundkind = VMANY;
				cursor = linebuf;
				linebuf[0] = 0;
				vappend('o', 1, ind);
				continue;
			case 'a':	/* append */
				setLAST();
				if (*cursor) {
					vgotoCL(column(cursor));
					cursor++;
				}
				vappend(c, cnt, 0);
				continue;
			case 'R':
			case 'i':	/* insert */
				setLAST();
				if (*cursor == '\t')
					vgotoCL(column(cursor - 1));
				oglobp = cursor;
				vappend(c, cnt, 0);
/*
				cursor = oglobp;
				vmove();
*/
				continue;
			case DELETE:
				beep();
				if (peekkey() != DELETE)
					continue;
			case FS:	/* quit gets you out like a q */
				if (inglobal)
					onintr();
			case 'q':	/* quit */
				vch = 'q';
				vsave();
				return (-1);
/*
			case 'U':
				strcpy(genbuf, linebuf);
				putmk1(dot, vmunddot);
				getDOT();
				ic = vundcurs();
				first = 1;
				c = vreopen(vliny[vcline], dot - zero);
				vsync(vliny[vcline] + c);
				goto vmark;
*/
			case 'P':
			case 'p':
				if (vyancnt == 0) {
					if (DEL[0] == OVERBUF) {
						beep();
						continue;
					}
					vglobp = DEL;
					ungetkey(c == 'p' ? 'a' : 'i');
					goto vagain;
				}
				setLAST();
				addr = dot - vcline;
				vsave();
				if (c == 'P') {
					dot--;
					vcline--;
					c = 'p';
				}
				vdelcnt = vyancnt;
				vresaddr = dot + 1;
				vrescurs = 0;
				copy(vulines, vylines, vyancnt * sizeof vylines[0]);
				vresCNT = 0;
				vrescnt = vyancnt;
				append(vrestore, dot);
				vrescnt = 0;
				dot = vresaddr;
				vundkind = VMANY;
				goto putem;
			case 'u':	/* undo */
				switch (vundkind) {

				case VMANY:
					if (vdelcnt > TUBELINES)
						goto fonfon;
					vsave();
					addr = dot - vcline;
					if (vrescnt > 0) {
						vresCNT = 0;
						append(vrestore, vresaddr - 1);
					}
					if (vdelcnt > 0) {
						dot = vresaddr + vrescnt;
						copy(vulines, dot, vdelcnt * sizeof vulines[0]);
						vremote(vdelcnt, delete);
					}
					c = vrescnt;
					vrescnt = vdelcnt;
					vdelcnt = c;
					dot = vresaddr;
					if (dot > dol)
						dot--;
					/* buffer can't be empty */
putem:
					cnt = dot - addr;
					if (!visual || cnt < 0 || cnt > vcnt) {
						vch = '.';
						nextic = vrescurs;
						return (1);
					}
					dot--;
					vcnt = cnt;
					vcline = cnt - 1;
					if (vcline >= 0) {
						getDOT();
						vlast = vliny[vcline] + vdepth();
					} else
						vlast = ZERO;
					vroll(1);
					vredraw(vliny[vcline]);
					if (vrescurs) {
						ic = vrescurs;
						vrescurs = 0;
						first = 1;
					}
					goto vnline;
				case VNONE:
fonfon:
					beep();
					continue;
				case VCHNG:
					strcpy(genbuf, linebuf);
					strcLIN(vutmp);
					strcpy(vutmp, genbuf);
					goto vundoit;
				default:
					error("Internal error: vmain undo@- please tell someone");
				}
/*
			case 'U':
				strcpy(genbuf, linebuf);
				putmk1(dot, vmunddot);
				getDOT();
*/
vundoit:
				ic = vundcurs();
				first = 1;
				c = vreopen(vliny[vcline], dot - zero);
				vsync(vliny[vcline] + c);
				goto vmark;
			case '/':
			case '?':
				if (hadcnt) {
					beep();
					continue;
				}
				splitw++;
				ic = cursor;
				vsave();
				if (!visual && !CA)
					vup1();
				vgoto(LINES - 1, 0);
				putchar(c);
				if (CA) {
					vclreol();
					vgoto(LINES - 1, 1);
				}
				cursor = linebuf;
				linebuf[0] = 0;
				genbuf[0] = c;
				OP = Pline;
				Pline = &normline;
				c = vcline;
				i = LINES - 1 - vliny[vcnt];
				vliny[vcnt] = LINES - 1;
				vcline = vcnt;
				if (peekbr()) {
					if (!INS[0] || INS[0] == OVERBUF) {
						Peekkey = DELETE;
						goto back;
					}
					vglobp = INS;
				}
				vgetline(0, genbuf + 1, &hadcnt);
back:
				vcline = c;
				vliny[vcnt] =- i;
				Pline = OP;
				splitw = 0;
				if (genbuf[0] == 0 || Peekkey == DELETE ||
				    Peekkey == FS) {
					if (Peekkey == DELETE)
						Peekkey = 0;
oops:
					if (!CA) {
						nextic = ic;
						vch = '.';
						return (1);
					}
					getDOT();
					vsetcurs(ic);
					continue;
				}
/*
				if (genbuf[0] != '/' && genbuf[0] != '?')
					error("Search delimiter must be / or ?");
*/
				if (!vglobp)
					vscandir[0] = genbuf[0];
				oglobp = globp;
				globp = genbuf + 1;
				compile(genbuf[0], 1);
				savere(&scanre);
				vch = -1;
				if (globp && *globp) {
					c = *globp++;
					if (!visual || c != 'v' && c != 'z')
xtra:
						error("Extra chars|Extra characters after search pattern");
					vch = *globp++;
					switch (vch) {
						case 0:
							vch = 0;
							break;
						case '^':
						case '-':
						case '+':
						case '.':
							if (*globp == 0)
								break;
						default:
							goto xtra;
					}
				}
				splitw = 1;
				globp = oglobp;
				vgoto(LINES - 1, 0);
				flusho();
				splitw = 0;
				addr = scanfor(genbuf[0]);
				if (addr == NIL) {
					putchar('F');
					beep();
					vraw();
					goto oops;
				}
				markDOT();
it:
				c = vcline + (addr - dot);
				if (vch == -1 && CA && c >= 0 && c < vcnt) {
					ic = loc1;
					first = 1;
					vmoving = 0;
					vcline = c;
					dot = addr;
					goto vnline;
				}
				if (CA && !visual)
					vup1();
				nextic = loc1;
				dot = addr;
				if (vch == -1)
					vch = '.';
				return (1);
			default:
				operate(c, cnt);
				continue;
		}
	}
}

vyank(cnt)
	int cnt;
{

	DEL[0] = OVERBUF;
	vyancnt = cnt;
	copy(vylines, dot, cnt * sizeof vylines[0]);
}

vgetcnt()
{
	register int c, cnt;

	cnt = 0;
	for (;;) {
		c = getkey();
		if (!digit(c))
			break;
		cnt =* 10, cnt =+ c - '0';
	}
	ungetkey(c);
	Xhadcnt = 1;
	Xcnt = cnt;
	return(cnt);
}

vremote(cnt, f)
	int cnt, (*f)();
{

	addr1 = dot;
	addr2 = dot + cnt - 1;
	inglobal++;
	(*f)(0);
	inglobal--;
}

vfindcol(i)
	int i;
{
	register char *cp;

	qcolumn(linebuf - 1, 0);
	for (cp = linebuf; *cp && vcntcol < i; cp++)
		qcount(*cp);
	if (cp != linebuf)
		cp--;
	return (cp);
}

vsave()
{
	char genbuf[LBSIZE];

	strcpy(genbuf, linebuf);
	getDOT();
	if (strcmp(linebuf, genbuf) == 0)
		return (0);
	strcLIN(genbuf);
	putmark(dot);
	return (1);
}

vundcurs()
{
	register char *lp, *gp;

	for (lp = linebuf, gp = genbuf; *lp && *gp && *lp == *gp; lp++, gp++)
		continue;
	if (*lp)
		return (lp);
	if (*gp)
		return (lp > linebuf ? lp - 1 : lp);
	return (vskipwh(linebuf));
}

vrestor()
{

	if (vresCNT == vrescnt)
		return (EOF);
	getline(vulines[vresCNT]);
	vresCNT++;
	return (0);
}

vsyncCL()
{

	vsync(vliny[vcline]);
}

getDOT()
{

	getline(*dot);
}

vshift()
{
	shift(op, 1);
}
#endif
