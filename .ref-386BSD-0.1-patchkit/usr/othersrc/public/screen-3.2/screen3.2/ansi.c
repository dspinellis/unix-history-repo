/* Copyright (c) 1991
 *      Juergen Weigert (jnweiger@immd4.informatik.uni-erlangen.de)
 *      Michael Schroeder (mlschroe@immd4.informatik.uni-erlangen.de)
 * Copyright (c) 1987 Oliver Laumann
 * All rights reserved.  Not derived from licensed software.
 *
 * Permission is granted to freely use, copy, modify, and redistribute
 * this software, provided that no attempt is made to gain profit from it,
 * the authors are not construed to be liable for any results of using the
 * software, alterations are clearly marked as such, and this notice is
 * not modified.
 *
 * Noteworthy contributors to screen's design and implementation:
 *	Wayne Davison (davison@borland.com)
 *	Patrick Wolfe (pat@kai.com, kailand!pat)
 *	Bart Schaefer (schaefer@cse.ogi.edu)
 *	Nathan Glasser (nathan@brokaw.lcs.mit.edu)
 *	Larry W. Virden (lwv27%cas.BITNET@CUNYVM.CUNY.Edu)
 *	Howard Chu (hyc@hanauma.jpl.nasa.gov)
 *	Tim MacKenzie (tym@dibbler.cs.monash.edu.au)
 *	Markku Jarvinen (mta@{cc,cs,ee}.tut.fi)
 *	Marc Boucher (marc@CAM.ORG)
 *
 ****************************************************************
 */

#ifndef lint
  static char rcs_id[] = "$Id: ansi.c,v 1.2 92/02/03 02:27:30 jnweiger Exp $ FAU";
#endif

#include <stdio.h>
#include <sys/types.h>
#ifdef BSDI
#include <sys/signal.h>
#endif /* BSDI */
#include <fcntl.h>
#include "config.h"
#include "screen.h"
#include "ansi.h"
#include "extern.h"
#ifndef sun /* we want to know about TIOCGWINSZ. jw. */
# include <sys/ioctl.h>
#endif

extern char *getenv(), *tgetstr(), *tgoto();
#ifndef __STDC__
extern char *malloc();
#endif

extern struct win *fore;
extern int ForeNum;
extern force_vt, assume_LP;
extern int BellDisplayed;
extern int MsgMinWait;
extern int all_norefresh;

int TermcapROWS, TermcapCOLS;	/* defaults that we learned from termcap */
int default_width, default_height;	/* width/height a new window will get */

int maxwidth;

int Z0width, Z1width;		/* widths for Z0/Z1 switching */

char display_tty[MAXPATH];
int screenwidth, screenheight;	/* width/height of the screen */
int screentop, screenbot;	/* scrollregion start/end */
int screenx, screeny;		/* cursor position */
char GlobalAttr;		/* current attributes */
char GlobalCharset;		/* current font */
int insert;			/* insert mode */
int keypad;			/* application keypad */
int flow = 1;			/* flow control */

int status;			/* status is displayed */
static int status_lastx, status_lasty;

static int rows, cols;		/* window size of the curr window */

int default_flow = -1, wrap = 1, default_monitor = 0; 
int visual_bell = 0, termcapHS, use_hardstatus = 1;
char *Termcap, *extra_incap, *extra_outcap;
static int Termcaplen;
char *blank, *null, *LastMsg;
char Term[MAXSTR+5];	/* +5: "TERM=" */
char screenterm[20] = "screen";
char *Z0, *Z1;
int ISO2022, HS;
time_t TimeDisplayed, time();

/*
 * the termcap routines need this to work
 */
short ospeed;
char *BC;
char *UP;

static void AddCap __P((char *));
static void MakeString __P((char *, char *, int, char *));
static int Special __P((int));
static void DoESC __P((int, int ));
static void DoCSI __P((int, int ));
static void CPutStr __P((char *, int));
static void SetChar __P(());
static void StartString __P((enum string_t));
static void AddChar __P((int ));
static void PrintChar __P((int ));
static void PrintFlush __P((void));
static void KeypadMode __P((int));
static void DesignateCharset __P((int, int ));
static void MapCharset __P((int));
static void SaveCursor __P((void));
static void RestoreCursor __P((void));
static void CountChars __P((int));
static int CalcCost __P((char *));
static int Rewrite __P((int, int, int, int));
static void BackSpace __P((void));
static void Return __P((void));
static void LineFeed __P((int));
static void ReverseLineFeed __P((void));
static void InsertAChar __P((int));
static void InsertChar __P((int));
static void DeleteChar __P((int));
static void DeleteLine __P((int));
static void InsertLine __P((int));
static void ScrollUpMap __P((int));
static void ScrollDownMap __P((int));
static void Scroll __P((char *, int, int, char *));
static void ForwardTab __P((void));
static void BackwardTab __P((void));
static void ClearScreen __P((void));
static void ClearFromBOS __P((void));
static void ClearToEOS __P((void));
static void ClearLine __P((void));
static void ClearToEOL __P((void));
static void ClearFromBOL __P((void));
static void ClearInLine __P((int, int, int, int ));
static void CursorRight __P(());
static void CursorUp __P(());
static void CursorDown __P(());
static void CursorLeft __P(());
static void ASetMode __P((int));
static void SelectRendition __P((void));
static void FillWithEs __P((void));
static void RedisplayLine __P((char *, char *, char *, int, int, int ));
static void FindAKA __P((void));
static void SetCurr __P((struct win *));
static void inpRedisplayLine __P((int, int, int, int));
static void process_inp_input __P((char **, int *));
static void AbortInp __P((void));
static void AKAfin __P((char *, int));
static void Colonfin __P((char *, int));
static void RAW_PUTCHAR __P((int));
static char *e_tgetstr __P((char *, char **));
static int e_tgetflag __P((char *));
static int e_tgetnum __P((char *));


static char *tbuf, *tentry, *termname;
static char *tp;
static char *TI, *TE, *BL, *VB, *CR, *NL, *CL, *IS;
char *WS;	/* used in ResizeScreen() */
char *CE;	/* used in help.c */
static char *CM, *US, *UE, *SO, *SE, *CD, *DO, *SR, *SF, *AL;
static char *CS, *DL, *DC, *IC, *IM, *EI, *ND, *KS, *KE;
static char *MB, *MD, *MH, *MR, *ME, *PO, *PF, *HO;
static char *TS, *FS, *DS, *VI, *VE, *VS;
static char *CDC, *CDL, *CAL, *CUP, *CDO, *CLE, *CRI, *CIC;
static char *attrtab[NATTR];
static AM, MS, COP;
int LP;
/*
 * Do not confuse S0 (es-zero), E0 (e-zero) with SO (es-oh), PO (pe-oh),
 * Z0 (z-zero), DO (de-oh)... :-)
 */
static char *C0, *S0, *E0;
static char c0_tab[256];
/*
 */
static screencap = 0;
char *OldImage, *OldAttr, *OldFont;
static struct win *curr;
static display = 1;
static StrCost;
static UPcost, DOcost, LEcost, NDcost, CRcost, IMcost, EIcost, NLcost;
static tcLineLen;
static StatLen;
static lp_missing = 0;

int in_ovl;
int ovl_blockfore;
void (*ovl_process)();
void (*ovl_RedisplayLine)();
int (*ovl_Rewrite)();

static char *KeyCaps[] =
{
  "k0", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9",
  "kb", "kd", "kh", "kl", "ko", "kr", "ku",
  "K1", "K2", "K3", "K4", "K5",
  "l0", "l1", "l2", "l3", "l4", "l5", "l6", "l7", "l8", "l9"
};
#define NKEYCAPS ((int)(sizeof(KeyCaps)/sizeof(*KeyCaps)))
static char *KeyCapsArr[NKEYCAPS];

static char TermcapConst[] = "\\\n\
\t:DO=\\E[%dB:LE=\\E[%dD:RI=\\E[%dC:UP=\\E[%dA:bs:bt=\\E[Z:\\\n\
\t:cd=\\E[J:ce=\\E[K:cl=\\E[H\\E[J:cm=\\E[%i%d;%dH:ct=\\E[3g:\\\n\
\t:do=^J:nd=\\E[C:pt:rc=\\E8:rs=\\Ec:sc=\\E7:st=\\EH:up=\\EM:\\\n\
\t:le=^H:bl=^G:cr=^M:it#8:ho=\\E[H:nw=\\EE:ta=^I:is=\\E)0:xv:";

void
InitTermcap()
{
  register char *s;
  int i;

  screencap = 0;
  if ((s = getenv("SCREENCAP")) != 0)
    {
      if ((Termcap = malloc(strlen(s) + 10)) != 0)
	{
	  sprintf(Termcap, "TERMCAP=%s", s);
	  screencap = 1;
	}
    }
  else
    Termcap = malloc((unsigned) 1024);
  Termcaplen = 0;
  tbuf = malloc((unsigned) 1024);
  tentry = tp = malloc((unsigned) 1024);
  if (!(Termcap && tbuf && tentry))
    Msg_nomem;
  bzero(tbuf, 1024);
  if ((termname = getenv("TERM")) == 0)
    Msg(0, "No TERM in environment.");
  debug1("InitTermcap: looking for tgetent('%s');\n", termname);
  if (tgetent(tbuf, termname) != 1)
    Msg(0, "Cannot find termcap entry for %s.", termname);
  debug1("got it:\n%s\n",tbuf);
#ifdef DEBUG
  if (extra_incap)
    debug1("Extra incap: %s\n", extra_incap);
  if (extra_outcap)
    debug1("Extra outcap: %s\n", extra_outcap);
#endif

  TermcapCOLS = TermcapROWS = 0;
  if (s = getenv("COLUMNS"))
    TermcapCOLS = atoi(s);
  if (TermcapCOLS <= 0)
    TermcapCOLS = e_tgetnum("co");
  if (TermcapCOLS <= 0)
    TermcapCOLS = 80;
  if (s = getenv("LINES"))
    TermcapROWS = atoi(s);
  if (TermcapROWS <= 0)
    TermcapROWS = e_tgetnum("li");
  if (TermcapROWS <= 0)
    TermcapROWS = 24;

  if (e_tgetflag("hc"))
    Msg(0, "You can't run screen on a hardcopy terminal.");
  if (e_tgetflag("os"))
    Msg(0, "You can't run screen on a terminal that overstrikes.");
  if (e_tgetflag("ns"))
    Msg(0, "Terminal must support scrolling.");
  if (!(CL = e_tgetstr("cl", &tp)))
    Msg(0, "Clear screen capability required.");
  if (!(CM = e_tgetstr("cm", &tp)))
    Msg(0, "Addressable cursor capability required.");
  if (default_flow < 0)
    default_flow = e_tgetflag("NF") ? FLOW_NOW * 0 : 
		   e_tgetflag("xo") ? FLOW_NOW * 1 :
		   FLOW_AUTOFLAG;
  AM = e_tgetflag("am");
  LP = assume_LP || (!extra_incap && !strncmp(termname, "vt", 2))
  		 || !AM || e_tgetflag("LP") || e_tgetflag("xv");
  COP = e_tgetflag("OP");
  HO = e_tgetstr("ho", &tp);
  TI = e_tgetstr("ti", &tp);
  TE = e_tgetstr("te", &tp);
  if (!(BL = e_tgetstr("bl", &tp)))
    BL = "\007";
  VB = e_tgetstr("vb", &tp);
  if (!(BC = e_tgetstr("bc", &tp)))
    {
      if (e_tgetflag("bs"))
	BC = "\b";
      else
	BC = e_tgetstr("le", &tp);
    }
  if (!(CR = e_tgetstr("cr", &tp)))
    CR = "\r";
  if (!(NL = e_tgetstr("nl", &tp)))
    NL = "\n";
  IS = e_tgetstr("is", &tp);
  MS = 1;
  if (e_tgetnum("sg") <= 0 && e_tgetnum("ug") <= 0)
    {
      MS = e_tgetflag("ms");
      attrtab[ATTR_DI] = MH = e_tgetstr("mh", &tp);	/* Dim */
      attrtab[ATTR_US] = US = e_tgetstr("us", &tp);	/* Underline */
      attrtab[ATTR_BD] = MD = e_tgetstr("md", &tp);	/* Bold */
      attrtab[ATTR_RV] = MR = e_tgetstr("mr", &tp);	/* Reverse */
      attrtab[ATTR_SO] = SO = e_tgetstr("so", &tp);	/* Standout */
      attrtab[ATTR_BL] = MB = e_tgetstr("mb", &tp);	/* Blinking */
      ME = e_tgetstr("me", &tp);
      SE = e_tgetstr("se", &tp);
      UE = e_tgetstr("ue", &tp);
      /*
       * Does ME also reverse the effect of SO and/or US?  This is not
       * clearly specified by the termcap manual. Anyway, we should at
       * least look whether ME and SE/UE are equal:
       */
      if (UE && ((SE && strcmp(SE, UE) == 0) || (ME && strcmp(ME, UE) == 0)))
	UE = 0;
      if (SE && (ME && strcmp(ME, SE) == 0))
	SE = 0;

      /* Set up missing entries */
      s = 0;
      for (i = NATTR-1; i >= 0; i--)
	if (attrtab[i])
	  s = attrtab[i];
      for (i = 0; i < NATTR; i++)
	{
	  if (attrtab[i] == 0)
	    attrtab[i] = s;
	  else
	    s = attrtab[i];
	}
    }
  else
    {
      US = UE = SO = SE = MB = MD = MH = MR = ME = 0;
      for (i = 0; i < NATTR; i++)
	attrtab[i] = 0;
    }
  CE = e_tgetstr("ce", &tp);
  CD = e_tgetstr("cd", &tp);
  if (!(DO = e_tgetstr("do", &tp)))
    DO = NL;
  UP = e_tgetstr("up", &tp);
  ND = e_tgetstr("nd", &tp);
  SR = e_tgetstr("sr", &tp);
  if (!(SF = e_tgetstr("sf", &tp)))
    SF = NL;
  AL = e_tgetstr("al", &tp);
  DL = e_tgetstr("dl", &tp);
  CS = e_tgetstr("cs", &tp);
  DC = e_tgetstr("dc", &tp);
  IC = e_tgetstr("ic", &tp);
  CIC = e_tgetstr("IC", &tp);
  CDC = e_tgetstr("DC", &tp);
  CDL = e_tgetstr("DL", &tp);
  CAL = e_tgetstr("AL", &tp);
  CUP = e_tgetstr("UP", &tp);
  CDO = e_tgetstr("DO", &tp);
  CLE = e_tgetstr("LE", &tp);
  CRI = e_tgetstr("RI", &tp);
  IM = e_tgetstr("im", &tp);
  EI = e_tgetstr("ei", &tp);
  if (e_tgetflag("in"))
    IC = IM = 0;
  if (IC && IC[0] == '\0')
    IC = 0;
  if (CIC && CIC[0] == '\0')
    CIC = 0;
  if (IM && IM[0] == '\0')
    IM = 0;
  if (EI && EI[0] == '\0')
    EI = 0;
  if (EI == 0)
    IM = 0;
  if (IC && IM && strcmp(IC, IM) == 0)
    IC = 0;
  KS = e_tgetstr("ks", &tp);
  KE = e_tgetstr("ke", &tp);
  if (KE == 0)
    KS = 0;
  ISO2022 = e_tgetflag("G0");
  if (ISO2022)
    {
      if ((S0 = e_tgetstr("S0", &tp)) == NULL)
#ifdef TERMINFO
	S0 = "\033(%p1%c";
#else
	S0 = "\033(%.";
#endif
      if ((E0 = e_tgetstr("E0", &tp)) == NULL)
	E0 = "\033(B";
      C0 = e_tgetstr("C0", &tp);
    }
  else if ((S0 = e_tgetstr("as", &tp)) != NULL
        && (E0 = e_tgetstr("ae", &tp)) != NULL)
    {
      ISO2022 = 1;
      C0 = e_tgetstr("ac", &tp);
    }
  else
    {
      S0 = E0 = "";
      C0 = "g.h.i'j-k-l-m-n+o~p\"q-r-s_t+u+v+w+x|y<z>";
    }
  for (i = 0; i < 256; i++)
    c0_tab[i] = i;
  if (C0)
    for (i = strlen(C0)&~1; i >= 0; i-=2)
      c0_tab[C0[i]] = C0[i+1];
  debug1("ISO2022 = %d\n", ISO2022);
  /* WS changes the window size */
  WS = e_tgetstr("WS", &tp);
  VI = e_tgetstr("vi", &tp);
  VE = e_tgetstr("ve", &tp);
  VS = e_tgetstr("vs", &tp);
  PO = e_tgetstr("po", &tp);
  if (!(PF = e_tgetstr("pf", &tp)))
    PO = 0;
  debug2("terminal size is %d, %d (says TERMCAP)\n", TermcapCOLS, TermcapROWS);
  /* Termcap fields Z0 & Z1 contain width-changing sequences. */
  if ((Z0 = e_tgetstr("Z0", &tp)) != NULL
      && (Z1 = e_tgetstr("Z1", &tp)) == NULL)
    Z0 = NULL;

  Z0width = 132;
  Z1width = 80;

  CheckScreenSize(0);
  if ((HS = e_tgetflag("hs")) != 0)
    {
      debug("oy! we have a hardware status line, says termcap\n");
      TS = e_tgetstr("ts", &tp);
      FS = e_tgetstr("fs", &tp);
      DS = e_tgetstr("ds", &tp);
      if ((HS = e_tgetnum("ws")) <= 0)
	HS = screenwidth;
      if (!TS || !FS || !DS)
	HS = 0;
    }
  termcapHS = HS;
  if (!use_hardstatus)
    HS = 0;

  UPcost = CalcCost(UP);
  DOcost = CalcCost(DO);
  NLcost = CalcCost(NL);
  LEcost = CalcCost(BC);
  NDcost = CalcCost(ND);
  CRcost = CalcCost(CR);
  IMcost = CalcCost(IM);
  EIcost = CalcCost(EI);
  for (i = 0; i < NKEYCAPS; i++)
    KeyCapsArr[i] = e_tgetstr(KeyCaps[i], &tp);
  MakeTermcap(0);
}

/*
 * if the adaptflag is on, we keep the size of this display, else
 * we may try to restore our old window sizes.
 */
void
InitTerm(adapt)
int adapt;
{
  display = 1;
  screentop = screenbot = -1;
  PutStr(IS);
  PutStr(TI);
  if (IM && strcmp(IM, EI))
    PutStr(EI);
  insert = 0;
  if (KS && strcmp(KS, KE))
    PutStr(KE);
  keypad = 0;
  PutStr(E0);
  GlobalCharset = ASCII;
  ResizeScreen((struct win *)0);
  ChangeScrollRegion(0, screenheight-1);
  PutStr(CL);
  screenx = screeny = 0;
  fflush(stdout);
  debug1("we %swant to adapt all our windows to the display\n", 
	 (adapt) ? "" : "don't ");
  /* In case the size was changed by a init sequence */
  CheckScreenSize((adapt) ? 2 : 0);
}

void
FinitTerm()
{
  display = 1;
  InsertMode(0);
  KeypadMode(0);
  ResizeScreen((struct win *)0);
  ChangeScrollRegion(0, screenheight - 1);
  SaveSetAttr(0, ASCII);
  screenx = screeny = -1;
  GotoPos(0, screenheight - 1);
  PutStr(TE);
  fflush(stdout);
  if (Termcap) 
    {
      Free(Termcap);
      debug("FinitTerm: old termcap freed\n");
    }
  if (tbuf) 
    {
      Free(tbuf);
      debug("FinitTerm: old tbuf freed\n");
    }
  if (tentry) 
    {
      Free(tentry);
      debug("FinitTerm: old tentry freed\n");
    }
}

static void AddCap(s)
char *s;
{
  register int n;

  if (tcLineLen + (n = strlen(s)) > 55 && Termcaplen < 1024-4)
    {
      strcpy(Termcap + Termcaplen, "\\\n\t:");
      Termcaplen += 4;
      tcLineLen = 0;
    }
  if (Termcaplen + n < 1024)
    {
      strcpy(Termcap + Termcaplen, s);
      Termcaplen += n;
      tcLineLen += n;
    }
  else
    Msg(0, "TERMCAP overflow - sorry.");
}

char *MakeTermcap(aflag)
int aflag;
{
  char buf[1024];
  register char *p, *cp, ch;
  int i;

  if (screencap)
    {
      sprintf(Term, "TERM=screen");
      return Termcap;
    }
  if (screenterm == 0 || *screenterm == '\0')
    {
      debug("MakeTermcap sets screenterm=screen\n");
      strcpy(screenterm, "screen");
    }
  for (;;)
    {
      sprintf(Term, "TERM=");
      p = Term + 5;
      if (!aflag && strlen(screenterm) + strlen(termname) < MAXSTR-1)
	{
	  sprintf(p, "%s.%s", screenterm, termname);
	  if (tgetent(buf, p) == 1)
	    break;
	}
      if (screenwidth >= 132)
	{
	  sprintf(p, "%s-w", screenterm);
          if (tgetent(buf, p) == 1)
	    break;
	}
      sprintf(p, "%s", screenterm);
      if (tgetent(buf, p) == 1)
	break;
      sprintf(p, "vt100");
      break;
    }
  tcLineLen = 100;	/* Force NL */
  sprintf(Termcap,
	  "TERMCAP=SC|%s|VT 100/ANSI X3.64 virtual terminal|", p);
  Termcaplen = strlen(Termcap);
  if (extra_outcap && *extra_outcap)
    {
      for (cp = extra_outcap; p = index(cp, ':'); cp = p)
	{
	  ch = *++p;
	  *p = '\0';
	  AddCap(cp);
	  *p = ch;
	}
      tcLineLen = 100;	/* Force NL */
    }
  if (Termcaplen + strlen(TermcapConst) < 1024)
    {
      strcpy(Termcap + Termcaplen, TermcapConst);
      Termcaplen += strlen(TermcapConst);
    }
  sprintf(buf, "li#%d:co#%d:", screenheight, screenwidth);
  AddCap(buf);
  if ((force_vt && !COP) || LP || !AM)
    AddCap("LP:");
  else
    AddCap("am:");
  if (VB)
    AddCap("vb=\\E[?5h\\E[?5l:");
  if (US)
    {
      AddCap("us=\\E[4m:");
      AddCap("ue=\\E[24m:");
    }
  if (SO)
    {
      AddCap("so=\\E[3m:");
      AddCap("se=\\E[23m:");
    }
  if (MB)
    AddCap("mb=\\E[5m:");
  if (MD)
    AddCap("md=\\E[1m:");
  if (MH)
    AddCap("mh=\\E[2m:");
  if (MR)
    AddCap("mr=\\E[7m:");
  if (MB || MD || MH || MR)
    AddCap("me=\\E[m:ms:");
  if ((CS && SR) || AL || CAL || aflag)
    {
      AddCap("sr=\\EM:");
      AddCap("al=\\E[L:");
      AddCap("AL=\\E[%dL:");
    }
  else if (SR)
    AddCap("sr=\\EM:");
  if (CS || DL || CDL || aflag)
    {
      AddCap("dl=\\E[M:");
      AddCap("DL=\\E[%dM:");
    }
  if (CS)
    AddCap("cs=\\E[%i%d;%dr:");
  if (DC || CDC || aflag)
    {
      AddCap("dc=\\E[P:");
      AddCap("DC=\\E[%dP:");
    }
  if (CIC || IC || IM || aflag)
    {
      AddCap("im=\\E[4h:");
      AddCap("ei=\\E[4l:");
      AddCap("mi:");
      AddCap("ic=\\E[@:");
      AddCap("IC=\\E[%d@:");
    }
  if (KS)
    AddCap("ks=\\E=:");
  if (KE)
    AddCap("ke=\\E>:");
  if (ISO2022)
    AddCap("G0:");
  if (PO)
    {
      AddCap("po=\\E[5i:");
      AddCap("pf=\\E[4i:");
    }
  if (Z0)
    {
      AddCap("Z0=\\E[?3h:");
      AddCap("Z1=\\E[?3l:");
    }
  if (WS)
    AddCap("WS=\\E[8;%d;%dt:");
  for (i = 0; i < NKEYCAPS; i++)
    {
      if (KeyCapsArr[i] == 0)
	continue;
      MakeString(KeyCaps[i], buf, sizeof(buf), KeyCapsArr[i]);
      AddCap(buf);
    }
  return Termcap;
}

static void MakeString(cap, buf, buflen, s)
char *cap, *buf;
int buflen;
char *s;
{
  register char *p, *pmax;
  register unsigned int c;

  p = buf;
  pmax = p + buflen - (3+4+2);
  *p++ = *cap++;
  *p++ = *cap;
  *p++ = '=';
  while ((c = *s++) && (p < pmax))
    {
      switch (c)
	{
	case '\033':
	  *p++ = '\\';
	  *p++ = 'E';
	  break;
	case ':':
	  sprintf(p, "\\072");
	  p += 4;
	  break;
	case '^':
	case '\\':
	  *p++ = '\\';
	  *p++ = c;
	  break;
	default:
	  if (c >= 200)
	    {
	      sprintf(p, "\\%03o", c & 0377);
	      p += 4;
	    }
	  else if (c < ' ')
	    {
	      *p++ = '^';
	      *p++ = c + '@';
	    }
	  else
	    *p++ = c;
	}
    }
  *p++ = ':';
  *p = '\0';
}

void
Activate(norefresh)
int norefresh;
{
  debug1("Activate(%d)\n", norefresh);
  if (display)
    RemoveStatus();
  display = fore->active = 1;
  ResizeScreen(fore);
  SetCurr(fore);
  debug3("Fore (%d) has size %dx%d", ForeNum, curr->width, curr->height);
  debug1("(%d)\n", curr->histheight);
  ChangeScrollRegion(curr->top, curr->bot);
  KeypadMode(curr->keypad);
  SetFlow(curr->flow & FLOW_NOW);
  if (curr->monitor != MON_OFF)
    curr->monitor = MON_ON;
  curr->bell = BELL_OFF;
  Redisplay(norefresh || all_norefresh);
}

void
ResetScreen(p)
register struct win *p;
{
  register int i;

  p->wrap = wrap;
  p->origin = 0;
  p->insert = 0;
  p->vbwait = 0;
  p->keypad = 0;
  p->top = 0;
  p->bot = p->height - 1;
  p->saved = 0;
  p->LocalAttr = 0;
  p->x = p->y = 0;
  p->state = LIT;
  p->StringType = NONE;
  p->ss = 0;
  p->LocalCharset = G0;
  bzero(p->tabs, p->width);
  for (i = 8; i < p->width; i += 8)
    p->tabs[i] = 1;
  for (i = G0; i <= G3; i++)
    p->charsets[i] = ASCII;
}

void
WriteString(wp, buf, len)
struct win *wp;
char *buf;
int len;
{
  register int c, intermediate = 0;

  if (!len)
    return;
  if (wp->logfp != NULL)
    if ((int)fwrite(buf, len, 1, wp->logfp) < 1)
      {
	extern int errno;

	Msg(errno, "Error writing logfile");
	fclose(wp->logfp);
	wp->logfp = NULL;
      }
  /*
   * SetCurr() here may prevent output, as it may set display = 0
   */
  SetCurr(wp);
  if (display)
    {
      if (!HS)
	RemoveStatus();
    }
  else if (curr->monitor == MON_ON)
    curr->monitor = MON_FOUND;

  do
    {
      c = (unsigned char)*buf++;
      if (c == '\0' || c == '\177')
	continue;
    NextChar:
      switch (curr->state)
	{
	case PRIN:
	  switch (c)
	    {
	    case '\033':
	      curr->state = PRINESC;
	      break;
	    default:
	      PrintChar(c);
	    }
	  break;
	case PRINESC:
	  switch (c)
	    {
	    case '[':
	      curr->state = PRINCSI;
	      break;
	    default:
	      PrintChar('\033');
	      PrintChar(c);
	      curr->state = PRIN;
	    }
	  break;
	case PRINCSI:
	  switch (c)
	    {
	    case '4':
	      curr->state = PRIN4;
	      break;
	    default:
	      PrintChar('\033');
	      PrintChar('[');
	      PrintChar(c);
	      curr->state = PRIN;
	    }
	  break;
	case PRIN4:
	  switch (c)
	    {
	    case 'i':
	      curr->state = LIT;
	      PrintFlush();
	      break;
	    default:
	      PrintChar('\033');
	      PrintChar('[');
	      PrintChar('4');
	      PrintChar(c);
	      curr->state = PRIN;
	    }
	  break;
	case STRESC:
	  switch (c)
	    {
	    case '\\':
	      curr->state = LIT;
	      *(curr->stringp) = '\0';
	      switch (curr->StringType)
		{
		case PM:
		  if (!display)
		    break;
		  MakeStatus(curr->string);
		  if (!HS && status && len > 1)
		    {
		      curr->outlen = len - 1;
		      bcopy(buf, curr->outbuf, curr->outlen);
		      return;
		    }
		  break;
		case DCS:
		  if (display)
		    printf("%s", curr->string);
		  break;
		case AKA:
		  if (curr->akapos == 0 && !*curr->string)
		    break;
		  strncpy(curr->cmd + curr->akapos, curr->string, 20);
		  if (!*curr->string)
		    curr->autoaka = curr->y + 1;
		  break;
		default:
		  break;
		}
	      break;
	    default:
	      curr->state = ASTR;
	      AddChar('\033');
	      AddChar(c);
	    }
	  break;
	case ASTR:
	  switch (c)
	    {
	    case '\0':
	      break;
	    case '\033':
	      curr->state = STRESC;
	      break;
	    default:
	      AddChar(c);
	    }
	  break;
	case ESC:
	  switch (c)
	    {
	    case '[':
	      curr->NumArgs = 0;
	      intermediate = 0;
	      bzero((char *) curr->args, MAXARGS * sizeof(int));
	      curr->state = CSI;
	      break;
	    case ']':
	      StartString(OSC);
	      break;
	    case '_':
	      StartString(APC);
	      break;
	    case 'P':
	      StartString(DCS);
	      break;
	    case '^':
	      StartString(PM);
	      break;
	    case '"':
	    case 'k':
	      StartString(AKA);
	      break;
	    default:
	      if (Special(c))
		break;
	      if (c >= ' ' && c <= '/')
		intermediate = intermediate ? -1 : c;
	      else if (c >= '0' && c <= '~')
		{
		  DoESC(c, intermediate);
		  curr->state = LIT;
		}
	      else
		{
		  curr->state = LIT;
		  goto NextChar;
		}
	    }
	  break;
	case CSI:
	  switch (c)
	    {
	    case '0':
	    case '1':
	    case '2':
	    case '3':
	    case '4':
	    case '5':
	    case '6':
	    case '7':
	    case '8':
	    case '9':
	      if (curr->NumArgs < MAXARGS)
		{
		  curr->args[curr->NumArgs] =
		    10 * curr->args[curr->NumArgs] + c - '0';
		}
	      break;
	    case ';':
	    case ':':
	      curr->NumArgs++;
	      break;
	    default:
	      if (Special(c))
		break;
	      if (c >= '@' && c <= '~')
		{
		  curr->NumArgs++;
		  DoCSI(c, intermediate);
		  if (curr->state != PRIN)
		    curr->state = LIT;
		}
	      else if ((c >= ' ' && c <= '/') || (c >= '<' && c <= '?'))
		intermediate = intermediate ? -1 : c;
	      else
		{
		  curr->state = LIT;
		  goto NextChar;
		}
	    }
	  break;
	case LIT:
	default:
	  if (!Special(c))
	    {
	      if (c == '\033')
		{
		  intermediate = 0;
		  curr->state = ESC;
		  if (display && lp_missing && (CIC || IC || IM))
		    {
		      RedisplayLine(blank, null, null, screenbot,
				    cols - 2, cols - 1);
		      GotoPos(curr->x, curr->y);
		    }
		  if (curr->autoaka < 0)
		    curr->autoaka = 0;
		}
	      else if (c < ' ')
		break;
	      else
		{
		  NewRendition(curr->LocalAttr);
		  NewCharset(curr->charsets[(curr->ss) ? curr->ss :
					     curr->LocalCharset]);
		  if (curr->x < cols - 1)
		    {
		      if (curr->insert)
			InsertAChar(c);
		      else
			{
			  if (display)
			    PUTCHAR(c);
			  SetChar(c);
			}
		      curr->x++;
		    }
		  else if (curr->x == cols - 1)
		    {
		      if (curr->wrap && (LP || !force_vt || COP))
			{
			  if (display)
			    RAW_PUTCHAR(c);
			  SetChar(c);
			  if (AM && !LP)
			    {
			      curr->x = 0; /* terminal auto-wrapped */
			      LineFeed(0);
			    }
			  else
			    curr->x++;
			}
		      else
			{
			  if (display)
			    {
			      if (LP || curr->y != screenbot)
				{
				  RAW_PUTCHAR(c);
				  GotoPos(curr->x, curr->y);
				}
			      else
				CheckLP(c);
			    }
			  SetChar(c);
			  if (curr->wrap)
			    curr->x++;
			}
		    }
		  else
		    {
		      LineFeed(2); /* cr+lf, handle LP */
		      if (curr->insert)
			InsertAChar(c);
		      else
			{
			  if (display)
			    PUTCHAR(c);
			  SetChar(c);
			}
		      curr->x = 1;
		    }
		  if (curr->ss)
		    {
		      NewCharset(curr->charsets[curr->LocalCharset]);
		      curr->ss = 0;
		    }
		}
	    }
	}
    } while (--len);
  curr->outlen = 0;
  if (curr->state == PRIN)
    PrintFlush();
}

static int Special(c)
register int c;
{
  switch (c)
    {
    case '\b':
      BackSpace();
      return 1;
    case '\r':
      Return();
      return 1;
    case '\n':
      if (curr->autoaka)
	FindAKA();
      LineFeed(1);
      return 1;
    case '\007':
      if (!visual_bell)
	PutStr(BL);
      else
	{
	  if (!VB)
	    curr->bell = BELL_VISUAL;
	  else
	    PutStr(VB);
	}
      if (!display)
	curr->bell = BELL_ON;
      return 1;
    case '\t':
      ForwardTab();
      return 1;
    case '\017':		/* SI */
      MapCharset(G0);
      return 1;
    case '\016':		/* SO */
      MapCharset(G1);
      return 1;
    }
  return 0;
}

static void DoESC(c, intermediate)
int c, intermediate;
{
  switch (intermediate)
    {
    case 0:
      switch (c)
	{
	case 'E':
	  LineFeed(2);
	  break;
	case 'D':
	  LineFeed(1);
	  break;
	case 'M':
	  ReverseLineFeed();
	  break;
	case 'H':
	  curr->tabs[curr->x] = 1;
	  break;
	case 'Z':		/* jph: Identify as VT100 */
	  Report(curr, "\033[?%d;%dc", 1, 2);
	  break;
	case '7':
	  SaveCursor();
	  break;
	case '8':
	  RestoreCursor();
	  break;
	case 'c':
	  ClearScreen();
	  ResetScreen(curr);
	  NewRendition(0);
	  NewCharset(ASCII);
	  InsertMode(0);
	  KeypadMode(0);
	  ChangeScrollRegion(0, rows-1);
	  break;
	case '=':
	  KeypadMode(curr->keypad = 1);
#if !defined(TIOCPKT) || defined(sgi)
	  NewAutoFlow(curr, 0);
#endif /* !TIOCPKT || sgi */
	  break;
	case '>':
	  KeypadMode(curr->keypad = 0);
#if !defined(TIOCPKT) || defined(sgi)
	  NewAutoFlow(curr, 1);
#endif /* !TIOCPKT || sgi */
	  break;
	case 'n':		/* LS2 */
	  MapCharset(G2);
	  break;
	case 'o':		/* LS3 */
	  MapCharset(G3);
	  break;
	case 'N':		/* SS2 */
	  if (curr->charsets[curr->LocalCharset] != curr->charsets[G2])
	    curr->ss = G2;
	  else
	    curr->ss = 0;
	  break;
	case 'O':		/* SS3 */
	  if (curr->charsets[curr->LocalCharset] != curr->charsets[G3])
	    curr->ss = G3;
	  else
	    curr->ss = 0;
	  break;
	}
      break;
    case '#':
      switch (c)
	{
	case '8':
	  FillWithEs();
	  break;
	}
      break;
    case '(':
      DesignateCharset(c, G0);
      break;
    case ')':
      DesignateCharset(c, G1);
      break;
    case '*':
      DesignateCharset(c, G2);
      break;
    case '+':
      DesignateCharset(c, G3);
      break;
    }
}

static void DoCSI(c, intermediate)
int c, intermediate;
{
  register int i, a1 = curr->args[0], a2 = curr->args[1];

  if (curr->NumArgs > MAXARGS)
    curr->NumArgs = MAXARGS;
  switch (intermediate)
    {
    case 0:
      switch (c)
	{
	case 'H':
	case 'f':
	  if (a1 < 1)
	    a1 = 1;
	  if (curr->origin)
	    a1 += curr->top;
	  if (a1 > rows)
	    a1 = rows;
	  if (a2 < 1)
	    a2 = 1;
	  if (a2 > cols)
	    a2 = cols;
	  GotoPos(--a2, --a1);
	  curr->x = a2;
	  curr->y = a1;
	  if (curr->autoaka)
	    curr->autoaka = a1 + 1;
	  break;
	case 'J':
	  if (a1 < 0 || a1 > 2)
	    a1 = 0;
	  switch (a1)
	    {
	    case 0:
	      ClearToEOS();
	      break;
	    case 1:
	      ClearFromBOS();
	      break;
	    case 2:
	      ClearScreen();
	      GotoPos(curr->x, curr->y);
	      break;
	    }
	  break;
	case 'K':
	  if (a1 < 0 || a1 > 2)
	    a1 %= 3;
	  switch (a1)
	    {
	    case 0:
	      ClearToEOL();
	      break;
	    case 1:
	      ClearFromBOL();
	      break;
	    case 2:
	      ClearLine();
	      break;
	    }
	  break;
	case 'A':
	  CursorUp(a1 ? a1 : 1);
	  break;
	case 'B':
	  CursorDown(a1 ? a1 : 1);
	  break;
	case 'C':
	  CursorRight(a1 ? a1 : 1);
	  break;
	case 'D':
	  CursorLeft(a1 ? a1 : 1);
	  break;
	case 'm':
	  SelectRendition();
	  break;
	case 'g':
	  if (a1 == 0)
	    curr->tabs[curr->x] = 0;
	  else if (a1 == 3)
	    bzero(curr->tabs, cols);
	  break;
	case 'r':
	  if (!a1)
	    a1 = 1;
	  if (!a2)
	    a2 = rows;
	  if (a1 < 1 || a2 > rows || a1 >= a2)
	    break;
	  curr->top = a1 - 1;
	  curr->bot = a2 - 1;
	  ChangeScrollRegion(curr->top, curr->bot);
	  if (curr->origin)
	    {
	      GotoPos(0, curr->top);
	      curr->y = curr->top;
	      curr->x = 0;
	    }
	  else
	    {
	      GotoPos(0, 0);
	      curr->y = curr->x = 0;
	    }
	  break;
	case 's':
	  SaveCursor();
	  break;
	case 't':
	  if (a1 != 8)
	    break;
	  a1 = curr->args[2];
	  if (a1 < 1)
	    a1 = curr->width;
	  if (a2 < 1)
	    a2 = curr->height;
	  if (WS == NULL)
	    {
	      a2 = curr->height;
	      if (Z0 == NULL || (a1 != Z0width && a1 != Z1width))
	        a1 = curr->width;
 	    }
	  if (a1 == curr->width && a2 == curr->height)
	    break;
          ChangeWindowSize(curr, a1, a2);
	  SetCurr(curr);
	  if (display)
	    Activate(0);
	  break;
	case 'u':
	  RestoreCursor();
	  break;
	case 'I':
	  if (!a1)
	    a1 = 1;
	  while (a1--)
	    ForwardTab();
	  break;
	case 'Z':
	  if (!a1)
	    a1 = 1;
	  while (a1--)
	    BackwardTab();
	  break;
	case 'L':
	  InsertLine(a1 ? a1 : 1);
	  break;
	case 'M':
	  DeleteLine(a1 ? a1 : 1);
	  break;
	case 'P':
	  DeleteChar(a1 ? a1 : 1);
	  break;
	case '@':
	  InsertChar(a1 ? a1 : 1);
	  break;
	case 'h':
	  ASetMode(1);
	  break;
	case 'l':
	  ASetMode(0);
	  break;
	case 'i':
	  if (PO && a1 == 5)
	    {
	      curr->stringp = curr->string;
	      curr->state = PRIN;
	    }
	  break;
	case 'n':
	  if (a1 == 6)		/* Report cursor position */
	    Report(curr, "\033[%d;%dR", curr->y + 1, curr->x + 1);
	  break;
	case 'c':		/* Identify as VT100 */
	  Report(curr, "\033[?%d;%dc", 1, 2);
	  break;
	}
      break;
    case '?':
      debug2("\\E[?%d%c\n",a1,c);
      if (c != 'h' && c != 'l')
	break;
      i = (c == 'h');
      switch (a1)
	{
	case 3:
	  i = (i ? Z0width : Z1width);
	  if ((Z0 || WS) && curr->width != i)
	    {
              ChangeWindowSize(curr, i, curr->height);
	      SetCurr(curr);
	      if (display)
		Activate(0);
	    }
	  break;
	case 5:
	  if (i)
	    curr->vbwait = 1;
	  else
	    {
	      if (curr->vbwait)
		PutStr(VB);
	      curr->vbwait = 0;
	    }
	  break;
	case 6:
	  if ((curr->origin = i) != 0)
	    {
	      GotoPos(0, curr->top);
	      curr->y = curr->top;
	      curr->x = 0;
	    }
	  else
	    {
	      GotoPos(0, 0);
	      curr->y = curr->x = 0;
	    }
	  break;
	case 7:
	  curr->wrap = i;
	  break;
	case 35:
	  debug1("Cursor %svisible\n", i?"in":"");
	  curr->cursor_invisible = i;
	  break;
	}
      break;
    }
}

void
INSERTCHAR(c)
int c;
{
  if (!insert && (IC || CIC))
    {
      if (IC)
        PutStr(IC);
      else
        CPutStr(CIC, 1);
      RAW_PUTCHAR(c);
      return;
    }
  InsertMode(1);
  if (insert)
    RAW_PUTCHAR(c);
  else
    RefreshLine(screeny, screenx, screenwidth-1);
}

void
PUTCHAR(c)
int c;
{
  if (insert)
    InsertMode(0);
  RAW_PUTCHAR(c);
}

/*
 * RAW_PUTCHAR() is for all text that will be displayed.
 * NOTE, that charset Nr. 0 has a conversion table, but c1, c2, ... don't.
 */

static void
RAW_PUTCHAR(c)
int c;
{
  if (GlobalCharset == '0')
    putchar(c0_tab[c]);
  else
    putchar(c);
  if (screenx < screenwidth - 1)
    screenx++;
  else
    {
      screenx++;
      if ((AM && !LP) || screenx > screenwidth)
	{
	  screenx -= screenwidth;
	  if (screeny < screenheight-1 && screeny != screenbot)
	    screeny++;
	}
    }
}

void
PutChar(c)
int c;
{
  /* this PutChar for ESC-sequences only */
  putchar(c);
}

void
PutStr(s)
char *s;
{
  if (display && s)
    tputs(s, 1, PutChar);
}

static void CPutStr(s, c)
char *s;
int c;
{
  if (display && s)
    tputs(tgoto(s, 0, c), 1, PutChar);
}

static void SetChar(c)
register int c;
{
  register struct win *p = curr;

  p->image[p->y][p->x] = c;
  p->attr[p->y][p->x] = p->LocalAttr;
  p->font[p->y][p->x] = p->charsets[p->ss ? p->ss : p->LocalCharset];
}

static void StartString(type)
enum string_t type;
{
  curr->StringType = type;
  curr->stringp = curr->string;
  curr->state = ASTR;
}

static void AddChar(c)
int c;
{
  if (curr->stringp >= curr->string + MAXSTR - 1)
    curr->state = LIT;
  else
    *(curr->stringp)++ = c;
}

static void PrintChar(c)
int c;
{
  if (curr->stringp >= curr->string + MAXSTR - 1)
    PrintFlush();
  *(curr->stringp)++ = c;
}

static void PrintFlush()
{
  if (curr->stringp > curr->string)
    {
      tputs(PO, 1, PutChar);
      (void) fflush(stdout);
      (void) write(1, curr->string, curr->stringp - curr->string);
      tputs(PF, 1, PutChar);
      (void) fflush(stdout);
      curr->stringp = curr->string;
    }
}

/* Insert mode is a toggle on some terminals, so we need this hack:
 */
void
InsertMode(on)
int on;
{
  if (display && on != insert && IM)
    {
      insert = on;
      if (insert)
	PutStr(IM);
      else
	PutStr(EI);
    }
}

/* ...and maybe keypad application mode is a toggle, too:
 */
static void KeypadMode(on)
int on;
{
  if (display && keypad != on && KS)
    {
      keypad = on;
      if (keypad)
	PutStr(KS);
      else
	PutStr(KE);
    }
}

void
NewAutoFlow(win, on)
struct win *win;
int on;
{
  debug1("NewAutoFlow: %d\n", on);
  SetCurr(win);
  if (win->flow & FLOW_AUTOFLAG)
    win->flow = FLOW_AUTOFLAG | (FLOW_AUTO|FLOW_NOW) * on;
  else
    win->flow = (win->flow & ~FLOW_AUTO) | FLOW_AUTO * on;
  if (display)
    SetFlow(win->flow & FLOW_NOW);
}

static void DesignateCharset(c, n)
int c, n;
{
  curr->ss = 0;
  if (c == 'B')
    c = ASCII;
  if (curr->charsets[n] != c)
    {
      curr->charsets[n] = c;
      if (curr->LocalCharset == n)
	NewCharset(c);
    }
}

static void MapCharset(n)
int n;
{
  curr->ss = 0;
  if (curr->LocalCharset != n)
    {
      curr->LocalCharset = n;
      NewCharset(curr->charsets[n]);
    }
}

void
NewCharset(new)
int new;
{
  if (!display || GlobalCharset == new)
    return;
  GlobalCharset = new;
  if (new == ASCII)
    PutStr(E0);
  else
    CPutStr(S0, new);
}

static void SaveCursor()
{
  curr->saved = 1;
  curr->Saved_x = curr->x;
  curr->Saved_y = curr->y;
  curr->SavedLocalAttr = curr->LocalAttr;
  curr->SavedLocalCharset = curr->LocalCharset;
  bcopy((char *) curr->charsets, (char *) curr->SavedCharsets,
	4 * sizeof(int));
}

static void RestoreCursor()
{
  if (curr->saved)
    {
      GotoPos(curr->Saved_x, curr->Saved_y);
      curr->x = curr->Saved_x;
      curr->y = curr->Saved_y;
      curr->LocalAttr = curr->SavedLocalAttr;
      NewRendition(curr->LocalAttr);
      bcopy((char *) curr->SavedCharsets, (char *) curr->charsets,
	    4 * sizeof(int));
      curr->LocalCharset = curr->SavedLocalCharset;
      NewCharset(curr->charsets[curr->LocalCharset]);
    }
}

/*ARGSUSED*/
static void CountChars(c)
int c;
{
  StrCost++;
}

static int CalcCost(s)
register char *s;
{
  if (s)
    {
      StrCost = 0;
      tputs(s, 1, CountChars);
      return StrCost;
    }
  else
    return EXPENSIVE;
}

void
GotoPos(x2, y2)
int x2, y2;
{
  register int dy, dx, x1, y1;
  register int costx, costy;
  register int m;
  register char *s;
  int CMcost;
  enum move_t xm = M_NONE, ym = M_NONE;

  if (!display)
    return;

  x1 = screenx;
  y1 = screeny;

  if (x1 == screenwidth)
    if (LP && AM)
      x1 = -1;		/* don't know how the terminal treats this */
    else
      x1--;
  if (x2 == screenwidth)
    x2--;
  dx = x2 - x1;
  dy = y2 - y1;
  if (dy == 0 && dx == 0)
    {
      return;
    }
  if (!MS && GlobalAttr)	/* Safe to move in SO mode ? */
    NewRendition(0);
  if (y1 < 0			/* don't know the y position */
      || (y2 > screenbot && y1 <= screenbot)	/* have to cross border */
      || (y2 < screentop && y1 >= screentop))	/* of scrollregion ?    */
    {
    DoCM:
      if (HO && !x2 && !y2)
        PutStr(HO);
      else
        PutStr(tgoto(CM, x2, y2));
      screenx = x2;
      screeny = y2;
      return;
    }
  /* Calculate CMcost */
  if (HO && !x2 && !y2)
    s = HO;
  else
    s = tgoto(CM, x2, y2);
  CMcost = CalcCost(s);

  /* Calculate the cost to move the cursor to the right x position */
  costx = EXPENSIVE;
  if (x1 >= 0)	/* relativ x positioning only if we know where we are */
    {
      if (dx > 0)
	{
	  if (CRI && (dx > 1 || !ND))
	    {
	      costx = CalcCost(tgoto(CRI, 0, dx));
	      xm = M_CRI;
	    }
	  if ((m = NDcost * dx) < costx)
	    {
	      costx = m;
	      xm = M_RI;
	    }
	  /* Speedup: dx <= Rewrite() */
	  if (dx < costx && (m = Rewrite(y1, x1, x2, 0)) < costx)
	    {
	      costx = m;
	      xm = M_RW;
	    }
	}
      else if (dx < 0)
	{
	  if (CLE && (dx < -1 || !BC))
	    {
	      costx = CalcCost(tgoto(CLE, 0, -dx));
	      xm = M_CLE;
	    }
	  if ((m = -dx * LEcost) < costx)
	    {
	      costx = m;
	      xm = M_LE;
	    }
	}
      else
	costx = 0;
    }
  /* Speedup: Rewrite() >= x2 */
  if (x2 + CRcost < costx && (m = Rewrite(y1, 0, x2, 0) + CRcost) < costx)
    {
      costx = m;
      xm = M_CR;
    }

  /* Check if it is already cheaper to do CM */
  if (costx >= CMcost)
    goto DoCM;

  /* Calculate the cost to move the cursor to the right y position */
  costy = EXPENSIVE;
  if (dy > 0)
    {
      if (CDO && dy > 1)	/* DO & NL are always != 0 */
	{
	  costy = CalcCost(tgoto(CDO, 0, dy));
	  ym = M_CDO;
	}
      if ((m = dy * ((x2 == 0) ? NLcost : DOcost)) < costy)
	{
	  costy = m;
	  ym = M_DO;
	}
    }
  else if (dy < 0)
    {
      if (CUP && (dy < -1 || !UP))
	{
	  costy = CalcCost(tgoto(CUP, 0, -dy));
	  ym = M_CUP;
	}
      if ((m = -dy * UPcost) < costy)
	{
	  costy = m;
	  ym = M_UP;
	}
    }
  else
    costy = 0;

  /* Finally check if it is cheaper to do CM */
  if (costx + costy >= CMcost)
    goto DoCM;

  switch (xm)
    {
    case M_LE:
      while (dx++ < 0)
	PutStr(BC);
      break;
    case M_CLE:
      CPutStr(CLE, -dx);
      break;
    case M_RI:
      while (dx-- > 0)
	PutStr(ND);
      break;
    case M_CRI:
      CPutStr(CRI, dx);
      break;
    case M_CR:
      PutStr(CR);
      screenx = 0;
      x1 = 0;
      /* FALLTHROUGH */
    case M_RW:
      if (x1 < x2)
	(void) Rewrite(y1, x1, x2, 1);
      break;
    default:
      break;
    }
  switch (ym)
    {
    case M_UP:
      while (dy++ < 0)
	PutStr(UP);
      break;
    case M_CUP:
      CPutStr(CUP, -dy);
      break;
    case M_DO:
      s =  (x2 == 0) ? NL : DO;
      while (dy-- > 0)
	PutStr(s);
      break;
    case M_CDO:
      CPutStr(CDO, dy);
      break;
    default:
      break;
    }
  screenx = x2;
  screeny = y2;
}

static int
Rewrite(y, x1, x2, doit)
int y, x1, x2, doit;
{
  register int cost, dx;
  register char *p, *f, *i;

  if (x1 == x2)
    return(0);
  if (in_ovl)
    {
      if (ovl_Rewrite == 0)
        return EXPENSIVE;
      else
        return ((*ovl_Rewrite)(y, x1, x2, doit));
    }
  dx = x2 - x1;
  if (doit)
    {
      i = curr->image[y] + x1;
      while (dx-- > 0)
	PUTCHAR(*i++);
      return(0);
    }
  p = curr->attr[y] + x1;
  f = curr->font[y] + x1;

  cost = dx = x2 - x1;
  if (insert)
    cost += EIcost + IMcost;
  while(dx-- > 0)
    {
      if (*p++ != GlobalAttr || *f++ != GlobalCharset)
	return EXPENSIVE;
    }
  return cost;
}

static void BackSpace()
{
  if (curr->x > 0)
    {
      curr->x--;
    }
  else if (curr->wrap && curr->y > 0)
    {
      curr->x = cols - 1;
      curr->y--;
    }
  if (display)
    GotoPos(curr->x, curr->y);
}

static void Return()
{
  if (curr->x > 0)
    {
      curr->x = 0;
      if (display)
        GotoPos(curr->x, curr->y);
    }
}

static void LineFeed(out_mode)
int out_mode;
{
  /* out_mode: 0=no-output lf, 1=lf, 2=cr+lf */
  if (out_mode == 2)
    curr->x = 0;
  if (curr->y != curr->bot)		/* Don't scroll */
    {
      if (curr->y < rows-1)
	curr->y++;
      if (out_mode && display)
	GotoPos(curr->x, curr->y);
      return;
    }
  ScrollUpMap(1);
  if (curr->autoaka > 1)
    curr->autoaka--;
  if (out_mode && display)
    {
      ScrollRegion(curr->top, curr->bot, 1);
      GotoPos(curr->x, curr->y);
    }
}

static void ReverseLineFeed()
{
  if (curr->y == curr->top)
    {
      ScrollDownMap(1);
      if (!display)
	return;
      ScrollRegion(curr->top, curr->bot, -1);
      GotoPos(curr->x, curr->y);
    }
  else if (curr->y > 0)
    CursorUp(1);
}

static void InsertAChar(c)
int c;
{
  register int y = curr->y, x = curr->x;

  if (x == cols)
    x--;
  bcopy(curr->image[y], OldImage, cols);
  bcopy(curr->attr[y], OldAttr, cols);
  bcopy(curr->font[y], OldFont, cols);
  bcopy(curr->image[y] + x, curr->image[y] + x + 1, cols - x - 1);
  bcopy(curr->attr[y] + x, curr->attr[y] + x + 1, cols - x - 1);
  bcopy(curr->font[y] + x, curr->font[y] + x + 1, cols - x - 1);
  SetChar(c);
  if (!display)
    return;
  if (CIC || IC || IM)
    {
      InsertMode(curr->insert);
      INSERTCHAR(c);
      if (y == screenbot)
	lp_missing = 0;
    }
  else
    {
      RedisplayLine(OldImage, OldAttr, OldFont, y, x, cols - 1);
      GotoPos(++x, y);
    }
}

static void InsertChar(n)
int n;
{
  register int i, y = curr->y, x = curr->x;

  if (n <= 0)
    return;
  /*
   * The termcap manual states that only one of IM and IC is
   * to be defined unless the terminal needs both sequences.
   * We don't like this because we think that there may be cases
   * where it is preferable to send IC instead of IM/EI.
   * The hack is to ignore the IC sequence if we are already
   * in insert mode, so that programs which follow the termcap
   * guidelines still work. (I don't believe that there are
   * terminals which need IC in the insert mode. Why switch to
   * insert mode if you must send IC before every character ?)
   */
  if (curr->insert)
    return;
  if (x == cols)
    --x;
  bcopy(curr->image[y], OldImage, cols);
  bcopy(curr->attr[y], OldAttr, cols);
  bcopy(curr->font[y], OldFont, cols);
  if (n > cols - x)
    n = cols - x;
  bcopy(curr->image[y] + x, curr->image[y] + x + n, cols - x - n);
  bcopy(curr->attr[y] + x, curr->attr[y] + x + n, cols - x - n);
  bcopy(curr->font[y] + x, curr->font[y] + x + n, cols - x - n);
  ClearInLine(0, y, x, x + n - 1);
  if (!display)
    return;
  if (IC || CIC || IM)
    {
      if (y == screenbot)
	lp_missing = 0;
      if (!insert)
	{
	  if (n == 1 && IC)
	    {
	      PutStr(IC);
	      return;
            }
	  if (CIC)
	    {
	      CPutStr(CIC, n);
	      return;
            }
	}
      InsertMode(1);
      for (i = n; i--; )
	INSERTCHAR(' ');
      GotoPos(x, y);
    }
  else
    {
      RedisplayLine(OldImage, OldAttr, OldFont, y, x, cols - 1);
      GotoPos(x, y);
    }
}

static void DeleteChar(n)
int n;
{
  register int i, y = curr->y, x = curr->x;

  if (x == cols)
    --x;
  bcopy(curr->image[y], OldImage, cols);
  bcopy(curr->attr[y], OldAttr, cols);
  bcopy(curr->font[y], OldFont, cols);
  if (n > cols - x)
    n = cols - x;
  bcopy(curr->image[y] + x + n, curr->image[y] + x, cols - x - n);
  bcopy(curr->attr[y] + x + n, curr->attr[y] + x, cols - x - n);
  bcopy(curr->font[y] + x + n, curr->font[y] + x, cols - x - n);
  ClearInLine(0, y, cols - n, cols - 1);
  if (!display)
    return;
  if (CDC && !(n == 1 && DC))
    {
      CPutStr(CDC, n);
      if (lp_missing && y == screenbot)
	{
	  FixLP(cols - 1 - n, y);
          GotoPos(x, y);
	}
    }
  else if (DC)
    {
      for (i = n; i; i--)
	PutStr(DC);
      if (lp_missing && y == screenbot)
	{
	  FixLP(cols - 1 - n, y);
          GotoPos(x, y);
	}
    }
  else
    {
      RedisplayLine(OldImage, OldAttr, OldFont, y, x, cols - 1);
      GotoPos(x, y);
    }
}

static void DeleteLine(n)
int n;
{
  register int old = curr->top;
  
  if (curr->y < curr->top || curr->y > curr->bot)
    return;
  if (n > curr->bot - curr->y + 1)
    n = curr->bot - curr->y + 1;
  curr->top = curr->y;
  ScrollUpMap(n);
  curr->top = old;
  if (!display)
    return;
  ScrollRegion(curr->y, curr->bot, n);
  GotoPos(curr->x, curr->y);
}

static void InsertLine(n)
int n;
{
  register int old = curr->top;

  if (curr->y < curr->top || curr->y > curr->bot)
    return;
  if (n > curr->bot - curr->y + 1)
    n = curr->bot - curr->y + 1;
  curr->top = curr->y;
  ScrollDownMap(n);
  curr->top = old;
  if (!display)
    return;
  ScrollRegion(curr->y, curr->bot, -n);
  GotoPos(curr->x, curr->y);
}

void
ScrollRegion(ys, ye, n)
int ys, ye, n;
{
  int i;
  int up;
  int oldtop, oldbot;
  int alok, dlok, aldlfaster;
  int missy = 0;

  if (n == 0)
    return;
  if (ys == 0 && ye == screenheight-1 && 
      (n >= screenheight || -n >= screenheight))
    {
      PutStr(CL);
      screeny = screenx = 0;
      lp_missing = 0;
      return;
    }

  if (lp_missing)
    {
      if (screenbot>ye || screenbot<ys)
	missy = screenbot;
      else
	{
	  missy = screenbot - n;
          if (missy>ye || missy<ys)
	    lp_missing = 0;
	}
    }

  up = 1;
  if (n < 0)
    {
      up = 0;
      n = -n;
    }
  if (n >= ye-ys+1)
    n = ye-ys+1;

  oldtop = screentop;
  oldbot = screenbot;
  if (screenbot != ye)
    ChangeScrollRegion(ys, ye);
  alok = (AL || CAL || (ye == screenbot &&  up));
  dlok = (DL || CDL || (ye == screenbot && !up));
  if (screentop != ys && !(alok && dlok))
    ChangeScrollRegion(ys, ye);

  if (lp_missing && 
      (oldbot != screenbot ||
       (oldbot == screenbot && up && screentop == ys && screenbot == ye)))
    {
      /* Can't use FixLP */
      GotoPos(screenwidth-1, oldbot);
      SaveSetAttr(curr->attr[missy][screenwidth-1], curr->font[missy][screenwidth-1]);
      PUTCHAR(curr->image[missy][screenwidth-1]);
      RestoreAttr();
      lp_missing = 0;
      if (oldbot == screenbot)		/* have scrolled */
	{
	  if (--n == 0)
	    {
	      ChangeScrollRegion(oldtop, oldbot);
	      return;
	    }
	}
    }

  aldlfaster = (n > 1 && ye == screenbot && ((up && CDL) || (!up && CAL)));

  if ((up || SR) && screentop == ys && screenbot == ye && !aldlfaster)
    {
      if (up)
	{
	  GotoPos(0, ye);
	  while (n-- > 0)
	    PutStr(NL); /* was SF, I think NL is faster */
	}
      else
	{
	  GotoPos(0, ys);
	  while (n-- > 0)
	    PutStr(SR);
	}
    }
  else if (alok && dlok)
    {
      if (up || ye != screenbot)
	{
          GotoPos(0, up ? ys : ye+1-n);
          if (CDL && !(n == 1 && DL))
	    CPutStr(CDL, n);
	  else
	    for(i=n; i--; )
	      PutStr(DL);
	}
      if (!up || ye != screenbot)
	{
          GotoPos(0, up ? ye+1-n : ys);
          if (CAL && !(n == 1 && AL))
	    CPutStr(CAL, n);
	  else
	    for(i=n; i--; )
	      PutStr(AL);
	}
    }
  else
    {
      Redisplay(0);
      return;
    }
  if (lp_missing && missy != screenbot)
    FixLP(screenwidth-1, missy);
  ChangeScrollRegion(oldtop, oldbot);
  if (lp_missing && missy != screenbot)
    FixLP(screenwidth-1, missy);
}

static void ScrollUpMap(n)
int n;
{
  char tmp[256 * sizeof(char *)];
  register int ii, i, cnt1, cnt2;
  register char **ppi, **ppa, **ppf;

  i = curr->top + n;
  cnt1 = n * sizeof(char *);
  cnt2 = (curr->bot - i + 1) * sizeof(char *);
  ppi = curr->image + i;
  ppa = curr->attr + i;
  ppf = curr->font + i;
  for(ii = curr->top; ii < i; ii++)
     AddLineToHist(curr, &curr->image[ii], &curr->attr[ii], &curr->font[ii]);
  for (i = n; i; --i)
    {
      bclear(*--ppi, cols);
      bzero(*--ppa, cols);
      bzero(*--ppf, cols);
    }
  Scroll((char *) ppi, cnt1, cnt2, tmp);
  Scroll((char *) ppa, cnt1, cnt2, tmp);
  Scroll((char *) ppf, cnt1, cnt2, tmp);
}

static void ScrollDownMap(n)
int n;
{
  char tmp[256 * sizeof(char *)];
  register int i, cnt1, cnt2;
  register char **ppi, **ppa, **ppf;

  i = curr->top;
  cnt1 = (curr->bot - i - n + 1) * sizeof(char *);
  cnt2 = n * sizeof(char *);
  Scroll((char *) (ppi = curr->image + i), cnt1, cnt2, tmp);
  Scroll((char *) (ppa = curr->attr + i), cnt1, cnt2, tmp);
  Scroll((char *) (ppf = curr->font + i), cnt1, cnt2, tmp);
  for (i = n; i; --i)
    {
      bclear(*ppi++, cols);
      bzero(*ppa++, cols);
      bzero(*ppf++, cols);
    }
}

static void Scroll(cp, cnt1, cnt2, tmp)
char *cp, *tmp;
int cnt1, cnt2;
{
  if (!cnt1 || !cnt2)
    return;
  if (cnt1 <= cnt2)
    {
      bcopy(cp, tmp, cnt1);
      bcopy(cp + cnt1, cp, cnt2);
      bcopy(tmp, cp + cnt2, cnt1);
    }
  else
    {
      bcopy(cp + cnt1, tmp, cnt2);
      bcopy(cp, cp + cnt2, cnt1);
      bcopy(tmp, cp, cnt2);
    }
}

static void ForwardTab()
{
  register int x = curr->x;

  if (x == cols)
    {
      LineFeed(2);
      x = 0;
    }
  if (curr->tabs[x] && x < cols - 1)
    x++;
  while (x < cols - 1 && !curr->tabs[x])
    x++;
  GotoPos(x, curr->y);
  curr->x = x;
}

static void BackwardTab()
{
  register int x = curr->x;

  if (curr->tabs[x] && x > 0)
    x--;
  while (x > 0 && !curr->tabs[x])
    x--;
  GotoPos(x, curr->y);
  curr->x = x;
}

static void ClearScreen()
{
  register int i;
  register char **ppi = curr->image, **ppa = curr->attr, **ppf = curr->font;

  for (i = 0; i < rows; ++i)
    {
      AddLineToHist(curr, ppi, ppa, ppf);
      bclear(*ppi++, cols);
      bzero(*ppa++, cols);
      bzero(*ppf++, cols);
    }
  if (display)
    {
      PutStr(CL);
      screenx = screeny = 0;
      lp_missing = 0;
    }
}

static void ClearFromBOS()
{
  register int n, y = curr->y, x = curr->x;

  for (n = 0; n < y; ++n)
    ClearInLine(1, n, 0, cols - 1);
  ClearInLine(1, y, 0, x);
  GotoPos(x, y);
  RestoreAttr();
}

static void ClearToEOS()
{
  register int n, y = curr->y, x = curr->x;

  if (!y && !x)
    {
      ClearScreen();
      return;
    }
  if (display && CD)
    {
      PutStr(CD);
      lp_missing = 0;
    }
  ClearInLine(!CD, y, x, cols - 1);
  for (n = y + 1; n < rows; n++)
    ClearInLine(!CD, n, 0, cols - 1);
  GotoPos(x, y);
  RestoreAttr();
}

static void ClearLine()
{
  register int y = curr->y, x = curr->x;

  ClearInLine(1, y, 0, cols - 1);
  GotoPos(x, y);
  RestoreAttr();
}

static void ClearToEOL()
{
  register int y = curr->y, x = curr->x;

  ClearInLine(1, y, x, cols - 1);
  GotoPos(x, y);
  RestoreAttr();
}

static void ClearFromBOL()
{
  register int y = curr->y, x = curr->x;

  ClearInLine(1, y, 0, x);
  GotoPos(x, y);
  RestoreAttr();
}

static void ClearInLine(displ, y, x1, x2)
int displ, y, x1, x2;
{
  register int n;

  if (x1 == cols)
    x1--;
  if (x2 == cols)
    x2--;
  if ((n = x2 - x1 + 1) != 0)
    {
      if (displ && display)
	{
	  if (x2 == cols - 1 && CE)
	    {
	      GotoPos(x1, y);
	      PutStr(CE);
	      if (y == screenbot)
		lp_missing = 0;
	    }
	  else
	    DisplayLine(curr->image[y], curr->attr[y], curr->font[y],
			blank, null, null, y, x1, x2);
	}
      if (curr)
	{
          bclear(curr->image[y] + x1, n);
          bzero(curr->attr[y] + x1, n);
          bzero(curr->font[y] + x1, n);
	}
    }
}

static void CursorRight(n)
register int n;
{
  register int x = curr->x;

  if (x == cols)
    {
      LineFeed(2);
      x = 0;
    }
  if ((curr->x += n) >= cols)
    curr->x = cols - 1;
  GotoPos(curr->x, curr->y);
}

static void CursorUp(n)
register int n;
{
  if (curr->y < curr->top)		/* if above scrolling rgn, */
    {
      if ((curr->y -= n) < 0)		/* ignore its limits      */
         curr->y = 0;
    }
  else
    if ((curr->y -= n) < curr->top)
      curr->y = curr->top;
  GotoPos(curr->x, curr->y);
}

static void CursorDown(n)
register int n;
{
  if (curr->y > curr->bot)		/* if below scrolling rgn, */
    {
      if ((curr->y += n) > rows - 1)	/* ignore its limits      */
        curr->y = rows - 1;
    }
  else
    if ((curr->y += n) > curr->bot)
      curr->y = curr->bot;
  GotoPos(curr->x, curr->y);
}

static void CursorLeft(n)
register int n;
{
  if ((curr->x -= n) < 0)
    curr->x = 0;
  GotoPos(curr->x, curr->y);
}

static void ASetMode(on)
int on;
{
  register int i;

  for (i = 0; i < curr->NumArgs; ++i)
    {
      switch (curr->args[i])
	{
	case 4:
	  curr->insert = on;
	  InsertMode(on);
	  break;
	}
    }
}

static void SelectRendition()
{
  register int i = 0, a = curr->LocalAttr;

  do
    {
      switch (curr->args[i])
	{
	case 0:
	  a = 0;
	  break;
	case 1:
	  a |= A_BD;
	  break;
	case 2:
	  a |= A_DI;
	  break;
	case 3:
	  a |= A_SO;
	  break;
	case 4:
	  a |= A_US;
	  break;
	case 5:
	  a |= A_BL;
	  break;
	case 7:
	  a |= A_RV;
	  break;
	case 22:
	  a &= ~(A_BD | A_SO | A_DI);
	  break;
	case 23:
	  a &= ~A_SO;
	  break;
	case 24:
	  a &= ~A_US;
	  break;
	case 25:
	  a &= ~A_BL;
	  break;
	case 27:
	  a &= ~A_RV;
	  break;
	}
    } while (++i < curr->NumArgs);
  NewRendition(curr->LocalAttr = a);
}

void
NewRendition(new)
register int new;
{
  register int i, old = GlobalAttr;

  if (!display || old == new)
    return;
  GlobalAttr = new;
  for (i = 1; i <= A_MAX; i <<= 1)
    {
      if ((old & i) && !(new & i))
	{
	  PutStr(UE);
	  PutStr(SE);
	  PutStr(ME);
	  if (new & A_DI)
	    PutStr(attrtab[ATTR_DI]);
	  if (new & A_US)
	    PutStr(attrtab[ATTR_US]);
	  if (new & A_BD)
	    PutStr(attrtab[ATTR_BD]);
	  if (new & A_RV)
	    PutStr(attrtab[ATTR_RV]);
	  if (new & A_SO)
	    PutStr(attrtab[ATTR_SO]);
	  if (new & A_BL)
	    PutStr(attrtab[ATTR_BL]);
	  return;
	}
    }
  if ((new & A_DI) && !(old & A_DI))
    PutStr(attrtab[ATTR_DI]);
  if ((new & A_US) && !(old & A_US))
    PutStr(attrtab[ATTR_US]);
  if ((new & A_BD) && !(old & A_BD))
    PutStr(attrtab[ATTR_BD]);
  if ((new & A_RV) && !(old & A_RV))
    PutStr(attrtab[ATTR_RV]);
  if ((new & A_SO) && !(old & A_SO))
    PutStr(attrtab[ATTR_SO]);
  if ((new & A_BL) && !(old & A_BL))
    PutStr(attrtab[ATTR_BL]);
}

void
SaveSetAttr(newattr, newcharset)
int newattr, newcharset;
{
  NewRendition(newattr);
  NewCharset(newcharset);
}

void
RestoreAttr()
{
  NewRendition(curr->LocalAttr);
  NewCharset(curr->charsets[curr->LocalCharset]);
}

static void FillWithEs()
{
  register int i;
  register char *p, *ep;

  curr->y = curr->x = 0;
  for (i = 0; i < rows; ++i)
    {
      bzero(curr->attr[i], cols);
      bzero(curr->font[i], cols);
      p = curr->image[i];
      ep = p + cols;
      while (p < ep)
	*p++ = 'E';
    }
  if (display)
    Redisplay(0);
}

/*
 * if cur_only, we only redisplay current line, as a full refresh is
 * too expensive.
 */
void Redisplay(cur_only)
int cur_only;
{
  register int i, stop;

  PutStr(CL);
  screenx = screeny = 0;
  lp_missing = 0;
  stop = rows; i = 0;
  if (cur_only)
    {
      i = stop = curr->y;
      stop++;
    }
  for (; i < stop; ++i)
    {
      if (in_ovl)
	(*ovl_RedisplayLine)(i, 0, cols - 1, 1);
      else
        DisplayLine(blank, null, null, curr->image[i], curr->attr[i],
		    curr->font[i], i, 0, cols - 1);
    }
  if (!in_ovl)
    {
      GotoPos(curr->x, curr->y);
      NewRendition(curr->LocalAttr);
      NewCharset(curr->charsets[curr->LocalCharset]);
    }
}

void
DisplayLine(os, oa, of, s, as, fs, y, from, to)
int from, to, y;
register char *os, *oa, *of, *s, *as, *fs;
{
  register int x;
  int last2flag = 0, delete_lp = 0;

  if (!LP && y == screenbot && to == cols - 1)
    if (lp_missing
	|| s[to] != os[to] || as[to] != oa[to] || of[to] != fs[to])
      {
	if ((IC || IM) && (from < to || !in_ovl))
	  {
	    if ((to -= 2) < from - 1)
	      from--;
	    last2flag = 1;
	    lp_missing = 0;
	  }
	else
	  {
	    to--;
	    delete_lp = (CE || DC || CDC);
	    lp_missing = (s[to] != ' ' || as[to] || fs[to]);
	  }
      }
    else
      to--;
  for (x = from; x <= to; ++x)
    {
      if (s[x] == os[x] && as[x] == oa[x] && of[x] == fs[x])
	continue;
      GotoPos(x, y);
      NewRendition(as[x]);
      NewCharset(fs[x]);
      PUTCHAR(s[x]);
    }
  if (last2flag)
    {
      GotoPos(x, y);
      NewRendition(as[x + 1]);
      NewCharset(fs[x + 1]);
      PUTCHAR(s[x + 1]);
      GotoPos(x, y);
      NewRendition(as[x]);
      NewCharset(fs[x]);
      INSERTCHAR(s[x]);
    }
  else if (delete_lp)
    {
      if (DC)
	PutStr(DC);
      else if (CDC)
	CPutStr(CDC, 1);
      else if (CE)
	PutStr(CE);
    }
}

void
RefreshLine(y, from, to)
int y, from, to;
{
  char *oi = null;

  if (CE && to == screenwidth-1)
    {
      GotoPos(from, y);
      PutStr(CE);
      oi = blank;
    }
  if (in_ovl)
    (*ovl_RedisplayLine)(y, from, to, (oi == blank));
  else
    DisplayLine(oi, null, null, curr->image[y], curr->attr[y],
                curr->font[y], y, from, to);
}

static void RedisplayLine(os, oa, of, y, from, to)
int from, to, y;
char *os, *oa, *of;
{
  DisplayLine(os, oa, of, curr->image[y], curr->attr[y],
	      curr->font[y], y, from, to);
  NewRendition(curr->LocalAttr);
  NewCharset(curr->charsets[curr->LocalCharset]);
}

void
FixLP(x2, y2)
register int x2, y2;
{
  register struct win *p = curr;

  GotoPos(x2, y2);
  SaveSetAttr(p->attr[y2][x2], p->font[y2][x2]);
  PUTCHAR(p->image[y2][x2]);
  RestoreAttr();
  lp_missing = 0;
}

void
CheckLP(n_ch)
char n_ch;
{
  register int y = screenbot, x = cols - 1;
  register char n_at, n_fo, o_ch, o_at, o_fo;

  o_ch = curr->image[y][x];
  o_at = curr->attr[y][x];
  o_fo = curr->font[y][x];

  n_at = curr->LocalAttr;
  n_fo = curr->charsets[curr->LocalCharset];

  lp_missing = 0;
  if (n_ch == o_ch && n_at == o_at && n_fo == o_fo)
    {
      return;
    }
  if (n_ch != ' ' || n_at || n_fo)
    lp_missing = 1;
  if (o_ch != ' ' || o_at || o_fo)
    {
      if (DC)
	PutStr(DC);
      else if (CDC)
	CPutStr(CDC, 1);
      else if (CE)
	PutStr(CE);
      else
	lp_missing = 1;
    }
}

static void FindAKA()
{
  register char *cp, *line, ch;
  register struct win *wp = curr;
  register int len = strlen(wp->cmd);
  int y;

  y = (wp->autoaka > 0 && wp->autoaka <= wp->height) ? wp->autoaka - 1 : wp->y;
  cols = wp->width;
 try_line:
  cp = line = wp->image[y];
  if (wp->autoaka > 0 && (ch = *wp->cmd) != '\0')
    {
      for (;;)
	{
	  if ((cp = index(cp, ch)) != NULL
	      && !strncmp(cp, wp->cmd, len))
	    break;
	  if (!cp || ++cp - line >= cols - len)
	    {
	      if (++y == wp->autoaka && y < rows)
		goto try_line;
	      return;
	    }
	}
      cp += len;
    }
  for (len = cols - (cp - line); len && *cp == ' '; len--, cp++)
    ;
  if (len)
    {
      if (wp->autoaka > 0 && (*cp == '!' || *cp == '%' || *cp == '^'))
	wp->autoaka = -1;
      else
	wp->autoaka = 0;
      line = wp->cmd + wp->akapos;
      while (len && *cp != ' ')
	{
	  if ((*line++ = *cp++) == '/')
	    line = wp->cmd + wp->akapos;
	  len--;
	}
      *line = '\0';
    }
  else
    wp->autoaka = 0;
}


/* We dont use HS status line with Input.
 * If we would use it, then we should check e_tgetflag("es") if
 * we are allowed to use esc sequences there.
 * For now, we hope that Goto(,,STATLINE,0) brings us in the bottom
 * line. jw.
 */

static char inpbuf[101];
static int inplen;
static int inpmaxlen;
static char *inpstring;
static int inpstringlen;
static void (*inpfinfunc)();

void
Input(istr, len, finfunc)
char *istr;
int len;
void (*finfunc)();
{
  int maxlen;

  inpstring = istr;
  inpstringlen = strlen(istr);
  if (len > 100)
    len = 100;
  maxlen = screenwidth - inpstringlen;
  if (!LP && STATLINE == screenbot)
    maxlen--;
  if (len > maxlen)
    len = maxlen;
  if (len < 2)
    {
      Msg(0, "Width too small");
      return;
    }
  inpmaxlen = len;
  inpfinfunc = finfunc;
  InitOverlayPage(process_inp_input, inpRedisplayLine, (int (*)())0, 1);
  inplen = 0;
  GotoPos(0, STATLINE);
  if (CE)
    PutStr(CE);
  else
    {
      DisplayLine(curr->image[screeny], curr->attr[screeny],
		  curr->font[screeny],
		  blank, null, null, screeny, 0, cols - 1);
    }
  inpRedisplayLine(STATLINE, 0, inpstringlen - 1, 0);
  GotoPos(inpstringlen, STATLINE);
}

static void
process_inp_input(ppbuf, plen)
char **ppbuf;
int *plen;
{
  int len, x;
  char *pbuf;
  char ch;

  if (ppbuf == 0)
    {
      AbortInp();
      return;
    }
  x = inpstringlen+inplen;
  len = *plen;
  pbuf = *ppbuf;
  while (len)
    {
      ch = *pbuf++;
      len--;
      if (ch >= ' ' && ch <= '~' && inplen < inpmaxlen)
	{
	  inpbuf[inplen++] = ch;
  	  GotoPos(x, STATLINE);
	  SaveSetAttr(A_SO, ASCII);
	  PUTCHAR(ch);
	  x++;
	}
      else if ((ch == '\b' || ch == 0177) && inplen > 0)
	{
	  inplen--;
	  x--;
  	  GotoPos(x, STATLINE);
	  SaveSetAttr(0, ASCII);
	  PUTCHAR(' ');
  	  GotoPos(x, STATLINE);
	}
      else if (ch == '\004' || ch == '\003' || ch == '\000' || ch == '\n' || ch == '\r')
	{
          if (ch != '\n' && ch != '\r')
	    inplen = 0;
	  inpbuf[inplen] = 0;
          AbortInp(); /* redisplays... */
          (*inpfinfunc)(inpbuf, inplen);
	  break;
	}
    }
  *ppbuf = pbuf;
  *plen = len;
}

static void
AbortInp()
{
  in_ovl = 0;	/* So we can use RefreshLine() */
  RefreshLine(STATLINE, 0, screenwidth-1);
  ExitOverlayPage();
}

static void
inpRedisplayLine(y, xs, xe, isblank)
int y, xs, xe, isblank;
{
  int q, r, s, l, v;

  if (y != STATLINE)
    return;
  inpbuf[inplen] = 0;
  GotoPos(xs,y);
  q = xs;
  v = xe - xs + 1;
  s = 0;
  r = inpstringlen;
  if (v > 0 && q < r)
    {
      SaveSetAttr(A_SO, ASCII);
      l = v;
      if (l > r-q)
	l = r-q;
      printf("%-*.*s", l, l, inpstring + q - s);
      q += l;
      v -= l;
    }
  s = r;
  r += inplen;
  if (v > 0 && q < r)
    {
      SaveSetAttr(A_SO, ASCII);
      l = v;
      if (l > r-q)
	l = r-q;
      printf("%-*.*s", l, l, inpbuf + q - s);
      q += l;
      v -= l;
    }
  s = r;
  r = screenwidth;
  if (!isblank && v > 0 && q < r)
    {
      SaveSetAttr(0, ASCII);
      l = v;
      if (l > r-q)
	l = r-q;
      printf("%-*.*s", l, l, "");
      q += l;
    }
  SetLastPos(q, y);
}

static void
AKAfin(buf, len)
char *buf;
int len;
{
  if (len)
    {
      strcpy(curr->cmd + curr->akapos, buf);
    }
}

void
InputAKA()
{
  void Input(), AKAfin();

  Input("Set window's a.k.a. to: ", 20, AKAfin);
}

static void
Colonfin(buf, len)
char *buf;
int len;
{
  if (len)
    RcLine(buf);
}

void
InputColon()
{
  void Input(), Colonfin();

  Input(":", 100, Colonfin);
}

void
MakeBlankLine(p, n)
register char *p;
register int n;
{
  while (n--)
    *p++ = ' ';
}

void
MakeStatus(msg)
char *msg;
{
  register char *s, *t;
  register int max, ti;

  SetCurr(fore);
  display = 1;
  if (!(max = HS))
    {
      max = !LP ? cols - 1 : cols;
    }
  if (status)
    {
      if (!BellDisplayed)
	{
	  ti = time((time_t *) 0) - TimeDisplayed;
	  if (ti < MsgMinWait)
	    sleep(MsgMinWait - ti);
	}
      RemoveStatus();
    }
  for (s = t = msg; *s && t - msg < max; ++s)
    if (*s == BELL)
      PutStr(BL);
    else if (*s >= ' ' && *s <= '~')
      *t++ = *s;
  *t = '\0';
  if (t > msg)
    {
      strncpy(LastMsg, msg, maxwidth);
      status = 1;
      status_lastx = screenx;
      status_lasty = screeny;
      StatLen = t - msg;
      if (!HS)
	{
	  GotoPos(0, STATLINE);
          SaveSetAttr(A_SO, ASCII);
	  InsertMode(0);
	  printf("%s", msg);
          screenx = -1;
	}
      else
	{
	  debug("HS:");
          SaveSetAttr(0, ASCII);
	  InsertMode(0);
	  CPutStr(TS, 0);
	  printf("%s", msg);
	  PutStr(FS);
	}
      (void) fflush(stdout);
      (void) time(&TimeDisplayed);
    }
}

void
RemoveStatus()
{
  if (!status)
    return;
  status = 0;
  BellDisplayed = 0;
  SetCurr(fore);
  display = 1;
  if (!HS)
    {
      GotoPos(0, STATLINE);
      if (in_ovl)
	(*ovl_RedisplayLine)(STATLINE, 0, StatLen - 1, 0);
      else
	RedisplayLine(null, null, null, STATLINE, 0, StatLen - 1);
      GotoPos(status_lastx, status_lasty);
    }
  else
    {
      SaveSetAttr(0, ASCII);
      PutStr(DS);
    }
}

void
ClearDisplay()
{
  PutStr(CL);
  screeny = screenx = 0;
  fflush(stdout);
}

static void SetCurr(wp)
struct win *wp;
{
  curr = wp;
  cols = curr->width;
  rows = curr->height;
  display = curr->active;
}

void
InitOverlayPage(pro, red, rewrite, blockfore)
void (*pro)();
void (*red)();
int (*rewrite)();
int blockfore;
{
  RemoveStatus();
  SetOvlCurr();
  ChangeScrollRegion(0, screenheight - 1);
  SetFlow(1);
  ovl_process = pro;
  ovl_RedisplayLine = red;
  ovl_Rewrite = rewrite;
  ovl_blockfore = blockfore;
  curr->active = 0;
  in_ovl = 1;
}

void
ExitOverlayPage()
{
  ChangeScrollRegion(curr->top, curr->bot);
  GotoPos(curr->x, curr->y);
  RestoreAttr();
  SetFlow(curr->flow & FLOW_NOW);
  curr->active = 1;
  in_ovl = 0;
}

void
SetOvlCurr()
{
  SetCurr(fore);
  SaveSetAttr(0, ASCII);
  InsertMode(0);
  display = 1;
}

void
SetLastPos(x,y)
int x,y;
{
  screenx = x;
  screeny = y;
}

void
WSresize(width, height)
int width, height;
{
  debug2("(display=%d:WSresize says:'%s'\n", display, tgoto(WS, width, height));
  PutStr(tgoto(WS, width, height));
}

void
ChangeScrollRegion(top, bot)
int top, bot;
{
  if (display == 0)
    return;
  if (CS == 0)
    {
      screentop = 0;
      screenbot = screenheight - 1;
      return;
    }
  if (top == screentop && bot == screenbot)
    return;
  debug2("ChangeScrollRegion: (%d - %d)\n", top, bot);
  PutStr(tgoto(CS, bot, top));
  screentop = top;
  screenbot = bot;
  screeny = screenx = -1;		/* Just in case... */
}


void AddLineToHist(wp, pi, pa, pf)
struct win *wp;
char **pi, **pa, **pf;
{
  register char *q;

  if (wp->histheight == 0)
    return;
  q = *pi; *pi = wp->ihist[wp->histidx]; wp->ihist[wp->histidx] = q;
  q = *pa; *pa = wp->ahist[wp->histidx]; wp->ahist[wp->histidx] = q;
  q = *pf; *pf = wp->fhist[wp->histidx]; wp->fhist[wp->histidx] = q;
  if (++wp->histidx >= wp->histheight)
    wp->histidx = 0;
}


/*
 *
 *  Termcap routines that use our extra_incap
 *
 */

/* findcap:
 *   cap = capability we are looking for
 *   tepp = pointer to bufferpointer
 *   n = size of buffer (0 = infinity)
 */

char *
findcap(cap, tepp, n)
char *cap;
char **tepp;
int n;
{
  char *tep;
  char c, *p, *cp;
  int mode;	/* mode: 0=LIT  1=^  2=\x  3,4,5=\nnn */
  int num = 0, capl;

  if (!extra_incap)
    return (0);
  tep = *tepp;
  capl = strlen(cap);
  cp = 0;
  mode = 0;
  for (p = extra_incap; *p; )
    {
      if (strncmp(p, cap, capl) == 0)
	{
	  p+=capl;
	  c = *p;
	  if (c && c != ':' && c != '@')
	    p++;
	  if (c == 0 || c == '@' || c == '=' || c == ':' || c == '#')
	    cp = tep;
	}
      while (c = *p)
	{
	  p++;
	  if (mode == 0)
	    {
	      if (c == ':')
	        break;
	      if (c == '^')
		mode = 1;
	      if (c == '\\')
		mode = 2;
	    }
	  else if (mode == 1)
	    {
	      c = c & 0x1f;
	      mode = 0;
	    }
	  else if (mode == 2)
	    {
	      switch(c)
		{
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
		  mode = 3;
		  num = 0;
		  break;
		case 'E':
		  c = 27;
		  break;
		case 'n':
		  c = '\n';
		  break;
		case 'r':
		  c = '\r';
		  break;
		case 't':
		  c = '\t';
		  break;
		case 'b':
		  c = '\b';
		  break;
		case 'f':
		  c = '\f';
		  break;
		}
	      if (mode == 2)
		mode = 0;
	    }
	  if (mode > 2)
	    {
	      num = num * 8 + (c - '0');
	      if (mode++ == 5 || (*p < '0' || *p > '9'))
		{
		  c = num;
		  mode = 0;
		}
	    }
	  if (mode)
	    continue;

	  if (cp && n != 1)
	    {
	      *cp++ = c;
	      n--;
	    }
	}
      if (cp)
	{
	  *cp++ = 0;
	  *tepp = cp;
	  debug2("'%s' found in extra_incap -> %s\n", cap, tep);
	  return(tep);
	}
    }
  return(0);
}

static char *
e_tgetstr(cap, tepp)
char *cap;
char **tepp;
{
  char *tep;
  if (tep = findcap(cap, tepp, 0))
    return((*tep == '@') ? 0 : tep);
  return (tgetstr(cap, tepp));
}

static int
e_tgetflag(cap)
char *cap;
{
  char buf[2], *bufp;
  char *tep;
  bufp = buf;
  if (tep = findcap(cap, &bufp, 2))
    return((*tep == '@') ? 0 : 1);
  return (tgetflag(cap));
}

static int
e_tgetnum(cap)
char *cap;
{
  char buf[20], *bufp;
  char *tep, c;
  int res, base = 10;

  bufp = buf;
  if (tep = findcap(cap, &bufp, 20))
    {
      c = *tep;
      if (c == '@')
	return(-1);
      if (c == '0')
	base = 8;
      res = 0;
      while ((c = *tep++) >= '0' && c <= '9')
	res = res * base + (c - '0');
      return(res);
    }
  return (tgetnum(cap));
}
