/*
 *      BIND.C
 *      UTREE key binding routines.
 *      3.03-um klin, Sun Feb 23 18:45:19 1992
 *              klin, Fri Mar  6 07:24:23 1992, Minor changes
 *            a klin, Sun Mar 15 19:08:25 1992, Bug fixes in bindings()
 *                                              and insertbinding()
 *            b klin, Sun Mar 22 10:41:52 1992, Minor fixes
 *
 *      Copyright (c) 1991/92 by Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
#ifndef lint
static char sccsid[] = "@(#) utree 3.03b-um (klin) Mar 22 1992 bind.c";
#endif

#include "defs.h"

/* ---- Local variables and definitions ------------------------------- */

/*
 *      All default keys and some function keys are defined in static
 *      arrays of type struct _ktable. The default keys are hold in the
 *      array defkeys[] and bound at startup to the defined utree keys.
 *      The function keys whose termcap names are stored in the array
 *      are extracted from terminfo/termcap and bound to the default
 *      utree keys if they are defined in the database.
 */

LOCAL struct {                 /* Default key bindings                 */
  char *string;                 /*   Key string                         */
  ksym symbol;                  /*   Key symbol known by utree          */
} defkeys[] = {
  { "\200", K_MARK },           /*   C-@: Set mark                      */
  { "\001", K_HOME },           /*   C-a: Beginning                     */
  { "\002", K_BACK },           /*   C-b: Backward                      */
  { "\003", K_BRK  },           /*   C-c: Break                         */
  { "\004", K_DOWN },           /*   C-d: Scroll down/delete char       */
  { "\005", K_END  },           /*   C-e: End                           */
  { "\006", K_FORW },           /*   C-f: Forward                       */
  { "\007", K_GOTO },           /*   C-g: Goto mark                     */
  { "\010", K_DEL  },           /*   C-h: Goto parent/delete char back  */
  { "\011", K_TAG  },           /*   C-i: Goto tag/transpose chars      */
  { "\012", K_SEL  },           /*   C-j: Select/accept line            */
  { "\013", K_KILL },           /*   C-k: Kill line                     */
  { "\014", K_REFR },           /*   C-l: Refresh screen/line           */
  { "\015", K_SEL  },           /*   C-m: Select/accept line            */
  { "\016", K_NEXT },           /*   C-n: Next                          */
  { "\017", K_INS  },           /*   C-o: Change to/toggle insert mode  */
  { "\020", K_PREV },           /*   C-p: Previous                      */
  { "\022", K_HELP },           /*   C-r: Help                          */
  { "\024", K_TAG  },           /*   C-t: Goto tag/transpose characters */
  { "\025", K_UP   },           /*   C-u: Scroll up                     */
  { "\026", K_NPAG },           /*   C-v: Next page/scroll forw line    */
  { "\027", K_PPAG },           /*   C-w: Prev page/scroll back line    */
  { "\030", K_CANC },           /*   C-x: Delete input line             */
  { "\031", K_BRK  },           /*   C-y: Break                         */
  { "\032", K_EOF  },           /*   C-z: Exit                          */
  { "\177", K_DEL  },           /*   C-?: Delete                        */
  { NULL,   K_INV  },           /* End of default key table             */
};

#define DTSTR(n)        (defkeys[n].string)
#define DTSYM(n)        (defkeys[n].symbol)

LOCAL struct {                 /* Function key table                   */
  char *string;                 /*   Termcap name                       */
  ksym symbol;                  /*   Key symbol known by utree          */
  char *name;                   /*   Key name                           */
} funkeys[] = {
  { "ku", K_PREV, "CursorUp-Key"        },
  { "kd", K_NEXT, "CursorDown-Key"      },
  { "kl", K_BACK, "CursorBackward-Key"  },
  { "kr", K_FORW, "CursorForward-Key"   },
  { "kh", K_HOME, "Home-Key"            },
#ifdef  XENIX   /* RG 11/22/91 */
  { "HM", K_HOME, "Begin-Key"           },
  { "EN", K_END,  "End-Key"             },
  { "PD", K_NPAG, "NextPage-Key"        },
  { "PU", K_PPAG, "PrevPage-Key"        },
#else   /* !XENIX */
  { "@1", K_HOME, "Begin-Key"           },
  { "@7", K_END,  "End-Key"             },
  { "kN", K_NPAG, "NextPage-Key"        },
  { "kP", K_PPAG, "PrevPage-Key"        },
#endif  /* XENIX */
  { "kR", K_UP,   "ScrollUp-Key"        },
  { "kF", K_DOWN, "ScrollDown-Key"      },
  { "kI", K_INS,  "Insert-Key"          },
  { "kD", K_DOWN, "Delete-Key"          },
  { "kC", K_REFR, "Clear-Key"           },
  { "%1", K_HELP, "Help-Key"            },
  { "*6", K_SEL,  "Select-Key"          },
  { "@4", K_SEL,  "Do/Command-Key"      },
  { "%2", K_MARK, "Mark-Key"            },
  { "@8", K_SEL,  "Enter-Key"           },
  { NULL, K_INV,  NULL   },     /*   End function key table             */
};

#define FTSTR(n)        (funkeys[n].string)
#define FTSYM(n)        (funkeys[n].symbol)
#define FTNAM(n)        (funkeys[n].name)

#define KEYLEN 32               /* Max length of key strings            */
#define KCOL   12
#define EOL(c)          (c==' ' || c=='\t' || c=='#' || c=='\n' || c=='\0')

LOCAL int klchg = 0;            /* Changes in key bindings list         */
LOCAL char *unknown = "UNKNOWN";

/* ---- External variables and functions ------------------------------ */

EXTRN char *tgetstr();
EXTRN char *getversion();
EXTRN char *strclean();

/* ---- Functions and procedures -------------------------------------- */

/*
 *      INTERNAL USED ROUTINES
 */

/* Insert keybinding from string str to symbol sym into binding list */
LOCAL int insertbinding(str, sym, nam, ins, usr)
  register kchar *str;
  register ksym sym;
  register char *nam, *ins;
  register int usr;
{
  register klist *kp, *pp, *p;

  if(str == UNULL)
    return(0);

  /* Search for string in key binding list */
  for(kp = kroot; kp; kp = (klist *) KBNXT(kp))
    if(EQU(str, KBSTR(kp)))
      break;

  /* Replace an existing key binding */
  if(kp)  {
    if(KBSYM(kp) == K_STR && KBINS(kp))
      ufree(KBINS(kp));
    if(sym == K_STR && ins)
      KBINS(kp) = strsav(ins);
    else
      KBINS(kp) = NULL;
    KBSYM(kp) = sym;
    KBUSR(kp) = usr;
    return(1);
  }

  /* Create a new binding */
  kp = (klist *) ualloc(1, sizeof(klist));
  KBSTR(kp) = (kchar *) strsav((char *) str);
  KBINS(kp) = sym == K_STR && ins ? strsav(ins) : NULL;
  KBNAM(kp) = nam ? strsav(nam) : NULL;
  KBSYM(kp) = sym;
  KBUSR(kp) = usr;

  /* Insert the binding into key binding list in reverse lexical order */
  if(kroot && CMP(str, KBSTR(kroot)) < 0) {
    for(pp = kroot; KBNXT(pp); pp = (klist *) KBNXT(pp)) {
      p = (klist *) KBNXT(pp);
      if(CMP((char *) str, KBSTR(p)) > 0)
	break;
    }
    KBPRV(kp) = (glist *) pp;
    KBNXT(kp) = KBNXT(pp);
    if(p = (klist *) KBNXT(pp))
      KBPRV(p) = (glist *) kp;
    KBNXT(pp) = (glist *) kp;
  }
  else {
    if(kroot)
      KBPRV(kroot) = (glist *) kp;
    KBPRV(kp) = GNULL;
    KBNXT(kp) = (glist *) kroot;
    kroot = kp;
  }
  return(1);

} /* insertbinding() */

/* Compare key name and return symbol */
LOCAL ksym getkeysymbol(s)
  register char *s;
{
  register int i;

  if(*s == '\"') {              /* Bind key to string to insert */
    *s++ = '\0';
    while(*s && *s != '\"')
      ++s;
    if(*s == '\"') {
      *s = '\0';
      return(K_STR);
    }
  }
  else {                        /* Bind key to utree key */
    strupper(s);
    for(i = 0; KNNAM(i); i++)
      if(EQU(s, KNNAM(i)))
	return(KNSYM(i));
  }
  return(K_INV);

} /* getkeysymbol() */

/* Create a key string from ascii string s */
LOCAL kchar *getkeystring(s)
  register char *s;
{
  static kchar st[KEYLEN+2];
  register int i, o;

  for(i = 0; !EOL(*s) && i < KEYLEN; i++) {
    if(*s == '\\') {
      ++s;
      if(EOL(*s))
	return(UNULL);
      switch(*s) {
	default:                /* As it is */
	  st[i] = *s;
	  break;
	case 'e':               /* Escape */
	case 'E':
	  st[i] = 0x1b;
	  break;
	case 'n':               /* Newline */
	  st[i] = '\n';
	  break;
	case 'r':               /* Carriage return */
	  st[i] = '\r';
	  break;
	case 't':               /* Tab */
	  st[i] = '\t';
	  break;
	case 's':               /* Space */
	  st[i] = ' ';
	  break;
	case 'f':               /* Formfeed */
	  st[i] = '\f';
	  break;
	case 'b':               /* Backspace */
	  st[i] = '\b';
	  break;
	case '0':               /* Octal given value */
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	  o = 0;
	  do {
	    if(o < 0377) {
	      o <<= 3;
	      o |= *s - '0';
	    }
	    ++s;
	  } while(*s && *s >= '0' && *s <= '7');
	  st[i] = (kchar) o;
	  break;
      }
    }
    else if(*s == '^') {
      ++s;
      if(EOL(*s))
	return(UNULL);
      if(*s == '?')                     /* DEL */
	st[i] = 0x7f;
      else if(*s == '@')                /* NUL */
	st[i] = 0x80;
      else if(*s >= 'A' && *s <= '_')   /* SOH .. US */
	st[i] = *s - '@';
      else if(*s >= 'a' && *s <= 'z')   /* SOH .. SUB */
	st[i] = *s - '`';
      else
	return(UNULL);
    }
    else
      st[i] = *s;
    if(*s)
      ++s;
  }
  st[i] = '\0';

  return(i > 0 ? st : UNULL);

} /* getkeystring() */

/* Get and return ascii readable string from key string s */
LOCAL char *keystring(s, buf)
  register kchar *s;
  register char *buf;

{
  if(*s == '\0')
    return("INVALID");
  *buf = '\0';
  while(*s) {
    switch(*s) {
      case 0x80:                /* Terminfo: ASCII-NUL */
	(void) strcat(buf, "^@");
	break;
      case 0x1b:                /* ESC */
	(void) strcat(buf, "\\e");
	break;
      case '\b':                /* BS */
	(void) strcat(buf, "\\b");
	break;
      case 0x7f:                /* DEL */
	(void) strcat(buf, "^?");
	break;
      case ' ':                 /* SP */
	(void) strcat(buf, "\\s");
	break;
      case '\n':                /* NL */
	(void) strcat(buf, "\\n");
	break;
      case '\r':                /* CR */
	(void) strcat(buf, "\\r");
	break;
      case '\t':                /* TAB */
	(void) strcat(buf, "\\t");
	break;
      case '\f':                /* FF */
	(void) strcat(buf, "\\f");
	break;
      default:                  /* Others */
	if(*s < ' ')            /* Control character */
	  (void) sprintf(buf, "%s^%c", buf, *s + '@');
	else if(*s > 0x80)      /* Most significant bit set */
	  (void) sprintf(buf, "%s\\%03o", buf, *s);
	else                    /* Printable character */
	  (void) sprintf(buf, "%s%c", buf, *s);
	break;
    }
    ++s;
  }
  return(buf);

} /* keystring() */

/* Get and return name of symbol k */
LOCAL char *keysymbol(k)
  register ksym k;
{
  register int i;

  for(i = 0; KNNAM(i); i++)
    if(KNSYM(i) == k)
      return(KNNAM(i));
  return(unknown);

} /* keysymbol() */

/* Get and return meaning of symbol k */
LOCAL char *keymeaning(k)
  register ksym k;
{
  register int i;

  for(i = 0; KNNAM(i); i++)
    if(KNSYM(i) == k)
      return(KNCOM(i));
  return(unknown);

} /* keymeaning() */

/* Show all key bindings */
LOCAL int showbindings()
{
  char str[INPLEN];
  register klist *kp;
  register char *ks;
  register int l, c, kl;

  l = firstline;
  c = RV_OK;
  clearwindow(firstline, lastline);
  for(kp = kroot; KBNXT(kp); kp = (klist *) KBNXT(kp))
    ;
  for(l = firstline; kp; kp = (klist *) KBPRV(kp)) {
    ks = keystring((kchar *) KBSTR(kp), str);
    kl = strlen(ks);
    if(KBSYM(kp) == K_STR && KBINS(kp))
      c = putfxy(kl < KCOL ? KCOL-kl : 0,  l, 0, "%s=\"%s\"", ks, KBINS(kp));
    else
      c = putfxy(kl < KCOL ? KCOL-kl : 0,  l, 0, "%s=%s", ks, keysymbol(KBSYM(kp)));
    if(KBNAM(kp))
      (void) putfxy(c > columns/2 ? c : columns/2, l, 0, "#%s", KBNAM(kp));
    else
      (void) putfxy(c > columns/2 ? c : columns/2, l, 0, "#%s", keymeaning(KBSYM(kp)));
    if(++l > lastline && KBPRV(kp)) {
      puthelp("%s (CR:continue  ELSE:quit)", who);
      c = hitakey("More key bindings ?", echoline, DA_NONE);
      if( !(c == ' ' || c == '\n'))
	return(c);
      clearwindow(firstline, lastline);
      l = firstline;
    }
  }
  return(c);

} /* showbindings() */

/* Insert a key binding from a startup file line s. Ingore errors */
LOCAL int setbinding(s)
  register char *s;
{
  register char *sp, *cp;
  register kchar *str;
  register ksym sym;

  /* First scan and clean keystring and keyname/string to insert */
  if((sp = strchr(s, '=')) == NULL)
    return(0);
  *sp++ = '\0';
  s = strclean(s);

  /* Get additional comment and clean binding/comment */
  if(cp = strchr(sp, '#')) {
    *cp++ = '\0';
    cp = strclean(cp);
  }
  sp = strclean(sp);

  /* Get key string from s and key symbol from sp */
  if((str = getkeystring(s)) == UNULL || (sym = getkeysymbol(sp)) == K_INV)
    return(1);
  return(insertbinding(str, sym, cp && *cp ? cp : NULL, sym == K_STR ? ++sp : NULL, 1));

} /* setbinding() */

/*
 *      KEY BINDING ROUTINES
 */

/* Initialize key bindings */
GLOBL VOID initbindings(term, cp)
  register char *term, **cp;
{
  char buf[NAMELEN], bind[NAMELEN];
  register FILE *fp;
  register kchar *k;
  register int i;

  /* First: Initialize and link keyname list */
  KNNXT(0) = KNLST(1);
  for(i = 1; KNNAM(i+1); i++) {
    KNNXT(i) = KNLST(i+1);
    KNPRV(i) = KNLST(i-1);
  }
  KNPRV(i) = KNLST(i-1);

  /* Second: Get default keys and insert into list */
  for(i = 0; DTSTR(i); i++)
    (void) insertbinding((kchar *) DTSTR(i), DTSYM(i), NULL, NULL, 0);

  /* Third: Get default function keys from database and insert into list */
  for(i = 0; FTSTR(i) ; i++)
    if(k = (kchar *) tgetstr(FTSTR(i), cp))
      (void) insertbinding(k, FTSYM(i), FTNAM(i), NULL, 0);

#ifdef  UTSTART
  /* Last: Get global or user defined key bindings and insert into list */
  (void) sprintf(bind, "%s-%s", UTSTART, term);
  if(startup(buf, bind) && (fp = fopen(buf, "r"))) {
    while(fgets(buf, sizeof(buf), fp))
      if(VALID(buf[0]))
	(void) setbinding(buf);
    (void) fclose(fp);
  }
#endif  /* UTSTART */

} /* initbindings() */

/* Save user defined key bindings */
GLOBL VOID savebindings(term)
  register char *term;
{
#ifdef  UTSTART
  char buf[NAMELEN];
  register klist *kp;
  register FILE *fp;
  time_t t;

  if(VARVAL(V_AS) && klchg) {
    (void) sprintf(buf, ".%s-%s", UTSTART, term);
    (void) strcpy(buf, pathname(buf, home));
    if(fp = fopen(buf, "w")) {
      t = time((time_t *) 0);
      (void) fprintf(fp, "# %s: ~/.%s-%s, %s", getversion(), UTSTART, term, ctime(&t));
      for(kp = kroot; kp; kp = (klist *) KBNXT(kp))
	if(KBUSR(kp)) {
	  (void) fprintf(fp, "%s=", keystring((kchar *) KBSTR(kp), buf));
	  if(KBSYM(kp) == K_STR && KBINS(kp))
	    (void) fprintf(fp, "\"%s\"", KBINS(kp));
	  else
	    (void) fprintf(fp, "%s", keysymbol(KBSYM(kp)));
	  if(KBNAM(kp))
	    (void) fprintf(fp, "\t#%s\n", KBNAM(kp));
	  else
	    (void) fprintf(fp, "\n");
	}
      (void) fclose(fp);
    }
  }
#endif  /* UTSTART */

} /* savebindings() */

/* Show all key bindings */
GLOBL int bindings()
{
  char buf[INPLEN], key[KEYLEN], com[INPLEN], k[2];
  register klist *kp;
  register char *kb;
  register int c, f;

  who = "KEY BINDINGS";
  /* Key bindings loop */
  f = 1;
  while(1) {
    if(f && ((c = showbindings())  < RV_NUL || c == 'q'))
      break;
    buf[0] = com[0] = k[1] = '\0';
    /* Get key sequence terminated with CR */
    puthelp("%s: Enter key to bind (NO EDIT! CR:quit)", who);
    (void) putecho("Bind:");
    cursorset(CF_VISIBLE);
    k[0] = getchar();
    if(k[0] == '\n' || k[0] == '\r')
      break;
    else if(k[0] == '\0')
      k[0] = '\200';
    (void) strcat(buf, keystring((kchar *) k, key));
    puthelp("%s: Enter key to bind (NO EDIT! CR:terminate string)", who);
    while(1){
      (void) putecho("Bind: %s", buf);
      k[0] = getchar();
      if(k[0] == '\n' || k[0] == '\r')
	break;
      else if(k[0] == '\0')
	k[0] = '\200';
      (void) strcat(buf, keystring((kchar *) k, key));
    }
    /* Get utree key to bind to */
    for(kp = kroot; kp; kp = (klist *) KBNXT(kp))
      if(EQU(KBSTR(kp), getkeystring(buf)))
	break;
    if(kp) {
      if(KBSYM(kp) == K_STR && KBINS(kp)) {
	(void) sprintf(key, "\"%s\"", KBINS(kp));
	kb = key;
      }
      else
	kb = keysymbol(KBSYM(kp));
    }
    else
      kb = NULL;
    puthelp("%s: Give utree keyname for binding (CR:quit)", who);
    c = putecho("Bind %s to:", buf);
    if((c = getline(key, sizeof(key), c, 'k', kb, KNLST(0), 1)) <= RV_NUL)
      break;
    /* Get additional comment */
    (void) sprintf(buf, "%s=%s", buf, key);
    if(getkeysymbol(key) != K_INV) {
      puthelp("%s: Give comment for binding", who);
      c = putecho("Comment for key binding:");
      if((c = getline(com, sizeof(com), c, 0, NULL, GNULL, 0)) < RV_NUL)
	break;
      if(com[0])
	(void) sprintf(buf, "%s #%s", buf, com);
      /* Do the binding */
      if(setbinding(buf)) {
	f = 1;
	klchg = 1;
	continue;
      }
    }
    f = 0;
    puthelp("%s %s", who, hitkey);
    if((c = errequest(buf, "Error in setting")) < RV_NUL)
      break;
  }

  cursorset(CF_INVISIBLE);
  treeflag = fileflag = SF_FULL;
  return(c);

} /* bindings() */
