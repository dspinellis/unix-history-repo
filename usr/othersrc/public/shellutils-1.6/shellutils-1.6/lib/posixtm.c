#ifndef lint
static char zzsccsid[] = "@(#)yaccpar	1.8 (Berkeley) 01/20/90";
#endif
#define YYBYACC 1
#line 20 "posixtm.y"
#ifdef __GNUC__
#define alloca __builtin_alloca
#else
#ifdef sparc
#include <alloca.h>
#else
#ifdef _AIX
 #pragma alloca
#else
char *alloca ();
#endif
#endif
#endif

#include <stdio.h>
#include <sys/types.h>
#include <time.h>

#define YYDEBUG 1

/* Lexical analyzer's current scan position in the input string. */
static char *curpos;

/* The return value. */
static struct tm t;

time_t mktime ();

#define zzparse posixtime_zzparse
static int zzlex ();
static int zzerror ();
#line 38 "y.tab.c"
#define DIGIT 257
#define YYERRCODE 256
short zzlhs[] = {                                        -1,
    0,    2,    2,    2,    3,    3,    1,
};
short zzlen[] = {                                         2,
    6,    1,    2,    0,    0,    2,    2,
};
short zzdefred[] = {                                      0,
    0,    0,    0,    7,    0,    0,    0,    0,    0,    3,
    0,    1,    6,
};
short zzdgoto[] = {                                       2,
    3,    9,   12,
};
short zzsindex[] = {                                   -257,
 -254,    0, -257,    0, -257, -257, -257, -257,  -42,    0,
 -257,    0,    0,
};
short zzrindex[] = {                                      0,
    0,    0,    0,    0,    0,    0,    1,    2,    5,    0,
    0,    0,    0,
};
short zzgindex[] = {                                      0,
    3,    0,    0,
};
#define YYTABLESIZE 48
short zztable[] = {                                       1,
    4,    2,    4,   11,    5,    5,    0,    6,    7,    8,
   10,    0,    0,   13,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    4,    2,
};
short zzcheck[] = {                                     257,
    0,    0,  257,   46,    0,    3,   -1,    5,    6,    7,
    8,   -1,   -1,   11,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   46,   46,
};
#define YYFINAL 2
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#define YYMAXTOKEN 257
#if YYDEBUG
char *zzname[] = {
"end-of-file",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,"'.'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"DIGIT",
};
char *zzrule[] = {
"$accept : date",
"date : digitpair digitpair digitpair digitpair year seconds",
"year : digitpair",
"year : digitpair digitpair",
"year :",
"seconds :",
"seconds : '.' digitpair",
"digitpair : DIGIT DIGIT",
};
#endif
#ifndef YYSTYPE
typedef int YYSTYPE;
#endif
#define zzclearin (zzchar=(-1))
#define zzerrok (zzerrflag=0)
#ifdef YYSTACKSIZE
#ifndef YYMAXDEPTH
#define YYMAXDEPTH YYSTACKSIZE
#endif
#else
#ifdef YYMAXDEPTH
#define YYSTACKSIZE YYMAXDEPTH
#else
#define YYSTACKSIZE 500
#define YYMAXDEPTH 500
#endif
#endif
int zzdebug;
int zznerrs;
int zzerrflag;
int zzchar;
short *zzssp;
YYSTYPE *zzvsp;
YYSTYPE zzval;
YYSTYPE zzlval;
short zzss[YYSTACKSIZE];
YYSTYPE zzvs[YYSTACKSIZE];
#define zzstacksize YYSTACKSIZE
#line 127 "posixtm.y"
static int
zzlex ()
{
  char ch = *curpos++;

  if (ch >= '0' && ch <= '9')
    {
      zzlval = ch - '0';
      return DIGIT;
    }
  else if (ch == '.' || ch == 0)
    return ch;
  else
    return '?';			/* Cause an error.  */
}

static int
zzerror ()
{
  return 0;
}

/* Parse a POSIX-style date and return it, or (time_t)-1 for an error.  */

time_t
posixtime (s)
     char *s;
{
  curpos = s;
  /* Let mktime decide whether it is daylight savings time.  */
  t.tm_isdst = -1;
  if (zzparse ())
    return (time_t)-1;
  else
    return mktime (&t);
}

/* Parse a POSIX-style date and return it, or NULL for an error.  */

struct tm *
posixtm (s)
     char *s;
{
  if (posixtime (s) == -1)
    return NULL;
  return &t;
}
#line 182 "y.tab.c"
#define YYABORT goto zzabort
#define YYACCEPT goto zzaccept
#define YYERROR goto zzerrlab
int
zzparse()
{
    register int zzm, zzn, zzstate;
#if YYDEBUG
    register char *zzs;
    extern char *getenv();

    if (zzs = getenv("YYDEBUG"))
    {
        zzn = *zzs;
        if (zzn >= '0' && zzn <= '9')
            zzdebug = zzn - '0';
    }
#endif

    zznerrs = 0;
    zzerrflag = 0;
    zzchar = (-1);

    zzssp = zzss;
    zzvsp = zzvs;
    *zzssp = zzstate = 0;

zzloop:
    if (zzn = zzdefred[zzstate]) goto zzreduce;
    if (zzchar < 0)
    {
        if ((zzchar = zzlex()) < 0) zzchar = 0;
#if YYDEBUG
        if (zzdebug)
        {
            zzs = 0;
            if (zzchar <= YYMAXTOKEN) zzs = zzname[zzchar];
            if (!zzs) zzs = "illegal-symbol";
            printf("zzdebug: state %d, reading %d (%s)\n", zzstate,
                    zzchar, zzs);
        }
#endif
    }
    if ((zzn = zzsindex[zzstate]) && (zzn += zzchar) >= 0 &&
            zzn <= YYTABLESIZE && zzcheck[zzn] == zzchar)
    {
#if YYDEBUG
        if (zzdebug)
            printf("zzdebug: state %d, shifting to state %d\n",
                    zzstate, zztable[zzn]);
#endif
        if (zzssp >= zzss + zzstacksize - 1)
        {
            goto zzoverflow;
        }
        *++zzssp = zzstate = zztable[zzn];
        *++zzvsp = zzlval;
        zzchar = (-1);
        if (zzerrflag > 0)  --zzerrflag;
        goto zzloop;
    }
    if ((zzn = zzrindex[zzstate]) && (zzn += zzchar) >= 0 &&
            zzn <= YYTABLESIZE && zzcheck[zzn] == zzchar)
    {
        zzn = zztable[zzn];
        goto zzreduce;
    }
    if (zzerrflag) goto zzinrecovery;
#ifdef lint
    goto zznewerror;
#endif
zznewerror:
    zzerror("syntax error");
#ifdef lint
    goto zzerrlab;
#endif
zzerrlab:
    ++zznerrs;
zzinrecovery:
    if (zzerrflag < 3)
    {
        zzerrflag = 3;
        for (;;)
        {
            if ((zzn = zzsindex[*zzssp]) && (zzn += YYERRCODE) >= 0 &&
                    zzn <= YYTABLESIZE && zzcheck[zzn] == YYERRCODE)
            {
#if YYDEBUG
                if (zzdebug)
                    printf("zzdebug: state %d, error recovery shifting\
 to state %d\n", *zzssp, zztable[zzn]);
#endif
                if (zzssp >= zzss + zzstacksize - 1)
                {
                    goto zzoverflow;
                }
                *++zzssp = zzstate = zztable[zzn];
                *++zzvsp = zzlval;
                goto zzloop;
            }
            else
            {
#if YYDEBUG
                if (zzdebug)
                    printf("zzdebug: error recovery discarding state %d\n",
                            *zzssp);
#endif
                if (zzssp <= zzss) goto zzabort;
                --zzssp;
                --zzvsp;
            }
        }
    }
    else
    {
        if (zzchar == 0) goto zzabort;
#if YYDEBUG
        if (zzdebug)
        {
            zzs = 0;
            if (zzchar <= YYMAXTOKEN) zzs = zzname[zzchar];
            if (!zzs) zzs = "illegal-symbol";
            printf("zzdebug: state %d, error recovery discards token %d (%s)\n",
                    zzstate, zzchar, zzs);
        }
#endif
        zzchar = (-1);
        goto zzloop;
    }
zzreduce:
#if YYDEBUG
    if (zzdebug)
        printf("zzdebug: state %d, reducing by rule %d (%s)\n",
                zzstate, zzn, zzrule[zzn]);
#endif
    zzm = zzlen[zzn];
    zzval = zzvsp[1-zzm];
    switch (zzn)
    {
case 1:
#line 62 "posixtm.y"
{
	         if (zzvsp[-5] >= 1 && zzvsp[-5] <= 12)
		   t.tm_mon = zzvsp[-5] - 1;
		 else {
		   YYABORT;
		 }
		 if (zzvsp[-4] >= 1 && zzvsp[-4] <= 31)
		   t.tm_mday = zzvsp[-4];
		 else {
		   YYABORT;
		 }
		 if (zzvsp[-3] >= 0 && zzvsp[-3] <= 23)
		   t.tm_hour = zzvsp[-3];
		 else {
		   YYABORT;
		 }
		 if (zzvsp[-2] >= 0 && zzvsp[-2] <= 59)
		   t.tm_min = zzvsp[-2];
		 else {
		   YYABORT;
		 }
	       }
break;
case 2:
#line 85 "posixtm.y"
{
                   t.tm_year = zzvsp[0];
		   /* Deduce the century based on the year.
		      See POSIX.2 section 4.63.3.  */
		   if (zzvsp[0] <= 68)
		     t.tm_year += 100;
		 }
break;
case 3:
#line 92 "posixtm.y"
{
                            t.tm_year = zzvsp[-1] * 100 + zzvsp[0];
			    if (t.tm_year < 1900) {
			      YYABORT;
			    } else
			      t.tm_year -= 1900;
			  }
break;
case 4:
#line 99 "posixtm.y"
{
                    time_t now;
		    struct tm *tmp;

                    /* Use current year.  */
                    time (&now);
		    tmp = localtime (&now);
		    t.tm_year = tmp->tm_year;
		  }
break;
case 5:
#line 110 "posixtm.y"
{
                        t.tm_sec = 0;
		      }
break;
case 6:
#line 113 "posixtm.y"
{
	                  if (zzvsp[0] >= 0 && zzvsp[0] <= 61)
			    t.tm_sec = zzvsp[0];
			  else {
			    YYABORT;
			  }
			}
break;
case 7:
#line 122 "posixtm.y"
{
                          zzval = zzvsp[-1] * 10 + zzvsp[0];
			}
break;
#line 401 "y.tab.c"
    }
    zzssp -= zzm;
    zzstate = *zzssp;
    zzvsp -= zzm;
    zzm = zzlhs[zzn];
    if (zzstate == 0 && zzm == 0)
    {
#if YYDEBUG
        if (zzdebug)
            printf("zzdebug: after reduction, shifting from state 0 to\
 state %d\n", YYFINAL);
#endif
        zzstate = YYFINAL;
        *++zzssp = YYFINAL;
        *++zzvsp = zzval;
        if (zzchar < 0)
        {
            if ((zzchar = zzlex()) < 0) zzchar = 0;
#if YYDEBUG
            if (zzdebug)
            {
                zzs = 0;
                if (zzchar <= YYMAXTOKEN) zzs = zzname[zzchar];
                if (!zzs) zzs = "illegal-symbol";
                printf("zzdebug: state %d, reading %d (%s)\n",
                        YYFINAL, zzchar, zzs);
            }
#endif
        }
        if (zzchar == 0) goto zzaccept;
        goto zzloop;
    }
    if ((zzn = zzgindex[zzm]) && (zzn += zzstate) >= 0 &&
            zzn <= YYTABLESIZE && zzcheck[zzn] == zzstate)
        zzstate = zztable[zzn];
    else
        zzstate = zzdgoto[zzm];
#if YYDEBUG
    if (zzdebug)
        printf("zzdebug: after reduction, shifting from state %d \
to state %d\n", *zzssp, zzstate);
#endif
    if (zzssp >= zzss + zzstacksize - 1)
    {
        goto zzoverflow;
    }
    *++zzssp = zzstate;
    *++zzvsp = zzval;
    goto zzloop;
zzoverflow:
    zzerror("yacc stack overflow");
zzabort:
    return (1);
zzaccept:
    return (0);
}
