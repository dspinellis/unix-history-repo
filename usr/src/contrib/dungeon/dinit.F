#include "files.h"

#ifndef INDXFILE
#define INDXFILE '/usr/games/lib/dunlib/dindx.dat'
#endif
#ifndef TEXTFILE
#define TEXTFILE '/usr/games/lib/dunlib/dtext.dat'
#endif
#ifndef WIZARDID
#define WIZARDID 0
#endif

C INIT-- DUNGEON INITIALIZATION SUBROUTINE
C
C COPYRIGHT 1980, INFOCOM COMPUTERS AND COMMUNICATIONS, CAMBRIDGE MA. 02142
C ALL RIGHTS RESERVED, COMMERCIAL USAGE STRICTLY PROHIBITED
C WRITTEN BY R. M. SUPNIK
C
C DECLARATIONS
C
	LOGICAL FUNCTION INIT(X)
	IMPLICIT INTEGER (A-Z)
#ifndef PDP
	LOGICAL PROTCT
	INTEGER DATARR(3)
#endif PDP
#include "parser.h"
#include "gamestate.h"
#include "state.h"
#include "screen.h"
#include "mindex.h"
C
C MISCELLANEOUS VARIABLES
C
	COMMON /STAR/ MBASE,STRBIT
	COMMON /VERS/ VMAJ,VMIN,VEDIT
	COMMON /TIME/ PLTIME,SHOUR,SMIN,SSEC
#include "io.h"
#include "debug.h"
	COMMON /HYPER/ HFACTR
#include "rooms.h"
#include "rflag.h"
#include "rindex.h"
#include "exits.h"
#include "curxt.h"
#include "xpars.h"
#include "objects.h"
#include "oindex.h"
#include "clock.h"
#include "villians.h"
#include "advers.h"
#include "flags.h"
C INIT, PAGE 2
C
#ifndef PDP
#ifdef SYSV
C make output unbuffered
	call unbuf
C
#endif
C FIRST CHECK FOR PROTECTION VIOLATION
C
	IF(PROTCT(X)) GO TO 10000
C						!PROTECTION VIOLATION?
	PRINT 10100
#ifdef NOCC
10100	FORMAT('There appears before you a threatening figure clad '
&	'all over'/'in heavy black armor.  His legs seem like the '
&	'massive trunk'/'of the oak tree.  His broad shoulders and '
&	'helmeted head loom'/'high over your own puny frame, and '
&	'you realize that his powerful'/'arms could easily crush the '
&	'very life from your body.  There'/'hangs from his belt a '
&	'veritable arsenal of deadly weapons:'/'sword, mace, ball '
&	'and chain, dagger, lance, and trident.'/'He speaks with a '
&	'commanding voice:'//20X,'"You shall not pass."'//'As '
&	'he grabs you by the neck all grows dim about you.')
#else NOCC
10100	FORMAT(' There appears before you a threatening figure clad '
&	'all over'/' in heavy black armor.  His legs seem like the '
&	'massive trunk'/' of the oak tree.  His broad shoulders and '
&	'helmeted head loom'/' high over your own puny frame, and '
&	'you realize that his powerful'/' arms could easily crush the '
&	'very life from your body.  There'/' hangs from his belt a '
&	'veritable arsenal of deadly weapons:'/' sword, mace, ball '
&	'and chain, dagger, lance, and trident.'/' He speaks with a '
&	'commanding voice:'//20X,'"You shall not pass."'//' As '
&	'he grabs you by the neck all grows dim about you.')
#endif NOCC
	CALL EXIT
#endif PDP
C
C NOW START INITIALIZATION PROPER
C
#ifdef PDP
C
C   Note: arrays FLAGS & SWITCH are initialized in the following
C           DATA statements, instead of using DO loops and assignments
C           as used before.  This saves some code space.
C
	DATA FLAGS/.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
&		   .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
&		   .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
&		    .TRUE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.TRUE.,
&		   .FALSE.,.FALSE.,.FALSE.,.TRUE.,.FALSE.,.FALSE.,
&		   .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
&		   .FALSE.,.FALSE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,
&		    .TRUE.,.FALSE.,.FALSE.,.FALSE./
C
	DATA SWITCH/0,0,0,0,0,0,0,0,0,
&		    4,0,270,0,0,0,0,0,
&		    1,1,0,0,10/
C
C   Note: SWITCH(13) or MLOC is initialized equal to MRB later.
C
C
	DATA LTSHFT/10/
	DATA EGSCOR/0/
	DATA EGMXSC/0/
	DATA MXLOAD/100/
	DATA RWSCOR/0/
	DATA DEATHS/0/
	DATA MOVES/0/
	DATA PLTIME/0/
	DATA MUNGRM/0/
	DATA HS/0/
	DATA PRSA/0/
	DATA PRSI/0/
	DATA PRSO/0/
	DATA PRSCON/1/
	DATA OFLAG/0/
	DATA OACT/0/
	DATA OSLOT/0/
	DATA OPREP/0/
	DATA ONAME/0/
	DATA THFFLG/.FALSE./
	DATA THFACT/.TRUE./
	DATA SWDACT/.FALSE./
	DATA SWDSTA/0/
C
	DATA RECNO/1/
	DATA MBASE/0/
	DATA INPCH/5/
	DATA OUTCH/5/
	DATA DBCH/2/
C
C INIT, PAGE 3
C
C
	DATA DBGFLG/0/
	DATA PRSFLG/0/
	DATA GDTFLG/0/
C
	FROMDR=0
	SCOLRM=0
	SCOLAC=0
	INIT=.FALSE.		
	MLOC=MRB
C
C INIT, PAGE 4
C
C NOW RESTORE FROM EXISTING INDEX FILE.
C
	call intrd(i)
	call intrd(j)
	call intrd(k)
	IF((I.NE.VMAJ).OR.(J.NE.VMIN))
&		GO TO 1925			
C
	call intrd(MXSCOR)
	call intrd(STRBIT)
	call intrd(EGMXSC)
C
	call intrd(RLNT)
	call intrd(RDESC2)
	call aryrd(200,RDESC1)
	call aryrd(200,REXIT)
	call aryrd(200,RACTIO)
	call aryrd(200,RVAL)
	call aryrd(200,RFLAG)
C
	call intrd(XLNT)
	call aryrd(900,TRAVEL)
	call intrd(OLNT)
	call aryrd(220,ODESC1)
	call aryrd(220,ODESC2)
	call aryrd(220,ODESCO)
	call aryrd(220,OACTIO)
	call aryrd(220,OFLAG1)
	call aryrd(220,OFLAG2)
	call aryrd(220,OFVAL)
	call aryrd(220,OTVAL)
	call aryrd(220,OSIZE)
	call aryrd(220,OCAPAC)
	call aryrd(220,OROOM)
	call aryrd(220,OADV)
	call aryrd(220,OCAN)
	call aryrd(220,OREAD)
C
	call intrd(R2LNT)
	call aryrd(20,OROOM2)
	call aryrd(20,RROOM2)
C
	call intrd(CLNT)
	call aryrd(25,CTICK)
	call aryrd(25,CACTIO)
C
	do 990 i=1,25
	cflag(i)=.TRUE.
	call logrd(j)
	if(j.EQ.0) CFLAG(i)=.FALSE.
990	continue
C
	call intrd(VLNT)
	call aryrd(4,VILLNS)
	call aryrd(4,VPROB)
	call aryrd(4,VOPPS)
	call aryrd(4,VBEST)
	call aryrd(4,VMELEE)
C
	call intrd(ALNT)
	call aryrd(4,AROOM)
	call aryrd(4,ASCORE)
	call aryrd(4,AVEHIC)
	call aryrd(4,AOBJ)
	call aryrd(4,AACTIO)
	call aryrd(4,ASTREN)
	call aryrd(4,AFLAG)
C
	call intrd(MBASE)
	call intrd(MLNT)
C
C   The RTEXT array is not used here, and isn't read (it's used
C   in "speak.F")
C
	call initnd
C
C INIT, PAGE 5
C
C THE INTERNAL DATA BASE IS NOW ESTABLISHED.
C SET UP TO PLAY THE GAME.
C
1025	CALL ITIME(SHOUR,SMIN,SSEC)		
	CALL INIRND(or(SHOUR,or(SMIN,SSEC)))
C
	WINNER=PLAYER
	LASTIT=AOBJ(PLAYER)
	HERE=AROOM(WINNER)
	THFPOS=OROOM(THIEF)
	BLOC=OROOM(BALLO)
	INIT=.TRUE.
#ifdef debug
C
C	Normally, PRSFLG is setable in gdt to allow seeing various
C	parse results.  Since the pdp version does not have gdt,
C	PRSFLG is set to show full debugging info when debug is enabled.
C
	PRSFLG=65535
#endif debug
C
C
	RETURN
C INIT, PAGE 6
C
C ERRORS-- INIT FAILS.
C
1925	continue
	END
#else PDP
10000	INIT=.FALSE.
C						!ASSUME INIT FAILS.
	MMAX=1050
C						!SET UP ARRAY LIMITS.
	OMAX=220
	RMAX=200
	VMAX=4
	AMAX=4
	CMAX=25
	FMAX=46
	SMAX=22
	XMAX=900
	R2MAX=20
	DIRMAX=15
C
	MLNT=0
C						!INIT ARRAY COUNTERS.
	OLNT=0
	RLNT=0
	VLNT=0
	ALNT=0
	CLNT=0
	XLNT=1
	R2LNT=0
C
	LTSHFT=10
C						!SET UP STATE VARIABLES.
	MXSCOR=LTSHFT
	EGSCOR=0
	EGMXSC=0
	MXLOAD=100
	RWSCOR=0
	DEATHS=0
	MOVES=0
	PLTIME=0
	MUNGRM=0
	HS=0
	PRSA=0
C						!CLEAR PARSE VECTOR.
	PRSI=0
	PRSO=0
	PRSCON=1
	OFLAG=0
C						!CLEAR ORPHANS.
	OACT=0
	OSLOT=0
	OPREP=0
	ONAME=0
	THFFLG=.FALSE.
C						!THIEF NOT INTRODUCED BUT
	THFACT=.TRUE.
C						!IS ACTIVE.
	SWDACT=.FALSE.
C						!SWORD IS INACTIVE.
	SWDSTA=0
C						!SWORD IS OFF.
C
	RECNO=1
C						!INIT DB FILE POINTER.
	MBASE=0
C						!INIT MELEE BASE.
C   LOGICAL UNIT NRS: 5=STDIN, 6=STDOUT
	INPCH=5
C						!TTY INPUT
	OUTCH=6
	DBCH=2
C						!DATA BASE.
C INIT, PAGE 3
C
C INIT ALL ARRAYS.
C
	DO 5 I=1,CMAX
C						!CLEAR CLOCK EVENTS
	  CFLAG(I)=.FALSE.
	  CTICK(I)=0
	  CACTIO(I)=0
5	CONTINUE
C
	DO 10 I=1,FMAX
C						!CLEAR FLAGS.
	  FLAGS(I)=.FALSE.
10	CONTINUE
	BUOYF=.TRUE.
C						!SOME START AS TRUE.
	EGYPTF=.TRUE.
	CAGETF=.TRUE.
	MR1F=.TRUE.
	MR2F=.TRUE.
	FOLLWF=.TRUE.
	DO 12 I=1,SMAX
C						!CLEAR SWITCHES.
	  SWITCH(I)=0
12	CONTINUE
	ORMTCH=4
C						!NUMBER OF MATCHES.
	LCELL=1
	PNUMB=1
	MDIR=270
	MLOC=MRB
	CPHERE=10
C
	DO 15 I=1,R2MAX
C						!CLEAR ROOM 2 ARRAY.
	  RROOM2(I)=0
	  OROOM2(I)=0
15	CONTINUE
C
	DO 20 I=1,XMAX
C						!CLEAR TRAVEL ARRAY.
	  TRAVEL(I)=0
20	CONTINUE
C
	DO 30 I=1,VMAX
C						!CLEAR VILLAINS ARRAYS.
	  VOPPS(I)=0
	  VPROB(I)=0
	  VILLNS(I)=0
	  VBEST(I)=0
	  VMELEE(I)=0
30	CONTINUE
C
	DO 40 I=1,OMAX
C						!CLEAR OBJECT ARRAYS.
	  ODESC1(I)=0
	  ODESC2(I)=0
	  ODESCO(I)=0
	  OREAD(I)=0
	  OACTIO(I)=0
	  OFLAG1(I)=0
	  OFLAG2(I)=0
	  OFVAL(I)=0
	  OTVAL(I)=0
	  OSIZE(I)=0
	  OCAPAC(I)=0
	  OCAN(I)=0
	  OADV(I)=0
	  OROOM(I)=0
40	CONTINUE
C
	RDESC2=0
C						!CLEAR DESC BASE PTR.
	DO 50 I=1,RMAX
C						!CLEAR ROOM ARRAYS.
	  RDESC1(I)=0
	  RACTIO(I)=0
	  RFLAG(I)=0
	  RVAL(I)=0
	  REXIT(I)=0
50	CONTINUE
C
	DO 60 I=1,MMAX
C						!CLEAR MESSAGE DIRECTORY.
	  RTEXT(I)=0
60	CONTINUE
C
	DO 70 I=1,AMAX
C						!CLEAR ADVENTURER'S ARRAYS.
	  AROOM(I)=0
	  ASCORE(I)=0
	  AVEHIC(I)=0
	  AOBJ(I)=0
	  AACTIO(I)=0
	  ASTREN(I)=0
	  AFLAG(I)=0
70	CONTINUE
C
	DBGFLG=0
	PRSFLG=0
	GDTFLG=0
C
C allow setting gdtflg true if user id matches wizard id
C this way, the wizard doesn't have to recompile to use gdt
C
	if (getuid() .eq. WIZARDID) gdtflg=1
C
	FROMDR=0
C						!INIT SCOL GOODIES.
	SCOLRM=0
	SCOLAC=0
C INIT, PAGE 4
C
C NOW RESTORE FROM EXISTING INDEX FILE.
C
	OPEN(UNIT=1,file=INDXFILE,status='OLD',
#ifdef XELOS
&		FORM='FORMATTED',ACCESS='SEQUENTIAL',ERR=1900,recl=1)
#else
&		FORM='FORMATTED',ACCESS='SEQUENTIAL',ERR=1900)
#endif
	rewind(unit=1, err=1900)
	READ(1,130) I,J,K
C						!GET VERSION.
	IF((I.NE.VMAJ).OR.(J.NE.VMIN))
&		GO TO 1925

	OPEN(UNIT=DBCH,file=TEXTFILE,status='OLD',
&		FORM='UNFORMATTED',ACCESS='DIRECT',
&		recl=76,ERR=1950)
	rewind(unit=dbch, err=1950)

#ifdef debug
	PRINT 150
#ifdef NOCC
150	FORMAT('RESTORING FROM "dindx.dat"')
#else NOCC
150	FORMAT(' RESTORING FROM "dindx.dat"')
#endif NOCC
#endif debug
	READ(1,130) MXSCOR,STRBIT,EGMXSC
	READ(1,130) RLNT,RDESC2,RDESC1,REXIT,RACTIO,RVAL,RFLAG
	READ(1,130) XLNT,TRAVEL
	READ(1,130) OLNT,ODESC1,ODESC2,ODESCO,OACTIO,OFLAG1,OFLAG2,
&		OFVAL,OTVAL,OSIZE,OCAPAC,OROOM,OADV,OCAN,
&		OREAD
	READ(1,130) R2LNT,OROOM2,RROOM2
	READ(1,130) CLNT,CTICK,CACTIO
	READ(1,135) CFLAG
	READ(1,130) VLNT,VILLNS,VPROB,VOPPS,VBEST,VMELEE
	READ(1,130) ALNT,AROOM,ASCORE,AVEHIC,AOBJ,AACTIO,ASTREN,AFLAG
	READ(1,130) MBASE,MLNT,RTEXT
C
	CLOSE(1)
	GO TO 1025
C						!INIT DONE.
C
C 130	FORMAT(I8)
130	FORMAT(I6)
135	FORMAT(L4)
C INIT, PAGE 5
C
C THE INTERNAL DATA BASE IS NOW ESTABLISHED.
C SET UP TO PLAY THE GAME.
C
1025	CALL ITIME(SHOUR,SMIN,SSEC)
C						!GET TIME AND DATE.
C	CALL IDATE(I,J,K)
	CALL IDATE(DATARR(1))
	CALL INIRND(or(DATARR(1),or(DATARR(2),DATARR(3))),
&		or(SHOUR,or(SMIN,SSEC)))
C
	WINNER=PLAYER
	LASTIT=AOBJ(PLAYER)
	HERE=AROOM(WINNER)
	THFPOS=OROOM(THIEF)
	BLOC=OROOM(BALLO)
	INIT=.TRUE.
C
#ifdef debug
	PRINT 1050,RLNT,RMAX,XLNT,XMAX,OLNT,OMAX,MLNT,MMAX,
&	  VLNT,VMAX,ALNT,AMAX,CLNT,CMAX,R2LNT,R2MAX
#ifdef NOCC
1050	FORMAT('USED:'/I5,' OF',I5,' ROOMS'/
&	  I5,' OF',I5,' EXITS'/
&	  I5,' OF',I5,' OBJECTS'/
&	  I5,' OF',I5,' MESSAGES'/
&	  I5,' OF',I5,' VILLAINS'/
&	  I5,' OF',I5,' ADVENTURERS'/
&	  I5,' OF',I5,' CLOCK EVENTS'/
&	  I5,' OF',I5,' ROOM2 SLOTS')
#else NOCC
1050	FORMAT(' USED:'/1X,I5,' OF',I5,' ROOMS'/
&	  1X,I5,' OF',I5,' EXITS'/
&	  1X,I5,' OF',I5,' OBJECTS'/
&	  1X,I5,' OF',I5,' MESSAGES'/
&	  1X,I5,' OF',I5,' VILLAINS'/
&	  1X,I5,' OF',I5,' ADVENTURERS'/
&	  1X,I5,' OF',I5,' CLOCK EVENTS'/
&	  1X,I5,' OF',I5,' ROOM2 SLOTS')
#endif NOCC
	PRINT 1150,MXSCOR,EGMXSC,RECNO,RDESC2,MBASE,STRBIT
#ifdef NOCC
1150	FORMAT('MAX SCORE=',I5/'EG SCORE=',I5/
&	  'MAX RECNO=',I5/'RDESC2 BASE=',I5/
&	  'MELEE START=',I5/'STAR MASK=',I7)
#else NOCC
1150	FORMAT(' MAX SCORE=',I5/' EG SCORE=',I5/
&	  ' MAX RECNO=',I5/' RDESC2 BASE=',I5/
&	  ' MELEE START=',I5/' STAR MASK=',I7)
#endif NOCC
	PAUSE 1
#endif debug
C
	RETURN
C INIT, PAGE 6
C
C ERRORS-- INIT FAILS.
C
1900	PRINT 910
	PRINT 980
	RETURN
1925	PRINT 920,I,J,K,VMAJ,VMIN,VEDIT
	PRINT 980
	RETURN
1950	PRINT 960
	PRINT 980
	RETURN
#ifdef NOCC
910	FORMAT('I can''t open ',INDXFILE,'.')
920	FORMAT('"dindx.dat" is version ',I1,'.',I1,A1,'.'/
&		'I require version ',I1,'.',I1,A1,'.')
960	FORMAT('I can''t open ',TEXTFILE,'.')
980	FORMAT('Suddenly a sinister, wraithlike figure appears before '
&	'you,'/'seeming to float in the air.  In a low, sorrowful voice'
&	' he says,'/'"Alas, the very nature of the world has changed, '
&	'and the dungeon'/'cannot be found.  All must now pass away."'
&	'  Raising his oaken staff'/'in farewell, he fades into the '
&	'spreading darkness.  In his place'/'appears a tastefully '
&	'lettered sign reading:'//23X,'INITIALIZATION FAILURE'//
&	'The darkness becomes all encompassing, and your vision fails.')
#else NOCC
910	FORMAT(' I can''t open ',INDXFILE,'.')
920	FORMAT(' "dindx.dat" is version ',I1,'.',I1,A1,'.'/
&		' I require version ',I1,'.',I1,A1,'.')
960	FORMAT(' I can''t open ',TEXTFILE,'.')
980	FORMAT(' Suddenly a sinister, wraithlike figure appears before '
&	'you,'/' seeming to float in the air.  In a low, sorrowful voice'
&	' he says,'/' "Alas, the very nature of the world has changed, '
&	'and the dungeon'/' cannot be found.  All must now pass away."'
&	'  Raising his oaken staff'/' in farewell, he fades into the '
&	'spreading darkness.  In his place'/' appears a tastefully '
&	'lettered sign reading:'//23X,'INITIALIZATION FAILURE'//
&	' The darkness becomes all encompassing, and your vision fails.')
#endif NOCC
C
	END
C PROTCT-- CHECK FOR USER VIOLATION
C
C THIS ROUTINE SHOULD BE MODIFIED IF YOU WISH TO ADD SYSTEM
C DEPENDANT PROTECTION AGAINST ABUSE.
C
C AT THE MOMENT, PLAY IS PERMITTED UNDER ALL CIRCUMSTANCES.
C
	LOGICAL FUNCTION PROTCT(X)
	IMPLICIT INTEGER(A-Z)
C
	PROTCT=.TRUE.
	RETURN
	END
#endif PDP
