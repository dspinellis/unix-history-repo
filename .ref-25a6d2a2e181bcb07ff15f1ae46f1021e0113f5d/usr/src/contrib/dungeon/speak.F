#include "files.h"

#ifndef RTEXTFILE
#define RTEXTFILE '/usr/games/lib/dunlib/rtext.dat'
#endif

#ifndef TEXTFILE
#define TEXTFILE '/usr/games/lib/dunlib/dtext.dat'
#endif

C
C	manual speak routine
C	gets dungeon messages and prints them
C	(only used for pdp version)
C
	program speak
	IMPLICIT      INTEGER(A-Z)
C
	COMMON /CHAN/ INPCH,OUTCH,DBCH
#include "mindex.h"
C	
C	load the lookup table
C
	OPEN(UNIT=9,file=RTEXTFILE,
&		status='OLD',IOSTAT=IO,
&		FORM='formatted',ACCESS='SEQUENTIAL',err=50)
C
	call load
C
C	open the message file
C
	DBCH=2
C
	OPEN(UNIT=DBCH,file=TEXTFILE,
&		status='OLD',IOSTAT=IO,
&		FORM='UNFORMATTED',ACCESS='DIRECT',recl=76,err=60)
C
	print 20
#ifdef NOCC
20	format('Sigh... '/)
#else NOCC
20	format(' Sigh... '/)
#endif NOCC
C
C	get numbers and call speaking program
C
10	continue 
C
	call inprd(mesage,i,j)
	call RSPSB2(mesage,i,j)
	goto 10
C
C INITIALIZATION ERROR
C
50	print 960
	print 980
	goto 99
60	print 970
	print 980
	goto 99
#ifdef NOCC
960	FORMAT('I can''t open ',RTEXTFILE,'.')
970	FORMAT('I can''t open ',TEXTFILE,'.')
980	FORMAT('Suddenly a sinister, wraithlike figure appears before '
&	'you,'/'seeming to float in the air.  In a low, sorrowful voice'
&	' he says,'/'"Alas, the very nature of the world has changed, '
&	'and the dungeon'/'cannot be found.  All must now pass away."'
&	'  Raising his oaken staff'/'in farewell, he fades into the '
&	'spreading darkness.  In his place'/'appears a tastefully '
&	'lettered sign reading:'//23X,'INITIALIZATION FAILURE'//
&	'The darkness becomes all encompassing, and your vision fails.')
#else NOCC
960	FORMAT(' I can''t open ',RTEXTFILE,'.')
970	FORMAT(' I can''t open ',TEXTFILE,'.')
980	FORMAT(' Suddenly a sinister, wraithlike figure appears before '
&	'you,'/' seeming to float in the air.  In a low, sorrowful voice'
&	' he says,'/' "Alas, the very nature of the world has changed, '
&	'and the dungeon'/' cannot be found.  All must now pass away."'
&	'  Raising his oaken staff'/' in farewell, he fades into the '
&	'spreading darkness.  In his place'/' appears a tastefully '
&	'lettered sign reading:'//23X,'INITIALIZATION FAILURE'//
&	' The darkness becomes all encompassing, and your vision fails.')
#endif NOCC
99	stop
	end
C
C RSPSB2-- OUTPUT RANDOM MESSAGE WITH SUBSTITUTABLE ARGUMENTS
C
C CALLED BY--
C
C	CALL RSPSB2(MSGNUM,S1,S2)
C
	SUBROUTINE    RSPSB2(A,B,C)
	IMPLICIT      INTEGER(A-Z)
	CHARACTER*74  B1,B2,B3
	INTEGER*2     OLDREC,NEWREC,JREC
C
C DECLARATIONS
C
C
	COMMON /RMSG/ MLNT,RTEXT(1050)
	COMMON /CHAN/ INPCH,OUTCH,DBCH
C
C CONVERT ALL ARGUMENTS FROM DICTIONARY NUMBERS (IF POSITIVE)
C TO ABSOLUTE RECORD NUMBERS.
C
	X=A					
	Y=B
	Z=C
	IF(X.GT.0) X=RTEXT(X)			
	IF(Y.GT.0) Y=RTEXT(Y)
	IF(Z.GT.0) Z=RTEXT(Z)
	X=IABS(X)				
	Y=IABS(Y)
	Z=IABS(Z)
	IF(X.EQ.0) RETURN			
C
	READ(UNIT=DBCH,REC=X) OLDREC,B1
C
100	DO 150 I=1,74
	  X1=and(X,31)+I
	  B1(I:I)=char(xor(ichar(B1(I:I)),X1))
150	CONTINUE
C
200	IF(Y.EQ.0) GO TO 400			
	DO 300 I=1,74				
	  IF(B1(I:I).EQ.'#') GO TO 1000
300	CONTINUE
C
400	DO 500 I=74,1,-1			
	  IF(B1(I:I).NE.' ') GO TO 600
500	CONTINUE
C
C  600	WRITE(OUTCH,650) (B1(J:J),J=1,I)		
600	PRINT 650, (B1(J:J),J=1,I)		
#ifdef NOCC
650	FORMAT(74A1)
#else NOCC
650	FORMAT(1X,74A1)
#endif NOCC
	X=X+1					
	READ(UNIT=DBCH,REC=X) NEWREC,B1
	IF(OLDREC.EQ.NEWREC) GO TO 100		
	RETURN					
C
C SUBSTITUTION WITH SUBSTITUTABLE AVAILABLE.
C I IS INDEX OF # IN B1.
C Y IS NUMBER OF RECORD TO SUBSTITUTE.
C
C PROCEDURE:
C   1) COPY REST OF B1 TO B2
C   2) READ SUBSTITUTABLE OVER B1
C   3) RESTORE TAIL OF ORIGINAL B1
C
C THE IMPLICIT ASSUMPTION HERE IS THAT THE SUBSTITUTABLE STRING
C IS VERY SHORT (i.e. MUCH LESS THAN ONE RECORD).
C
1000	K2=1					
	DO 1100 K1=I+1,74			
	  B2(K2:K2)=B1(K1:K1)
	  K2=K2+1
1100	CONTINUE
C
C   READ SUBSTITUTE STRING INTO B3, AND DECRYPT IT:
C
	READ(UNIT=DBCH,REC=Y) JREC,B3
	DO 1150 K1=1,74
	  X1=and(Y,31)+K1
	  B3(K1:K1)=char(xor(ICHAR(B3(K1:K1)),X1))
1150	CONTINUE
C
C   FILL REMAINDER OF B1 WITH CHARACTERS FROM B3:
C
	K2=1
	DO 1180 K1=I,74
	  B1(K1:K1)=B3(K2:K2)
	  K2=K2+1
1180	CONTINUE
C
C   FIND END OF SUBSTITUTE STRING IN B1:
C
	DO 1200 J=74,1,-1			
	  IF(B1(J:J).NE.' ') GO TO 1300
1200	CONTINUE
C
C   PUT TAIL END OF B1 (NOW IN B2) BACK INTO B1 AFTER SUBSTITUTE STRING:
C
1300	K1=1					
	DO 1400 K2=J+1,74			
	  B1(K2:K2)=B2(K1:K1)
	  K1=K1+1
1400	CONTINUE
C
	Y=Z					
	Z=0					
	GO TO 200				
C
	END
	SUBROUTINE LOAD	
	IMPLICIT INTEGER (A-Z)
C
C	load rtext data 
C
C
C MESSAGE INDEX
C
	COMMON /RMSG/ MLNT,RTEXT(1050)
C
C
	rewind 9
C
C	 load the data
C
C
	READ(9,130) RTEXT
130	FORMAT(I8)
	close(9)
C
C
	return
	END
