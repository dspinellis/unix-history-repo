%term	LCURL RCURL LPAR RPAR SCOL DIGITS
%term	XIF XELSE XFOR XWHILE XBREAK NEXT 
%term	OLDDO NEWDO
%term	XGOK XDEFINE XINCLUDE
%term	REPEAT UNTIL
%%

statl	: statl  stat
	|
	;
stat	: if stat	={ outcont($1); }
	| ifelse stat	={ outcont($1+1); }
	| while stat	={ whilestat($1); }
	| for stat	={ forstat($1); }
	| repeat stat UNTIL	={ untils($1); }
	| XBREAK	={ breakcode($1); }
	| NEXT		={ nextcode($1); }
	| newdo stat	={ dostat($1); }
	| OLDDO		={ docode(0,$1); }
	| XGOK		={ gokcode($1); }
	| SCOL
	| LCURL statl RCURL
	| label stat
	| error		={ errcode($1); yyclearin; }
	;
label	: DIGITS	={ outcode($1); outcode("\t"); }
	;
if	: XIF		={ ifcode($1); }
	;
ifelse	: if stat XELSE	={ outgoto($1+1); outcont($1); }
	;
while	: XWHILE	={ whilecode($1); }
	;
for	: XFOR		={ forcode($1); }
	;
repeat	: REPEAT	={ repcode($1); }
	;
newdo	: NEWDO		={ docode(1,$1); }
	;
%%
