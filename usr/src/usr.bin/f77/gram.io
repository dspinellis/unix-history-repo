  /*  Input/Output Statements */

io:	  io1
		{ endio(); }
	;

io1:	  iofmove ioctl
	| iofmove unpar_fexpr
		{ ioclause(IOSUNIT, $2); endioctl(); }
	| iofmove SSTAR
		{ ioclause(IOSUNIT, PNULL); endioctl(); }
	| iofmove SPOWER
		{ ioclause(IOSUNIT, IOSTDERR); endioctl(); }
	| iofctl ioctl
	| read ioctl
		{ doio(PNULL); }
	| read infmt
		{ doio(PNULL); }
	| read ioctl inlist
		{ doio($3); }
	| read infmt SCOMMA inlist
		{ doio($4); }
	| read ioctl SCOMMA inlist
		{ doio($4); }
	| write ioctl
		{ doio(PNULL); }
	| write ioctl outlist
		{ doio($3); }
	| print
		{ doio(PNULL); }
	| print SCOMMA outlist
		{ doio($3); }
	;

iofmove:   fmkwd end_spec in_ioctl
	;

fmkwd:	  SBACKSPACE
		{ iostmt = IOBACKSPACE; }
	| SREWIND
		{ iostmt = IOREWIND; }
	| SENDFILE
		{ iostmt = IOENDFILE; }
	;

iofctl:  ctlkwd end_spec in_ioctl
	;

ctlkwd:	  SINQUIRE
		{ iostmt = IOINQUIRE; }
	| SOPEN
		{ iostmt = IOOPEN; }
	| SCLOSE
		{ iostmt = IOCLOSE; }
	;

infmt:	  unpar_fexpr
		{
		ioclause(IOSUNIT, PNULL);
		ioclause(IOSFMT, $1);
		endioctl();
		}
	| SSTAR
		{
		ioclause(IOSUNIT, PNULL);
		ioclause(IOSFMT, PNULL);
		endioctl();
		}
	;

ioctl:	  SLPAR fexpr SRPAR
		{ if($2->headblock.vtype == TYCHAR)
			{
			ioclause(IOSUNIT, PNULL);
			ioclause(IOSFMT, $2);
			}
		  else
			ioclause(IOSUNIT, $2);
		  endioctl();
		}
	| SLPAR ctllist SRPAR
		{ endioctl(); }
	;

ctllist:  ioclause
	| ctllist SCOMMA ioclause
	;

ioclause:  fexpr
		{ ioclause(IOSPOSITIONAL, $1); }
	| SSTAR
		{ ioclause(IOSPOSITIONAL, PNULL); }
	| SPOWER
		{ ioclause(IOSPOSITIONAL, IOSTDERR); }
	| nameeq expr
		{ ioclause($1, $2); }
	| nameeq SSTAR
		{ ioclause($1, PNULL); }
	| nameeq SPOWER
		{ ioclause($1, IOSTDERR); }
	;

nameeq:  SNAMEEQ
		{ $$ = iocname(); }
	;

read:	  SREAD end_spec in_ioctl
		{ iostmt = IOREAD; }
	;

write:	  SWRITE end_spec in_ioctl
		{ iostmt = IOWRITE; }
	;

print:	  SPRINT end_spec fexpr in_ioctl
		{
		iostmt = IOWRITE;
		ioclause(IOSUNIT, PNULL);
		ioclause(IOSFMT, $3);
		endioctl();
		}
	| SPRINT end_spec SSTAR in_ioctl
		{
		iostmt = IOWRITE;
		ioclause(IOSUNIT, PNULL);
		ioclause(IOSFMT, PNULL);
		endioctl();
		}
	;

inlist:	  inelt
		{ $$ = mkchain($1, CHNULL); }
	| inlist SCOMMA inelt
		{ $$ = hookup($1, mkchain($3, CHNULL)); }
	;

inelt:	  lhs
		{ $$ = (tagptr) $1; }
	| SLPAR inlist SCOMMA dospec SRPAR
		{ $$ = (tagptr) mkiodo($4,$2); }
	;

outlist:  uexpr
		{ $$ = mkchain($1, CHNULL); }
	| other
		{ $$ = mkchain($1, CHNULL); }
	| out2
	;

out2:	  uexpr SCOMMA uexpr
		{ $$ = mkchain($1, mkchain($3, CHNULL) ); }
	| uexpr SCOMMA other
		{ $$ = mkchain($1, mkchain($3, CHNULL) ); }
	| other SCOMMA uexpr
		{ $$ = mkchain($1, mkchain($3, CHNULL) ); }
	| other SCOMMA other
		{ $$ = mkchain($1, mkchain($3, CHNULL) ); }
	| out2  SCOMMA uexpr
		{ $$ = hookup($1, mkchain($3, CHNULL) ); }
	| out2  SCOMMA other
		{ $$ = hookup($1, mkchain($3, CHNULL) ); }
	;

other:	  complex_const
		{ $$ = (tagptr) $1; }
	| SLPAR uexpr SCOMMA dospec SRPAR
		{ $$ = (tagptr) mkiodo($4, mkchain($2, CHNULL) ); }
	| SLPAR other SCOMMA dospec SRPAR
		{ $$ = (tagptr) mkiodo($4, mkchain($2, CHNULL) ); }
	| SLPAR out2  SCOMMA dospec SRPAR
		{ $$ = (tagptr) mkiodo($4, $2); }
	;

in_ioctl:
		{ startioctl(); }
	;
