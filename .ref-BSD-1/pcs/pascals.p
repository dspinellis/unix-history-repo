program pascals(input,output);     (*1.7.75*)
(*$t-,p-  n.wirth,  e.t.h.
	   clausiusstr.55   ch-8006 zurich    *)
#include "globals.i"
#include "error.i"
#include "scanner.i"
#include "tables.i"
#include "block.i"
#include "interpret.i"
  
begin writeln;
   key[ 1] := 'and       '; key[ 2] := 'array     ';
   key[ 3] := 'begin     '; key[ 4] := 'case      ';
   key[ 5] := 'const     '; key[ 6] := 'div       ';
   key[ 8] := 'downto    '; key[ 7] := 'do        ';
   key[ 9] := 'else      '; key[10] := 'end       ';
   key[11] := 'for       '; key[12] := 'function  ';
   key[13] := 'if        '; key[14] := 'mod       ';
   key[15] := 'not       '; key[16] := 'of        ';
   key[17] := 'or        '; key[18] := 'procedure ';
   key[19] := 'program   '; key[20] := 'record    ';
   key[21] := 'repeat    '; key[22] := 'then      ';
   key[23] := 'to        '; key[24] := 'type      ';
   key[25] := 'until     '; key[26] := 'var       ';
   key[27] := 'while     ';
   ksy[ 1] := andsy;        ksy[ 2] := arraysy;
   ksy[ 3] := beginsy;      ksy[ 4] := casesy;
   ksy[ 5] := constsy;      ksy[ 6] := idiv;
   ksy[ 8] := downtosy;     ksy[ 7] := dosy;
   ksy[ 9] := elsesy;       ksy[10] := endsy; 
   ksy[11] := forsy;        ksy[12] := functionsy;
   ksy[13] := ifsy;         ksy[14] := imod;
   ksy[15] := notsy;        ksy[16] := ofsy;
   ksy[17] := orsy;         ksy[18] := proceduresy; 
   ksy[19] := programsy;    ksy[20] := recordsy; 
   ksy[21] := repeatsy;     ksy[22] := thensy;
   ksy[23] := tosy;         ksy[24] := typesy;
   ksy[25] := untilsy;      ksy[26] := varsy; 
   ksy[27] := whilesy;
   sps['+'] := plus;        sps['-'] := minus;
   sps['*'] := times;       sps['/'] := rdiv; 
   sps['('] := lparent;     sps[')'] := rparent; 
   sps['='] := eql;         sps[','] := comma;
   sps['['] := lbrack;      sps[']'] := rbrack;
   sps['#'] := neq;         sps['&'] := andsy;
   sps[';'] := semicolon; 
  constbegsys := [plus,minus,intcon,realcon,charcon,ident]; 
  typebegsys := [ident,arraysy,recordsy]; 
  blockbegsys := [constsy,typesy,varsy,proceduresy,functionsy,beginsy]; 
  facbegsys := [intcon,realcon,charcon,ident,lparent,notsy];
  statbegsys := [beginsy,ifsy,whilesy,repeatsy,forsy,casesy]; 
  stantyps := [notyp,ints,reals,bools,chars]; 
  lc := 0; ll := 0; cc := 0; ch := ' '; 
  errpos := 0; errs := []; insymbol;
  t := -1; a := 0; b := 1; sx := 0; c2 := 0;
  display[0] := 1; 
  iflag := false; oflag := false; 
  if sy <> programsy then error(3) else 
  begin insymbol;
    if sy <> ident then error(2) else
    begin progname := id; insymbol; 
      if sy <> lparent then error(9) else
      repeat insymbol;
	if sy <> ident then error(2) else
	begin if id = 'input     ' then iflag := true else 
	      if id = 'output    ' then oflag := true else error(0); 
	   insymbol 
	end 
      until sy <> comma; 
      if sy = rparent then insymbol else error(4);
      if not oflag then error(20)
    end
  end ;
  enter('          ', variable, notyp, 0);  (*sentinel*)
  enter('false     ', konstant, bools, 0);
  enter('true      ', konstant, bools, 1);
  enter('real      ', type1, reals, 1); 
  enter('char      ', type1, chars, 1); 
  enter('boolean   ', type1, bools, 1); 
  enter('integer   ', type1, ints , 1); 
  enter('abs       ', funktion, reals,0); 
  enter('sqr       ', funktion, reals,2); 
  enter('odd       ', funktion, bools,4); 
  enter('chr       ', funktion, chars,5); 
  enter('ord       ', funktion, ints, 6); 
  enter('succ      ', funktion, chars,7); 
  enter('pred      ', funktion, chars,8); 
  enter('round     ', funktion, ints, 9); 
  enter('trunc     ', funktion, ints, 10);
  enter('sin       ', funktion, reals, 11); 
  enter('cos       ', funktion, reals, 12); 
  enter('exp       ', funktion, reals, 13); 
  enter('ln        ', funktion, reals, 14); 
  enter('sqrt      ', funktion, reals, 15); 
  enter('arctan    ', funktion, reals, 16); 
  enter('eof       ', funktion, bools, 17); 
  enter('eoln      ', funktion, bools, 18); 
  enter('read      ', prozedure, notyp, 1); 
  enter('readln    ', prozedure, notyp, 2); 
  enter('write     ', prozedure, notyp, 3); 
  enter('writeln   ', prozedure, notyp, 4); 
  enter('          ', prozedure, notyp, 0); 
  with btab[1] do
    begin last := t; lastpar := 1; psize := 0; vsize := 0
    end ; 
  
  block(blockbegsys+statbegsys, false, 1);
  if sy <> period then error(22);
  emit(31);  (*halt*) 
  if btab[2].vsize > stacksize then error(49);
  if progname = 'test0     ' then printtables;
  
  if errs = [] then 
  begin
{ must block all of this out for now.
    if iflag then
    begin  getseg(input); 
      if eof(input) then writeln(' input data missing') else
      begin writeln(' (eor)'); (*copy input data*)
	while not eof(input) do
	begin write(' '); 
	  while not eoln(input) do 
	    begin read(ch); write(ch)
	    end ;
	  writeln; read(ch)
	end ; 
	getseg(input,0) 
      end 
    end ; 
 all this because of segmented file repositioning!! }
    writeln(' (eof)');
    interpret 
  end 
  else errormsg;
99: 
end .
