label 99; 
const nkw = 27;     (*no. of key words*)
	TAB = tab;
    alng =  10;     (*no. of significant chars in identifiers*) 
    llng = 120;     (*input line length*) 
    emax = 322;     (*max exponent of real numbers*)
    emin =-292;     (*min exponent*)
    kmax =  15;     (*max no. of significant digits*) 
    tmax = 100;     (*size of table*)
    bmax =  20;     (*size of block-table*) 
    amax =  30;     (*size of array-table*) 
    c2max = 20;     (*size of real constant table*) 
    csmax = 30;     (*max no. of cases*)
    cmax = 500;     (*size of code*)
    lmax =   7;     (*maximum level*)
    smax = 400;     (*size of string-table*)
    ermax = 58;     (*max error no.*)
    omax =  66;     (*highest order code*)
    xmax = 131071;  (*2**17 - 1*) 
{	shit, look at this number!!!!!
    nmax = 281474976710655;   (*2**48-1*) 
}
   nmax = maxint;
    lineleng = 136; (*output line length*)
    linelimit = 200;
    stacksize = 200; 
  
type symbol = (intcon,realcon,charcon,string, 
	       notsy,plus,minus,times,idiv,rdiv,imod,andsy,orsy,
	       eql,neq,gtr,geq,lss,leq, 
	       lparent,rparent,lbrack,rbrack,comma,semicolon,period, 
	       colon,becomes,constsy,typesy,varsy,functionsy, 
	       proceduresy,arraysy,recordsy,programsy,ident,
	       beginsy,ifsy,casesy,repeatsy,whilesy,forsy, 
	       endsy,elsesy,untilsy,ofsy,dosy,tosy,downtosy,thensy); 
  
    index  = -xmax .. +xmax; 
    alfa = packed array [1..alng] of char;
    object = (konstant,variable,type1,prozedure,funktion); 
    types  = (notyp,ints,reals,bools,chars,arrays,records); 
    symset = set of symbol;
    typset = set of types;
    item   = record 
	       typ: types; ref: index; 
	     end ; 
    order  = packed record
	       f: -omax..+omax; 
	       x: -lmax..+lmax; 
	       y: -nmax..+nmax; 
	     end ; 
  
var sy: symbol;          (*last symbol read by insymbol*)
    id: alfa;            (*identifier from insymbol*) 
    inum: integer;       (*integer from insymbol*)
    rnum: real;          (*real number from insymbol*)
    sleng: integer;      (*string length*)
    ch: char;            (*last character read from source program*) 
    line: array [1..llng] of char;
    cc: integer;         (*character counter*)
    lc: integer;         (*program location counter*) 
    ll: integer;         (*length of current line*) 
    errs: set of 0..ermax;
    errpos: integer;
    progname: alfa; 
    iflag, oflag: boolean;
    constbegsys,typebegsys,blockbegsys,facbegsys,statbegsys: symset; 
    key: array [1..nkw] of alfa;
    ksy: array [1..nkw] of symbol;
    sps: array [char] of symbol;  (*special symbols*) 
  
    t,a,b,sx,c1,c2: integer;  (*indices to tables*) 
    stantyps: typset; 
    display: array [0 .. lmax] of integer;
  
    tab:     array [0 .. tmax] of     (*identifier table*) 
	       packed record 
		 name: alfa;  link: index;
		 obj: object; typ: types; 
		 ref: index;  normal: boolean;
		 lev: 0 .. lmax; adr: integer;
	       end ;
    atab:    array [1 .. amax] of     (*array-table*) 
	       packed record 
		 inxtyp, eltyp: types; 
		 elref, low, high, elsize, size: index; 
	       end ;
    btab:    array [1 .. bmax] of    (*block-table*)
	       packed record 
		  last, lastpar, psize, vsize: index
	       end ;
    stab:    packed array [0..smax] of char;  (*string table*)
    rconst:  array [1 .. c2max] of real;
    code:    array [0 .. cmax] of order;
