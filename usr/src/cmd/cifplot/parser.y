
%{
/*******************************************************************
*                                                                  *
*    File: CIFPLOT/parser.y                                        *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/

/* This is a YACC description for the parser */

/* In the offical CIF language description SEMI = BLANKLIST ';' BLANKLIST
 * but this causes ambiguities for the LALR(1) parser so the scanner
 * returns ';' when it spots the pattern BLANKLIST ';'
 */

#include <stdio.h>
#include "defs.h"
#include "globals.h"
#include "structs.h"
#include "alloc.h"

#define yyparse parser
#define yylex scanner

#define null  Concat("",0);
int Definning=0;
int SendAll=0;		/* If set causes the lexical analyzer to return
			 * BLANKLIST and ';' seperately */
int A = 1;
int B = 1;

#define SCALE(x)	( (((real) A) * ((real) x)) / ((real) B ))

%}

%start CIF_FILE
%token BLANK
%token OTHERCHAR
%token COMMENT_COMMAND

%%
CIF_FILE
	:	COMMANDLIST END_COMMAND BLANKLIST
	|	COMMANDLIST END_COMMAND error
					{ Error("Improper Ending",FATAL); }
	|	COMMANDLIST END_COMMAND SEMI
					{ Error("Semi-Colon found after End",
							RECOVERABLE); }
	|	COMMANDLIST
					{ Error("No End Statement",RECOVERABLE); }
	|	COMMANDLIST END_COMMAND BLANKLIST GARBAGE error	
					{ Error("Garbage after End Statement",
							RECOVERABLE); }
	;

COMMANDLIST
	:	COMMANDLIST COMMAND SEMI
	|	COMMANDLIST SEMI
	|	BLANKLIST
	|	COMMANDLIST error 
					{ Error("Unrecognizable Command",FATAL); }
				 SEMI
	;

COMMAND 
	:	FCOMMAND
	;

FCOMMAND	
	:	PRIM_COMMAND
					{ Execute($1);		}
	|	DEF_DELETE_COMMAND
	|	SYMB_LIST DEF_FINNISH_COMMAND
					{ Execute($1);
					  Definning = 0;
					  A=1; B=1;	 	}
	|	ERROR_COMMAND
	;

ERROR_COMMAND
	:	COMMENT_COMMAND 
					{ Error("Comments must end with a semi-colon",
							RECOVERABLE); 		}
			BLANKLIST FCOMMAND			
					{ $$ = $1; }
	|	DEF_FINNISH_COMMAND				
					{ Error("Define Finnished found outside of Definition",
							FATAL); 
					  $$ = $1; 	}
	|	DEF_ERROR		
					{ Error("Unrecognized Definition Command",
							FATAL);	}
	;

SYMB_LIST
	:	SYMB_LIST SYMB_COMMAND
					{ $$ = AddCmd($1,$2);	}
	|	SYMB_LIST SEMI
					{ $$ = $1; }
	|	DEF_START_COMMAND SEMI
					{ Definning = 1;
					  A=(int) ((Command *) $1)->Ctype.Symbl.a;
					  B=(int) ((Command *) $1)->Ctype.Symbl.b;
					  $$ = $1;		}
	|	SYMB_LIST error					
					{ $$ = $1;
					  Error("Unrecognized or Illegal Command in Definition",
							FATAL); }
				 SEMI
	;

SYMB_COMMAND
	:	PRIM_COMMAND SEMI
					{ $$ = $1; }
	|	COMMENT_COMMAND
					{ Error("Comments must end with a semi-colon",RECOVERABLE); }
				BLANKLIST SYMB_COMMAND
					{ $$ = $4;	}
	;

PRIM_COMMAND
	:	POLYGON_COMMAND
	|	BOX_COMMAND
	|	ROUNDFLASH_COMMAND
	|	WIRE_COMMAND
	|	LAYER_COMMAND
	|	CALL_COMMAND
	|	USEREXTENSION_COMMAND
	|	COMMENT_COMMAND
	;

POLYGON_COMMAND	
	:	'P' PATH					
					{ /* Polygons must have more than two
					   * vertices to be well defined */
					  if( ((struct PathHeader *) $2)->PNo < 3)
						Error("Degenerate Polygon",WARNING);
					  /* Polygons with less than two vertices are
					    * useless	*/
					  if( ((struct PathHeader *) $2)->PNo < 2) {
						Error("Command Ignored",WARNING);
						$$ = MakeComment();
						}
					      else
					  	$$ = MakePoly($2);	}
	|	'P' error	
					{ Error("Bad Path Descriptor in Polygon",FATAL);		
					  $$ = MakeComment();	}
	;

BOX_COMMAND
	:	'B' INTEGER SEP INTEGER SEP POINT		
					{ $$ = MakeBox(SCALE($2),SCALE($4),$6,
							MakePoint(1.0,0.0)); 
					  Free($6);		}
	|	'B' INTEGER SEP INTEGER SEP POINT SEP POINT	
					{ if(!CheckPoint($8))
						Error("Bad Direction Vector in Box Command",FATAL);
					  $$ = MakeBox(SCALE($2),SCALE($4),$6,$8);
			  		  Free($6); Free($8);	}
	|	'B' error
					{ Error("Illegal Box Description",FATAL);
					  $$ = MakeComment();	}
	;

ROUNDFLASH_COMMAND 
	:	'R' INTEGER SEP POINT				
					{ $$ = MakeFlash(SCALE($2),$4);}
	|	'R' error
					{ Error("Illegal Round Flash Description",FATAL);
					  $$ = MakeComment();	}
	;

WIRE_COMMAND
	:	'W' INTEGER SEP PATH				
					{ if( ((struct PathHeader *) $4)->PNo < 2) 
						Error("Degenerate Wire",WARNING);
					  $$ = MakeWire(SCALE($2),$4); }
	|	'W' error
					{ Error("Illegal Wire Description",FATAL);
					  $$ = MakeComment();	}
	;

LAYER_COMMAND
	:	'L' BLANKLIST SHORTNAME					
					{ $$ = MakeLayer($3);	}
	|	'L' error
					{ Error("Illegal Layer Command",FATAL);
					  $$ = MakeComment();	}
	;

DEF_START_COMMAND
	:	'D' BLANKLIST 'S' INTEGER
					{ $$ = MakeSymbol($4,1,1);}
	|	'D' BLANKLIST 'S' INTEGER SEP INTEGER SEP INTEGER
					{ $$ = MakeSymbol($4,$6,$8);}
	;

DEF_FINNISH_COMMAND 
	:	'D' BLANKLIST 'F'				
					{ $$ = 0; }
	;

DEF_DELETE_COMMAND 
	:	'D' BLANKLIST 'D' INTEGER			
					{ DeleteDefintion($4);
					  $$ = MakeComment();	}

	;

DEF_ERROR
	:	'D' error
	;

CALL_COMMAND
	:	'C' INTEGER TRANSFORMATION			
					{ $$ = MakeCall($2,$3);	}
	|	'C' error
					{ Error("Illegal Call Command",FATAL);		
					  $$ = MakeComment();	}
	;

USEREXTENSION_COMMAND 
	:	SENDALL '0' SP NAME
					{ $$ = MakeComment();
					  if(!standard)
					     Include($4);
					   else
					     Error("User Extention Ignored\n",WARNING);
					 SendAll = 0;
					 Free($4);		}
	|	SENDALL '0' 'I' SP NAME
					{ $$ = MakeComment();
					  if(!standard)
					     Include($4);
					   else
					     Error("User Extention Ignored\n",WARNING);
					  SendAll = 0;
					  Free($4);		}
	|	SENDALL '0' 'A' INTEGER SEP INTEGER SEP INTEGER SEP SINTEGER SEP SINTEGER BLANKLIST
					{ if(!standard)
					     $$ = MakeArray($4,$6,$8,SCALE($10),SCALE($12));
					   else {
					     Error("User Extention Ignored\n",WARNING);
					     $$ = MakeComment();
					     }
					  SendAll = 0;		}
	|	SENDALL '1' BLANK USERLIST
					{ $$ = MakeComment();
					  if(!standard)
					     fprintf(stderr,"%s\n",$4);
					   else
					     Error("User Extention Ignored\n",WARNING);
					  SendAll = 0;
					  Free($4);		}
	|	SENDALL '2' SP '"' TEXT '"' TRANSFORMATION
					{ if(!standard)
					     $$ = MakeText($5,$7,'l');
					   else {
					     Error("User Extention Ignored\n",WARNING);
					     $$ = MakeComment();
					     }
					  SendAll = 0;		}
	|	SENDALL '2' 'C' SP '"' TEXT '"' TRANSFORMATION
					{ if(!standard)
					     $$ = MakeText($6,$8,'c');
					   else {
					     Error("User Extention Ignored\n",WARNING);
					     $$ = MakeComment();
					     }
					  SendAll = 0;		}
	|	SENDALL '9' SP NAME
					{ if(!standard)
					     $$ = MakeName($4);
					   else {
					     Error("User Extention Ignored\n",WARNING);
					     $$ = MakeComment();
					     }
					  SendAll = 0;		}
	|	SENDALL '9' '4' SP NAME BLANK POINT
					{ if(!standard) {
					    $$ = MakePointName($5,$7,"all");
					    Free($7);
					    }
					  else {
					     Error("User Extention Ignored\n",WARNING);
					     $$ = MakeComment();
					     }
					  SendAll = 0;		}
	|	SENDALL '9' '4' SP NAME BLANK POINT SP SHORTNAME
					{ if(!standard) {
					    $$ = MakePointName($5,$7,$9);
					    Free($7);
					    }
					  else {
					     Error("User Extention Ignored\n",WARNING);
					     $$ = MakeComment();
					     }
					  SendAll = 0;		}
	|	SENDALL EXTEN_DIGIT  error 	
					{ $$ = MakeComment();
					  Error("Bad Syntax in Extension Command --- Command Ignored",
								WARNING);
					   SendAll = 0;   }
	|	SENDALL OTHER_DIGIT  error 	
					{ $$ = MakeComment();
					  Error("Unimplemented User Extension",
								WARNING);
					  SendAll = 0;     }
	;

EXTEN_DIGIT
	:	'0' | '1' | '2' | '9'
	;

OTHER_DIGIT
	:	'3' | '4' | '5' | '6' | '7' | '8'
	;

SENDALL
	:	EMPTY			{SendAll = 1; }
	;

END_COMMAND	
	:	'E'						
	;

TRANSFORMATION
	:	BLANKLIST 'T' POINT TRANSFORMATION		
					{ $$ = Translate($3,$4);	
					  if(output == NOPLOT)
					      Free(((transform *) $4)->TransString);
					  Free($3);  FreeTransform($4);	}
	|	BLANKLIST 'M' BLANKLIST 'X' TRANSFORMATION	
					{ $$ = Mirror('x',$5);	
					  if(output == NOPLOT)
					      Free(((transform *) $5)->TransString);
					  FreeTransform($5);		}
	|	BLANKLIST 'M' BLANKLIST 'Y' TRANSFORMATION	
					{ $$ = Mirror('y',$5);	
					  if(output == NOPLOT)
					      Free(((transform *) $5)->TransString);
					  FreeTransform($5);		}
	|	BLANKLIST 'R' POINT TRANSFORMATION		
					{ if(!CheckPoint($3))
					      Error("Bad Rotation Vector",FATAL);
					  $$ = Rotate($3,$4);
					  if(output == NOPLOT)
					      Free(((transform *) $4)->TransString);
					  Free($3);  FreeTransform($4);	}
	|	EMPTY						
					{ $$ = MakeTransform();		}
	;

PATH	
	:	PATH SEP POINT					
					{ $$ = AddPath($1,$3);
					  Free($3);		}
	|	POINT
					{ $$ = MakePath($1);		}
	;

POINT	
	:	SINTEGER SEP SINTEGER				
					{ $$ = MakePoint(SCALE($1),SCALE($3));	}
	|	SINTEGER error				
					{ Error("Odd number of values encountered",
							FATAL); 
					  $$ = MakePoint(SCALE($1),0.0);}
	;

SINTEGER
	:	SEPLIST INTEGERD				
					{ $$ = $2; }
	|	SEPLIST '-' INTEGERD				
					{ $$ = - $3;		}
	;

INTEGER	
	:	SEPLIST INTEGERD				
					{ $$ = $2; }
	|	SEPLIST '-' INTEGERD
					{ Error("Expected Positive Integer",FATAL);
					  $$ = $3;			}
	;

INTEGERD
	:	INTEGERD DIGIT					
					{ if ($1 > (0x800000 / 10)) {
						Error("Integers may not exceed 2**24",
								FATAL);
						$$ = 0;
						}
					    else
						$$ = ($1*10)+$2-'0';	}
	|	DIGIT						
					{ $$ = $1 - '0';	}
	;

SHORTNAME
	:	C
	|	C C						
					{ $$ = Concat($1,$2,0);
					  Free($1); Free($2);		 }
	|	C C C						
					{ $$ = Concat($1,$2,$3,0);
					  Free($1); Free($2); Free($3);	}
	|	C C C C						
					{ $$ = Concat($1,$2,$3,$4,0);
					  Free($1); Free($2);
					  Free($3); Free($4);		}
	;

C	
	:	DIGIT						
					{ $$ = MakeString($1); }
	|	UPPERCHAR					
					{ $$ = MakeString($1); }
	;

SEPLIST		
	:	SEPLIST SEP					
	|	EMPTY
	;

SEP	
	:	UPPERCHAR					
	|	BLANK
	|	OTHERCHAR
	;

UPPERCHAR
	:	'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G'
	|	'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N'
	|	'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U'
	|	'V' | 'W' | 'X' | 'Y' | 'Z'
	;

DIGIT
	:	'0' | '1' | '2' | '3' | '4'
	|	'5' | '6' | '7' | '8' | '9'
	;

NAME
	:	NAME NAMECHAR
					{ $$ = Concat($1,$2,0);
					  Free($1); Free($2);		}
	|	NAMECHAR
	;

NAMECHAR
	:	BASECHAR
	|	 '"'						
					{ $$ = MakeString('"'); }
	;

TEXT
	:	TEXT TEXTCHAR				
					{ $$ = Concat($1,$2,0);
					  Free($1); Free($2);		}
	|	EMPTY
					{ $$ = null;		}
	;
		
TEXTCHAR
	:	BLANK
					{ $$ = MakeString($1); }
	|	BASECHAR						
	;

USERLIST
	:	USERLIST USERCHAR				
					{ $$ = Concat($1,$2,0);
					  Free($1); Free($2);		}
	|	EMPTY
					{ $$ = null;		}
	;
		
USERCHAR
	:	BLANK
					{ $$ = MakeString($1); }
	|	BASECHAR						
	|	'"'
					{ $$ = MakeString('"'); }
	;

BASECHAR
	:	DIGIT						
					{ $$ = MakeString($1); }
	|	UPPERCHAR					
					{ $$ = MakeString($1); }
	|	OTHERCHAR					
					{ $$ = MakeString($1); }
	|	'('						
					{ $$ = MakeString('('); }
	| 	')'						
					{ $$ = MakeString(')'); }
	|	 '-'						
					{ $$ = MakeString('-'); }
	;

SP
	:	BLANK BLANKLIST
	;

BLANKLIST
	:	BLANKLIST BLANK
	|	EMPTY
	;

GARBAGE	
	:	DIGIT
	|	UPPERCHAR
	|	'-' | '(' | ')'
	;

SEMI
	:	';' BLANKLIST					
	;


EMPTY	
	:	
	;
%%

#include "scanner.c"

