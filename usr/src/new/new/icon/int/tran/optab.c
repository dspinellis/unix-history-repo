#include "lex.h"

/*
 * state tables for operator recognition
 */

struct optab state0[] = {       /* initial state */
   { ',', A_IMMRET, &toktab[ 59] },      /* ","     */
   { '.', A_IMMRET, &toktab[ 63] },      /* "."     */
   { '[', A_IMMRET, &toktab[ 70] },      /* "["     */
   { ']', A_IMMRET, &toktab[ 99] },      /* "]"     */
   { '(', A_IMMRET, &toktab[ 79] },      /* "("     */
   { ')', A_IMMRET, &toktab[100] },      /* ")"     */
   { ';', A_IMMRET, &toktab[101] },      /* ";"     */
   { '{', A_IMMRET, &toktab[ 69] },      /* "{"     */
   { '}', A_IMMRET, &toktab[ 98] },      /* "}"     */
   { '!', A_IMMRET, &toktab[ 54] },      /* "!"     */
   { '\\',A_IMMRET, &toktab[ 53] },      /* "\\"    */
   { ':', A_GOTO,   state1       },      /* ":" ... */
   { '<', A_GOTO,   state2       },      /* "<" ... */
   { '>', A_GOTO,   state4       },      /* ">" ... */
   { '=', A_GOTO,   state5       },      /* "=" ... */
   { '|', A_GOTO,   state3       },      /* "|" ... */
   { '+', A_GOTO,   state7       },      /* "+" ... */
   { '-', A_GOTO,   state8       },      /* "-" ... */
   { '*', A_GOTO,   state9       },      /* "*" ... */
   { '^', A_GOTO,   state6       },      /* "^" ... */
   { '~', A_GOTO,   state29      },      /* "~" ... */
   { '/', A_GOTO,   state21      },      /* "/" ... */
   { '%', A_GOTO,   state30      },      /* "%" ... */
   { '?', A_GOTO,   state36      },      /* "?" ... */
   { '&', A_GOTO,   state38      },      /* "&" ... */
   { '@', A_GOTO,   state40      },      /* "@" ... */
   { 0,   A_ERROR,  0            }
   };

struct optab state1[] = {       /* ":" */
   { '=', A_GOTO,   state10      },      /* ":=" ... */
   { 0,   A_RETURN, &toktab[ 58] }       /* ":"      */
   };

struct optab state2[] = {       /* "<" */
   { '-', A_GOTO,   state11      },      /* "<-" ... */
   { '<', A_GOTO,   state32      },      /* "<<" ... */
   { ':', A_GOTO,   state46      },      /* "<:" ... */
   { '=', A_GOTO,   state56      },      /* "<=" ... */
   { 0,   A_RETURN, &toktab[ 90] }       /* "<"      */
   };

struct optab state3[] = {       /* "|" */
   { '|', A_GOTO,   state22      },      /* "||" ... */
   { 0,   A_RETURN, &toktab[ 55] }       /* "|"      */
   };

struct optab state4[] = {       /* ">" */
   { '>', A_GOTO,   state33      },      /* ">>" ... */
   { ':', A_GOTO,   state44      },      /* ">:" ... */
   { '=', A_GOTO,   state57      },      /* ">=" ... */
   { 0,   A_RETURN, &toktab[ 88] }       /* ">"      */
   };

struct optab state5[] = {        /* "=" */
   { '=', A_GOTO,   state12      },      /* "==" ... */
   { ':', A_GOTO,   state42      },      /* "=:" ... */
   { 0,   A_RETURN, &toktab[ 86] }       /* "="      */
   };

struct optab state6[] = {        /* "^" */
   { ':', A_GOTO,   state23      },      /* "^:" ... */
   { 0,   A_RETURN, &toktab[ 56] }       /* "^"      */
   };

struct optab state7[] = {       /* "+" */
   { ':', A_GOTO,   state15      },      /* "+:" ... */
   { '+', A_GOTO,   state16      },      /* "++" ... */
   { 0,   A_RETURN, &toktab[ 93] }       /* "+"      */
   };

struct optab state8[] = {        /* "-" */
   { ':', A_GOTO,   state17      },      /* "-:" ... */
   { '-', A_GOTO,   state18      },      /* "--" ... */
   { 0,   A_RETURN, &toktab[ 81] }       /* "-"      */
   };

struct optab state9[] = {        /* "*" */
   { ':', A_GOTO,   state19      },      /* "*:" ... */
   { '*', A_GOTO,   state20      },      /* "**" ... */
   { 0,   A_RETURN, &toktab[105] }       /* "*"      */
   };

struct optab state10[] = {       /* ":=" */
   { ':', A_IMMRET, &toktab[107] },      /* ":=:" */
   { 0,   A_RETURN, &toktab[ 35] }       /* ":="  */
   };

struct optab state11[] = {       /* "<-" */
   { '>', A_IMMRET, &toktab[ 97] },      /* "<->" */
   { 0,   A_RETURN, &toktab[ 96] }       /* "<-"  */
   };

struct optab state12[] = {       /* "==" */
   { '=', A_GOTO,   state61      },      /* "===" ... */
   { ':', A_GOTO,   state48      },      /* "==:" ... */
   { 0,   A_RETURN, &toktab[ 73] }       /* "=="  */
   };

struct optab state13[] = {       /* "~=" */
   { '=', A_GOTO,   state14      },      /* "~==" ... */
   { ':', A_GOTO,   state43      },      /* "~=:" ... */
   { 0,   A_RETURN, &toktab[ 91] }       /* "~="      */
   };

struct optab state14[] = {       /* "~==" */
   { ':', A_GOTO,   state49      },      /* "~==:" ... */
   { '=', A_GOTO,   state60      },      /* "~===" ... */
   { 0,   A_RETURN, &toktab[ 78] }       /* "~=="  */
   };

struct optab state15[] = {       /* "+:" */
   { '=', A_IMMRET, &toktab[ 94] },      /* "+:=" */
   { 0,   A_RETURN, &toktab[ 92] }       /* "+:"  */
   };

struct optab state16[] = {       /* "++" */
   { ':', A_GOTO,   state24      },      /* "++:" ... */
   { 0,   A_RETURN, &toktab[109] }       /* "++"      */
   };

struct optab state17[] = {       /* "-:" */
   { '=', A_IMMRET, &toktab[ 82] },      /* "-:=" */
   { 0,   A_RETURN, &toktab[ 80] }       /* "-:"  */
   };

struct optab state18[] = {       /* "--" */
   { ':', A_GOTO,   state25      },      /* "--:" ... */
   { 0,   A_RETURN, &toktab[ 64] }       /* "--" */
   };

struct optab state19[] = {      /* "*:" */
   { '=', A_IMMRET, &toktab[106] },      /* "*:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state20[] = {       /* "**" */
   { ':', A_GOTO,   state26      },      /* "**:" ... */
   { 0,   A_RETURN, &toktab[ 67] }       /* "**"      */
   };

struct optab state21[] = {       /* "/" */
   { ':', A_GOTO,   state27      },      /* "/:" ... */
   { 0,   A_RETURN, &toktab[103] }       /* "/"      */
   };

struct optab state22[] = {       /* "||" */
   { ':', A_GOTO,   state28      },      /* "||:" ... */
   { '|', A_GOTO,   state34      },      /* "|||" ... */
   { 0,   A_RETURN, &toktab[ 60] }       /* "||"      */
   };

struct optab state23[] = {       /* "^:" */
   { '=', A_IMMRET, &toktab[ 57] },      /* "^:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state24[] = {       /* "++:" */
   { '=', A_IMMRET, &toktab[110] },      /* "++:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state25[] = {       /* "--:" */
   { '=', A_IMMRET, &toktab[ 65] },      /* "--:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state26[] = {       /* "**:" */
   { '=', A_IMMRET, &toktab[ 68] },      /* "**:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state27[] = {       /* "/:" */
   { '=', A_IMMRET, &toktab[104] },      /* "/:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state28[] = {      /* "||:" */
   { '=', A_IMMRET, &toktab[ 61] },      /* "||:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state29[] = {       /* "~" */
   { '=', A_GOTO,   state13      },      /* "~=" ... */
   { 0,   A_RETURN, &toktab[108] }       /* "~"      */
   };

struct optab state30[] = {       /* "%" */
   { ':', A_GOTO,   state31      },      /* "%:" ... */
   { 0,   A_RETURN, &toktab[ 83] }       /* "%"      */
   };

struct optab state31[] = {       /* "%:" */
   { '=', A_IMMRET, &toktab[ 84] },      /* "%:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state32[] = {       /* "<<" */
   { ':', A_GOTO,   state52      },      /* "<<:" ... */
   { '=', A_GOTO,   state58      },      /* "<<=" ... */
   { 0,   A_RETURN, &toktab[ 77] }       /* "<<"     */
   };

struct optab state33[] = {       /* ">>" */
   { ':', A_GOTO,   state50      },      /* ">>:" ... */
   { '=', A_GOTO,   state59      },      /* ">>=" ... */
   { 0,   A_RETURN, &toktab[ 75] }       /* ">>"     */
   };

struct optab state34[] = {       /* "|||" */
   { ':', A_GOTO,   state35      },      /* "|||:" ... */
   { 0,   A_RETURN, &toktab[ 71] }       /* "|||"      */
   };

struct optab state35[] = {       /* "|||:" */
   { '=', A_IMMRET, &toktab[ 72] },      /* "|||:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state36[] = {        /* "?" */
   { ':', A_GOTO,   state37      },      /* "?:" ... */
   { 0,   A_RETURN, &toktab[ 95] }       /* "?"      */
   };

struct optab state37[] = {       /* "?:" */
   { '=', A_IMMRET, &toktab[102] },      /* "?:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state38[] = {        /* "&" */
   { ':', A_GOTO,   state39      },      /* "&:" ... */
   { 0,   A_RETURN, &toktab[ 62] }       /* "&"      */
   };

struct optab state39[] = {       /* "&:" */
   { '=', A_IMMRET, &toktab[ 38] },      /* "&:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state40[] = {        /* "@" */
   { ':', A_GOTO,   state41      },      /* "@:" ... */
   { 0,   A_RETURN, &toktab[ 36] }       /* "@"      */
   };

struct optab state41[] = {      /* "@:" */
   { '=', A_IMMRET, &toktab[ 37] },      /* "@:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state42[] = {       /* "=:" */
   { '=', A_IMMRET, &toktab[ 39] },      /* "=:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state43[] = {       /* "~=:" */
   { '=', A_IMMRET, &toktab[ 45] },      /* "~=:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state44[] = {       /* ">:" */
   { '=', A_IMMRET, &toktab[ 42] },      /* ">:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state45[] = {       /* ">=:" */
   { '=', A_IMMRET, &toktab[ 41] },      /* ">=:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state46[] = {      /* "<:" */
   { '=', A_IMMRET, &toktab[ 44] },      /* "<:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state47[] = {       /* "<=:" */
   { '=', A_IMMRET, &toktab[ 43] },      /* "<=:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state48[] = {       /* "==:" */
   { '=', A_IMMRET, &toktab[ 47] },      /* "==:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state49[] = {       /* "~==:" */
   { '=', A_IMMRET, &toktab[ 52] },      /* "~==:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state50[] = {      /* ">>:" */
   { '=', A_IMMRET, &toktab[ 49] },      /* ">>:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state51[] = {       /* ">>=:" */
   { '=', A_IMMRET, &toktab[ 48] },      /* ">>=:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state52[] = {       /* "<<:" */
   { '=', A_IMMRET, &toktab[ 51] },      /* "<<:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state53[] = {       /* "<<=:" */
   { '=', A_IMMRET, &toktab[ 50] },      /* "<<=:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state54[] = {      /* "===:" */
   { '=', A_IMMRET, &toktab[ 40] },      /* "===:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state55[] = {       /* "~===:" */
   { '=', A_IMMRET, &toktab[ 46] },      /* "~===:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state56[] = {        /* "<=" */
   { ':', A_GOTO,   state47      },      /* "<=:" ... */
   { 0,   A_RETURN, &toktab[ 89] }       /* "<="      */
   };

struct optab state57[] = {        /* ">=" */
   { ':', A_GOTO,   state45      },      /* ">=:" ... */
   { 0,   A_RETURN, &toktab[ 87] }       /* ">="      */
   };

struct optab state58[] = {        /* "<<=" */
   { ':', A_GOTO,   state53      },      /* "<<=:" ... */
   { 0,   A_RETURN, &toktab[ 76] }       /* "<<="      */
   };

struct optab state59[] = {       /* ">>=" */
   { ':', A_GOTO,   state51     },      /* ">>=:" ... */
   { 0,   A_RETURN, &toktab[ 74] }       /* ">>="      */
   };

struct optab state60[] = {        /* "~===" */
   { ':', A_GOTO,   state55      },      /* "~===:" ... */
   { 0,   A_RETURN, &toktab[ 85] }       /* "~==="      */
   };

struct optab state61[] = {        /* "===" */
   { ':', A_GOTO,   state54      },      /* "===:" ... */
   { 0,   A_RETURN, &toktab[ 66] }       /* "==="      */
   };
