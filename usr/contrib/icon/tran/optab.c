#include "lex.h"

/*
 * State tables for operator recognition.
 */

struct optab state0[] = {       /* initial state */
   { ',', A_IMMRET, (int) &toktab[ 59] },      /* ","     */
   { '.', A_IMMRET, (int) &toktab[ 63] },      /* "."     */
   { '[', A_IMMRET, (int) &toktab[ 70] },      /* "["     */
   { ']', A_IMMRET, (int) &toktab[ 99] },      /* "]"     */
   { '(', A_IMMRET, (int) &toktab[ 79] },      /* "("     */
   { ')', A_IMMRET, (int) &toktab[100] },      /* ")"     */
   { ';', A_IMMRET, (int) &toktab[101] },      /* ";"     */
   { '{', A_IMMRET, (int) &toktab[ 69] },      /* "{"     */
   { '}', A_IMMRET, (int) &toktab[ 98] },      /* "}"     */
   { '!', A_IMMRET, (int) &toktab[ 54] },      /* "!"     */
   { '\\', A_IMMRET, (int) &toktab[ 53] },      /* "\\"    */
   { ':', A_GOTO,   (int) state1       },      /* ":" ... */
   { '<', A_GOTO,   (int) state2       },      /* "<" ... */
   { '>', A_GOTO,   (int) state4       },      /* ">" ... */
   { '=', A_GOTO,   (int) state5       },      /* "=" ... */
   { '|', A_GOTO,   (int) state3       },      /* "|" ... */
   { '+', A_GOTO,   (int) state7       },      /* "+" ... */
   { '-', A_GOTO,   (int) state8       },      /* "-" ... */
   { '*', A_GOTO,   (int) state9       },      /* "*" ... */
   { '^', A_GOTO,   (int) state6       },      /* "^" ... */
   { '~', A_GOTO,   (int) state29      },      /* "~" ... */
   { '/', A_GOTO,   (int) state21      },      /* "/" ... */
   { '%', A_GOTO,   (int) state30      },      /* "%" ... */
   { '?', A_GOTO,   (int) state36      },      /* "?" ... */
   { '&', A_GOTO,   (int) state38      },      /* "&" ... */
   { '@', A_GOTO,   (int) state40      },      /* "@" ... */
   { 0,   A_ERROR,  0            }
   };

struct optab state1[] = {       /* ":" */
   { '=', A_GOTO,   (int) state10      },      /* ":=" ... */
   { 0,   A_RETURN, (int) &toktab[ 58] }       /* ":"      */
   };

struct optab state2[] = {       /* "<" */
   { '-', A_GOTO,   (int) state11      },      /* "<-" ... */
   { '<', A_GOTO,   (int) state32      },      /* "<<" ... */
   { ':', A_GOTO,   (int) state46      },      /* "<:" ... */
   { '=', A_GOTO,   (int) state56      },      /* "<=" ... */
   { 0,   A_RETURN, (int) &toktab[ 90] }       /* "<"      */
   };

struct optab state3[] = {       /* "|" */
   { '|', A_GOTO,   (int) state22      },      /* "||" ... */
   { 0,   A_RETURN, (int) &toktab[ 55] }       /* "|"      */
   };

struct optab state4[] = {       /* ">" */
   { '>', A_GOTO,   (int) state33      },      /* ">>" ... */
   { ':', A_GOTO,   (int) state44      },      /* ">:" ... */
   { '=', A_GOTO,   (int) state57      },      /* ">=" ... */
   { 0,   A_RETURN, (int) &toktab[ 88] }       /* ">"      */
   };

struct optab state5[] = {        /* "=" */
   { '=', A_GOTO,   (int) state12      },      /* "==" ... */
   { ':', A_GOTO,   (int) state42      },      /* "=:" ... */
   { 0,   A_RETURN, (int) &toktab[ 86] }       /* "="      */
   };

struct optab state6[] = {        /* "^" */
   { ':', A_GOTO,   (int) state23      },      /* "^:" ... */
   { 0,   A_RETURN, (int) &toktab[ 56] }       /* "^"      */
   };

struct optab state7[] = {       /* "+" */
   { ':', A_GOTO,   (int) state15      },      /* "+:" ... */
   { '+', A_GOTO,   (int) state16      },      /* "++" ... */
   { 0,   A_RETURN, (int) &toktab[ 93] }       /* "+"      */
   };

struct optab state8[] = {        /* "-" */
   { ':', A_GOTO,   (int) state17      },      /* "-:" ... */
   { '-', A_GOTO,   (int) state18      },      /* "--" ... */
   { 0,   A_RETURN, (int) &toktab[ 81] }       /* "-"      */
   };

struct optab state9[] = {        /* "*" */
   { ':', A_GOTO,   (int) state19      },      /* "*:" ... */
   { '*', A_GOTO,   (int) state20      },      /* "**" ... */
   { 0,   A_RETURN, (int) &toktab[105] }       /* "*"      */
   };

struct optab state10[] = {       /* ":=" */
   { ':', A_IMMRET, (int) &toktab[107] },      /* ":=:" */
   { 0,   A_RETURN, (int) &toktab[ 35] }       /* ":="  */
   };

struct optab state11[] = {       /* "<-" */
   { '>', A_IMMRET, (int) &toktab[ 97] },      /* "<->" */
   { 0,   A_RETURN, (int) &toktab[ 96] }       /* "<-"  */
   };

struct optab state12[] = {       /* "==" */
   { '=', A_GOTO,   (int) state61      },      /* "===" ... */
   { ':', A_GOTO,   (int) state48      },      /* "==:" ... */
   { 0,   A_RETURN, (int) &toktab[ 73] }       /* "=="  */
   };

struct optab state13[] = {       /* "~=" */
   { '=', A_GOTO,   (int) state14      },      /* "~==" ... */
   { ':', A_GOTO,   (int) state43      },      /* "~=:" ... */
   { 0,   A_RETURN, (int) &toktab[ 91] }       /* "~="      */
   };

struct optab state14[] = {       /* "~==" */
   { ':', A_GOTO,   (int) state49      },      /* "~==:" ... */
   { '=', A_GOTO,   (int) state60      },      /* "~===" ... */
   { 0,   A_RETURN, (int) &toktab[ 78] }       /* "~=="  */
   };

struct optab state15[] = {       /* "+:" */
   { '=', A_IMMRET, (int) &toktab[ 94] },      /* "+:=" */
   { 0,   A_RETURN, (int) &toktab[ 92] }       /* "+:"  */
   };

struct optab state16[] = {       /* "++" */
   { ':', A_GOTO,   (int) state24      },      /* "++:" ... */
   { 0,   A_RETURN, (int) &toktab[109] }       /* "++"      */
   };

struct optab state17[] = {       /* "-:" */
   { '=', A_IMMRET, (int) &toktab[ 82] },      /* "-:=" */
   { 0,   A_RETURN, (int) &toktab[ 80] }       /* "-:"  */
   };

struct optab state18[] = {       /* "--" */
   { ':', A_GOTO,   (int) state25      },      /* "--:" ... */
   { 0,   A_RETURN, (int) &toktab[ 64] }       /* "--" */
   };

struct optab state19[] = {      /* "*:" */
   { '=', A_IMMRET, (int) &toktab[106] },      /* "*:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state20[] = {       /* "**" */
   { ':', A_GOTO,   (int) state26      },      /* "**:" ... */
   { 0,   A_RETURN, (int) &toktab[ 67] }       /* "**"      */
   };

struct optab state21[] = {       /* "/" */
   { ':', A_GOTO,   (int) state27      },      /* "/:" ... */
   { 0,   A_RETURN, (int) &toktab[103] }       /* "/"      */
   };

struct optab state22[] = {       /* "||" */
   { ':', A_GOTO,   (int) state28      },      /* "||:" ... */
   { '|', A_GOTO,   (int) state34      },      /* "|||" ... */
   { 0,   A_RETURN, (int) &toktab[ 60] }       /* "||"      */
   };

struct optab state23[] = {       /* "^:" */
   { '=', A_IMMRET, (int) &toktab[ 57] },      /* "^:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state24[] = {       /* "++:" */
   { '=', A_IMMRET, (int) &toktab[110] },      /* "++:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state25[] = {       /* "--:" */
   { '=', A_IMMRET, (int) &toktab[ 65] },      /* "--:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state26[] = {       /* "**:" */
   { '=', A_IMMRET, (int) &toktab[ 68] },      /* "**:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state27[] = {       /* "/:" */
   { '=', A_IMMRET, (int) &toktab[104] },      /* "/:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state28[] = {      /* "||:" */
   { '=', A_IMMRET, (int) &toktab[ 61] },      /* "||:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state29[] = {       /* "~" */
   { '=', A_GOTO,   (int) state13      },      /* "~=" ... */
   { 0,   A_RETURN, (int) &toktab[108] }       /* "~"      */
   };

struct optab state30[] = {       /* "%" */
   { ':', A_GOTO,   (int) state31      },      /* "%:" ... */
   { 0,   A_RETURN, (int) &toktab[ 83] }       /* "%"      */
   };

struct optab state31[] = {       /* "%:" */
   { '=', A_IMMRET, (int) &toktab[ 84] },      /* "%:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state32[] = {       /* "<<" */
   { ':', A_GOTO,   (int) state52      },      /* "<<:" ... */
   { '=', A_GOTO,   (int) state58      },      /* "<<=" ... */
   { 0,   A_RETURN, (int) &toktab[ 77] }       /* "<<"     */
   };

struct optab state33[] = {       /* ">>" */
   { ':', A_GOTO,   (int) state50      },      /* ">>:" ... */
   { '=', A_GOTO,   (int) state59      },      /* ">>=" ... */
   { 0,   A_RETURN, (int) &toktab[ 75] }       /* ">>"     */
   };

struct optab state34[] = {       /* "|||" */
   { ':', A_GOTO,   (int) state35      },      /* "|||:" ... */
   { 0,   A_RETURN, (int) &toktab[ 71] }       /* "|||"      */
   };

struct optab state35[] = {       /* "|||:" */
   { '=', A_IMMRET, (int) &toktab[ 72] },      /* "|||:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state36[] = {        /* "?" */
   { ':', A_GOTO,   (int) state37      },      /* "?:" ... */
   { 0,   A_RETURN, (int) &toktab[ 95] }       /* "?"      */
   };

struct optab state37[] = {       /* "?:" */
   { '=', A_IMMRET, (int) &toktab[102] },      /* "?:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state38[] = {        /* "&" */
   { ':', A_GOTO,   (int) state39      },      /* "&:" ... */
   { 0,   A_RETURN, (int) &toktab[ 62] }       /* "&"      */
   };

struct optab state39[] = {       /* "&:" */
   { '=', A_IMMRET, (int) &toktab[ 38] },      /* "&:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state40[] = {        /* "@" */
   { ':', A_GOTO,   (int) state41      },      /* "@:" ... */
   { 0,   A_RETURN, (int) &toktab[ 36] }       /* "@"      */
   };

struct optab state41[] = {      /* "@:" */
   { '=', A_IMMRET, (int) &toktab[ 37] },      /* "@:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state42[] = {       /* "=:" */
   { '=', A_IMMRET, (int) &toktab[ 39] },      /* "=:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state43[] = {       /* "~=:" */
   { '=', A_IMMRET, (int) &toktab[ 45] },      /* "~=:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state44[] = {       /* ">:" */
   { '=', A_IMMRET, (int) &toktab[ 42] },      /* ">:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state45[] = {       /* ">=:" */
   { '=', A_IMMRET, (int) &toktab[ 41] },      /* ">=:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state46[] = {      /* "<:" */
   { '=', A_IMMRET, (int) &toktab[ 44] },      /* "<:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state47[] = {       /* "<=:" */
   { '=', A_IMMRET, (int) &toktab[ 43] },      /* "<=:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state48[] = {       /* "==:" */
   { '=', A_IMMRET, (int) &toktab[ 47] },      /* "==:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state49[] = {       /* "~==:" */
   { '=', A_IMMRET, (int) &toktab[ 52] },      /* "~==:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state50[] = {      /* ">>:" */
   { '=', A_IMMRET, (int) &toktab[ 49] },      /* ">>:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state51[] = {       /* ">>=:" */
   { '=', A_IMMRET, (int) &toktab[ 48] },      /* ">>=:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state52[] = {       /* "<<:" */
   { '=', A_IMMRET, (int) &toktab[ 51] },      /* "<<:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state53[] = {       /* "<<=:" */
   { '=', A_IMMRET, (int) &toktab[ 50] },      /* "<<=:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state54[] = {      /* "===:" */
   { '=', A_IMMRET, (int) &toktab[ 40] },      /* "===:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state55[] = {       /* "~===:" */
   { '=', A_IMMRET, (int) &toktab[ 46] },      /* "~===:=" */
   { 0,   A_ERROR,  0            }
   };

struct optab state56[] = {        /* "<=" */
   { ':', A_GOTO,   (int) state47      },      /* "<=:" ... */
   { 0,   A_RETURN, (int) &toktab[ 89] }       /* "<="      */
   };

struct optab state57[] = {        /* ">=" */
   { ':', A_GOTO,   (int) state45      },      /* ">=:" ... */
   { 0,   A_RETURN, (int) &toktab[ 87] }       /* ">="      */
   };

struct optab state58[] = {        /* "<<=" */
   { ':', A_GOTO,   (int) state53      },      /* "<<=:" ... */
   { 0,   A_RETURN, (int) &toktab[ 76] }       /* "<<="      */
   };

struct optab state59[] = {       /* ">>=" */
   { ':', A_GOTO,   (int) state51     },      /* ">>=:" ... */
   { 0,   A_RETURN, (int) &toktab[ 74] }       /* ">>="      */
   };

struct optab state60[] = {        /* "~===" */
   { ':', A_GOTO,   (int) state55      },      /* "~===:" ... */
   { 0,   A_RETURN, (int) &toktab[ 85] }       /* "~==="      */
   };

struct optab state61[] = {        /* "===" */
   { ':', A_GOTO,   (int) state54      },      /* "===:" ... */
   { 0,   A_RETURN, (int) &toktab[ 66] }       /* "==="      */
   };
