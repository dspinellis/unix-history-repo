/*******************************************************************
*                                                                  *
*    File: CIFPLOT/scanner.c                                       *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/

#include "lextable.h"
#include "scanner.h"

/* LexState holds the state of the scanner */
int LexState = 0;
/* LexChar hold any character that must be remembered */
char LexChar;
/* The level of nesting in comments */
int CmtLevel;

scanner() {
     
  upToDate = 1;
  TokenLine = LineNo;
  TokenChar = CharNo;
  while(1)
     /* The scanner is a finite state machine with 4 states
      * State 0 reads input and return appropriate value.
      * State 1 reads blanks and output just one blank unless
      * the next character is a semicolon, when it ouputs
      * just the semi-colon. If the next character is not a
      * semicolon it goes into state 2 which holds the character
      * till the next call, then moves into state 3 to output
      * the appropriate value.		*/
     switch(LexState) {
	case 0:
		yylval = input();
	case 3:
		LexState = 0;
		switch(CharClass[yylval]) {

		case DIGIT:
			return(yylval);
		case LETTER:
			return(yylval);
		case EOF:
			/* Return a 0 if nothing more to read */
			if(Next() == 1)
				return(0);
			break;
		case STARTCOMMENT:
			CmtLevel = 1;
			while(CmtLevel)
			    switch(CharClass[input()]) {
				case STARTCOMMENT:
					CmtLevel++;
					break;
				case ENDCOMMENT:
					if(--CmtLevel == 0 && RetCmt) {
						yylval = MakeComment();
						return(COMMENT_COMMAND);
						}
					break;
				case EOF:
					if(Next() == 1) {
						Error("Non-Terminated Comment",FATAL);
						return(0);
						}
					    else
						Error("Comment crosses file boundary",WARNING);
					break;
				}
		case BLANK:
			if(SendAll) 
				switch(yylval) {
					case '"':
						return(yylval);
					case ' ':
					case '\t':
					case '\n':
						return(BLANK);
					default:
						return(OTHERCHAR);
						
					}
			LexState = 1;
			/* fall through to State 1 */
			break;
		case ENDCOMMENT:
			Error("Unmatched ')' Ignored",RECOVERABLE);
			break;
		default:
			Error("Unknown Character Class encountered in scanner",INTERNAL);
		}
	case 1:
		/* Tight loop to read blanks */
		while(CharClass[yylval=input()] == BLANK);

		if(';' == yylval) {
			LexState = 0;
			return(yylval);
			}
		if('(' == yylval) {
			LexState = 3;
			break;
			}
		LexState = 2;
		LexChar = yylval;
		return(BLANK);
	case 2:
		LexState = 3;
		yylval = LexChar;
		break;
	default:
		Error("Scanner is in unknown state",INTERNAL);
	}
    }
