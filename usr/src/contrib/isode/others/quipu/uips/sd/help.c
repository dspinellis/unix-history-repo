/* help.c - Display of assorted help texts */

#ifndef lint
static char *rcsid = "$Header: /f/osi/others/quipu/uips/sd/RCS/help.c,v 7.2 91/02/22 09:32:16 mrose Interim $";
#endif

/*
 * $Header: /f/osi/others/quipu/uips/sd/RCS/help.c,v 7.2 91/02/22 09:32:16 mrose Interim $
 */


#include "sequence.h"
#include <stdio.h>
#include "quipu/util.h"
#include "tailor.h"

#define BUFLEN 1024
#define TEXT 1

#ifdef lint
#define ETCDIR "/etc"
#endif 

#define  HELPDIR "sd/helpdir/"

extern str_seq textseq;
extern str_seq curr_dnseq;
extern int text_state;
extern int current_entry, entry_number, display_entry;

void tprint(), scrollbar();

void get_help();

void help_cncs()
{
  get_help("help", "   Press <KEY> to get detailed help.\n");
}

void help_init()
{
  get_help("help", "   SD X.500 Directory Agent - Concise Help\n");
}

void help_up()
{
  get_help("widen", "   The \"Widen Area\" Function.\n");
}

void help_back()
{
  get_help("look", "   The \"Look Back\" Function.\n");
}

void help_number()
{
  get_help("number", "   The \"Go To Number\" Function.\n");
}

void help_srch()
{
    get_help("search", "   The \"Search\" Function");
}

void help_list()
{
  get_help("list", "   The \"List\" Function");
}

void get_help(filename, line)
char *filename, *line;
{
  FILE * helpfp;
  char helpbuf[BUFLEN];
  char filebuf[BUFLEN];
  char *str;

  text_state = TEXT;
  entry_number = 0;
  display_entry = current_entry = 1;
  free_seq(curr_dnseq);
  free_seq(textseq);
  curr_dnseq = 0;
  textseq = 0;
  
  (void) strcpy(filebuf, HELPDIR);
  (void) strcat(filebuf, filename);
  (void) strcpy(helpbuf, isodefile(filebuf, 0));
  
  if ((helpfp = fopen(helpbuf, "r")) == (FILE *)NULL ) {
    tprint("Can't open help file '%s'.\n",helpbuf);
    return;
  }
  
  if (line) {
    add_seq(&textseq, line);
    entry_number++;
  }

  while(fgets(filebuf, BUFLEN, helpfp) != (char *) NULL) {
    str = filebuf;
    while(*str != '\n' && *str != '\0') str++;
    *str = '\0';
    add_seq(&textseq, filebuf);
    entry_number++;
  }

  (void) fclose(helpfp);
  scrollbar('\0');
}
	 
            
