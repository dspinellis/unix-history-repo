/* Driver program for directory listing facility. */
/* Type the -h option for program usage information. */
#include "option-handler.h"
#include "screen-handler.h"
#include "directory-handler.h"

/* Manages program options, globally visible to other modules. */
Option_Handler option;

/* Initializes the screen object. */
Screen_Handler screen;

/* Set the program options and turn over the dirty work
   to the main directory handling module. */

int
main (int argc, char *argv[])
{
  option (argc, argv);
  Directory_Handler files;
  files.print ();
  return 0;
}
