/*	usage.c
 *
 * Show usage message.
 */

#include <stdio.h>
#include <string.h>


usage(char *myname)
{
char	* p;
char 	* name_p;

name_p = myname;
if ( p = strrchr(myname,'/') )
	name_p = p+1;	/* point after final '/' */
#ifdef MSDOS
if ( p = strrchr(name_p,'\\') )
	name_p = p+1;	/* point after final '\\' */
if ( p = strrchr(name_p,':') )
	name_p = p+1;	/* point after final ':' */
  printf("\nUsage: %s [-acdnpsSvw] [-Dnumber] [-i[extension]] [-Idirectory]"
#else
  printf("\nUsage: %s [-acdnpPsSuUvw] [-Dnumber] [-i[extension]] [-Idirectory]"
#endif
         "\n            [-e \"command\"] [-x[directory]] [filename] [arguments]\n", name_p);

  printf("\n  -a  autosplit mode with -n or -p"
         "\n  -c  syntaxcheck only"
         "\n  -d  run scripts under debugger"
         "\n  -n  assume 'while (<>) { ...script... }' loop arround your script"
         "\n  -p  assume loop like -n but print line also like sed"
#ifndef MSDOS
         "\n  -P  run script through C preprocessor befor compilation"
#endif
         "\n  -s  enable some switch parsing for switches after script name"
         "\n  -S  look for the script using PATH environment variable");
#ifndef MSDOS
  printf("\n  -u  dump core after compiling the script"
         "\n  -U  allow unsafe operations");
#endif
  printf("\n  -v  print version number and patchlevel of perl"
         "\n  -w  turn warnings on for compilation of your script\n"
         "\n  -Dnumber        set debugging flags"
         "\n  -i[extension]   edit <> files in place (make backup if extension supplied)"
         "\n  -Idirectory     specify include directory in conjunction with -P"
         "\n  -e command      one line of script, multiple -e options are allowed"
         "\n                  [filename] can be ommitted, when -e is used"
         "\n  -x[directory]   strip off text before #!perl line and perhaps cd to directory\n");
}
