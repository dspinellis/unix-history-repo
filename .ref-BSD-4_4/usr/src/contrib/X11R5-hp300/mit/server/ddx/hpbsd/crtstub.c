/* 
 * crtstub : stubs for displays not linked into the server
 */
#include "X.h"
#include "scrnintstr.h"

static void 
notinstalled(name)
     char *name;
{
  printf("%s is not installed in server\n",name);
  puts("Run ??? to install the display");
  exit(1);
}

#define ScreenInit(ScreenInitRoutine,msg)	\
/* Bool */					\
   ScreenInitRoutine(index, pScreen, argc,argv)  \
	int index, argc; ScreenPtr pScreen; char **argv; \
  { notinstalled(msg); return 0; }

#define ScreenInfo(ScreenInfoRoutine,msg) \
/* Bool */					\
   ScreenInfoRoutine(index, argv,argc)		\
	int index, argc; char **argv;		\
  { notinstalled(msg); return 0; }

/*--------------------------------------------------------------------*/

ScreenInit(mobScreenInit,"Moberly")
ScreenInfo(mobScreenInfo,"Moberly")

/*--------------------------------------------------------------------*/

ScreenInit(gbxScreenInit,"Gator Box")
ScreenInfo(gbxScreenInfo,"Gator Box")

/*--------------------------------------------------------------------*/

ScreenInit(mrtopcatScreenInit,"mrtopcat")
ScreenInfo(mrtopcatScreenInfo,"mrtopcat")

/*--------------------------------------------------------------------*/

ScreenInit(renScreenInit,"Renaissance")
ScreenInfo(renScreenInfo,"Renaissance")

ScreenInit(orenScreenInit,"Renaissance")
ScreenInfo(orenScreenInfo,"Renaissance")

/*--------------------------------------------------------------------*/

ScreenInit(davinciScreenInit,"DaVinci")
ScreenInfo(davinciScreenInfo,"DaVinci")

ScreenInit(oDavinciScreenInit,"DaVinci")
ScreenInit(sbDavinciScreenInit,"DaVinci")
ScreenInfo(oDavinciScreenInfo,"DaVinci")

/*--------------------------------------------------------------------*/
