#include <stdio.h>
#include "def.h"

int routnum;
FILE *debfd;
LOGICAL routerr;
int nodenum, accessnum;
int **graph;
int progtype;
VERT stopvert, retvert;
VERT START;
