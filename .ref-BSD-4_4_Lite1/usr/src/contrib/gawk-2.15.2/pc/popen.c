#include <stdio.h>
#include "popen.h"
#include <io.h>
#include <string.h>
#include <process.h>

static char template[] = "piXXXXXX";
typedef enum { unopened = 0, reading, writing } pipemode;
static
struct {
    char *command;
    char *name;
    pipemode pmode;
} pipes[_NFILE];

FILE *
popen( char *command, char *mode ) {
    FILE *current;
    char *name;
    int cur;
    pipemode curmode;
    /*
    ** decide on mode.
    */
    if(strcmp(mode,"r") == 0)
        curmode = reading;
    else if(strcmp(mode,"w") == 0)
        curmode = writing;
    else
        return NULL;
    /*
    ** get a name to use.
    */
    if((name = tempnam(".","pip"))==NULL)
        return NULL;
    /*
    ** If we're reading, just call system to get a file filled with
    ** output.
    */
    if(curmode == reading) {
        char cmd[256];
        sprintf(cmd,"%s > %s",command,name);
        system(cmd);
        if((current = fopen(name,"r")) == NULL)
            return NULL;
    } else {
        if((current = fopen(name,"w")) == NULL)
            return NULL;
    }
    cur = fileno(current);
    pipes[cur].name = name;
    pipes[cur].pmode = curmode;
    pipes[cur].command = strdup(command);
    return current;
}

int
pclose( FILE * current) {
    int cur = fileno(current),rval;
    /*
    ** check for an open file.
    */
    if(pipes[cur].pmode == unopened)
        return -1;
    if(pipes[cur].pmode == reading) {
        /*
        ** input pipes are just files we're done with.
        */
        rval = fclose(current);
        unlink(pipes[cur].name);
    } else {
        /*
        ** output pipes are temporary files we have
        ** to cram down the throats of programs.
        */
        char command[256];
        fclose(current);
        sprintf(command,"%s < %s",pipes[cur].command,pipes[cur].name);
        rval = system(command);
        unlink(pipes[cur].name);
    }
    /*
    ** clean up current pipe.
    */
    pipes[cur].pmode = unopened;
    free(pipes[cur].name);
    free(pipes[cur].command);
    return rval;
}

