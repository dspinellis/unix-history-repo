/* $Header: /a/guest/moore/talk/RCS/table.c,v 1.9 83/07/06 00:11:38 moore Exp $ */

/* routines to handle insertion, deletion, etc on the table
   of requests kept by the daemon. Nothing fancy here, linear
   search on a double-linked list. A time is kept with each 
   entry so that overly old invitations can be eliminated.

   Consider this a mis-guided attempt at modularity
 */

#include "ctl.h"
#include <sys/time.h>

#define MAX_ID 16000 /* << 2^15 so I don't have sign troubles */

#define NIL ( (TABLE_ENTRY *) 0)

extern int debug;
struct timeval tp;
struct timezone *txp;

typedef struct table_entry TABLE_ENTRY;

struct table_entry {
    CTL_MSG request;
    long time;
    TABLE_ENTRY *next;
    TABLE_ENTRY *last;
};

TABLE_ENTRY *table = NIL;
CTL_MSG *find_request();
CTL_MSG *find_match();
char *malloc();

    /*
     * Look in the table for an invitation that matches the current
     * request looking for an invitation
     */

CTL_MSG *find_match(request)
CTL_MSG *request;
{
    TABLE_ENTRY *ptr;
    long current_time;

    gettimeofday(&tp, &txp);
    current_time = tp.tv_sec;

    ptr = table;

    if (debug) {
	printf("Entering Look-Up with : \n");
	print_request(request);
    }

    while (ptr != NIL) {

	if ( (ptr->time - current_time) > MAX_LIFE ) {
		/* the entry is too old */
	    if (debug) printf("Deleting expired entry : \n");
	    if (debug) print_request(&ptr->request);
	    delete(ptr);
	    ptr = ptr->next;
	    continue;
	}

	if (debug) print_request(&ptr->request);

	if ( strcmp(request->l_name, ptr->request.r_name) == 0 &&
	     strcmp(request->r_name, ptr->request.l_name) == 0 &&
	     ptr->request.type == LEAVE_INVITE ) {
	    return(&ptr->request);
	}
	
	ptr = ptr->next;
    }
    
    return((CTL_MSG *) 0);
}

    /*
     * look for an identical request, as opposed to a complimentary
     * one as find_match does 
     */

CTL_MSG *find_request(request)
CTL_MSG *request;
{
    TABLE_ENTRY *ptr;
    long current_time;

    gettimeofday(&tp, &txp);
    current_time = tp.tv_sec;

	/* See if this is a repeated message, and check for
	   out of date entries in the table while we are it.
	 */

    ptr = table;

    if (debug) {
	printf("Entering find_request with : \n");
	print_request(request);
    }

    while (ptr != NIL) {

	if ( (ptr->time - current_time) > MAX_LIFE ) {
		/* the entry is too old */
	    if (debug) printf("Deleting expired entry : \n");
	    if (debug) print_request(&ptr->request);
	    delete(ptr);
	    ptr = ptr->next;
	    continue;
	}

	if (debug) print_request(&ptr->request);

	if ( strcmp(request->r_name, ptr->request.r_name) == 0 &&
	     strcmp(request->l_name, ptr->request.l_name) == 0 &&
	     request->type == ptr->request.type &&
	     request->pid == ptr->request.pid) {
	    
		/* update the time if we 'touch' it */
	    ptr->time = current_time;
	    return(&ptr->request);
	}

	ptr = ptr->next;
    }

    return((CTL_MSG *) 0);
}

insert_table(request, response)
CTL_MSG *request;
CTL_RESPONSE *response;
{
    TABLE_ENTRY *ptr;
    long current_time;

    gettimeofday(&tp, &txp);
    current_time = tp.tv_sec;

    response->id_num = request->id_num = new_id();

	/* insert a new entry into the top of the list */
    
    ptr = (TABLE_ENTRY *) malloc(sizeof(TABLE_ENTRY));

    if (ptr == NIL) {
	print_error("malloc in insert_table");
    }

    ptr->time = current_time;
    ptr->request = *request;

    ptr->next = table;
    if (ptr->next != NIL) {
	ptr->next->last = ptr;
    }
    ptr->last = NIL;
    table = ptr;
}

    /* generate a unique non-zero sequence number */

new_id()
{
    static int current_id = 0;

    current_id = (current_id + 1) % MAX_ID;

	/* 0 is reserved, helps to pick up bugs */

    if (current_id == 0) current_id = 1;

    return(current_id);
}

    /* delete the invitation with id 'id_num' */

delete_invite(id_num)
int id_num;
{
    TABLE_ENTRY *ptr;

    ptr = table;

    if (debug) printf("Entering delete_invite with %d\n", id_num);

    while (ptr != NIL && ptr->request.id_num != id_num) {
	if (debug) print_request(&ptr->request);
	ptr = ptr->next;
    }
	
    if (ptr != NIL) {
	delete(ptr);
	return(SUCCESS);
    }
    
    return(NOT_HERE);
}

    /* classic delete from a double-linked list */

delete(ptr)
TABLE_ENTRY *ptr;
{
    if (debug) printf("Deleting : ");
    if (debug) print_request(&ptr->request);

    if (table == ptr) {
	table = ptr->next;
    } else if (ptr->last != NIL) {
	ptr->last->next = ptr->next;
    }

    if (ptr->next != NIL) {
	ptr->next->last = ptr->last;
    }

    free((char *) ptr);
}
