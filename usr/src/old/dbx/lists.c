/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)lists.c	5.3 (Berkeley) 6/1/90";
#endif /* not lint */

/*
 * General list definitions.
 *
 * The assumption is that the elements in a list are words,
 * usually pointers to the actual information.
 */

#include "defs.h"
#include "lists.h"

#ifndef public

typedef struct List *List;
typedef struct ListItem *ListItem;
typedef char *ListElement;

#define list_item(element) generic_list_item((ListElement) (element))
#define list_element(type, item) ((type) (item == nil ? nil : (item)->element))
#define list_head(list) ((list == nil) ? nil : (list)->head)
#define list_tail(list) ((list == nil) ? nil : (list)->tail)
#define list_next(item) ((item == nil) ? nil : (item)->next)
#define list_prev(item) ((item == nil) ? nil : (item)->prev)
#define list_size(list) (((list) == nil) ? 0 : (list)->nitems)

#define foreach(type, i, list) \
{ \
    register ListItem _item; \
 \
    _item = list_head(list); \
    while (_item != nil) { \
	i = list_element(type, _item); \
	_item = list_next(_item);

#define endfor \
    } \
}

/*
 * Iterate through two equal-sized lists.
 */

#define foreach2(type1, i, list1, type2, j, list2) \
{ \
    register ListItem _item1, _item2; \
 \
    _item1 = list_head(list1); \
    _item2 = list_head(list2); \
    while (_item1 != nil) { \
	i = list_element(type1, _item1); \
	j = list_element(type2, _item2); \
	_item1 = list_next(_item1); \
	_item2 = list_next(_item2);

#define list_islast() (_item == nil)
#define list_curitem(list) (_item == nil ? list_tail(list) : list_prev(_item))

/*
 * Representation should not be used outside except through macros.
 */

struct List {
    Integer nitems;
    ListItem head;
    ListItem tail;
};

struct ListItem {
    ListElement element;
    ListItem next;
    ListItem prev;
};

#endif

/*
 * Allocate and initialize a list.
 */

public List list_alloc()
{
    List list;

    list = new(List);
    list->nitems = 0;
    list->head = nil;
    list->tail = nil;
    return list;
}

/*
 * Create a list item from an object (represented as pointer or integer).
 */

public ListItem generic_list_item(element)
ListElement element;
{
    ListItem i;

    i = new(ListItem);
    i->element = element;
    i->next = nil;
    i->prev = nil;
    return i;
}

/*
 * Insert an item before the item in a list.
 */

public list_insert(item, after, list)
ListItem item;
ListItem after;
List list;
{
    ListItem a;

    checkref(list);
    list->nitems = list->nitems + 1;
    if (list->head == nil) {
	list->head = item;
	list->tail = item;
    } else {
	if (after == nil) {
	    a = list->head;
	} else {
	    a = after;
	}
	item->next = a;
	item->prev = a->prev;
	if (a->prev != nil) {
	    a->prev->next = item;
	} else {
	    list->head = item;
	}
	a->prev = item;
    }
}

/*
 * Append an item after the given item in a list.
 */

public list_append(item, before, list)
ListItem item;
ListItem before;
List list;
{
    ListItem b;

    checkref(list);
    list->nitems = list->nitems + 1;
    if (list->head == nil) {
	list->head = item;
	list->tail = item;
    } else {
	if (before == nil) {
	    b = list->tail;
	} else {
	    b = before;
	}
	item->next = b->next;
	item->prev = b;
	if (b->next != nil) {
	    b->next->prev = item;
	} else {
	    list->tail = item;
	}
	b->next = item;
    }
}

/*
 * Delete an item from a list.
 */

public list_delete(item, list)
ListItem item;
List list;
{
    checkref(item);
    checkref(list);
    assert(list->nitems > 0);
    if (item->next == nil) {
	list->tail = item->prev;
    } else {
	item->next->prev = item->prev;
    }
    if (item->prev == nil) {
	list->head = item->next;
    } else {
	item->prev->next = item->next;
    }
    list->nitems = list->nitems - 1;
}

/*
 * Concatenate one list onto the end of another.
 */

public List list_concat(first, second)
List first, second;
{
    List r;

    if (first == nil) {
	r = second;
    } else if (second == nil) {
	r = first;
    } else {
	second->head->prev = first->tail;
	first->tail->next = second->head;
	first->tail = second->tail;
	first->nitems = first->nitems + second->nitems;
	r = first;
    }
    return r;
}
