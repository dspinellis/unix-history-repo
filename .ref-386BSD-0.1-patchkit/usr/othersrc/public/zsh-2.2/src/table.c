/*
 *
 * table.c - linked lists and hash tables
 *
 * This file is part of zsh, the Z shell.
 *
 * This software is Copyright 1992 by Paul Falstad
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 *
 */

#define TABLE_C
#include "zsh.h"

/* get an empty linked list header */

Lklist newlist() /**/
{
Lklist list;
 
	list = (Lklist) alloc(sizeof *list);
	list->first = 0;
	list->last = (Lknode) list;
	return list;
}

/* get an empty hash table */

Hashtab newhtable(size) /**/
int size;
{
Hashtab ret;
 
	ret = (Hashtab) zcalloc(sizeof *ret);
	ret->hsize = size;
	ret->nodes = (Hashnode*) zcalloc(size*sizeof(Hashnode));
	return ret;
}
 
/* Peter Weinberger's hash function */

int hasher(s) /**/
char *s;
{
unsigned hash = 0,g;
 
	for (; *s; s++) {
		hash = (hash << 4) + *s;
		if (g = hash & 0xf0000000) {
			hash ^= g;
			hash ^= g >> 24;
		}
	}
	return hash;
}

/* add a node to a hash table */

void Addhnode(nam,dat,ht,freefunc,canfree) /**/
char *nam;vptr dat;Hashtab ht;FFunc freefunc;int canfree;
{
int hval = hasher(nam) % ht->hsize;
struct hashnode **hp = ht->nodes+hval,*hn;
 
	for (; *hp; hp = &(*hp)->next)
		if (!strcmp((*hp)->nam,nam)) {
			if ((*hp)->canfree) free((*hp)->nam);
			hn = dat;
			hn->next = (*hp)->next;
			if (!freefunc) zerr("attempt to call NULL freefunc",NULL,0);
			else freefunc(*hp);
			*hp = hn;
			hn->nam = nam;
			hn->canfree = canfree;
			return;
		}
	hn = (Hashnode) dat;
	hn->nam = nam;
	hn->canfree = canfree;
	hn->next = ht->nodes[hval];
	ht->nodes[hval] = hn;
	if (++ht->ct == ht->hsize*4) expandhtab(ht);
}

/* add a node to command hash table */

void addhcmdnode(nam,pnam) /**/
char *nam;char **pnam;
{
int hval = hasher(nam) % cmdnamtab->hsize;
struct hashnode *hp = cmdnamtab->nodes[hval],*hn;
Cmdnam cc;
 
	for (; hp; hp = hp->next) if (!strcmp(hp->nam,nam)) return;
	cc = (Cmdnam) zcalloc(sizeof *cc);
	cc->type = EXCMD;
	cc->u.nam = tricat(*pnam,"/",nam);
	cc->pcomp = pnam;
	hn = (Hashnode) cc;
	hn->nam = ztrdup(nam);
	hn->canfree = 1;
	hn->next = cmdnamtab->nodes[hval];
	cmdnamtab->nodes[hval] = hn;
	if (++cmdnamtab->ct == cmdnamtab->hsize*4) expandhtab(cmdnamtab);
}

/* expand hash tables when they get too many entries */

void expandhtab(ht) /**/
Hashtab ht;
{
struct hashnode **arr,**ha,*hn,*hp;
int osize = ht->hsize,nsize = osize*8;

	ht->hsize = nsize;
	arr = ht->nodes;
	ht->nodes = (Hashnode*) zcalloc(nsize*sizeof(struct hashnode *));
	for (ha = arr; osize; osize--,ha++)
		for (hn = *ha; hn; ) {
			hp = hn->next;
			Addhnode(hn->nam,(vptr)hn,ht,NULL,hn->canfree);
			hn = hp;
		}
	free(arr);
}

/* get an entry in a hash table */

vptr gethnode(nam,ht) /**/
char *nam;Hashtab ht;
{
int hval = hasher(nam) % ht->hsize;
struct hashnode *hn = ht->nodes[hval];
 
	for (; hn; hn = hn->next) if (!strcmp(hn->nam,nam)) return (vptr)hn;
	return NULL;
}
 
void freehtab(ht,freefunc) /**/
Hashtab ht;FFunc freefunc;
{
int val;
struct hashnode *hn,**hp = &ht->nodes[0],*next;
 
	for (val = ht->hsize; val; val--,hp++)
		for (hn = *hp; hn; ) {
			next = hn->next;
			if (hn->canfree) free(hn->nam);
			freefunc(hn);
			hn = next;
		}
	free(ht->nodes);
	free(ht);
}

/* remove a hash table entry and return a pointer to it */

vptr remhnode(nam,ht) /**/
char *nam;Hashtab ht;
{
int hval = hasher(nam) % ht->hsize;
struct hashnode *hn = ht->nodes[hval],*hp;

	if (!hn) return NULL;
	if (!strcmp(hn->nam,nam)) {
		ht->nodes[hval] = hn->next;
		if (hn->canfree) free(hn->nam);
		ht->ct--;
		return (vptr)hn;
	}
	for (hp = hn, hn = hn->next; hn; hn = (hp = hn)->next)
		if (!strcmp(hn->nam,nam)) {
			hp->next = hn->next;
			if (hn->canfree) free(hn->nam);
			ht->ct--;
			return (vptr)hn;
		}
	return NULL;
}

/* insert a node in a linked list after 'llast' */

void insnode(list,llast,dat) /**/
Lklist list;Lknode llast;vptr dat;
{
Lknode tmp;
 
	tmp = llast->next;
	llast->next = (Lknode) alloc(sizeof *tmp);
	llast->next->last = llast;
	llast->next->dat = dat;
	llast->next->next = tmp;
	if (tmp) tmp->last = llast->next;
	else list->last = llast->next;
}

void addnodeinorder(x,dat) /**/
Lklist x; char *dat;
{
Lknode y, l = NULL;
int val = 123;

	for (y = firstnode(x); y; incnode(y)) {
		if ((val = forstrcmp((char **) &y->dat, &dat)) >= 0) break;
		l = y;
	}
	if (!val) return;
	if (l == NULL) insnode(x, (Lknode) x, dat);
	else insnode(x, l, dat);
}


/* remove a node from a linked list */

vptr remnode(list,nd) /**/
Lklist list;Lknode nd;
{
vptr dat;

	nd->last->next = nd->next;
	if (nd->next) nd->next->last = nd->last;
	else list->last = nd->last;
	dat = nd->dat;
	free(nd);
	return dat;
}

/* remove a node from a linked list */

vptr uremnode(list,nd) /**/
Lklist list;Lknode nd;
{
vptr dat;

	nd->last->next = nd->next;
	if (nd->next) nd->next->last = nd->last;
	else list->last = nd->last;
	dat = nd->dat;
	return dat;
}

/* delete a character in a string */

void chuck(str) /**/
char *str;
{
	while (str[0] = str[1]) str++;
}

/* get top node in a linked list */

vptr getnode(list) /**/
Lklist list;
{
vptr dat;
Lknode node = list->first;
 
	if (!node)
		return NULL;
	dat = node->dat;
	list->first = node->next;
	if (node->next)
		node->next->last = (Lknode) list;
	else
		list->last = (Lknode) list;
	free(node);
	return dat;
}

/* get top node in a linked list without freeing */

vptr ugetnode(list) /**/
Lklist list;
{
vptr dat;
Lknode node = list->first;
 
	if (!node)
		return NULL;
	dat = node->dat;
	list->first = node->next;
	if (node->next)
		node->next->last = (Lknode) list;
	else
		list->last = (Lknode) list;
	return dat;
}

void freetable(tab,freefunc) /**/
Lklist tab;FFunc freefunc;
{
Lknode node = tab->first,next;
 
	while (node) {
		next = node->next;
		if (freefunc) freefunc(node->dat);
		free(node);
		node = next;
	}
	free(tab);
}
 
char *ztrstr(s,t) /**/
char *s;char *t;
{
char *p1,*p2;
 
	for (; *s; s++) {
		for (p1 = s, p2 = t; *p2; p1++,p2++)
			if (*p1 != *p2) break;
		if (!*p2) return (char *) s;
	}
	return NULL;
}

/* insert a list in another list */

void inslist(l,where,x) /**/
Lklist l;Lknode where;Lklist x;
{
Lknode nx = where->next;

	if (!l->first) return;
	where->next = l->first;
	l->last->next = nx;
	l->first->last = where;
	if (nx) nx->last = l->last;
	else x->last = l->last;
}

int countnodes(x) /**/
Lklist x;
{
Lknode y;
int ct = 0;

	for (y = firstnode(x); y; incnode(y),ct++);
	return ct;
}

