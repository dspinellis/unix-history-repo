.NH 2
Associating X Resources to User Data Structures
.IN "Hash Lookup"
.IN "Window" "ID's"
.IN "Resource ID's"
.IN "XId"
.PP
Application programs often need to be able to easily refer to
their own data structures when an event arrives.
The \fIXAssocTable\fP system provides users of the X library with  a  method
of associating their own data structures with X resources.
(Bitmaps, Pixmaps, Fonts, Windows, and so on).
.PP
An  \fIXAssocTable\fP can be used to type X resources.  
For example, the user
may wish to have three or four `types' of windows each with  different
properties. 
This can be accomplished by associating each X window id
with a pointer to a `window property' data structure  defined  by  the
user.
.IN "Definitions" "XId"
A generic type has been defined in the X library for resource id's.
.IN "XId"
It is called ``XId''.
.PP
There  are  a  few  guidelines  that  should be observed when using an
\fIXAssocTable\fP:
.LP
.IN "XId"
First, all  \fIXId\fP's  are  relative  to  the  currently  active  display.
Therefore,  if you are using multiple displays you need to be sure the
correct display is active before performing an \fIXAssocTable\fP operation.
\fIXAssocTable\fP  imposes no restrictions on the number of X ids per table,
the number of X ids per display or the number of displays per table.
.LP
Second,  because  of  the  hashing  scheme  used  by  the  association
mechanism the following rules for determining the size of \fIXAssocTable\fPs
should be followed.  
Associations will be  made  and  looked  up  more
efficiently  if  the  table  size  (number  of  buckets in the hashing
system) is a power of two and if there are not more than 8 Xids  per
bucket.
.FD
.IN "XAssocTable"
.IN "Definitions" "XCreateAssocTable"
.IN "XCreateAssocTable"
XAssocTable *XCreateAssocTable(size)
	int size;	/* Desired size (in buckets) of the table. */
.FN
This routine returns a pointer to a newly created \fIXAssocTable\fP.
The "size" argument specifies the number of buckets  in  \fIXAssocTable\fP's
hashing  system.   For  reasons  of  efficiency  the number of buckets
should be a power of two.  Some size  suggestions  might  be:  use  32
buckets  per  100  objects;  a reasonable maximum number of object per
buckets is 8.   If  there  is  an  error  allocating  memory  for  the
\fIXAssocTable\fP, a NULL pointer is returned. 
.FD
.IN "XAssocTable"
.IN "Definitions" "XDestroyAssocTable"
.IN "XDestroyAssocTable"
XDestroyAssocTable(table)
	XAssocTable *table;	/* Table to destroy. */
.FN
Destroy (free the  memory  associated  with)  an  XAssocTable.
Using an \fIXAssocTable\fP after it has been destroyed is guaranteed to have
unpredictable and probably disastrous consequences!
.FD
.IN "XAssocTable"
.IN "Definitions" "XMakeAssoc"
.IN "XMakeAssoc"
XMakeAssoc(table, x_id, data)
	XAssocTable *table;	/* Table in which to make the association. */
	XId x_id;	/* X resource id. */
	caddr_t data;	/* Data to associate with the XId. */
.FN
.IN "XId"
Insert data into an \fIXAssocTable\fP keyed  on  an  XId.   Data  is
inserted  into the table only once.  Redundant inserts are meaningless
and cause no problems.  The queue in each association bucket is sorted
from the lowest XId to the highest XId.
.FD
.IN "XAssocTable"
.IN "Definitions" "XLookUpAssoc"
.IN "XLookupAssoc"
caddr_t XLookUpAssoc(table, x_id)
	XAssocTable *table;	/* Table to look in. */
	XId x_id;	/* XId to look for. */
.FN
.IN "XId"
Retrieve the data stored in an \fIXAssocTable\fP by its XId.  If  an
appropriately  matching XId can be found in the table the routine will
return the data associated with it. If the \fIx_id\fP can not be found in the
table the routine will return NULL.
.FD
.IN "XAssocTable"
.IN "Definitions" "XDeleteAssoc"
.IN "XDeleteAssoc"
XDeleteAssoc(table, x_id)
	XAssocTable *table;	/* Table to look in. */
	XId x_id;	/* XId to delete. */
.FN
.IN "XId"
Delete an association in an \fIXAssocTable\fP keyed on its XId.
Redundant deletes (and deletes of non-existant XId's) are meaningless
and cause no problems.  Deleteing associations in no way impares
the performance of an \fIXAssocTable\fP.
