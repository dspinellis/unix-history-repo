.TL
RCS Manual
.AU
Walter F. Tichy
.AI
Department of Computer Science
Purdue University
West Lafayette, IN 47906
.PP
.NH
Numbering of Revisions
.PP
Revisions are organized in a tree that grows from the initial
revision. The tree has a main trunk, along which the revisions
are normally numbered 1.1, 1.2, 1.3, ... 2.1, 2.2, ...
Every revision can sprout several branches. A branch starting
at revision X has number X.y, and revisions on that branch have
numbers X.y.z. For example, branches starting at revision 1.3
are numbered 1.3.1, 1.3.2, and 1.3.3, and revisions
on branch 1.3.1 are numbered 1.3.1.1, 1.3.1.2, 1.3.1.3, etc.
Note that revisions on branches may sprout new branches, and the numbering
works analogously.
.PP
Revisions and branches may also be labelled symbolically. For instance, 
branch 1.3.1 could be labelled "Experimental". Revisions on a labelled 
branch can then be identified using the branch label as a prefix. For 
example, revision 1.3.1.1 is identified with "Experimental.1". Of course, 
it is also possible to give a symbolic name to an individual revision. This 
label can then be used to identify the revision and as a prefix for 
branches starting with that revision. Note that labels are mapped to 
revision numbers. Labels start with letters and are followed by letters, 
digits, and underbars. 
