#define chkrtab(p);	\
	if(p!=lastrtab){ if(TYPE(p)!=ARRAY && TYPE(p->data)!=INT) rtaberr();\
			 else { lastrtab = p; ctable = p->data; } }
extern lispval lastrtab;
