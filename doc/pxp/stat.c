.LS .ID
.nf
statement(s)
        \*bregister\fP \*bint\fR *s;
{

        \*bif\fP (s == NIL) {
                ppitem();
                ppid("null");
                \*breturn\fP;
        }
        line = s[1];
        \*bswitch\fP (s[0]) {
                \*bdefault\fP:
                        panic("stat");
                \*bcase\fP T_IF:
                \*bcase\fP T_IFEL:
                        ppnl();
                        indent();
                        ifop(s);
                        \*breturn\fP;
                \*bcase\fP T_ASGN:
                        ppitem();
                        asgnop(s);
                        \*breturn\fP;
        }
}

asgnop(r)
        \*bregister\fP \*bint\fR *r;
{
        lvalue(r[2]);
        ppsep(" := ");
        rvalue(r[3], NIL);
}

ifop(r)
        \*bregister\fP \*bint\fR *r;
{
        \*bregister\fP \*bint\fR *s;
        \*bstruct\fP pxcnt scnt;

        ppkw("if");
        ppspac();
        rvalue(r[2], NIL);
        ppspac();
        ppkw("then");
        ppspac();
        s = r[3];
        savecnt(&scnt);
        getcnt();
        \*bif\fP (s != NIL && s[0] == T_BLOCK)
                ppstbl1(s, STAT);
        \*b\*belse\fP\fR {
                ppgoin(STAT);
                statement(s);
                ppgoout(STAT);
        }
        \*bif\fP (r[0] == T_IFEL) {
                setcnt(cntof(&scnt)-nowcnt());
                \*bif\fP (s == NIL || s[0] != T_BLOCK) {
                        ppnl();
                        indent();
                } \*belse\fP {
                        ppstbl2();
                        ppspac();
                }
                s = r[4];
                ppkw("else");
                unprint();
                ppspac();
                \*bif\fP (s == NIL)
                        \*bgoto\fP burp;
                \*bif\fP (s[0] == T_BLOCK)
                        ppstbl1(s, STAT);
                \*belse\fP \*bif\fR (s[0] == T_IF || s[0] == T_IFEL)
                        ifop(s);
                \*belse\fP {
burp:
                        ppgoin(STAT);
                        statement(s);
                        ppgoout(STAT);
                }
        }
        \*bif\fP (rescnt(&scnt))
                getcnt();
        \*bif\fP (r[4] != NIL)
                unprint();
        \*bif\fP (s != NIL && s[0] == T_BLOCK)
                ppstbl2();
}

ppstbl1(r, m)
	\*bint\fP *r, m;
{
        ppkw("begin");
        ppgoin(m);
        statlist(r[2]);
        ppgoout(m);
}

ppstbl2()
{
        ppnl();
        indent();
        ppkw("end");
}
.fi
.LE
