%token CHAR CCL NCCL STR DELIM SCON ITER NEWE NULLS
%left SCON '/' NEWE
%left '|'
%left '$' '^'
%left CHAR CCL NCCL '(' '.' STR NULLS
%left ITER
%left CAT
%left '*' '+' '?'

%{
# include "ldefs.c"
%}
%%
%{
int i;
int j,k;
int g;
char *p;
%}
acc	:	lexinput
	={	
# ifdef DEBUG
		if(debug) sect2dump();
# endif
	}
	;
lexinput:	defns delim prods end
	|	defns delim end
	={
		if(!funcflag)phead2();
		funcflag = TRUE;
	}
	| error
	={
# ifdef DEBUG
		if(debug) {
			sect1dump();
			sect2dump();
			}
# endif
		}
	;
end:		delim | ;
defns:	defns STR STR
	={	scopy($2,dp);
		def[dptr] = dp;
		dp += slength($2) + 1;
		scopy($3,dp);
		subs[dptr++] = dp;
		if(dptr >= DEFSIZE)
			error("Too many definitions");
		dp += slength($3) + 1;
		if(dp >= dchar+DEFCHAR)
			error("Definitions too long");
		subs[dptr]=def[dptr]=0;	/* for lookup - require ending null */
	}
	|
	;
delim:	DELIM
	={
# ifdef DEBUG
		if(sect == DEFSECTION && debug) sect1dump();
# endif
		sect++;
		}
	;
prods:	prods pr
	={	$$ = mn2(RNEWE,$1,$2);
		}
	|	pr
	={	$$ = $1;}
	;
pr:	r NEWE
	={
		if(divflg == TRUE)
			i = mn1(S1FINAL,casecount);
		else i = mn1(FINAL,casecount);
		$$ = mn2(RCAT,$1,i);
		divflg = FALSE;
		casecount++;
		}
	| error NEWE
	={
# ifdef DEBUG
		if(debug) sect2dump();
# endif
		}
r:	CHAR
	={	$$ = mn0($1); }
	| STR
	={
		p = $1;
		i = mn0(*p++);
		while(*p)
			i = mn2(RSTR,i,*p++);
		$$ = i;
		}
	| '.'
	={	symbol['\n'] = 0;
		if(psave == FALSE){
			p = ccptr;
			psave = ccptr;
			for(i=1;i<'\n';i++){
				symbol[i] = 1;
				*ccptr++ = i;
				}
			for(i='\n'+1;i<NCH;i++){
				symbol[i] = 1;
				*ccptr++ = i;
				}
			*ccptr++ = 0;
			if(ccptr > ccl+CCLSIZE)
				error("Too many large character classes");
			}
		else
			p = psave;
		$$ = mn1(RCCL,p);
		cclinter(1);
		}
	| CCL
	={	$$ = mn1(RCCL,$1); }
	| NCCL
	={	$$ = mn1(RNCCL,$1); }
	| r '*'
	={	$$ = mn1(STAR,$1); }
	| r '+'
	={	$$ = mn1(PLUS,$1); }
	| r '?'
	={	$$ = mn1(QUEST,$1); }
	| r '|' r
	={	$$ = mn2(BAR,$1,$3); }
	| r r %prec CAT
	={	$$ = mn2(RCAT,$1,$2); }
	| r '/' r
	={	if(!divflg){
			j = mn1(S2FINAL,-casecount);
			i = mn2(RCAT,$1,j);
			$$ = mn2(DIV,i,$3);
			}
		else {
			$$ = mn2(RCAT,$1,$3);
			warning("Extra slash removed");
			}
		divflg = TRUE;
		}
	| r ITER ',' ITER '}'
	={	if($2 > $4){
			i = $2;
			$2 = $4;
			$4 = i;
			}
		if($4 <= 0)
			warning("Iteration range must be positive");
		else {
			j = $1;
			for(k = 2; k<=$2;k++)
				j = mn2(RCAT,j,dupl($1));
			for(i = $2+1; i<=$4; i++){
				g = dupl($1);
				for(k=2;k<=i;k++)
					g = mn2(RCAT,g,dupl($1));
				j = mn2(BAR,j,g);
				}
			$$ = j;
			}
	}
	| r ITER '}'
	={
		if($2 < 0)warning("Can't have negative iteration");
		else if($2 == 0) $$ = mn0(RNULLS);
		else {
			j = $1;
			for(k=2;k<=$2;k++)
				j = mn2(RCAT,j,dupl($1));
			$$ = j;
			}
		}
	| r ITER ',' '}'
	={
				/* from n to infinity */
		if($2 < 0)warning("Can't have negative iteration");
		else if($2 == 0) $$ = mn1(STAR,$1);
		else if($2 == 1)$$ = mn1(PLUS,$1);
		else {		/* >= 2 iterations minimum */
			j = $1;
			for(k=2;k<$2;k++)
				j = mn2(RCAT,j,dupl($1));
			k = mn1(PLUS,dupl($1));
			$$ = mn2(RCAT,j,k);
			}
		}
	| SCON r
	={	$$ = mn2(RSCON,$2,$1); }
	| '^' r
	={	$$ = mn1(CARAT,$2); }
	| r '$'
	={	i = mn0('\n');
		if(!divflg){
			j = mn1(S2FINAL,-casecount);
			k = mn2(RCAT,$1,j);
			$$ = mn2(DIV,k,i);
			}
		else $$ = mn2(RCAT,$1,i);
		divflg = TRUE;
		}
	| '(' r ')'
	={	$$ = $2; }
	|	NULLS
	={	$$ = mn0(RNULLS); }
	;
%%
yylex(){
	register char *p;
	register int c, i;
	char  *t, *xp;
	int n, j, k, x;
	static int sectbegin;
	static char token[TOKENSIZE];
	static int iter;

# ifdef DEBUG
	yylval = 0;
# endif

	if(sect == DEFSECTION) {		/* definitions section */
		while(!eof) {
			if(prev == '\n'){		/* next char is at beginning of line */
				getl(p=buf);
				switch(*p){
				case '%':
					switch(c= *(p+1)){
					case '%':
						lgate();
						if(!ratfor)fprintf(fout,"# ");
						fprintf(fout,"define YYNEWLINE %d\n",ctable['\n']);
						if(!ratfor)fprintf(fout,"yylex(){\nint nstr; extern int yyprevious;\n");
						sectbegin = TRUE;
						i = treesize*(sizeof(*name)+sizeof(*left)+
							sizeof(*right)+sizeof(*nullstr)+sizeof(*parent))+ALITTLEEXTRA;
						c = myalloc(i,1);
						if(c == 0)
							error("Too little core for parse tree");
						p = c;
						cfree(p,i,1);
						name = myalloc(treesize,sizeof(*name));
						left = myalloc(treesize,sizeof(*left));
						right = myalloc(treesize,sizeof(*right));
						nullstr = myalloc(treesize,sizeof(*nullstr));
						parent = myalloc(treesize,sizeof(*parent));
						if(name == 0 || left == 0 || right == 0 || parent == 0 || nullstr == 0)
							error("Too little core for parse tree");
						return(freturn(DELIM));
					case 'p': case 'P':	/* has overridden number of positions */
						while(*p && !digit(*p))p++;
						maxpos = siconv(p);
# ifdef DEBUG
						if (debug) printf("positions (%%p) now %d\n",maxpos);
# endif
						if(report == 2)report = 1;
						continue;
					case 'n': case 'N':	/* has overridden number of states */
						while(*p && !digit(*p))p++;
						nstates = siconv(p);
# ifdef DEBUG
						if(debug)printf( " no. states (%%n) now %d\n",nstates);
# endif
						if(report == 2)report = 1;
						continue;
					case 'e': case 'E':		/* has overridden number of tree nodes */
						while(*p && !digit(*p))p++;
						treesize = siconv(p);
# ifdef DEBUG
						if (debug) printf("treesize (%%e) now %d\n",treesize);
# endif
						if(report == 2)report = 1;
						continue;
					case 'o': case 'O':
						while (*p && !digit(*p))p++;
						outsize = siconv(p);
						if (report ==2) report=1;
						continue;
					case 'a': case 'A':		/* has overridden number of transitions */
						while(*p && !digit(*p))p++;
						if(report == 2)report = 1;
						ntrans = siconv(p);
# ifdef DEBUG
						if (debug)printf("N. trans (%%a) now %d\n",ntrans);
# endif
						continue;
					case 'k': case 'K': /* overriden packed char classes */
						while (*p && !digit(*p))p++;
						if (report==2) report=1;
						cfree(pchar, pchlen, sizeof(*pchar));
						pchlen = siconv(p);
# ifdef DEBUG
						if (debug) printf( "Size classes (%%k) now %d\n",pchlen);
# endif
						pchar=pcptr=myalloc(pchlen, sizeof(*pchar));
						continue;
					case 't': case 'T': 	/* character set specifier */
						ZCH = atoi(p+2);
						if (ZCH < NCH) ZCH = NCH;
						if (ZCH > 2*NCH) error("ch table needs redeclaration");
						chset = TRUE;
						for(i = 0; i<ZCH; i++)
							ctable[i] = 0;
						while(getl(p) && scomp(p,"%T") != 0 && scomp(p,"%t") != 0){
							if((n = siconv(p)) <= 0 || n > ZCH){
								warning("Character value %d out of range",n);
								continue;
								}
							while(!space(*p) && *p) p++;
							while(space(*p)) p++;
							t = p;
							while(*t){
								c = ctrans(&t);
								if(ctable[c]){
									if (printable(c))
										warning("Character '%c' used twice",c);
									else
										warning("Character %o used twice",c);
									}
								else ctable[c] = n;
								t++;
								}
							p = buf;
							}
						{
						char chused[2*NCH]; int kr;
						for(i=0; i<ZCH; i++)
							chused[i]=0;
						for(i=0; i<NCH; i++)
							chused[ctable[i]]=1;
						for(kr=i=1; i<NCH; i++)
							if (ctable[i]==0)
								{
								while (chused[kr] == 0)
									kr++;
								ctable[i]=kr;
								chused[kr]=1;
								}
						}
						lgate();
						continue;
					case 'r': case 'R':
						c = 'r';
					case 'c': case 'C':
						if(lgatflg)
							error("Too late for language specifier");
						ratfor = (c == 'r');
						continue;
					case '{':
						lgate();
						while(getl(p) && scomp(p,"%}") != 0)
							fprintf(fout, "%s\n",p);
						if(p[0] == '%') continue;
						error("Premature eof");
					case 's': case 'S':		/* start conditions */
						lgate();
						while(*p && index(*p," \t,") < 0) p++;
						n = TRUE;
						while(n){
							while(*p && index(*p," \t,") >= 0) p++;
							t = p;
							while(*p && index(*p," \t,") < 0)p++;
							if(!*p) n = FALSE;
							*p++ = 0;
							if (*t == 0) continue;
							i = sptr*2;
							if(!ratfor)fprintf(fout,"# ");
							fprintf(fout,"define %s %d\n",t,i);
							scopy(t,sp);
							sname[sptr++] = sp;
							sname[sptr] = 0;	/* required by lookup */
							if(sptr >= STARTSIZE)
								error("Too many start conditions");
							sp += slength(sp) + 1;
							if(sp >= schar+STARTCHAR)
								error("Start conditions too long");
							}
						continue;
					default:
						warning("Invalid request %s",p);
						continue;
						}	/* end of switch after seeing '%' */
				case ' ': case '\t':		/* must be code */
					lgate();
					fprintf(fout, "%s\n",p);
					continue;
				default:		/* definition */
					while(*p && !space(*p)) p++;
					if(*p == 0)
						continue;
					prev = *p;
					*p = 0;
					bptr = p+1;
					yylval = buf;
					if(digit(buf[0]))
						warning("Substitution strings may not begin with digits");
					return(freturn(STR));
					}
				}
			/* still sect 1, but prev != '\n' */
			else {
				p = bptr;
				while(*p && space(*p)) p++;
				if(*p == 0)
					warning("No translation given - null string assumed");
				scopy(p,token);
				yylval = token;
				prev = '\n';
				return(freturn(STR));
				}
			}
		/* end of section one processing */
		}
	else if(sect == RULESECTION){		/* rules and actions */
		while(!eof){
			switch(c=gch()){
			case '\0':
				return(freturn(0));
			case '\n':
				if(prev == '\n') continue;
				x = NEWE;
				break;
			case ' ':
			case '\t':
				if(sectbegin == TRUE){
					cpyact();
					while((c=gch()) && c != '\n');
					continue;
					}
				if(!funcflag)phead2();
				funcflag = TRUE;
				if(ratfor)fprintf(fout,"%d\n",30000+casecount);
				else fprintf(fout,"case %d:\n",casecount);
				if(cpyact()){
					if(ratfor)fprintf(fout,"goto 30997\n");
					else fprintf(fout,"break;\n");
					}
				while((c=gch()) && c != '\n');
				if(peek == ' ' || peek == '\t' || sectbegin == TRUE){
					warning("Executable statements should occur right after %%");
					continue;
					}
				x = NEWE;
				break;
			case '%':
				if(prev != '\n') goto character;
				if(peek == '{'){	/* included code */
					getl(buf);
					while(!eof && getl(buf) && scomp("%}",buf) != 0)
						fprintf(fout,"%s\n",buf);
					continue;
					}
				if(peek == '%'){
					c = gch();
					c = gch();
					x = DELIM;
					break;
					}
				goto character;
			case '|':
				if(peek == ' ' || peek == '\t' || peek == '\n'){
					if(ratfor)fprintf(fout,"%d\n",30000+casecount++);
					else fprintf(fout,"case %d:\n",casecount++);
					continue;
					}
				x = '|';
				break;
			case '$':
				if(peek == '\n' || peek == ' ' || peek == '\t' || peek == '|' || peek == '/'){
					x = c;
					break;
					}
				goto character;
			case '^':
				if(prev != '\n' && scon != TRUE) goto character;	/* valid only at line begin */
				x = c;
				break;
			case '?':
			case '+':
			case '.':
			case '*':
			case '(':
			case ')':
			case ',':
			case '/':
				x = c;
				break;
			case '}':
				iter = FALSE;
				x = c;
				break;
			case '{':	/* either iteration or definition */
				if(digit(c=gch())){	/* iteration */
					iter = TRUE;
				ieval:
					i = 0;
					while(digit(c)){
						token[i++] = c;
						c = gch();
						}
					token[i] = 0;
					yylval = siconv(token);
					munput('c',c);
					x = ITER;
					break;
					}
				else {		/* definition */
					i = 0;
					while(c && c!='}'){
						token[i++] = c;
						c = gch();
						}
					token[i] = 0;
					i = lookup(token,def);
					if(i < 0)
						warning("Definition %s not found",token);
					else
						munput('s',subs[i]);
					continue;
					}
			case '<':		/* start condition ? */
				if(prev != '\n')		/* not at line begin, not start */
					goto character;
				t = slptr;
				do {
					i = 0;
					c = gch();
					while(c != ',' && c && c != '>'){
						token[i++] = c;
						c = gch();
						}
					token[i] = 0;
					if(i == 0)
						goto character;
					i = lookup(token,sname);
					if(i < 0) {
						warning("Undefined start condition %s",token);
						continue;
						}
					*slptr++ = i+1;
					} while(c && c != '>');
				*slptr++ = 0;
				/* check if previous value re-usable */
				for (xp=slist; xp<t; )
					{
					if (strcmp(xp, t)==0)
						break;
					while (*xp++);
					}
				if (xp<t)
					{
					/* re-use previous pointer to string */
					slptr=t;
					t=xp;
					}
				if(slptr > slist+STARTSIZE)		/* note not packed ! */
					error("Too many start conditions used");
				yylval = t;
				x = SCON;
				break;
			case '"':
				i = 0;
				while((c=gch()) && c != '"' && c != '\n'){
					if(c == '\\') c = usescape(c=gch());
					token[i++] = c;
					if(i > TOKENSIZE){
						warning("String too long");
						i = TOKENSIZE-1;
						break;
						}
					}
				if(c == '\n') {
					yyline--;
					warning("Non-terminated string");
					yyline++;
					}
				token[i] = 0;
				if(i == 0)x = NULLS;
				else if(i == 1){
					yylval = token[0];
					x = CHAR;
					}
				else {
					yylval = token;
					x = STR;
					}
				break;
			case '[':
				for(i=1;i<NCH;i++) symbol[i] = 0;
				x = CCL;
				if((c = gch()) == '^'){
					x = NCCL;
					c = gch();
					}
				while(c != ']' && c){
					if(c == '\\') c = usescape(c=gch());
					symbol[c] = 1;
					j = c;
					if((c=gch()) == '-' && peek != ']'){		/* range specified */
						c = gch();
						if(c == '\\') c = usescape(c=gch());
						k = c;
						if(j > k) {
							n = j;
							j = k;
							k = n;
							}
						if(!(('A' <= j && k <= 'Z') ||
						     ('a' <= j && k <= 'z') ||
						     ('0' <= j && k <= '9')))
							warning("Non-portable Character Class");
						for(n=j+1;n<=k;n++)
							symbol[n] = 1;		/* implementation dependent */
						c = gch();
						}
					}
				/* try to pack ccl's */
				i = 0;
				for(j=0;j<NCH;j++)
					if(symbol[j])token[i++] = j;
				token[i] = 0;
				p = ccptr;
				if(optim){
					p = ccl;
					while(p <ccptr && scomp(token,p) != 0)p++;
					}
				if(p < ccptr)	/* found it */
					yylval = p;
				else {
					yylval = ccptr;
					scopy(token,ccptr);
					ccptr += slength(token) + 1;
					if(ccptr >= ccl+CCLSIZE)
						error("Too many large character classes");
					}
				cclinter(x==CCL);
				break;
			case '\\':
				c = usescape(c=gch());
			default:
			character:
				if(iter){	/* second part of an iteration */
					iter = FALSE;
					if('0' <= c && c <= '9')
						goto ieval;
					}
				if(alpha(peek)){
					i = 0;
					yylval = token;
					token[i++] = c;
					while(alpha(peek))
						token[i++] = gch();
					if(peek == '?' || peek == '*' || peek == '+')
						munput('c',token[--i]);
					token[i] = 0;
					if(i == 1){
						yylval = token[0];
						x = CHAR;
						}
					else x = STR;
					}
				else {
					yylval = c;
					x = CHAR;
					}
				}
			scon = FALSE;
			if(x == SCON)scon = TRUE;
			sectbegin = FALSE;
			return(freturn(x));
			}
		}
	/* section three */
	ptail();
# ifdef DEBUG
	if(debug)
		fprintf(fout,"\n/*this comes from section three - debug */\n");
# endif
	while(getl(buf) && !eof)
		fprintf(fout,"%s\n",buf);
	return(freturn(0));
	}
/* end of yylex */
# ifdef DEBUG
freturn(i)
  int i; {
	if(yydebug) {
		printf("now return ");
		if(i < NCH) allprint(i);
		else printf("%d",i);
		printf("   yylval = ");
		switch(i){
			case STR: case CCL: case NCCL:
				strpt(yylval);
				break;
			case CHAR:
				allprint(yylval);
				break;
			default:
				printf("%d",yylval);
				break;
			}
		putchar('\n');
		}
	return(i);
	}
# endif
