/*-
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)pscan.c	4.3 (Berkeley) %G%";
#endif /* not lint */

#include "names.h"
#include "conp.h"
#define isadv(c)	(c == ADV || c == ADJ_ADV)
#define isadj(c)	(c == ADJ || c == NOUN_ADJ || c == ADJ_ADV || c == UNK)
#define notnoun(c)	(c != NOUN && c != UNK && c != NOUN_VERB && c != NV_PL && c != PNOUN)
#define issing(c)	(c == UNK || c == NOUN_VERB || c == NOUN || c == NOM || c == NOUN_ADJ)
#define isnoun(c)	(c == NOUN || c == NOUN_VERB || c == NV_PL || c == UNK || c == PNOUN)
#define nounnom(c)	(c == NOUN || c == NOM)
char lastc,nextc;
int savsub;
scan(si,ce,command)	/*scan a phrase */
int si; 
char ce;
{
	int savi;
	char c;
	i=si;
	if(command == 1)subty = PLURAL;
	else subty = 0;
	if(sent[i].cc==ADJ_ADV && sent[i+1].cc != NOUN && sent[i+1].cc != UNK && sent[i+1].cc != ADJ)
		sent[i++].cc = ADV;
	done=0;
	verb = 0;
	verb=getv(i,ce);	/*get verb if obvious*/
	if(command == 0)j=getnoun(i,ce);		/*get subject*/
	else j = si;
	if(i != j || ((i==j) && sent[i].cc == NOUN)){
		i = j+1;
	}
	for(;((c=sent[i].cc) != ce) && (c != END)  && (c != '\0');i++){
		nextc=sent[i+1].cc;
		if(i>0){
			lastc=sent[i-1].cc;
			if(lastc==BE)be=1;
			else{
				if(lastc != ADV)be=0;
			}
		}
		else lastc=0;
		if(verb==1)question=0;
		switch(c){
		case '"':
			if(nextc==ED || nextc == MVERB){
				verb=1;
				sent[++i].cc=VERB;
				subty=SING;
				continue;
			}
			subty=0;
			verb=getv(++i,ce);
			i=getnoun(i,ce);
			continue;
		case MVERB:
			sent[i].cc = VERB;
			if(i < nsleng*.5){
				verb = getv(++i,ce);
				i = getnoun(i,ce);
				continue;
			}
			continue;
		case ART:
		case ADJ:
		case POS:	
		case ING:
			aflg=1;
			i=getnoun(i,ce);
			aflg=0;
			continue;
		case PREP:	
			if(nextc == ce){
				sent[i].cc=ADV;
				goto sdone;
			}
			prep=1;
			i=getnoun(++i,ce);
			prep=0;
			continue;
		case VERB_ADJ:
			if(verb==0){
				sent[i].cc=VERB;
				verb=1;
				continue;
			}
		case NOUN_ADJ:
			if(be==1){
				sent[i].cc=ADJ;
				continue;
			}
		case PRONP_ADJ:
		case PRONS_ADJ:
			i=getnoun(i,ce);
			continue;
		case NOUN_ADV:
			if(verb == 1 && iverb == i+1){
				sent[i].cc = NOUN;
			}
			else {
				sent[i].cc = ADV;
			}
			continue;
		case ADJ_ADV:
			if(be == 1){
				if (isadj(nextc)) {
					sent[i].cc = ADV;
					continue;
				}
				if(nextc == ',' && isadv(sent[i+2].cc)) {
					sent[i++].cc = ADV;
					sent[++i].cc = ADV;
					comma--;
					continue;
				}
				sent[i].cc = ADJ;
				continue;
			}
			if(lastc == NOUN && (nextc == ',' || nextc == END)){
				sent[i].cc=ADJ;
				continue;
			}
			if(notnoun(nextc)){
				sent[i].cc=ADV;
				continue;
			}
			sent[i].cc=ADJ;
			continue;
		case WHO:
			i=who(i,ce);
			continue;
		case PRONP:
			subty=PLURAL;
			continue;
		case NOUN:
			if(nextc==NOUN){
				sent[i].cc=ADJ;
				continue;
			}
		case PRONS:
			subty=SING;
			continue;
		case PNOUN:
			sent[i].cc = NOUN;
			if(subty == 0)subty=PLURAL;
			continue;
		case CONJ:
			if(nextc==UNK || nextc == NOUN_VERB || nextc == NV_PL){
				if(lastc == ADJ)sent[++i].cc = ADJ;
				else sent[++i].cc = VERB;
			}
			if(nextc == ED){
				sent[++i].cc = VERB;
			}
			continue;
		case AUXP:
		case AUXS:
		case BES:
		case BEP:
		case AUXV:
		case AUX:
		case HAS:
		case HAVE:
		case HAD:
			verb=getv(i,ce);
			continue;
		case ADV:
		case AUXX:
		case VERB:
		case BE:
		case INTER:
			continue;
		case THAT:
			if(nextc==SUBCONJ){
				sent[i].cc=PRONP;
				goto subc;
			}
			verb=getv(i+1,ce);
			j = i+1;
			while(isadv(sent[j].cc)) {
				j++;
			}
			nextc = sent[j].cc;
			if((verb==1 && iverb== j) || nextc==ED || nextc == VERB_ADJ){
				sent[i].cc=PRONP;
				while(i+1 < j)sent[++i].cc = ADV;
			}
			else if(verb==0 && nextc==NV_PL && ((lastc==NOUN
				&& issing(sent[i-1].ic)) || lastc == ',')) {
				sent[i].cc=PRONP;
				subty=SING;
				while(i+1 < j)sent[++i].cc = ADV;
			}
			else if(verb==0 && (nextc==UNK || nextc==NOUN_VERB) && (lastc==NOUN
				&& sent[i-1].ic==NV_PL)){
				subty=PLURAL;
				sent[i].cc=PRONP;
				while(i+1 < j)sent[++i].cc = ADV;
			}
			else {
				if(i == 0)sent[i].cc=ADJ;
				else sent[i].cc=SUBCONJ;
				j=i;
				subty=0;
				i=getnoun(i+1,ce);
				if(done)sent[j].cc=ADJ;
			}
			continue;
		case ',':
			if(nextc != CONJ){	/*parenthetical*/
				continue;
			}
			i++;
			comma--;
			if(sent[i+1].cc == ED || sent[i+1].cc == MVERB){
				sent[++i].cc = VERB;
				continue;
			}
			if(command == 1){
				verb = getv(++i,ce);
				continue;
			}
		case SUBCONJ:	
subc:
			savi = i;
			if(nextc==END||(lastc==',' && nextc==',')){
				sent[i].cc=ADV;
				continue;
			}
			subty=0;
			ce=',';
			verb=getv(++i,ce);
			if(sent[savi+1].cc == VERB || sent[savi+1].cc == AUXX)
				if(sent[savi].cc == SUBCONJ)sent[savi].cc = ADV;
			i=getnoun(i,ce);
			continue;
		case PREP_ADV:
			if(sent[i+2].cc==PREP_ADV &&(sent[i+1].cc != NOUN && sent[i+1].cc != NOUN_VERB
				&& sent[i+1].cc != NV_PL && sent[i+1].cc != PNOUN)){
				sent[i].cc=ADV;
				sent[i+2].cc=CONJ;
				continue;
			}
			sav=i++;
			savsub = subty;
			pverb=getv(i,ce);
			if((pverb==1 && iverb == i) || sent[i].cc== ED){
				sent[sav].cc=PRONP;
				sent[i].cc=VERB;
				continue;
			}
			i=getnoun(i,ce);
			if(done==1){
				sent[sav].cc=ADV;
				goto sdone;
			}
			if(pverb==1 && iverb == i+1){
				sent[sav].cc=SUBCONJ;
				ce=',';
				continue;
			}
			switch(sent[i+1].cc){
			case UNK:
			case NV_PL:
			case NOUN_VERB:
			case ED:
				sent[sav].cc=SUBCONJ;
				verb=0;
				ce=',';
				continue;
			default:
				sent[sav].cc=PREP;
				subty = savsub;
				continue;
			}
		case TO:	
			savi = i;
			sent[i++].cc=VERB;
sw:
			switch(nextc){
			case UNK:
			case AUXS:
			case VERB_ADJ:
			case NOUN_VERB:	
			case VERB:
			case MVERB:
				sent[i].cc=VERB;
				continue;
			case HAVE:
				sent[i].cc = VERB;
				if(sent[i+1].cc == ED){
					sent[++i].cc = VERB;
					continue;
				}
				if(sent[i+1].cc == ADV)i++;
				if(sent[i+1].cc != BE)continue;
				i++;
			case BE:	
				sent[i].cc=VERB;
				if(sent[i+1].cc == ADV)i++;
				if(sent[i+1].cc == ED || sent[i+1].cc == ING){
					sent[++i].cc = VERB;
				}
				else if(sent[i+1].cc == UNK){
					sent[++i].cc = ADJ;
				}
				continue;
			case ADV:
				nextc = sent[++i].cc;
				goto sw;
			default:
				sent[savi].cc = PREP;
				prep=1;
				i=getnoun(i,ce);
				prep=0;
				continue;
			}
		case NV_PL: 
			if(subty==0){
				i=getnoun(i,ce);
				subty=PLURAL;
			}
			else if(verb== 0){	/*need verb*/
				sent[i].cc=VERB;
				verb=1;
			}
			else{
				i=getnoun(i,ce);
			}
			continue;
		case UNK:	
		case NOUN_VERB:
			if(verb==1){
				if(be==1 && nextc != WHO){
					sent[i].cc=ADJ;
					continue;
				}
				i=getnoun(i,ce);
			}
			else if(nextc==NV_PL && isnoun(sent[i+2].cc)) {
				sent[i].cc=NOUN;
				if(sent[i-1].cc == NOUN){
					sent[i-1].cc = ADJ;
				}
				subty=SING;
				continue;
			}
			else if(subty==0){
				i=getnoun(i,ce);
				continue;
			}
			else {		/* desparation */
				sent[i].cc=VERB;
				verb=1;
			}
			continue;
		case ED:
			if(verb == 0){
				sent[i].cc=VERB;
				verb=1;
			}
			else if(sent[i-1].cc == BE){
				sent[i].cc = VERB;
			}
			else{
				i=getnoun(i,ce);
			}
			continue;
		default:	printf("got a %c %o on %sat %d\n",sent[i].cc,sent[i].cc,sent[i].sp,i);
		}
	}
sdone:
	if(sent[i].cc==END && sent[i].ic != ';')return(-1);
	else {
		if(sent[i].ic == ';')sent[i].cc=';';
		else comma--;
		return(i);
	}
}
getv(si,ce)
int si; 
char ce;
{
	int conj;
	char c;
	int ik;
	must=0;
	bflg=0;
	hflg=0;
	verbty=0;
	for(j=si;((c=sent[j].cc) != ce) && (c != END); j++){
		iverb=j;
		switch(c){
		case ED:
			continue;
		case HAS:
			hflg=1;
			verbty=SING;
			sent[j].cc=AUXX;
			goto next;
		case HAVE:
			if(sent[j-1].cc==TO){
				continue;
			}
		case HAD:
			hflg=1;
			verbty=PLURAL;
			sent[j].cc=AUXX;
			goto next;
		case BE:
			if(sent[j].ic != BE){
				return(1);
			}
			continue;
		case VERB:	
			if(sent[j-1].cc==TO){
				continue;
			}
			return(1);
		case AUXX:
			return(1);
		case AUXP:	
			must=1;
			verbty=PLURAL;
			sent[j].cc=AUXX;
			goto next;
		case AUXS:	
			if(sent[j-1].cc==TO){
				continue;
			}
			verbty=SING;
			sent[j].cc=AUXX;
			goto next;
		case AUX:
		case AUXV:
			must=1;
			sent[j].cc=AUXX;
next:	
			if(sent[j-1].cc == CONJ && verb == 0)conj = 0;
			else conj = 1;
			if(question==1){
				j=getnoun(j+1,ce);
				question=0;
			}
			getv2(ce);
			if(sent[j].cc == AUXX){
				sent[j].cc = VERB;
			}
			if((sent[j].cc!=VERB && sent[j].cc!=BE) && sent[iverb].cc==AUXX){
				sent[iverb].cc=VERB;
				for(ik=iverb+1;ik<=j;ik++){
					if(sent[ik].ic == NOM)sent[ik].cc=NOUN;
					else sent[ik].cc=sent[ik].ic;
				}
			}
			return(conj);
		case BES:	
			verbty=SING;
			bflg=1;
			sent[j].cc=BE;
			goto next;
		case BEP:	
			verbty=PLURAL;
			bflg=1;
			sent[j].cc=BE;
			goto next;
		case SUBCONJ:
			if(sent[j-1].cc== ',' && sent[j+1].cc == ','){
				continue;
			}
		case THAT:
		case WHO:
			goto vdone;
		case ',':	if(comma==1 && sent[j+1].cc==CONJ){
					goto vdone;
				}
		}
	}
vdone:
	return(0);
}
getv2(ce)
char ce;
{
	int vflg;
	char c;
	vflg=0;
	while(((c=sent[++j].cc) != ce) && (c != END)){
		switch(c){
		case NOUN_ADV:
		case ADJ_ADV:	
			if(bflg == 0 || sent[j+1].cc == ED || sent[j+1].cc == ING){
				sent[j].cc = ADV;
			}
			else{
				j -= 1;
				return;
			}
		case ADV:	
			continue;
		case SUBCONJ:
			if(sent[j+1].cc == ED || sent[j+1].cc == ING || sent[j+1].cc == ADV ||
				sent[j+1].cc == ADJ_ADV)sent[j].cc = ADV;
			else return;
			continue;
		case VERB:	
			return;
		case VERB_ADJ:
		case ED:	
			sent[j].cc=VERB;
			return;
		case BE:	
			must=0;
			bflg=1;
			continue;
		case ING:	
			if(bflg == 1){
				sent[j].cc = VERB;
			}
			else {
				j -= 1;
			}
			return;
		case NOUN_VERB:
		case UNK:	
			if(bflg==1 || vflg==1 || hflg==1){
				j -= 1;
				return;
			}
			sent[j].cc=VERB;
			return;
		case PRONP_ADJ:
		case PRONS_ADJ:	
			c=sent[j+1].cc;
			if(c==ED||c==ING||c==BE||must==1){
				sent[j].cc = ADV;
			}
			else{
				j -= 1;
				return;
			}
			continue;
		case AUXV:
		case AUX:
		case AUXP:
		case AUXS:
		case HAD:
		case HAVE:
		case HAS:
			must=0;
			vflg=1;
			sent[j].cc=AUXX;
			continue;
		default:
			j -= 1;
			return;
		}
	}
	j -= 1;
}
getnoun(k,ce)
int k,ce;
{
	char c;
	int st,t,nextst;
	int rep;
	int t1,tt;
	st=k;
	rep=0;
b:	
	nextst=sent[st+1].cc;
	switch(sent[st].cc){
	case END:
		done=1;
		break;
	case ',':
		if(ce==','){
			done=1;
			break;
		}
		else {
			goto getdef;
		}
	case '"':
		if(aflg == 1){
			st++;
			goto b;
		}
		break;
	case NOUN:	
		switch(nextst){
		case UNK:
		case NOUN_VERB:
			if(verb==1){
				goto nn;
			}
			if(subty != PLURAL){
				goto nn;
			}
			break;
		case NV_PL:
			if(verb==1){
				goto nn;
			}
			if(subty == PLURAL){
				goto nn;
			}
			break;
		case PNOUN:
		case NOUN_ADJ:
		case POS:
		case NOUN:
nn:
			sent[st].cc=ADJ;
			goto b;
		}
		if(prep==0)subty=SING;
		break;
	case PNOUN:
		sent[st].cc = NOUN;
		if(prep == 0)subty=PLURAL;
		break;
	case ADV:
		st++;
		goto b;
	case WHO:
		st=who(st,ce);
		st++;
		goto b;
	case ADJ_ADV:
	case VERB_ADJ:
	case ED:
		sent[st++].cc=ADJ;
		goto b;
	case ING:
		if(isnoun(nextst)){
			sent[st++].cc=ADJ;
			goto b;
		}
		sent[st].cc=NOUN;
		if(prep==0)subty=SING;
		break;
	case ART:
	case POS:
		if(st != k){
			goto getdef;
		}
	case ADJ:
		st++;
		goto b;
	case PRONP:	
		if(prep==0)subty=PLURAL;

		break;
	case PRONS:	
		if(prep==0)subty=SING;
		break;
	case NOUN_VERB:	
		if((nextst==NOUN|| nextst==PNOUN)||(verb==1 && (nextst== NV_PL|| nextst==NOUN_VERB) )){
			sent[st].cc=ADJ;
			goto b;
		}
	case NV_PL:
		sent[st].cc=NOUN;
		if(prep==0){
			if(sent[st].ic==NV_PL)subty=PLURAL;
			else subty=SING;
		}
		if(sent[st-1].cc == NOUN && nounnom(sent[st-1].ic)) {
			sent[st-1].cc = ADJ;
		}
		break;
	case PRONP_ADJ:	
		switch(nextst){
		case PNOUN:
		case NOUN_ADJ:
		case NV_PL:	
		case ADJ_ADV:
		case PRONS_ADJ:	
		case ADJ:
		case ING:
			sent[st++].cc=ADJ;
			goto b;
		case NOUN_VERB:
		case UNK:	
		case ED:
			if(verb == 0){
				goto def;
			}
			else{
				sent[st++].cc=ADJ;
				goto b;
			}
		case NOUN:	
			sent[st++].cc=ADJ;
			if(prep==0)subty=SING;
			break;
		case SUBCONJ:	sent[st++].cc=PRONP;	/* more than .. */
			sent[st++].cc=CONJ;
			if(verb==0)verb=getv(st,ce);
			goto b;
		case '"':
			sent[st++].cc = ADJ;
			st++;
			goto b;
		default:	
def:
			sent[st].cc=PRONP;
			if(prep==0)subty=PLURAL;

			break;
		}
		break;
	case PRONS_ADJ:	
		if(aflg == 1 && nextst != END && nextst != ART){
			sent[st++].cc=ADJ;
			goto b;
		}
		switch(nextst){
		case '"':
			sent[st++].cc = ADJ;
			st++;
			goto b;
		case PNOUN:
		case ING:
		case UNK:	
		case NOUN_VERB:
		case NOUN_ADJ:
		case ADJ_ADV:
		case ADJ:
			sent[st++].cc=ADJ;
			goto b;
		case NOUN:	
			sent[st++].cc=ADJ;
			if(prep==0)subty=SING;
			break;
		case PRONS_ADJ:	
			sent[st++].cc=ADJ;
			sent[st].cc=PRONP;
			if(prep==0)subty=SING;
			break;
		case NV_PL:	
			if(verb==1){
				sent[st++].cc=ADJ;
				sent[st].cc=NOUN;
				if(prep==0)subty=PLURAL;

				break;
			}
			sent[st].cc=PRONP;
			if(prep==0)subty=SING;
			break;
		default:	
			sent[st].cc=PRONP;
			if(prep==0)subty=SING;

			break;
		}
		break;
	case NOUN_ADJ:
		if(nextst==NOUN||nextst==ADJ||nextst == NOUN_ADJ||nextst==PRONS_ADJ||nextst==PRONP_ADJ||nextst==PNOUN){
			sent[st++].cc=ADJ;
			goto b;
		}
		if(nextst!=UNK && nextst != NOUN_VERB && nextst != NV_PL){
			sent[st].cc=NOUN;
			if(prep==0)subty=SING;
			break;
		}
		if(verb==1 || sent[st+2].cc==ED){
			sent[st++].cc=ADJ;
			goto b;
		}
		sent[st].cc=NOUN;
		if(prep==0)subty=SING;
		break;
	case UNK:
		if(nextst != UNK){
			if(nextst == ',' && rep==0){
				c=sent[st+2].cc;
				if((c==UNK||c==ADJ||c==NOUN_ADJ||c==ING) && isnoun(sent[st+3].cc)){
					comma--;
					sent[st].cc=ADJ;
					sent[st+2].cc=ADJ;
					sent[st+3].cc=NOUN;
					if(prep==0){
						if(sent[st+3].ic==NV_PL)subty=PLURAL;
						else subty=SING;
					}
					return(st+3);
				}
				else {
					sent[st].cc=NOUN;
					if(prep==0)subty=SING;
					break;
				}
			}
			if(nextst==NOUN||nextst==ING ||nextst==NOUN_ADJ|| sent[st-1].cc==ADV
				|| nextst==ADJ || nextst==PNOUN){
				sent[st++].cc=ADJ;
				goto b;
			}
			if((nextst==NV_PL || nextst==NOUN_VERB) &&(verb==1 ||(verb==0 && (sent[st+2].cc==UNK
				|| sent[st+2].cc==NOUN_VERB || sent[st+2].cc==ED||sent[st+2].cc==VERB_ADJ)))){
					sent[st++].cc=ADJ;
					sent[st].cc=NOUN;
					if(prep == 0)subty=PLURAL;
					break;
				}
			if(nextst == CONJ && isadv(sent[st+2].cc)){
				sent[st].cc=ADJ;
				goto b;
			}
			sent[st].cc=NOUN;
			if(prep==0)subty=SING;

			break;
		}
		for(t=st+1;sent[t].cc== UNK;t++);
		if(verb==0 && prep == 0){		/* UUU. */
			if(prep==0)subty=SING;
			if(sent[t].cc==NV_PL|| sent[t].cc == PNOUN){	/* UUZ.*/
				if(sent[t+1].cc==UNK || sent[t+1].cc==NOUN_VERB){	/* UUZU */
					sent[t+1].cc=VERB;
					verb=1;
					sent[t].cc=NOUN;
					t1=t;
				}
				else if(sent[t].cc == NV_PL){		/* UUZ. */
					sent[t].cc=VERB;
					verb=1;
					sent[t-1].cc=NOUN;
					t1=t-1;
				}
				else {
					sent[t].cc = NOUN;
					t1 = t;
				}
			}
			else{		/* UU. */
				if(sent[t].cc!= ED){
					sent[t-1].cc=VERB;
					verb=1;
					sent[t-2].cc=NOUN;
					t1=t-2;
					t--;
				}
				else {
					sent[--t].cc=NOUN;
					t1=t;
				}
			}
		}
		else{
			if(sent[t].cc==NOUN_VERB|| sent[t].cc==NOUN|| sent[t].cc==NV_PL|| sent[t].cc==PNOUN){
				sent[t].cc=NOUN;
				if(prep==0)subty=PLURAL;
				t1=t;
			}
			else{
				sent[--t].cc=NOUN;
				if(prep==0)subty=SING;
				t1=t;
			}
		}
		for(tt=st;tt<t1;tt++)sent[tt].cc=ADJ;
		st=t;
		break;
	}
getdef:
	if(sent[st].cc == CONJ && sent[st-1].cc == ADJ){
		st++;
		goto b;
	}
	if(sent[st-1].cc==PREP && ( sent[st].cc == THAT || sent[st].cc == SUBCONJ)){
		prep=0;
		return(--st);
	}
	if(sent[st].cc != NOUN && sent[st].cc != PRONP && sent[st].cc != PRONS){
		st--;
		if(sent[st].cc==ADJ && sent[st].ic==ING){
			sent[st].cc=NOUN;
			if(prep==0)subty=SING;
		}
		else if(sent[st].cc == PREP){
			sent[st].cc = ADV;
		}
		prep=0;
		return(st);
	}
	if(done==1){
		prep=0;
		return(st);
	}
	if(sent[st+1].cc== ','){
		if(sent[st+2].cc==CONJ){
			if(rep==0){
				prep=0;
				return(st);
			}
			else{
				st+=3;
				comma--;
				if(prep==0)subty=PLURAL;
				prep=1;
				goto b;
			}
		}
		if(comma==1){
			prep=0;
			return(st);
		}

		switch(sent[st+2].cc){
		case PREP:
		case SUBCONJ:
		case PRONS:
		case PRONP:
			prep=0;
			return(st);
		default:	rep++;
			comma--;
			st+=2;
			if(prep==0)subty=PLURAL;
			prep=1;
			goto b;
		}
	}
	if(sent[st+1].cc==CONJ && sent[st+2].cc != ED){
		st+=2;
		if(prep==0)subty=PLURAL;
		prep=1;
		goto b;
	}
	prep=0;
	return(st);
}
who(kk,ce)
int kk,ce;
{
	char c;
	sent[kk].cc=PRONP;
	c=sent[kk+1].cc;
	while(isadv(c)){
		sent[++kk].cc = ADV;
		c = sent[kk+1].cc;
	}
	if(c==UNK||c==NOUN_VERB||c==VERB_ADJ||c==ED|| c==NV_PL||c==MVERB){
		sent[++kk].cc=VERB;
		if(verb == 0)verb = getv(kk+1,ce);
		return(kk);
	}
w:	switch(sent[++kk].cc){
	case AUXX:
	case ADV:
	case BE:
	case VERB:
		goto w;
	default:
		verb=getv(kk,ce);
		return(--kk);
	}
}
