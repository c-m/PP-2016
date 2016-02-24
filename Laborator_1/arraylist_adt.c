#include <stdio.h>
#include <stdlib.h>

#define INDEX_OUT_OF_BOUNDS 1

int EXCEPTION = 0;
void throw (int e){
	EXCEPTION = e;
}

struct AList {
	int* v;
	int sz, len;
};

typedef struct AList AList;

AList Empty(){
	AList l;
	l.sz = 0;
	l.len = 0;
	return l;
}

void copy (int* src, int s_start, int end, int* dst, int d_start){
	int i;
	for (i = s_start; i<end; i++)
		dst[d_start++] = src[i];
}

AList cons (int e, AList l){
	if (l.v == 0){
		l.v = malloc(sizeof(int));
		l.sz = 1;
		l.len = 1;
		l.v[0] = e;
		return l;
	}
	if (l.sz == l.len){
		int* vp = malloc(l.len*2*sizeof(int));
		copy(l.v,0,l.len,vp,0);
		l.len *= 2;
		l.v = vp;
	}
	l.v[l.sz] = e;
	l.sz++;
	return l;
}

int head (AList l){
	return l.v[l.sz-1];
}

AList tail (AList l){
	l.sz --;
	return l; 
}

int isEmpty(AList l){
	return l.sz == 0;
}

AList add (AList l, int e){
	return cons(e,l);
}

int get (AList l, int pos){
	if (pos == 0) 
		return head(l);
	return get(tail(l),pos-1);
}

AList ins (AList l, int pos, int e){
	if (pos == 0)
		return cons(e,l);
	int h = head(l);
	AList t = tail(l);
	return cons(h,ins(t,pos-1,e));
}

void show (AList l){
	if (isEmpty(l))
		printf("[]\n");
	else{
		printf("%i ",head(l));
		show(tail(l));
	}
}

int main (){
	
	AList l = Empty();
	l = add(l,1);
	show(l);
	l = add(l,2);
	show(l);
	l = add(l,3);
	show(l);
	l = ins(l,2,9);
	
	show(l);

}
