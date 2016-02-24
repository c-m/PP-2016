#include <stdio.h>
#include <stdlib.h>

#define INDEX_OUT_OF_BOUNDS 1

int EXCEPTION = 0;
void throw (int e){
	EXCEPTION = e;
}

struct LList {
	struct LList* next;
	int val;
};

typedef struct LList* LList;

LList Empty(){
	return 0;
}

LList cons(int e, LList l){
	LList n = malloc(sizeof(struct LList));
	n->val = e;

	n->next = l;
	return n;
}

int isEmpty(LList l){
	return l==0;
}


int head(LList l){
	return l->val;
}


LList tail(LList l){
	return l->next;
}

LList add (LList l, int e){
	return cons(e,l);
}
int get (LList l, int pos){
	if (pos == 0) 
		return head(l);
	return get(tail(l),pos-1);
}

LList ins (LList l, int pos, int e){
	if (pos == 0)
		return cons(e,l);
	int h = head(l);
	LList t = tail(l);
	return cons(h,ins(t,pos-1,e));
}

void show (LList l){
	if (isEmpty(l))
		printf("[]\n");
	else{
		printf("%i ",head(l));
		show(tail(l));
	}
}

int main (){
	
	LList l = Empty();
	l = add(l,1);
	show(l);
	
	l = add(l,2);
	show(l);
	l = add(l,4);
	l = ins(l,3,9);
	
	show(l);
	
}
