#include <stdio.h>
#include <stdlib.h>
#include "stack.h" 

charstack newcharstack() /*�����ַ�ջ����ʼ��*/ 
{
	charstack s;
	s=malloc(sizeof(struct _charnode));
	s->next=NULL;
	return s;
}

doublestack newdoublestack() /*����������ջ����ʼ��*/
{
	doublestack s;
	s=malloc(sizeof(struct _doublenode));
	s->next=NULL;
	return s;
}

int csempty(charstack s) /*�ж��Ƿ��ջ*/
{
	if(s->next==NULL)
		return 1;
	else return 0;
	
}
int dsempty(doublestack s)
{
	if(s->next==NULL)
		return 1;
	else return 0;
}

void pushchar(charstack s,char a) /*ѹ���ַ�*/
{
	charstack newnode;
	newnode=malloc(sizeof(struct _charnode));
	newnode->a=a;
	newnode->next=s->next;
	s->next=newnode;
}

void popchar(charstack s) /*�����ַ�*/ 
{
	charstack newnode;
	if(csempty(s))
		perror("the stack's empty");
	else
	{
		newnode=s->next;
		s->next=s->next->next;
		free(newnode);
	}
}

void pushdouble(doublestack s,double a) /*ѹ������*/
{
	doublestack newnode;
	newnode=malloc(sizeof(struct _doublenode));
	newnode->a=a;
	newnode->next=s->next;
	s->next=newnode;
}

void popdouble(doublestack s) /*��������*/ 
{
	doublestack newnode;
	if(dsempty(s))
		perror("the stack's empty");
	else
	{
		newnode=s->next;
		s->next=s->next->next;
		free(newnode);
		
	}
}

char cstop(charstack s)/*���ض����ַ�*/
{
	if(!csempty(s))
		return s->next->a;
	perror("the stack's empty");
	return 0;
}

double dstop(doublestack s)/*���ض�������*/
{
	if(!dsempty(s))
		return s->next->a;
	perror("the stack's empty");
	return 0;
}
