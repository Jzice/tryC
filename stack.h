/*���ڱ��ʽ�����ջ������*/
#ifndef _stack_h
#define _stack_h

typedef struct _charnode /*�ַ����*/ 
{
	char a;
	struct _charnode *next;
} *charstack;

typedef struct _doublenode /*���������*/
{
	double a;
	struct _doublenode *next;
} *doublestack;


charstack newcharstack();
 /*�����ַ�ջ����ʼ��*/ 
 
doublestack newdoublestack();
 /*����������ջ����ʼ��*/
 
int csempty(charstack s);
 /*�ж��Ƿ��ջ*/
int dsempty(doublestack s);

void pushchar(charstack s,char a);
 /*ѹ���ַ�*/
 
void popchar(charstack s);
/*�����ַ�*/ 

void pushdouble(doublestack s,double a);
 /*ѹ������*/
 
void popdouble(doublestack s);
 /*��������*/ 
 
char cstop(charstack s);
 /*���ض����ַ�*/
 
double dstop(doublestack s);
/*���ض�������*/

#endif
