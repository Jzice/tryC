/*deal with rhe functions and namespaces*/
/*����ļ�������Ҫ��ϵͳ�Դ���һЩ�������ָ��ĺ������Լ��Զ��庯����ʵ�ֺͱ�������������ĳ�ʼ��*/
#ifndef FUNCTION_H_
#define FUNCTION_H_

#include "calculator.h"
#include "quadHash.h"
#include "list.h"

//�Զ��庯����ʵ�ֹ��̣�ʹ��c���Ժ���ģ�⣻
double fun_(char* input, int p,ElementType *pele);

//��ʼ�������������ϵͳ������
void ini_functions();

//�����ڱ���������ջ��Ѱ�ұ�����
ElementType *find_Elem(char* name);

#endif
