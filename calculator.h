
/*
 * actualize tiny interpreter
 * version 1.0
 *  2019.3.28
 * -------------
 * support decimal,sign,multiple brackets 
 * no blank is allowed between number and operator 
 * support sin/cos/tan/abs/pow/sqrt functions
 * support if statement, while statement, and defining varities and functions
 * 
 */
 
//main ����Ҳ��calculator.c����
#ifndef CALCULATOR_H_
#define CALCULATOR_H_

//for vs,because no s_scanf();
#define _CRT_SECURE_NO_WARNINGS

#include <ctype.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "quadHash.h"
#include "list.h" 

//�������������״̬���壻
#define ERROR 1e9//����������״̬����
#define PASS 1e-9//��������

double ans; //���ÿһ�μ����Ժ�Ľ���ɹ�ֱ��ʹ�ã�

/*functions buffer*/
char functions[2000];//�Զ��庯���Ĵ�ſռ䣻
int function_pos;//�Զ��庯����ʹ�ÿռ��ָ�룻

Tables T;/*name space*/

/*For input expressions character-by-character processing and generating infix expressions, the number position remains unchanged,while the operator position is changed*/ 
double expression(char* input, int start, int end);//���ڱ��ʽ�ļ���

//����һ����䣻����ֵ�����ж����״̬���Լ�������д��ں�����return��ʹ���䷵�أ�
double sentence(char* input, int start, int end);

//����������ֵ�жϣ�true����1��false����0��ERROR���ش����ǲ���ֵ��
int bool(char* input, int start, int end);

//����һ�����飺����ֵ�����ж����״̬���Լ�������д��ں�����return��ʹ���䷵�أ�
double block(char* input, int start, int end);

extern void ini_functions();//������ʼ��������ĺ����������ռ䣻

#endif
