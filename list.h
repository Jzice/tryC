/* list.h: the impliment of a list without headnode.
 * version 1.0 by ZhengYusheng 25/3/2018
 */

#ifndef _LIST_H
#define _LIST_H

#include <stdio.h>
#include <stdlib.h>
#include "quadHash.h"

/* the defination of the list structure*/
//������������Ľṹ����
typedef struct LNode *PtrToLNode;
struct LNode {
    ElementType Data;
    PtrToLNode Next;
};
typedef PtrToLNode List;
//��������������Ľṹ������
typedef struct HashNode *PtrToHNode;
struct HashNode {
	HashTable Data;
	PtrToHNode Next;
};
typedef PtrToHNode Tables;

List L;//�����й������Զ�����Ĺ�ϣ�������Ԫ�ᱻʵ�ʴ���������������

Tables T;//�ɹ�ϣ����ɵ�ջ������ʵ�֣���ģ���ں������й����еı���������ջ

/* Insert X before the node pointed to by location P, 
 * and return the list header of the linked list. 
 * If parameter P points to an illegal location, 
 * print "Wrong Position for Insertion" and return ERROR.
 */ 
List Insert_list( List L, ElementType X );//���������µı����ڵ㣻

/* Delete the element of position P and return to the 
 * list header of the linked list. If parameter P points to an illegal location, 
 * print "Wrong Position for Deletion" and return ERROR.
 * List Delete( List L, Position P );
 */
List Delete_list( List L, ElementType * X );//����ɾ�������µı����ڵ㣻

Tables Insert_tables(Tables L, HashTable H);//�ڴ���������ʱ����б����������Ĳ��룻

Tables RmFirst_tables(Tables T);//�ں�������ʱ���������������



#endif
