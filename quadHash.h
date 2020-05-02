/* ƽ��ɢ����hash������ÿ����ϣ�����һ������������*/

#ifndef _HashQuad_H
#define _HashQuad_H

#define _CRT_SECURE_NO_WARNINGS

	enum KindOfEntry { Legitimate, Empty, Deleted };//��ϣ��Ԫ״̬��ʶ��
	enum datatype { var , function , boolean, self, string, array };//Ԫ�����ͱ�ʶ��

	struct hashElem;
	typedef struct hashElem ElementType;//��ϣ�����ڣ������洢ָ��ÿ��Ԫ�ص�ָ�룻Ԫ��������ʱ�Զ������ڴ�ռ�
	
	struct HashEntry;
	typedef struct HashEntry Cell;/*��Ŀ����Ϊcell ,�������ָ��*/ 

	struct HashTbl;
	typedef struct HashTbl *HashTable;//��ϣ��

	typedef char* KeyType;//�����ڹ�ϣ������洢������
	
	typedef double (*afunc)(char* input, int p); /*���ú���*/

	typedef double(*self_fun)(char* input, int p,ElementType* pele);//�Լ�����ĺ���

	struct hashElem { //�Զ���Ԫ�ؽṹ�������洢�������ܹ���
		enum datatype type;//Ԫ������;
		double value1;//˫���ȸ����ͣ�Ĭ��Ԫ�����ͣ����Զ��庯����������ʶ�����鿪ͷ
		int value2;//���ͱ����Ͳ���ֵ�����Զ��庯����������ʶ�������β
		void* f;//ָ�룺�����Ǻ���ָ�룬Ҳ������������ַ���ָ��
	}; 

	struct HashEntry//ÿ����ϣ��Ԫ�����
	{
		char key[30];
		ElementType*  pElement;
		enum KindOfEntry Info;
	};

	struct HashTbl//��ϣ������ṹ
	{
		int TableSize;
		int num;
		Cell *TheCells;    /* Cell *TheCells will be an array of  HashEntry cells, allocated later*/
	};

	typedef unsigned int Index;
	typedef Index Position;

	HashTable InitializeTable(Index TableSize);/*����һ��hash�����ء� */
	void DestroyTable(HashTable H); /*�ɵ������ϣ�� */
	Position Find(KeyType Key, HashTable H);/*����ؼ��ֺ�hash��������hash���е�������Ԫ������ɲ鿴.info */
	int Insert(KeyType Key, ElementType* pElem, HashTable H);/*����hashֵ��Ԫ��ָ���Լ�hash����Ԫ�ز������С� */
	ElementType* Retrieve(Position P, HashTable H);/*��ȡhash��ĳ��λ�õ�Ԫ�ص�ָ�롣 */
	HashTable Rehash(HashTable H);/*���ɱ�����Ϊ2�����ϣ������±� */


#endif  /* _HashQuad_H */
