j
= �ѡ����ΥǥХå�

�����Ǥϡ�Racc ��ȤäƤ�������������������������ˤĤ��ƽ񤭤ޤ���

== ʸˡ�ե����뤬�ѡ������顼�ˤʤ�

���顼��å������˽ФƤ�����ֹ�Τ�����򸫤ƴְ㤤��
õ���Ƥ����������֥�å����Ĥ���Ԥǥ��顼�ˤʤ���ϡ�
�ɤ����ǳ�����̤ʤɤ����䤷�Ƥ��ޤäƤ����ǽ�����⤤�Ǥ���

== �ʤ󤿤� conflict �äƸ���줿

���֤��꤬���ǰ������ݤ�����Ͼ��� (conflict) �Ǥ��礦��
ʸˡ��˾��ͤ�����ȡ�racc �ϥ���ѥ�����
��5 shift/reduce conflict�פΤ褦�ʥ�å�������ɽ�����ޤ���
-v ��Ĥ���Ƚ��Ϥ���� .output �ե����뤫��Ϥ���˾ܤ������������ޤ���
�����ɤ��Ȥ������Ȥ������������Ȥ˴ؤ��Ƥϡ�����ʤ���ܤ��ɤ�Ǥ���������
�ȤƤ⤳���˽񤱤�褦��ñ����äǤϤ���ޤ���
�����ʤ����Ruby �� 256 �ܻȤ�������� ̵ƻ�ԡ�(������Ϻ��)��������Ǥ���

== �ѡ���������ʤ������Ǥ�������ͽ�ۤɤ����ư���ʤ�

racc �� -g ���ץ�����Ĥ��ƥѡ�������Ϥ���ȡ��ǥХå��ѤΥ����ɤ�
�ղä���ޤ��������ǡ��ѡ������饹�Υ��󥹥����ѿ� @yydebug �� true ��
���Ƥ����Ƥ��� do_parse/yyparse ��Ƥ֤ȡ��ǥХå��ѥ�å�����������
����ޤ����ѡ�����ư����ͻҤ�ľ�ܸ����ޤ��Τǡ������˸��ߤξ��֤�
�İ��Ǥ��ޤ�������򸫤Ƥɤ������������Τ��狼�ä��餢�Ȥ�ľ��������

== next_token �˴ؤ���

���ޤ���ʬ�Ǥ�˺��뤳�Ȥ�¿���Τ�
������ȡ����󤬿Ԥ����� [false,�ʤˤ�] ������פȤ������ȤǤ���
���ʤߤ� Racc 0.10.2 �ʹߤǤϰ��� [false,�ʤˤ�] �������ä���
����ʾ� next_token �ϸƤФʤ����Ȥ��ݾڤ���Ƥ��ޤ���

�ɵ��� �Ƕ�� [false,�ʤˤ�] �ǤϤʤ� nil �Ǥ�褤���Ȥˤʤä���
e
= Debugging

== Racc reported syntax error.

Isn't there too many "end"?
grammar of racc file is changed in v0.10.

Racc does not use '%' mark, while yacc uses huge number of '%' marks..

== Racc reported "XXXX conflicts".

Try "racc -v xxxx.y".
It causes producing racc's internal log file, xxxx.output.

== Generated parsers does not work correctly

Try "racc -g xxxx.y".
This command let racc generate "debugging parser".
Then set @yydebug=true in your parser.
It produces a working log of your parser.
