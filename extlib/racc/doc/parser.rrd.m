= class Racc::Parser
j
Racc ����������ѡ����Ϥ��٤� Racc::Parser ���饹��Ѿ����ޤ���
Racc::Parser ���饹�ˤϥѡ�����˻��Ѥ���᥽�åɤ������Ĥ����ꡢ
���Τ褦�ʥ᥽�åɤ򥪡��С����ɤ���ȡ��ѡ���������������
���뤳�Ȥ��Ǥ��ޤ���
.

== Super Class

Object

j
== Constants

�ץ�ե����� "Racc_" ���Ĥ�������ϥѡ�����ͽ������Ǥ���
���Τ褦������ϻȤ�ʤ��Ǥ���������ư���Բ�ǽ�ˤʤ�ޤ���
.
== Instance Methods
j
�����˺ܤäƤ����ΤΤۤ����ץ�ե����� "racc_" ����� "_racc_" ��
�Ĥ����᥽�åɤϥѡ�����ͽ��̾�Ǥ������Τ褦�ʥ᥽�åɤϻȤ�ʤ���
����������
.

: do_parse -> Object
j
    �ѡ����򳫻Ϥ��ޤ���
    �ޤ����ȡ�����ɬ�פˤʤä����� #next_token ��ƤӽФ��ޤ���
e
    The entry point of parser. This method is used with #next_token.
    If Racc wants to get token (and its value), calls next_token.
.

      --
      # Example
      ---- inner
        def parse
          @q = [[1,1],
                [2,2],
                [3,3],
                [false, '$']]
          do_parse
        end

        def next_token
          @q.shift
        end
      --

: next_token -> [Symbol, Object]
    [abstract method]

j
    �ѡ��������Υȡ�������ɤߤ�����˻Ȥ��ޤ���
    [����, ������] �η�����������֤��Ƥ���������
    ����ϥǥե���ȤǤ�

      * ʸˡ�桢������Ǥ����ޤ�Ƥ��ʤ����
        �� ����̾����ʸ����Υ���ܥ� (�㤨�� :ATOM )
      * ������Ǥ����ޤ�Ƥ�����<br>
        �� ����ʸ���󤽤Τޤ� (�㤨�� '=' )

    ��ɽ���ޤ���������ѹ�������ˡ�ˤĤ��Ƥϡ�
    ʸˡ��ե���󥹤򻲾Ȥ��Ƥ���������

    �ޤ����⤦���륷��ܥ뤬�ʤ��ʤä��Ȥ��ˤ� 
    [false, �ʤˤ�] �ޤ��� nil ���֤��Ƥ���������

    ���Υ᥽�åɤ���ݥ᥽�åɤʤΤǡ�#do_parse ��Ȥ�����
    ɬ���ѡ������饹��Ǻ��������ɬ�פ�����ޤ���
    ������ʤ��ޤޥѡ�����Ϥ����㳰 NotImplementedError ��
    ȯ�����ޤ���
e
    The method to fetch next token.  If you use #do_parse method,
    you must implement #next_token.  The format of return value is
    [TOKEN_SYMBOL, VALUE].  token-symbol is represented by Ruby's symbol
    by default, e.g. :IDENT for 'IDENT'.  ";" (String) for ';'.

    The final symbol (End of file) must be false.
.

: yyparse( receiver, method_id )
j
    �ѡ����򳫻Ϥ��ޤ������Υ᥽�åɤǤϻϤ�ƥȡ�����
    ɬ�פˤʤä������� receiver ���Ф��� method_id �᥽�åɤ�
    �ƤӽФ��ƥȡ���������ޤ���

    receiver �� method_id �᥽�åɤϥȡ������ yield ���ʤ����
    �ʤ�ޤ��󡣷����� #next_token ��Ʊ���� [����, ��] �Ǥ���
    �Ĥޤꡢreceiver �� method_id �᥽�åɤγ����ϰʲ��Τ褦��
    �ʤ�Ϥ��Ǥ���
      --
      def method_id
        until end_of_file
              :
          yield ����, ��
              :
        end
      end
      --
    ������դ�ɬ�פʤΤϡ�method_id ���ƤӽФ����ΤϻϤ��
    �ȡ�����ɬ�פˤʤä������Ǥ���Ȥ������ȤǤ���method_id
    �᥽�åɤ��ƤӽФ��줿�Ȥ��ϴ��˥ѡ������ʹ���ʤΤǡ�
    �����������ǻȤ��ѿ��� method_id ����Ƭ�ǽ���������
    �ޤ����Ԥ��ޤ���

    �ȡ�����ν�ü�򼨤� [false, �ʤˤ�] ���Ϥ����餽��ʾ�� 
    yield ���ʤ��Ǥ������������ξ��ˤ��㳰��ȯ�����ޤ���

    �Ǹ�ˡ�method_id �᥽�åɤ����ɬ�� yield ���Ƥ���������
    ���ʤ����ϲ��������뤫�狼��ޤ���
e
    The another entry point of parser.
    If you use this method, you must implement RECEIVER#METHOD_ID method.

    RECEIVER#METHOD_ID is a method to get next token.
    It must 'yield's token, which format is [TOKEN-SYMBOL, VALUE].
.

: on_error( error_token_id, error_value, value_stack )
j
    �ѡ���������ʸˡ���顼�򸡽Ф���ȸƤӽФ��ޤ� (yacc �� yyerror)��
    ���顼��å�������Ф��ʤꡢ�㳰��ȯ������ʤꤷ�Ƥ���������
    ���Υ᥽�åɤ����������ä���硢�ѡ����ϥ��顼�����⡼��
    �˰ܹԤ��ޤ���

    error_token �ϥѡ������顼�򵯤��������������ɽ�� (����) �Ǥ���
    #token_to_str ��ʸˡ�ե�������ʸ����ɽ����ľ���ޤ���

    error_value �Ϥ����ͤǤ���

    value_stack �ϥ��顼�λ����Ǥ��ͥ����å��Ǥ���
    value_stack ���ѹ����ƤϤ����ޤ���

    on_error �Υǥե���Ȥμ������㳰 ParseError ��ȯ�����ޤ���
e
    This method is called when parse error is found.

    ERROR_TOKEN_ID is an internal ID of token which caused error.
    You can get string replesentation of this ID by calling
    #token_to_str.

    ERROR_VALUE is a value of error token.

    value_stack is a stack of symbol values.
    DO NOT MODIFY this object.

    This method raises ParseError by default.

    If this method returns, parsers enter "error recovering mode".
.

: token_to_str( t ) -> String
j
    Racc �ȡ����������ɽ�� (����)
    ��ʸˡ�ե������ε���ɽ����ʸ������Ѵ����ޤ���

    t �������Ǥʤ����� TypeError ��ȯ�����ޤ���
    t ���ϰϳ����������ä����� nil ���֤��ޤ���
e
    Convert internal ID of token symbol to the string.
.

: yyerror
j
    ���顼�����⡼�ɤ�����ޤ������ΤȤ� #on_error �ϸƤФ�ޤ���
    ���������ʳ�����ϸƤӽФ��ʤ��Ǥ���������
e
    Enter error recovering mode.
    This method does not call #on_error.
.

: yyerrok
j
    ���顼�����⡼�ɤ����������ޤ���
    ���������ʳ�����ϸƤӽФ��ʤ��Ǥ���������
e
    Leave error recovering mode.
.

: yyaccept
j
    �������ͥ����å�����Ƭ���ͤ��֤��� #do_parse��#yyparse ��ȴ���ޤ���
e
    Exit parser.
    Return value is Symbol_Value_Stack[0].
.
