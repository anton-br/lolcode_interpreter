# -*- coding: utf-8 -*-
import ply.lex as lex
import ply.yacc as yacc
from ply.lex import TOKEN
import re
import os

keywords = (
    'VISIBLE', 'I', 'HAS', 'A', 'R', 'ITZ', 'AN', 'OF', 'WIN', 'FAIL', 'NOOB',
    'IS', 'NOW', 'TYPE', 'GIMMEH',
    ##math
    'SUM', 'DIFF', 'PRODUKT', 'QUOSHUNT', 'MOD', 'BIGGR', 'SMALLR',
    'BOTH', 'EITHER', 'WON', 'NOT', 'SAEM', 'DIFFRINT', 'ALL', 'ANY', 'MKAY',
)
tokens = keywords + (
    'COMMENT', 'LONG_COMMENT',
    'NAME', 'NEWLINE',
    'NUMBR', 'NUMBAR',
    'YARN', 'IF'
)

t_ignore = ' \r\t\f'

states = (
("comment", "exclusive"),
)

def t_comment(t):
    r'OBTW'
    t.lexer.begin('comment')

def t_comment_end(t):
    r'TLDR'
    t.lexer.lineno += t.value.count('\n')
    t.lexer.begin('INITIAL')

def t_comment_error(t):
    t.lexer.skip(1)

t_comment_ignore = ''

def t_TYPE(t):
    r'NUMBR|NUMBAR|YARN|TROOF'
    return t

def t_COMMENT(t):
    r'BTW.*'
    pass

def t_IF(t):
    r'O\ RLY\?'
    return t

def t_NUMBAR(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_NUMBR(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_YARN(t):
    r'"(\\.|[^"])*"'
    string = t.value[1:-1]
    string = string.replace(":)", "\n")
    string = string.replace(":>", "\t")
    string = string.replace(":о", "\g")
    string = string.replace("::", ":")
    t.value = string
    return t

@TOKEN(r'[a-zA-Z_][a-zA-Z0-9_]*')
def t_NAME(t):
    if t.value in keywords:
        t.type = t.value
    return t

reg = '%r'%os.linesep.replace(r'\\', r"\\\\")
@TOKEN(reg[1:-1])
def t_NEWLINE(t):
    t.lexer.lineno += 1

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex(debug=0)

precedence = (
    ('left', 'SUM', 'DIFF'),
    ('left','PRODUKT','QUOSHUNT'))

names = {}

operations = {
    'SUM': lambda a, b: a + b,
    'DIFF': lambda a, b: a - b,
    'PRODUKT': lambda a, b: a * b,
    'QUOSHUNT': lambda a, b: float(a / b),
    'MOD': lambda a, b: a % b,
    'BIGGR': lambda a, b: max(a, b),
    'SMALLR': lambda a, b: min(a, b),
    'BOTH': lambda a, b: a & b,
    'EITHER': lambda a, b: a | b,
    'WON': lambda a, b: a != b,
}

lol_type = {
    'WIN': True,
    'FAIL': False,
    'NOOB': None,
    'NUMBR': int,
    'NUMBAR': float,
    'YARN': str,
    'TROOF': bool
}
type_lol = {
    'True': 'WIN',
    'False': 'FAIL',
    'None': 'NOOB'
}

def p_program(p):
    '''program : program statement
               | statement'''
    if len(p) == 3:
        if not isinstance(p[1], list):
            p[1] = [p[1]]
        if not isinstance(p[2], list):
            p[2] = [p[2]]
        p[0] = p[1] + p[2]
    else:
        p[0] = [p[1]]

def p_statement(p):
    '''statement : command NEWLINE
                 | command empty'''
    p[0] = p[1] if isinstance(p[1], (list, tuple)) else (p[1],)

def p_statement_create(p):
    '''statement : I HAS A NAME '''
    if p[4] in names.keys():
        raise Exception('а variable with name {} already exists'.format(p[4]))
    names[p[4]] = 'NOOB'

def p_statement_create_assign(p):
    '''statement : I HAS A NAME ITZ expression '''
    names[p[4]] = p[6]

def p_command_assign(p):
    '''command : NAME R expression '''
    if p[1] not in names.keys():
        raise Exception("use variable {} before declaring".format(p[1]))
    names[p[1]] = p[3]

def p_expression_type(p):
    '''expression : WIN
                  | FAIL
                  | NUMBR
                  | NUMBAR
                  | YARN
                  | NOOB '''
    p[0] = p[1]

def lolbool_to_bool(var):
    new_var = []
    for v in var:
        new_var.append(lol_type[v] if v in lol_type.keys() else v)
    return new_var

def p_expression_op(p):
    '''expression : SUM OF expression AN expression
                  | DIFF OF expression AN expression
                  | PRODUKT OF expression AN expression
                  | QUOSHUNT OF expression AN expression
                  | MOD OF expression AN expression
                  | BIGGR OF expression AN expression
                  | SMALLR OF expression AN expression'''
    p[3], p[5] = lolbool_to_bool([p[3], p[5]])
    p[0] = operations[p[1]](p[3], p[5])
    if str(p[0]) in type_lol.keys():
        p[0] = type_lol[str(p[0])]

def p_expression_binop(p):
    '''expression : BOTH OF expression expression
                  | BOTH OF expression AN expression
                  | EITHER OF expression expression
                  | EITHER OF expression AN expression
                  | WON OF expression expression
                  | WON OF expression AN expression '''
    second = p[4] if len(p) == 5 else p[5]
    p[3], second = lolbool_to_bool([p[3], second])
    result = operations[p[1]](p[3], second)
    p[0] = type_lol[str(result)] if str(result) in type_lol.keys() else result

def p_expression_saem(p):
    '''expression : BOTH SAEM expression expression
                  | BOTH SAEM expression AN expression '''
    second_arg = p[4] if len(p) == 5 else p[5]
    p[0] = 'WIN' if p[3] == second_arg else 'FAIL'

def p_expression_diff(p):
    '''expression : DIFFRINT expression expression
                  | DIFFRINT expression AN expression '''
    second_arg = p[3] if len(p) == 4 else p[4]
    p[0] = 'FAIL' if p[2] == second_arg else 'WIN'

def p_expression_inf_ari(p):
    '''expression : ALL OF expression MKAY
                  | ALL OF expression AN expression MKAY
                  | ALL OF expression AN expression AN expression MKAY
                  | ANY OF expression MKAY
                  | ANY OF expression AN expression MKAY
                  | ANY OF expression AN expression AN expression MKAY'''
    result = []
    for exp in range(3, len(p), 2):
        val = lol_type[p[exp]] if p[exp] in lol_type.keys() else p[exp]
        result.append(val)
    if p[1] == 'ALL':
        result = reduce(lambda a, b: a * b, result)
    else:
        result = reduce(lambda a, b: a + b, result)
    if result in type_lol.keys():
        p[0] = type_lol[result]
    elif result:
        p[0] = 'WIN'
    else:
        p[0] = 'FAIL'

def p_expression_unaryop(p):
    '''expression : NOT expression'''
    if p[2] not in ['WIN', 'FAIL']:
        raise Exception('Unexpected type to unary negation')
    p[0] = 'WIN' if p[2] == 'FAIL' else 'FAIL'

# def p_expression_cast(p):
#     '''expression : NAME IS NOW A TYPE'''
#     names[p[1]] = lol_type[p[5]](names[p[1]])

def p_command_input(p):
    '''command : GIMMEH NAME'''
    names[p[2]] = raw_input()

def p_expression_variable(p):
    '''expression : NAME'''
    p[0] = names[p[1]]

def p_command_print(p):
    ''' command : VISIBLE expression'''
    # print(p.lineno(1))
    p[0] = ('print', p[2])

def p_start_if(p):
    '''empty : expression IF'''
    global if_started
    if_started = []
    print('HERE')

def p_statement_newline(p):
    '''statement : NEWLINE'''
    p[0] = None

def p_empty(p):
    ''' empty : '''
    p[0] = None

def p_error(p):
    if p:
        print("Syntax error at '%s'" % p.value)
    else:
        print("Syntax error at EOF")

yacc.yacc()

def run(p):
    if isinstance(p, tuple):
        if p[0] == 'print':
            print(p[1])
        # elif p[0] == 'OP':
        #
        # elif p[0] == 'BINOP':

    return p

if __name__=="__main__":
    data_1 = '''
    BOTH SAEM VAR1 AN VAR2 O RLY?
    '''
    data = '''
    I HAS A V ITZ 2
    VISIBLE V
    '''
    # data = re.sub(',', '\n', data)
    # data = re.sub(' +', ' ', data)
    lexer.input(data)
    # while True:
    #     tok = lexer.token() # читаем следующий токен
    #     if not tok: break      # закончились печеньки
    #     print(tok)
    commands = yacc.parse(data, debug=0)
    print(commands)
    for operation in commands:
        run(operation)
    print(operation)
    print(names)
