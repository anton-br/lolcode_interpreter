# -*- coding: utf-8 -*-
import ply.lex as lex
import ply.yacc as yacc
from ply.lex import TOKEN
import re
import os
import sys

keywords = (
    'VISIBLE', 'I', 'HAS', 'A', 'R', 'ITZ', 'AN', 'OF', 'WIN', 'FAIL', 'NOOB',
    'IS', 'NOW', 'TYPE', 'GIMMEH', 'YA', 'NO', 'RLY', 'WAI', 'OIC',
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

def t_COMMENT(t):
    r'BTW.*'
    pass

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

def p_program(p):
    '''program : program statement
               | statement'''
    if len(p) == 2:
        p[0] = {}
        line, stat = p[1]
        p[0][line] = stat
    if len(p) == 3:
        p[0] = p[1]
        if not p[0]:
            p[0] = { }
        line, stat = p[2]
        p[0][line] = stat

def p_statement(p):
    '''statement : command NEWLINE
                 | command empty'''
    line = p[1][0]
    p[0] = (line, p[1][1:])

def p_expression_variable(p):
    '''expression : NAME'''
    p[0] = p[1]

def p_command_create(p):
    '''command : I HAS A NAME '''
    p[0] = (p.lineno(1), 'none', p[4])

def p_command_create_assign(p):
    '''command : I HAS A NAME ITZ expression '''
    p[0] = (p.lineno(1), 'assign', p[4], p[6])

def p_statement_assign(p):
    '''statement : NAME R expression '''
    p[0] = (p.lineno(1), ('equal', p[1], p[3]))

def p_expression_type(p):
    '''expression : WIN
                  | FAIL
                  | NUMBR
                  | NUMBAR
                  | YARN
                  | NOOB '''
    p[0] = p[1]

def p_expression_op(p):
    '''expression : SUM OF expression AN expression
                  | DIFF OF expression AN expression
                  | PRODUKT OF expression AN expression
                  | QUOSHUNT OF expression AN expression
                  | MOD OF expression AN expression
                  | BIGGR OF expression AN expression
                  | SMALLR OF expression AN expression'''
    p[0] = (p[1], p[3], p[5])

def p_expression_binop(p):
    '''expression : BOTH OF expression expression
                  | BOTH OF expression AN expression
                  | EITHER OF expression expression
                  | EITHER OF expression AN expression
                  | WON OF expression expression
                  | WON OF expression AN expression '''
    second = p[4] if len(p) == 5 else p[5]
    p[0] = (p[1], p[3], second)

def p_expression_saem(p):
    '''expression : BOTH SAEM expression expression
                  | BOTH SAEM expression AN expression '''
    second = p[4] if len(p) == 5 else p[5]
    p[0] = ('saem', p[3], second)

def p_expression_diff(p):
    '''expression : DIFFRINT expression expression
                  | DIFFRINT expression AN expression '''
    second = p[3] if len(p) == 4 else p[4]
    p[0] = (p[1], p[2], second)

def p_expression_inf_ari(p):
    '''expression : ALL OF expression MKAY
                  | ALL OF expression AN expression MKAY
                  | ALL OF expression AN expression AN expression MKAY
                  | ANY OF expression MKAY
                  | ANY OF expression AN expression MKAY
                  | ANY OF expression AN expression AN expression MKAY'''
    p[0] = (p[1], p[3::2])

def p_expression_unaryop(p):
    '''expression : NOT expression'''
    p[0] = (p[1], p[2])

def p_statement_cast(p):
    '''statement : NAME IS NOW A TYPE'''
    p[0] = (p.lineno(1), ('cast', p[1], p[5]))

def p_command_input(p):
    '''command : GIMMEH NAME'''
    p[0] = ('input', p[2])

def p_command_print(p):
    ''' command : VISIBLE expression'''
    p[0] = (p.lineno(1), 'print', p[2])

def p_statement_newline(p):
    '''statement : NEWLINE'''
    p[0] = None

def p_statement_if(p):
    '''statement : expression IF'''
    p[0] = (p.lineno(2), ('if', 'if start', p[1]))

def p_statement_true_if(p):
    '''statement : YA RLY command'''
    p[0] = (p.lineno(1), ('if', 'if true', p[3]))

def p_statement_false_if(p):
    '''statement : NO WAI command'''
    p[0] = (p.lineno(1), ('if', 'if false', p[3]))

def p_statement_if_end(p):
    '''statement : OIC'''
    p[0] = (p.lineno(1), ('if', 'if end'))
# def p_code(p):


def p_empty(p):
    ''' empty : '''
    p[0] = None

def p_error(p):
    if p:
        print("Syntax error at '%s'" % p.value)
    else:
        print("Syntax error at EOF")

yacc.yacc()

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

def lolbool_to_bool(var):
    new_var = []
    for v in var:
        new_var.append(lol_type[v] if v in lol_type.keys() else v)
    print(new_var)
    return new_var

def eval(p):
    if isinstance(p, tuple):
        if p[0] == 'none':
            variables[p[1]] = 'NOOB'
        elif p[0] == 'equal':
            if p[1] not in variables:
                raise Exception('Variable with name {} not found!'.format(p[1]))
            variables[p[1]] = eval(p[2])
        elif p[0] == 'assign':
            variables[p[1]] = eval(p[2])
        elif p[0] in operations.keys():
            fir, sec = get_val_by_name([p[1], p[2]])
            res = operations[p[0]](fir, sec)
            if p[0] in ['BOTH', 'EITHER', 'WON']:
                return type_lol[str(bool(res))]
            if str(res) in type_lol.keys():
                 return type_lol[str(res)]
            return res
        elif p[0] == 'saem':
            fir, sec = get_val_by_name([p[1], p[2]])
            return 'WIN' if fir == sec else 'FAIL'
        elif p[0] == 'DIFFRINT':
            return 'FAIL' if p[1] == p[2] else 'WIN'
        elif p[0] == 'ALL' or p[0] == 'ANY':
            if p[0] == 'ALL':
                res = reduce(lambda a, b: a * b, p[1])
            else:
                res = reduce(lambda a, b: a + b, p[1])
            if res in type_lol.keys():
                return type_lol[res]
            elif res:
                return 'WIN'
            return 'FAIL'
        elif p[0] == 'NOT':
            if p[1] in variables.keys():
                unary = variables[p[1]]
            if unary not in ['WIN', 'FAIL']:
                raise Exception('Unexpected type to unary negation')
            return 'WIN' if unary == 'FAIL' else 'FAIL'
        elif p[0] == 'print':
            print_ev(p)
    return p

def get_val_by_name(p):
    list_p = []
    for i in p:

        list_p.append(variables[i] if i in variables.keys() else i)
    return list_p

def update(p, num, index):
    if index < len(num):
        return p[num[index]]
    else:
        return "BREAK"

def print_ev(p):
    eval_line = eval(p[1])
    if eval_line in variables.keys():
        print(variables[eval_line])
    else:
        print(eval_line)

def run(p):
    num = sorted(p.keys())
    global variables
    variables = {}
    is_if = 'noif'
    index = 0
    line = p[num[index]]
    while True:
        if line[0] == 'print':
            print_ev(line)
        elif line[0] == 'GIMMEH':
            if line[1] not in variables:
                raise Exception('Variable with name {} not found!'.format(line[1]))
            variables[line[1]] = raw_input()
        elif line[0] == 'cast':
            line_1 = eval(line[1])
            line_2 = eval(line[2])
            val = lol_type[line_2](variables[line_1])
            if line_2 == 'TROOF':
                val = 'WIN' if lol_type[line_2](variables[line_1]) else 'FAIL'
            variables[line_1] = val
        elif line[0] == 'if':
            while True:
                if line[1] == 'if start':
                    if is_if != 'noif':
                        raise Exception('BAD IF')
                    is_if = 'start'
                    bool = lol_type[eval(line[2])]
                    branches = ['if false', 'if true']
                    find_for = branches[int(bool)]
                elif line[1] == find_for:
                    while True:
                        if line[1] == 'if end':
                            is_if = 'end'
                            break
                        if line[1] == branches[not int(bool)]:
                            break
                        if line[0] == 'if':
                            if not isinstance(line[2][0], str):
                                inside_data = eval(line[2][1:])
                            else:
                                inside_data = eval(line[2])
                        else:
                            inside_data = eval(line)
                        index += 1
                        line = update(p, num, index)
                        if isinstance(line, str):
                            raise Exception('Something happend in IF operation.')
                if line[1] == 'if end' or is_if == 'end':
                    break
                index += 1
                line = update(p, num, index)
                if isinstance(line, str):
                    raise Exception('Lost OCI. If must ended with OCI.')
        else:
            a = eval(line)
        index += 1
        line = update(p, num, index)
        if isinstance(line, str):
            break
    return p

if __name__=="__main__":
    if len(sys.argv) == 2:
        data = open(sys.argv[1]).read()
    splited_data = data.split('\n')
    commands = {}
    for split in splited_data[:-1]:
        result = yacc.parse(split + '\n', debug=0)
        if result:
            commands.update(result)
    if commands:
        run(commands)
