#!/usr/bin/env python
# encoding: utf-8
"""
EEBNFParser.py

Created by Andrew Cain on 2011-04-05.
Copyright (c) 2011 Andrew Cain. All rights reserved.
"""

from eebnf_tokeniser import EEBNFTokeniser

import sys
import logging
logger = logging.getLogger("EEBNFParser")

class EEBNFParser():
    
    def __init__(self):
        self._tokeniser = EEBNFTokeniser()
        
        self._rule_parts = {
            'terminal': self._parse_terminal_or_id,
            'number': self._parse_terminal_or_id,
            'id': self._parse_terminal_or_id,
            'symbol': self._parse_compound,
        }
        
        self._compound_parts = {
            '|':    self._parse_selection, #empty first option...
            '[':    self._parse_option,
            '{':    self._parse_repeat,
            '(':    self._parse_group,
            '~':    self._add_spacer,
            '$':    self._add_newline
        }
        
        pass
    
    def parse(self, code_file):
        '''
            Parse the code_file, and return the syntax tree
        '''
        self._lookahead_toks = []
        self._current_file = code_file
        self._tokeniser.tokenise(code_file)
        
        return self._parse_rules()
    
    def _parse_rules(self):
        '''
            A file consists of rules, terminated by ;s
        '''
        
        # There is a list of rules...
        rules = list()
        
        # Read until the end of the file
        while not self._match_lookahead('end'):
            tok = self._match_token('id')
            rule_name = tok[1]
            self._match_token('symbol', '=', error = 'The file can only contain rule definitions in the form [rule name] = [rule];')
            
            rule_body = self._parse_rule()
            
            rule = ( rule_name, rule_body )
            
            rules.append(rule)
            
        return tuple(rules)
    
    def _parse_rule(self):
        rule = list()
        
        while not self._match_lookahead('symbol', ';', True):
            rule.append(self._parse_rule_part())
        
        return tuple(rule)
    
    def _parse_rule_part(self, in_selection = False):
        tok = self._next_token()
        
        result = self._rule_parts[tok[0]](tok)
        
        if (not in_selection) and self._match_lookahead('symbol', '|', False):
            result = self._parse_selection(result)
            
        # Remove the separating , if present
        self._match_lookahead('symbol', ',', True)
        
        return result
    
    def _parse_terminal_or_id(self, tok):
        return tok
    
    def _parse_compound(self, tok):
        return self._compound_parts[tok[1]]()
    
    def _parse_option(self):
        '''
            Parse an option = 
            [ ... ]
        '''
        
        option_body = list()
        
        while not self._match_lookahead('symbol', ']', True):
            option_body.append(self._parse_rule_part())
        
        return ("option", tuple(option_body))
    
    def _parse_selection(self, first):
        '''
            Parse a list of options = 
             first | second | third | ...
        '''
        
        selections = list()
        selections.append(first)
        
        while self._match_lookahead('symbol', '|', True):
            selections.append(self._parse_rule_part(in_selection = True))
        
        return ("selection", tuple(selections))
    
    def _add_spacer(self):
        '''
        Read a ~ from the input... add a spacer
        '''
        return ("spacer", None )
    
    def _add_newline(self):
        '''
        Read a $ from the input... add a new line
        '''
        return ("newline", None)
    
    def _parse_group(self):
        '''
            Parse a group = 
            ( ... )
        '''
        
        option_body = list()
        
        while not self._match_lookahead('symbol', ')', True):
            option_body.append(self._parse_rule_part())
            
        return ('group', tuple(option_body))
    
    def _parse_repeat(self):
        '''
            Parse a repeat = 
            { ... }(number|*|+)"("...")"
        '''
        
        repeat_body = list()
        
        while not self._match_lookahead('symbol', '}', True):
            repeat_body.append(self._parse_rule_part())
        
        if self._match_lookahead('symbol', '*', True):
            repeat_spec = '0 or more'
        elif self._match_lookahead('symbol', '+', True):
            repeat_spec = '1 or more'
        # elif self._match_lookahead('number'):
        #     tok = self._match_token('number')
        #     repeat_spec = '%s times' % tok[1]
        else:
            assert False
        
        # Read optional loop syntax
        if self._match_lookahead('symbol', '(', False):
            loop_rule = self._parse_rule_part()
        else:
            loop_rule = None
        
        return ("repeat", tuple(repeat_body), repeat_spec, loop_rule)
    
    def _next_token(self):
        current_token = None
        while current_token == None or current_token[0] == 'comment':
            if len(self._lookahead_toks) > 0:
                current_token = self._lookahead_toks[0]
                self._lookahead_toks = self._lookahead_toks[1:]
            else:
                current_token = self._tokeniser.next_token()
            
            if current_token[0] == 'comment':
                logger.debug('Parser    : Skipping comment: %s', current_token[1])
        return current_token
    
    def _lookahead(self,count=1):
        logger.debug('Parser    : Looking ahead %d', count)
        while len(self._lookahead_toks) < count:
            current_token = self._tokeniser.next_token()
            
            while current_token[0] == 'comment':
                logger.debug('Parser    : Skipping comment: %s', current_token[1])
                current_token = self._tokeniser.next_token()
            
            self._lookahead_toks.append(current_token)
        return self._lookahead_toks
    
    def _match_lookahead(self, token_kind, token_value = None, consume = False):
        logger.debug('Parser    : Looking to find %s (%s)%s', token_kind, token_value if token_value != None else 'any', ' will consume' if consume else '')
        token = self._lookahead(1)[0]
        result = token[0] == token_kind and (token_value == None or token_value == token[1].lower())
        
        if consume and result:
            self._match_token(token_kind, token_value)
        return result
    
    def _match_token(self, token_kind, token_value = None, error = None):
        tok = self._next_token()
        
        if tok[0] != token_kind or (token_value != None and token_value != tok[1].lower()):
            logger.error('Parse Error %s: %s: found a %s (%s) expected %s (%s)', 
                '%s: ' % error if error else '',
                self._tokeniser.line_details(), 
                tok[0], tok[1], token_kind, token_value)
            assert False
            
        logger.debug('Parser    : Matched token %s (%s)', tok[0], tok[1])
        return tok

#----------------------------------------------------------------------------

def test_basic():
    lines = [   
        '(* Syntax for Pascal Program declaration *) \n',
        'program = "program", $ identifier, ";", [uses clause], block, "." ;\n',
        'uses clause = "uses", { unit identifier }+(","), ";" ;\n',
        'test = { blah } + \n', 
        ' ( 1, 2, 3, 4, [blee] , "," ); \n',
        'digit = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 0 ;\n',
        'number = {1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | (0 , "fred" )}+ ;\n',
        ''
        ]
    
    p = EEBNFParser()
    tree = p.parse(lines)
    
    print(tree)

if __name__ == '__main__':
    logging.basicConfig(level=logging.ERROR,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    test_basic()
