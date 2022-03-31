#!/usr/bin/env python
# encoding: utf-8
"""
EEBNFToTikzPicture.py

Created by Andrew Cain on 2011-04-06.
Copyright (c) 2011 Andrew Cain. All rights reserved.
"""

from eebnf_parser import EEBNFParser

import tikz_picture

import sys
import logging
logger = logging.getLogger("EEBNFToTikzPicture")

class TikzPicture:
    def __init__(self):
        # The rows for output... each will contain a list of columns
        self._matrix = list()
        self._chain = list()
        self._chain.append("\t[start chain]")
        self._temp_chain = None
        self._temp_chain_stack = list()
        
        self._current_col = 0
        self._current_row = 0
    
    def _push_temp_chain(self):
        '''
            Temp chains are used to manage the options within a selection.
            This enables nested selections by pushing the previous temp chains onto a stack
        '''
        self._temp_chain_stack.append(self._temp_chain)
    
    def _pop_temp_chain(self):
        '''
            Temp chains are used to manage the options within a selection.
            This enables pops the stacks back to their previous state when the 
            old temp chains are completed
        '''
        self._temp_chain = self._temp_chain_stack.pop()
    
    def _start_temp_chain(self):
        '''
            Start a temporary chain, all additions to the chain will go
            to the last temp chain.
        '''
        if not self._temp_chain:
            self._temp_chain = list()
        
        self._temp_chain.append(list())
    
    def _end_temp_chains(self):
        '''
            Merge temp chains back into the main chain
        '''
        
        for chain in self._temp_chain:
            self._chain.extend(chain)
        
        self._pop_temp_chain()
    
    def _row_count(self):
        return len(self._matrix)
    
    def _col_count(self):
        # print self._matrix
        return len(self._matrix[0])
    
    def _get_or_add_row(self, row_idx):
        '''
            if there is not enough rows add one, otherwise 
            return idx
        '''
        while len(self._matrix) <= row_idx:
            self._matrix.append(list())

            for i in range(self._col_count()):
                # print i
                self._matrix[-1].append(None)

        return row_idx

    def _add_column(self):
        '''
            Adds a column to the matrix, ensures all rows are 
            expanded and returns the index of the new column
        '''
        # print "ADDING COLUMN"
        for row in self._matrix:
            row.append(None)
            # print " COL COUNT = ", len(row)
        
        return self._col_count() - 1
    
    def _add_data_to_matrix(self, to_row, node_prefix, kind, data):
        '''
            Add data to a row in the matrix. Allocates a column to store row.
        '''
        assert kind in ["terminal", "syntax-rule", "syntax-rule-cont", "nonterminal", "point"]
        
        to_row = self._get_or_add_row(to_row)
        
        # print self._current_col, len(self._matrix[to_row])
        
        if self._current_col >= len(self._matrix[to_row]):
            # print "Adding column"
            col_idx = self._add_column()
        else:
            # print "Store in existing column"
            col_idx = self._current_col
        
        if kind == "terminal":
            if data == "..." or data.startswith('any '):
                kind = "meta-terminal"
            elif data == "--":
                data = "-{-}"
        
        
        self._current_col = col_idx + 1
        node_ident = "%s%i-%i" % (node_prefix, col_idx, to_row)
        self._matrix[to_row][col_idx] = '\t\\node (%s) [%s] {%s};' % (node_ident, kind, data)
        
        return node_ident
    
    def _add_chain(self, chain):
        if not self._temp_chain:
            self._chain.append(chain)
        else:
            self._temp_chain[-1].append(chain)
        
    def _add_to_all_temp_chains(self, text):
        for chain in self._temp_chain:
            chain.append(text)
    
    def _build_output(self):
        result = tikz_picture.header_tex + "\n"
        result += "\\matrix[column sep=3mm] {\n"
        
        # build matrix output
        
        for row in self._matrix:
            row_txt = "\t"
            i = 0
            
            for col_data in row:
                if col_data:
                    row_txt += col_data
                
                if i != len(row)-1:
                    row_txt += " & "
                else:
                    row_txt += " \\\\"
                i += 1
            
            result += row_txt + "\n"
        
        result += "};\n"
        
        # build chain
        result += "{\n"
        
        for link in self._chain:
            # print result
            result += link
        
        result += "}\n"
            
        result += "\end{tikzpicture}\n"
        return result
    

class EEBNFToTikzPicture():
    
    def __init__(self):
        self._parser = EEBNFParser()
    
    def convert_file(self, eebnf_file):
        tree = self._parser.parse(eebnf_file)
        
        self._pictures = list()
        
        return self._convert_to_tikz_picture(tree)
    
    def _current_picture(self):
        return self._pictures[-1]
    
    def _add_picture(self):
        self._pictures.append(TikzPicture())
    
    def _count_cols_in(self, node):
        '''
            Get the number of columns that will be added when the rule is
            added to the graph
        '''
        result = 0
        
        if node[0] in ("terminal", "number", "id") :
            # print node[1], len(node[1])
            result = len(node[1]) * 0.75
        elif node[0] == "spacer":
            result = 3
        elif (node[0] == "option") or (node[0] == "repeat") or (node[0] == 'group'):
            for part in node[1]:
                result += self._count_cols_in(part) + 3
            result += 3 # Was 4??
        elif (node[0] == "selection"):
            result = 1
            # print node[1]
            for part in node[1]:
                p_count = self._count_cols_in(part)
                if p_count > result:
                    result = p_count
            result += 3 # was 4??
        else:
            print(node)
            assert False
        
        return result
    
    def _add_option_node_to_matrix(self, node, to_row, node_join_spec):
        '''
            Add option to row below...
        '''
        
        start_p = self._current_picture()._add_data_to_matrix(to_row, 'p', 'point', '')
        
        self._current_picture()._add_chain('\t\t\\chainin (%s) [join];\n' % start_p)
        self._current_picture()._add_chain('\t\t{ [start branch]\n')
        
        # bs_p = self._current_picture()._add_data_to_matrix(to_row + 1, 'p', 'point', 'bs-p')
        # self._current_picture()._add_chain("\t\t\\chainin (%s) [join=by {vh path}];\n" % bs_p)
        
        depth = 1;
        i = 0
        for n in node:
            if i == 0:
                (n_ident, nd) = self._add_node_to_matrix(n, to_row + 1, "join=by {vh path,tip}")
            else: 
                (n_ident, nd) = self._add_node_to_matrix(n, to_row + 1)
            i += 1
            nd += 1
            if nd > depth:
                depth = nd
        
        # print '** ', depth
        
        # Add end node to matrix
        end_p = self._current_picture()._add_data_to_matrix(to_row, 'p', 'point', 'be-p')
        
        self._current_picture()._add_chain("\t\t\\chainin (%s) [join=by {hv path,tip}];\n" % end_p)
        self._current_picture()._add_chain('\t\t}\n')
        
        self._current_picture()._add_chain("\t\t\\chainin (%s) [join];\n" % end_p)
        
        return (n_ident, depth)
    
    def _add_selection_node_to_matrix(self, nodes, to_row):
        '''
            Add a selection node to the picture
        '''
        
        start_p = self._current_picture()._add_data_to_matrix(to_row, 'p', 'point', '')
        self._current_picture()._add_chain('\t\t\\chainin (%s) [join];\n' % start_p)
                    
        depth = 0
        i = 0
        selection_row = to_row
        selection_col = self._current_picture()._current_col
        last_col = selection_col
        
        # print nodes
        
        self._current_picture()._push_temp_chain()
        
        for n in nodes:
            # print 'starting chain'
            self._current_picture()._start_temp_chain()
            
            if i > 0:
                self._current_picture()._add_chain('\t\t{ [start branch]\n')
                (n_ident, nd) = self._add_node_to_matrix(n, selection_row, "join=by {vh path,tip}")
            else: 
                (n_ident, nd) = self._add_node_to_matrix(n, selection_row)
            
            if i == 0:
                # remove first chain to add manually later...
                # print 'removing chain - ', self._current_picture()._temp_chain[0]
                first_chain = self._current_picture()._temp_chain[0]
                self._current_picture()._temp_chain = None
                
            i += 1
            
            depth += nd
            # print n, nd
            selection_row += nd
            # Remember the widest point
            if self._current_picture()._current_col > last_col:
                last_col = self._current_picture()._current_col
            # Go back to the selection column
            self._current_picture()._current_col = selection_col
        
        self._current_picture()._current_col = last_col + 1
        
        # print "** ", depth
        
        # Add end node to matrix - chain to all selections
        end_p = self._current_picture()._add_data_to_matrix(to_row, 'p', 'point', 'se-p')
        
        self._current_picture()._add_to_all_temp_chains("\t\t\\chainin (%s) [join=by {hv path}];\n" % end_p)
        self._current_picture()._add_to_all_temp_chains('\t\t}\n')
        self._current_picture()._end_temp_chains()
        
        # Restore first chain
        # print 'Restoring chain'
        for link in first_chain:
            self._current_picture()._add_chain(link)
        self._current_picture()._add_chain("\t\t\\chainin (%s) [join];\n" % end_p)
        
        
        fp = self._current_picture()._add_data_to_matrix(to_row, 'p', 'point', 'fp')
        self._current_picture()._add_chain('\t\t\\chainin (%s) [join];\n' % fp)
        
        return (n_ident, depth)
        
    
    def _add_repeat_node_to_matrix(self, node, to_row, repeat_kind, separator):
        '''
            Add repeat to row below...
        '''
        
        # print separator
        
        if repeat_kind == '0 or more':
            body_row = to_row + 1
            pre_loop = self._current_picture()._add_data_to_matrix(to_row, 'p', 'point', 'pre-loop')
            self._current_picture()._add_chain('\t\t\\chainin (%s) [join];\n' % pre_loop)
            self._current_picture()._add_chain('\t\t{ [start branch]\n')
            first_joiner = "=by {vh path}"
            depth = 2
        elif repeat_kind == '1 or more':
            body_row = to_row
            first_joiner = ''
            depth = 1
        else:
            assert False
        
        # Add a starting point for the loop
        start_p = self._current_picture()._add_data_to_matrix(body_row, 'p', 'point', 'loop-start')
        self._current_picture()._add_chain('\t\t\\chainin (%s) [join%s];\n' % (start_p, first_joiner))
        
        # Get the start column
        start_c = self._current_picture()._current_col - 1
        
        # bs_p = self._current_picture()._add_data_to_matrix(to_row + 1, 'p', 'point', 'bs-p')
        # self._current_picture()._add_chain("\t\t\\chainin (%s) [join=by {vh path}];\n" % bs_p)
        
        max_depth = 1
        for n in node:
            (n_ident, nd) = self._add_node_to_matrix(n, body_row)
            if nd > max_depth:
                max_depth = nd
        
        if max_depth > depth:
            # print 'setting depth to - ', max_depth
            depth = max_depth
        
        # Add end node to matrix
        end_p = self._current_picture()._add_data_to_matrix(body_row, 'p', 'point', 'be-p')
        self._current_picture()._add_chain('\t\t\\chainin (%s) [join];\n' % end_p)
        
        if separator:
            # Add the separator to the following line within the loop area...
            self._current_picture()._add_chain('\t\t{ [start branch]\n')
            
            end_c = self._current_picture()._current_col - 1
            col_span = end_c - start_c
            
            # print 'start col = ', start_c
            # print 'end col = ', end_c
            # print 'col span = ', col_span
            
            return_row = body_row + max_depth
            
            self._current_picture()._current_col = start_c + 1
            # return_end = self._current_picture()._add_data_to_matrix(return_row, 'p', 'point', 'ret-end')
            
            assert separator[0] == 'group'
            assert len(separator[1]) == 1
            
            self._current_picture()._current_col = end_c
            # return_start = self._current_picture()._add_data_to_matrix(return_row, 'p', 'point', 'ret-start')
            # self._current_picture()._add_chain("\t\t\\chainin (%s) [join=by vh path];\n" % return_start)
            
            self._current_picture()._current_col = start_c + 1
            
            sep_node = self._add_node_to_matrix(separator[1][0], return_row, 'join=by {vh path,tip}')
            
            # self._current_picture()._add_chain("\t\t\\chainin (%s) [join];\n" % return_end)
            self._current_picture()._add_chain("\t\t\\chainin (%s) [join=by {hv path,tip}];\n" % start_p)
            self._current_picture()._add_chain('\t\t}\n')
            
            self._current_picture()._current_col = end_c + 1
            
        else:
            # Add the return arrow
            self._current_picture()._add_chain('\t\t{ [start branch]\n')
            self._current_picture()._add_chain("\t\t\\chainin (%s) [join=by {skip loop=-%imm,tip}];\n" % (start_p, max_depth * 6))
            self._current_picture()._add_chain('\t\t}\n')
        
        if repeat_kind == '0 or more':
            post_loop = self._current_picture()._add_data_to_matrix(to_row, 'p', 'point', 'post-loop')
            self._current_picture()._add_chain("\t\t\\chainin (%s) [join=by {hv path,tip}];\n" % post_loop)
            self._current_picture()._add_chain('\t\t}\n')
            self._current_picture()._add_chain("\t\t\\chainin (%s) [join];\n" % post_loop)
        
        
        #self._current_picture()._add_chain("\t\t\\chainin (%s) [join];\n" % end_p)
        return (n_ident, depth)
    
    def _add_node_to_matrix(self, node, to_row, node_join_spec = 'join=by tip'):
        '''
            Add the node from the AST to the matrix... expands options, selections, repeats
        '''
        if node[0] == "terminal" or node[0] == "number":
            n_ident = self._current_picture()._add_data_to_matrix(to_row, 'n', 'terminal', node[1])
            self._current_picture()._add_chain("\t\t\\chainin (%s) [%s];\n" % (n_ident, node_join_spec))
            depth = 1
        elif node[0] == "id":
            n_ident = self._current_picture()._add_data_to_matrix(to_row, 'n', 'nonterminal', node[1])
            self._current_picture()._add_chain("\t\t\\chainin (%s) [%s];\n" % (n_ident, node_join_spec))
            depth = 1
        elif node[0] == "group":
            # (group, (nodes))
            depth = 0
            for n in node[1]:
                (n_ident, nd) = self._add_node_to_matrix(n, to_row, node_join_spec)
                node_join_spec = 'join=by tip'
                depth += nd
        elif node[0] == "option":
            # (option, (nodes))
            (n_ident, depth) = self._add_option_node_to_matrix(node[1], to_row, node_join_spec)
        elif node[0] == "repeat":
            # (option, (node))
            (n_ident, depth) = self._add_repeat_node_to_matrix(node[1], to_row, node[2], node[3])
        elif node[0] == "selection":
            # (selection, (nodes))
            (n_ident, depth) = self._add_selection_node_to_matrix(node[1], to_row)
        elif node[0] == "spacer":
            n_ident = self._current_picture()._add_data_to_matrix(to_row, 'p', 'point', 'spacer')
            depth = 1
        else:
            print(node)
            assert False
        
        return (n_ident, depth)
        
    def _add_rule_to_matrix(self, rule, to_row):
        '''
            Rule is a tuple from the AST.
        '''
        
        self._current_picture()._add_data_to_matrix(to_row, 'n', 'syntax-rule', rule[0])
        p_ident = self._current_picture()._add_data_to_matrix(to_row, 'p', 'point', '')
        
        self._current_picture()._add_chain("\t\t\\chainin (%s);\n" % p_ident)
        
        # 20 = minimum width of rules...
        width = len(rule[0]) if len(rule[0]) > 20 else 20
        
        # print '----------------- ', rule[0]
        
        for node in rule[1]:
            # 3 = number of characters between each node
            if node[0] == 'newline':
                width = 1000
                continue # dont try to add this as a node... just move to the next node in the rule
            else:
                width += self._count_cols_in(node) + 3
            # print node, width
            
            if width > 90:
                print('rule width = ', rule[0], width)
                # print 'Rule length overflow... moving to new picture'
                p_ident = self._current_picture()._add_data_to_matrix(to_row, 'p', 'point', '')
                p_ident = self._current_picture()._add_data_to_matrix(to_row, 'p', 'point', '')
                self._current_picture()._add_chain("\t\t\\chainin (%s) [join=by extender];\n" % p_ident)
                
                width = self._count_cols_in(node)
                self._add_picture()
                to_row = 0
                p_ident = self._current_picture()._add_data_to_matrix(to_row, 'p', 'syntax-rule-cont', '' ) #rule[0])
                p_ident = self._current_picture()._add_data_to_matrix(to_row, 'p', 'point', '')
                self._current_picture()._add_chain("\t\t\\chainin (%s);\n" % p_ident)
                p_ident = self._current_picture()._add_data_to_matrix(to_row, 'p', 'point', '')
                self._current_picture()._add_chain("\t\t\\chainin (%s) [join=by extender];\n" % p_ident)
            
            # print 'node = ', node
            # print 'Adding to ', to_row, ',', self._current_col
            self._add_node_to_matrix(node, to_row)
        
        p_ident = self._current_picture()._add_data_to_matrix(to_row, 'p', 'point', '')
        self._current_picture()._add_chain("\t\t\\chainin (%s) [join=by end];\n" % p_ident)
        
    
    def _build_matrix(self, tree):
        '''
            Allocate nodes and points to the matrix (table).
        '''
        
        for rule in tree:
            self._add_picture()
            self._add_rule_to_matrix(rule, 0)
        
    def _build_output(self):
        result = ''
        for pic in self._pictures:
            result += pic._build_output()
            result += '\n\n'
        return result
    
    def _convert_to_tikz_picture(self, tree):
        '''
            tree contains the tupples of the rules in the eebnf file
        '''
        
        self._build_matrix(tree)
        
        return self._build_output()
        
        # result = tikz_picture.header_tex + "\n"
        # 
        # i = 0;
        # 
        # for rule in tree:
        #     
        #     result += '\t\\node (n%i) [syntax-rule] {%s =};\n' % (i, rule[0])
        #     
        #     prev_node = i
        #     i += 1
        #     pos = "right"
        #     
        #     for node in rule[1]:
        #         # Nodes are terminals, ids, repeats, selections, or option
        #         if node[0] == 'terminal':
        #             result += '\t\\node (n%i) [terminal, %s=of n%i] {%s};\n' % (i, pos, prev_node, node[1])
        #             prev_node = i
        #             pos = "right"
        #             i = i + 1
        #         elif node[0] == 'id':
        #             result += '\t\\node (n%i) [nonterminal, %s=of n%i] {%s};\n' % (i, pos, prev_node, node[1])
        #             prev_node = i
        #             pos = "right"
        #             i = i + 1
        #         elif node[0] == 'option':
        #             #push option below...
        #             temp = i
        #             result, i = self._convert_option(node[1], i, prev_node, result)
        #             prev_node = temp
        #             pos = "above right"
        #     break
        #     
        # result += tikz_picture.footer_tex
        # return result
    
    # def _convert_option(self, option_node, i, prev_node, result):
    #     pos = "below right"
    #     for node in option_node:
    #         if node[0] == 'terminal':
    #             result += '\t\\node (n%i) [terminal, %s=of n%i] {%s};\n' % (i, pos, prev_node, node[1])
    #             prev_node = i
    #             i = i + 1
    #             pos = "right"
    #         elif node[0] == 'id':
    #             result += '\t\\node (n%i) [nonterminal, %s=of n%i] {%s};\n' % (i, pos, prev_node, node[1])
    #             prev_node = i
    #             i = i + 1
    #             pos = "right"
    #         elif node[0] == 'option':
    #             #push option below...
    #             temp = i
    #             result, i = self._convert_option(node[1], i, prev_node, result)
    #             prev_node = temp
    #             pos = "above right"
    #         else:
    #             assert false
    #     
    #     return result, i
    
#----------------------------------------------------------------------------

def test_basic():
    lines = [   
        '(* Syntax for Pascal Program declaration  *)\n',
        'blah=a | b | (c, e) | (d, e) ;\n',
        # 'test=$~,~;\n',
        # 'test={blah,"blee"}*(",");\n',
        # '(*program = "program", identifier, ";", {Uses Clause}+, {zero, [or more]}*(","), block, "." ; *)\n',    
        # '(*program = "program", identifier, ";", [Uses Clause, [blah]], block, "." ; \n',
        # 'program = "program", identifier, ";" ; \n',
        # 'program = "program", identifier, ";", [uses clause [blah] ], block, "." ;\n',
        # 'uses clause = "uses", { unit identifier }+(","), ";" ;\n',
        # 'test = { blah } + \n', 
        # ' ( [blee] , "," ); \n',
        # 'digit = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 0 ;\n',
        # 'number = {1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | (0 , "fred" )}+ ;\n',
        # '\n',
        # 'number = 1 | 2 | 3 | 4 ;*)',
        ]
        
    c = EEBNFToTikzPicture()
    output = c.convert_file(lines)
    
    print(output)

if __name__ == '__main__':
    logging.basicConfig(level=logging.ERROR,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    test_basic()
