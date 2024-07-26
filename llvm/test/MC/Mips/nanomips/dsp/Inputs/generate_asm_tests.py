#!/usr/bin/python3
import csv
import os
import re
import random
LITS = {'0', '1', 'x'}

GPR = re.compile('GPR\[(rs|rd|rt)\]\((\d+)\)')
ACC = re.compile('ACC\[(ac)\]\((\d+)\)')
MASK = re.compile('(mask)\((\d+)\)')

def parse_encoding(encoding):
    encoding = encoding.replace(' ','')
    parts = encoding.split('|')
    operands = []
    for part in parts:
        if set(part).issubset(LITS):
            operands.append({'type': 'literal', 'value':part.replace('x','0')})
            continue
        m = GPR.match(part)
        if m:
            operands.append({'type': 'GPR', 'name': m.group(1), 'width': int(m.group(2))})
            continue
        m = ACC.match(part)
        if m:
            operands.append({'type': 'ACC', 'name': m.group(1), 'width': int(m.group(2))})
            continue
        m = MASK.match(part)
        if m:
            operands.append({'type': 'IMM', 'name': m.group(1), 'width': int(m.group(2))})
            continue
        print (encoding)
        assert False, "unexpected pattern " + part
    return operands


def get_immediate(n):
    num = random.getrandbits(n)
    return format(num, f'0{n}b'), num

def insert_operand_to_format(instr_format, name, val, is_register = False):
    prefix = '$' if is_register else ''
    instr_format = instr_format.replace('$' + name, prefix + '{value}')
    return instr_format.format(value = val)


# Split the string into 8-character chunks
def split8(string):
   return [string[i:i+8] for i in range(0, len(string), 8)]


def tohex(chunks):
   return ['0x' + format(int(chunk, 2), '02x') for chunk in chunks]


# defined at MipsAsmParser::matchCPURegisterName
NM_REG_NAMES = {
    0:"zero", 1:"at", 2:"t4", 3:"t5", 4:"a0",
    5:"a1", 6:"a2", 7:"a3", 8:"a4", 9:"a5",
    10:"a6", 11:"a7", 12:"t0", 13:"t1", 14:"t2",
    15:"t3", 16:"s0", 17:"s1", 18:"s2", 19:"s3",
    20:"s4", 21:"s5", 22:"s6", 23:"s7", 24:"t8",
    25:"t9", 26:"k0", 27:"k1", 28:"gp", 29:"sp",
    30:"fp", 31:"ra"
}

def get_instance(instr, max_format):
    binary_string = ''
    instr_format = instr['format']
    for operand in instr['operands']:
        if operand['type'] == 'literal':
             binary_string += operand['value']
        elif operand['type'] == 'IMM':
             string, num = get_immediate(operand['width'])
             binary_string += string
             instr_format = insert_operand_to_format(instr_format, operand['name'], str(num))
        elif operand['type'] in ['GPR', 'ACC']:
             string, num = get_immediate(operand['width'])
             binary_string += string
             if operand['type'] == 'GPR':
                 string = NM_REG_NAMES.get(num, None)
             else:
                 string = 'ac' + str(num)
             assert string is not None, "No match for register number " + str(num)
             instr_format = insert_operand_to_format(instr_format, operand['name'], string, True)
    instr_format = instr_format.replace("@", instr['name'])
    instr_format = instr_format.ljust(max_format + 10)
    chunks = split8(binary_string)
    hex_chunks = tohex(chunks)
    test_line = f'  {instr_format} # CHECK: {instr_format}'
    test_line +=  "# encoding: [{e1},{e0},{e3},{e2}]"
    return test_line.format(e2 = hex_chunks[2], e3 = hex_chunks[3],
                            e0 = hex_chunks[0], e1 = hex_chunks[1])

def get_instances(instr, max_format, count):
    tests = [get_instance(instr, max_format) for i in range(count)]
    return '\n'.join(tests)


def read_file(file_path):
    data = []
    # Read the CSV file
    required_columns = {"name", "format", "encoding"}
    with open(file_path, mode='r', encoding='utf-8') as csvfile:
        # Create a CSV reader object
        csv_reader = csv.DictReader(csvfile)
       # Convert the fieldnames to a set for easy comparison
        header_set = set(csv_reader.fieldnames)
        assert required_columns.issubset(header_set), \
            "CSV file is missing required columns."
        # Iterate over the rows in the CSV file
        for row in csv_reader:
            # Each row is a dictionary where the keys are the column headers
            # and the values are the data in the current row
            data.append(row)
    return data

file_path = os.path.abspath(os.path.dirname(__file__))
# Define the path to the CSV file
file_path = os.path.join(file_path, 'instructions.csv')

data = read_file(file_path)

for row in data:
    row['operands'] = parse_encoding(row['encoding'])

max_format = max(len(item['format']) for item in data)

test_lines =  [ get_instances(row, max_format, 3) for row in data ]
test_lines = '\n'.join(test_lines)
test = f'''# RUN: llvm-mc -show-encoding -triple=nanomips-elf -mattr=dsp %s | FileCheck %s
#
# CHECK:   .text
  .set noat
{test_lines}'''
print(test)
