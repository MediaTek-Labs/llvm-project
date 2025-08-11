#!/usr/bin/python3

# generates assembly test for DSP ASE instructions
# provided by csv file specified by --input argument
# which defaults to instructions.csv
# dump into llvm/test/MC/Mips/nanomips/dsp_asm.s
# and run <build path>/bin/llvm-lit llvm/test/asm.s

import csv
import os
import re
import random
import argparse
LITS = {'0', '1', 'x'}
TESTS_PER_INSTR = 3
GPR = re.compile('GPR\[(rs|rd|rt)\]\((\d+)\)')
ACC = re.compile('ACC\[(ac)\]\((\d+)\)')
IMM = re.compile('(mask|sa|shift|size|s|u)\((\d+)(u|l)\)')
ADDRESS = re.compile('(s)\((\d+)(u|l)\)')
LABEL='.LBB0_2'
INSTR_WITH_ADDRESS=['bposge32c']

def parse_encoding(encoding, name):
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
        m = ADDRESS.match(part)
        if m and name in INSTR_WITH_ADDRESS:
            operands.append({'type': 'ADDRESS', 'name': m.group(1), 'width': int(m.group(2))})
            continue
        m = IMM.match(part)
        if m:
            signed = {'u': False, 'l': True}[m.group(3)]
            operands.append({'type': 'IMM',
                             'name': m.group(1),
                             'width': int(m.group(2)),
                             'signed': signed})
            continue
        print (encoding)
        assert False, "unexpected pattern " + part
    return operands


def get_immediate(n, is_signed = False):
    if is_signed:
        # Calculate the range based on the width n
        min_val = -2**(n-1)
        max_val = 2**(n-1) - 1
        # Generate a random number within the range
        random_number = random.randint(min_val, max_val)

        # Convert to 2's complement binary representation
        if random_number < 0:
            # Calculate 2's complement for negative numbers
            string_representation = bin(random_number & (2**n - 1))[2:]
        else:
            # Format positive numbers to maintain width n
            string_representation = format(random_number, f'0{n}b')
    else:
        random_number = random.getrandbits(n)
        string_representation = format(random_number, f'0{n}b')

    return string_representation, random_number


def insert_operand_to_format(instr_format, name, val, is_register = False):
    prefix = '$' if is_register else ''
    instr_format = instr_format.replace('%' + name, prefix + '{value}')
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


'''
BPOSGE32C: we can't test actual value of the address since it's
subjected to relocation.
Instead, every pair of address bits is marked as 'A'.
To make it more complicated, a prefix of the 'A's string is appended
to the suffix of non-address bits in binary form, up to the size of the address.
Then the suffix the the 'A's string is a emitted in a separate chunk
'''
def  bposge32c_fixup(hex_chunks, address_operand):
    width = address_operand['width']
    assert (width < 16 and width > 8), \
        f'Unexepected address operand width {width} of bposge32c'
    retained_bits = 16 - width
    bin_chunk2 = format(int(hex_chunks[2], 16), '08b')
    new_chunk2 ='0b' + bin_chunk2[0:retained_bits]
    new_chunk2 += 'A'*((width - retained_bits)>>1)
    new_chunk3 = 'A'* (retained_bits>>1)
    return [hex_chunks[0], hex_chunks[1], new_chunk2, new_chunk3]


def get_instance(instr, max_format):
    binary_string = ''
    instr_format = instr['format']
    for operand in instr['operands']:
        if operand['type'] == 'literal':
             binary_string += operand['value']
        elif operand['type'] == 'ADDRESS':
             binary_string += '0' * operand['width']
             instr_format = insert_operand_to_format(instr_format, operand['name'], LABEL)
        elif operand['type'] == 'IMM':
             string, num = get_immediate(operand['width'], operand['signed'])
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
    assert len(binary_string) == 32, \
        f'Unexepected binary length ({str(len(binary_string))})' + \
        f' instead of 32 in {instr["name"]}:\n\t {binary_string}'
    instr_format = instr_format.replace("@", instr['name'])
    instr_format = instr_format.ljust(max_format + 14)
    if instr['name'].startswith("#"):
        return f"  {instr_format} # yet to be implemened"
    chunks = split8(binary_string)
    hex_chunks = tohex(chunks)
    if instr['name'].startswith('bposge32c'):
        hex_chunks = bposge32c_fixup(hex_chunks, instr['operands'][-1])
    test_line = f'  {instr_format} # CHECK: {instr_format}'
    line_len = len(test_line)
    test_line +=  "# encoding: [{e1},{e0},{e3},{e2}]"
    test_line = test_line.format(e2 = hex_chunks[2], e3 = hex_chunks[3],
                                 e0 = hex_chunks[0], e1 = hex_chunks[1])
    binary_comment = '\n' + (' ' * line_len) + f'# 0b{binary_string}'
    test_line += binary_comment
    return test_line


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

parser = argparse.ArgumentParser()
parser.add_argument('--input', help='input csv', default='instructions.csv')
parser.add_argument('--cpu', help='CPU/arch', default='i7200')
args=parser.parse_args()

file_path = os.path.join(file_path, args.input)

data = read_file(file_path)

for row in data:
    row['operands'] = parse_encoding(row['encoding'], row['name'])

max_format = max(len(item['format']) for item in data)

test_lines =  [ get_instances(row, max_format, TESTS_PER_INSTR) for row in data ]
test_lines = '\n'.join(test_lines)
test = f'''# DO NOT MODIFY!
# This test was auto-generated by {os.path.basename(__file__)}
# Please modify its input ({args.input}) instead, and re-generate.
# RUN: llvm-mc -show-encoding -triple=nanomips-elf -mattr=dsp -mcpu={args.cpu} %s | FileCheck %s
#
  .set noat
{test_lines}
{LABEL}:
  balc    abort
'''
print(test)
