#!/usr/bin/python3

# generates tablegen entries
# provided by instructions.csv
# suggestion of a workflow:
# 1. run before adding new instruction and store at "before" file
# 2. add new instructions to instructions.csv
# 3. run and store at "after" file
# 4. diff before after
# 5. paste at the recommended files and adapt to actual encoding
import csv
import os
import datetime
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

ENC_ENTRIES=''
INSTR_ENTRIES=''
BUILTIN_ENTRIES=[]
SCHED_ENTRIES= ''
SCHED_JOIN2='\n                                 '
JOIN_IN_LINE=False
for row in data:
    name = row['name'].rstrip().replace('.','_')
    name_upper = name.upper()
    name_nm = name_upper + '_NM'
    name_enc = name_nm + '_ENC'
    name_desc = name_upper + '_DESC'
    ENC_ENTRIES += f'class {name_enc} : SOME_FMT<0bxxx>;\n'
    INSTR_ENTRIES += f'def {name_nm} : {name_enc}, {name_desc} <NanoMipsRO_TYPES>;\n'
    BUILTIN_ENTRIES.append(f'  case Mips::BI__builtin_mips_{name}:')
    SCHED_ENTRIES += name_nm + ',\n'
now = datetime.datetime.now()
print(f'//paste in llvm/lib/Target/Mips/NanoMipsDSPInstrInfo.td and adapt to actual encoding //{now}')
print(ENC_ENTRIES)
print(f'//paste in llvm/lib/Target/Mips/NanoMipsDSPInstrInfo.td and adapt to actual description//{now}')
print(INSTR_ENTRIES)
print(f'//paste in clang/lib/Sema/SemaChecking.cpp in  Sema::CheckNanomipsBuiltinCpu"//{now}')
BUILTIN_ENTRIES.sort()
print('\n'.join(BUILTIN_ENTRIES))
print (f'''//paste in llvm/lib/Target/Mips/MipsScheduleGeneric.td under //{now}
// nanoMIPS
// =============

def : InstRW<[GenericWriteALU],
:
''')
print(SCHED_ENTRIES)
