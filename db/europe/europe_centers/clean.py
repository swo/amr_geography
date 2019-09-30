#!/usr/bin/env python3

def fix_line(line):
    raw_fields = line.rstrip().split()
    fields = [' '.join(raw_fields[0:-3]), raw_fields[-3], raw_fields[-2], raw_fields[-1]]
    return fields

with open('raw2.txt') as f:
    for line in f:
        fields = fix_line(line)
        print(*fields, sep='\t')
