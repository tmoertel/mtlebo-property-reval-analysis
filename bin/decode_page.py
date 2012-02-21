#!/usr/bin/env python

import csv
import fileinput
import re
import sys


LABEL_RE = re.compile(r'<span\s+id="(.+?)".*?class="label"', re.I)
DATA_RE = re.compile(r' class="data">([^<]*)', re.I)

SEPERATOR = '|'

CLEAN_SUBS = {
    '&nbsp;': ' ',
    '<br>': ' / ',
    '&gt;': '>',
    '&lt;': '<'
    }



def main(label_map_lines, parcel_html_lines):

    labels_in, labels_out = zip(*csv.reader(label_map_lines))
    labels_in_set = set(labels_in)
    file_count = 0
    record = dict()
    for line in parcel_html_lines:
        line = line.strip()
        if line == '</HTML>':
            file_count += 1
            if file_count == 1:
                print SEPERATOR.join(labels_out)
            print SEPERATOR.join(record.get(l, '') for l in labels_in)
            record = dict()
        match = LABEL_RE.search(line)
        if match:
            current_label = clean(match.group(1))
        match = DATA_RE.search(line)
        if match and current_label in labels_in_set:
            record[current_label] = clean(match.group(1))

def clean(s):
    s = s.strip()
    for target, replacement in CLEAN_SUBS.iteritems():
        s = s.replace(target, replacement)
    s = ' '.join(s.split())  # normalize spacing
    if '$' in s:
        # normalize dollars
        if '(' in s:
            s = '-' + s   # ($xyz) == $-xyz
        s = re.sub(r'[^-0-9.]+', '', s)  # keep only minus, decimal, and digits
    return s


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print >>sys.stderr, (
            'Usage: %s LABELMAP PARCEL_HTML_FILES...' % (sys.argv[0],))
        sys.exit(1)
    main(fileinput.input(sys.argv[1:2]), fileinput.input(sys.argv[2:]))
