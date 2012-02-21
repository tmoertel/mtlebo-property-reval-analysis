#!/usr/bin/env python


import csv
import fileinput
import os
import sys
import time
import urllib
import urllib2



def main(urlbase, outdir, parcel_ids):
    parcel_ids = clean_parcel_ids(parcel_ids)
    count = len(parcel_ids)
    start_time = time.time()
    for (i, pid) in enumerate(parcel_ids):
        process_parcel(urlbase, outdir, pid)
        if i % 100 == 0:
            print progress_indicator(start_time, i + 1, count)


def clean_parcel_ids(parcel_ids):
    return [row[0] for row in csv.reader(parcel_ids) if row[0] != 'PIN']


def progress_indicator(start_time, finished, total):
    remaining = total - finished
    time_elapsed = time.time() - start_time
    remaining_hr = remaining * time_elapsed / finished / 3600.0
    return '%5d (est. remaing time = %.2f hr)' % (finished, remaining_hr)


def process_parcel(urlbase, outdir, pid):
    data = fetch_parcel(urlbase, pid)
    if data is not None:
        with file(os.path.join(outdir, '%s.html' % (pid,)), 'w') as f:
            f.write(data)


def fetch_parcel(urlbase, pid):
    url = urlbase + urllib.quote(pid)
    try:
        return urllib2.urlopen(url).read()
    except urllib2.HTTPError, e:
        print >>sys.stderr, '!!! skipping %s (reason: %s)' % (pid, e)
    return None  # signal failure


if __name__ == '__main__':
    if len(sys.argv) < 3:
        print >>sys.stderr, (
            'Usage: %s URLBASE OUTPUTDIR [PARCELIDFILE...]' % (sys.argv[0],))
        sys.exit(1)
    main(sys.argv[1], sys.argv[2], fileinput.input(sys.argv[3:]))
