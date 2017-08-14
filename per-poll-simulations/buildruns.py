#!/usr/bin/env python

import csv
import os


config = '''
MaxSims = 5000
DaysTo = %d
'''

with open('PollsForGam.csv') as f:
    reader = csv.reader(f)
    out = [reader.next()] # Header
    out.append(reader.next()) # 2014 results
    out.append(reader.next()) # Manually done
    for poll in reader:
        out.append(poll)
        slug = poll[0].replace(' ', '-').lower()
        days2 = int(poll[2])
        if days2 < 365:
            odir = str(days2).zfill(4) + '-' + slug
            if not os.path.exists(odir):
                os.mkdir(odir)
            with open(os.path.join(odir, 'PollsForGam.csv'), 'w') as w:
                writer = csv.writer(w)
                for wr in out:
                    writer.writerow(wr)
            with open(os.path.join(odir, 'config.R'), 'w') as r:
                r.write(config % days2)

