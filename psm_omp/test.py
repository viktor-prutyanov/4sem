#!/usr/bin/python

import os
import subprocess
import sys

if len(sys.argv) != 3:
   print "Usage: test.py <name> <num of tests>"
   exit(-1)

cpu_num = int(subprocess.check_output("nproc"))
print "nproc:", cpu_num

test_set = []
i = 2
while i <= cpu_num:
    test_set.append(i)
    i = i * 2

name = "./" + sys.argv[1]

test_nums = int(sys.argv[2])

for j in xrange(0, test_nums):
    print "test #", j + 1
    first_time = float(subprocess.check_output(["/usr/bin/time", "-f %e", name, "1"], stderr=subprocess.STDOUT))
    print "1 thread:", first_time
    for i in test_set:
        time = float(subprocess.check_output(["/usr/bin/time", "-f %e", name, str(i)], stderr=subprocess.STDOUT))
        print i, "threads:", time, "against", first_time / i, "relative error is", '{0:.0%}'.format(abs(first_time / i - time) / time)

