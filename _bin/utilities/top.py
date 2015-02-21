import sys
import subprocess
 
from time import sleep
 
cmd = ['top', '-l 0']
 
p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
 
for line in iter(p.stdout.readline, b''):
    sys.stdout.write("\r" + line.decode().replace('\n', ''))
    sys.stdout.flush()
    sleep(1)
