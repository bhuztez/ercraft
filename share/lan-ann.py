#!/usr/bin/env python2

import sys
import socket
import struct
import time

s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, True)

PORT = sys.argv[1]
MOTD = 'Unnamed Server'
if len(sys.argv) > 2:
    MOTD = sys.argv[2]

while True:
    s.sendto("[MOTD]%s[/MOTD][AD]%s[/AD]" % (MOTD, PORT), ("224.0.2.60", 4445))
    time.sleep(1)
