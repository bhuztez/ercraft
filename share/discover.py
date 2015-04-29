#!/usr/bin/env python2

import socket
import struct

s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
s.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, True)
s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, True)

s.bind(("0.0.0.0", 4445))
group = socket.inet_pton(socket.AF_INET, "224.0.2.60") + struct.pack('=I', socket.INADDR_ANY)
s.setsockopt(socket.SOL_IP, socket.IP_ADD_MEMBERSHIP, group)

while True:
    print s.recvfrom(2048)
