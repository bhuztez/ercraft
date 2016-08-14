#!/usr/bin/env python2

import os
import os.path
import errno
import urllib
import urlparse
import json
import hashlib

CACHEDIR=".cache"
OS="linux"


def reporthook(blocks, block_size, size):
    total = blocks * block_size
    text = "%d/%d" % (total, size)
    spaces = 76 - len(text)
    progress = total * spaces / size

    if progress >= spaces:
        print "\r[%s] %s" % ("="*spaces, text),
    else:
        print "\r[%s>%s] %s" % ("="*progress, " "*(spaces-1-progress), text),


def getfullpath(url):
    o = urlparse.urlparse(url)
    return os.path.join(CACHEDIR, o.netloc + o.path)


def fetchfile(url):
    fullpath = getfullpath(url)

    if os.path.exists(fullpath):
        return fullpath

    try:
        os.makedirs(os.path.dirname(fullpath))
    except OSError as e:
        if e.errno != errno.EEXIST:
            raise

    print 'DOWNLOAD:', url
    urllib.urlretrieve(url, fullpath+".temp", reporthook=reporthook)
    print
    os.rename(fullpath+".temp", fullpath)
    return fullpath


def readjson(url, sha1=None):
    fullpath = fetchfile(url)
    if(sha1 is not None):
        verify_sha1(url, sha1)
    with open(fullpath, 'r') as f:
        return json.load(f)


def iter_by_block(f, block_size):
    while True:
        buf = f.read(block_size)
        if buf == '':
            break
        yield buf


def verify_sha1(url, sha1):
    fullpath = fetchfile(url)
    h = hashlib.sha1()
    with open(fullpath, 'r') as f:
        for block in iter_by_block(f, 4096):
            h.update(block)
    assert h.hexdigest() == sha1


DOWNLOAD = "http://s3.amazonaws.com/Minecraft.Download"
RESOURCES = "http://resources.download.minecraft.net"
LIBRARIES = "https://libraries.minecraft.net"
META = "https://launchermeta.mojang.com"

fetchfile(DOWNLOAD + "/launcher/Minecraft.jar")
fetchfile(DOWNLOAD + "/launcher/launcher.pack.lzma")
versions = readjson(META + "/mc/game/version_manifest.json")

version = versions["latest"]["release"]
latest_version = readjson({v["id"]:v["url"] for v in versions["versions"]}[version])
verify_sha1(latest_version["downloads"]["client"]["url"], latest_version["downloads"]["client"]["sha1"])
verify_sha1(latest_version["downloads"]["server"]["url"], latest_version["downloads"]["server"]["sha1"])
verify_sha1(latest_version["logging"]["client"]["file"]["url"], latest_version["logging"]["client"]["file"]["sha1"])


for library in latest_version["libraries"]:
    package, name, version = library["name"].split(":")

    if "rules" in library:
        allow = False
        for rule in library["rules"]:
            allow1 = (rule["action"] == "allow")

            if "os" in rule:
                if rule["os"]["name"] != OS:
                    continue
                allow = allow1
                break

            allow = allow1

        if not allow:
            continue

    if "natives" in library:
        artifact = library["downloads"]["classifiers"][library["natives"][OS]]
    else:
        artifact = library["downloads"]["artifact"]
    print 'LIBRARY:', library["name"]
    verify_sha1(artifact["url"], artifact["sha1"])


index = readjson(latest_version["assetIndex"]["url"], latest_version["assetIndex"]["sha1"])

for filename, asset in index["objects"].iteritems():
    h = asset["hash"]
    print 'ASSET:', filename
    verify_sha1(RESOURCES + "/%s/%s" % (h[:2], h), h)
