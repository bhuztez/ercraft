#!/usr/bin/env bash

LAUNCHER_MD5="$(md5sum .cache/s3.amazonaws.com/Minecraft.Download/launcher/launcher.pack.lzma  | grep -Eo '^\S+')"

erl -pa "$(pwd)/ebin" -s ercraft_app -ercraft launcher_md5 "\"${LAUNCHER_MD5}\""
