#!/usr/bin/env bash

if [ ! -e .cert ]
then
ROOT="$(pwd)/.cert"

NSSDIR="${ROOT}"
PASSWD="${ROOT}/passwd"
KEYSTORE="${ROOT}/cacert"
CACERT="${ROOT}/ca.crt"
CAPEM="${ROOT}/ca.pem"

CANAME="MinecraftCacheCA"
DNSNAMES='s3.amazonaws.com,*.mojang.com,*.minecraft.net'
SUBJECT="CN=Minecraft Cache CA"

mkdir -p "${NSSDIR}"

cat > "${PASSWD}" <<EOF
qwertyuiop
EOF

certutil -N -d "${NSSDIR}" -f "${PASSWD}"
certutil -S -d "${NSSDIR}" -f "${PASSWD}" -n "${CANAME}" -s "${SUBJECT}" -v 120 -t "C,," -x -8 "${DNSNAMES}"
certutil -L -d "${NSSDIR}" -a -n "${CANAME}" > "${CACERT}"

keytool -import -storepass qwerty -noprompt -trustcacerts -file "${CACERT}" -alias "${CANAME}" -keystore "${KEYSTORE}"
pk12util -o /dev/stdout -n "${CANAME}" -d "${NSSDIR}" -k "${PASSWD}" -W '' | openssl pkcs12 -passin 'pass:' -nodes -out "${CAPEM}"
fi

echo "run 'userns connect minecraft' to start a shell in this namespace"
userns spawn -n minecraft -d localdomain --user --net -- ./init-ns.sh userns listen
