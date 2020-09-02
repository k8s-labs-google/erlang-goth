#!/bin/sh

# Chown the ssh-agent sock and docker sock if they exist
if [ -e /home/ex/.ssh-auth.sock ]; then sudo chown ex:ex /home/ex/.ssh-auth.sock; fi
if [ -e /home/ex/.docker.sock ];   then sudo chown ex:ex /home/ex/.docker.sock; fi

exec /bin/bash
