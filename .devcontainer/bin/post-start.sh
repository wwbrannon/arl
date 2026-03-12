#!/bin/bash
git config --global --add safe.directory /workspace

key=/home/agent/.ssh/git-signing/key
if [ -f "$key" ]; then
    git config --global gpg.format ssh
    git config --global user.signingkey "$key"
    git config --global commit.gpgsign true
    echo "Git commit signing configured."
else
    echo "No signing key found, commits will be unsigned."
fi
