#!/bin/bash

# NOTE this script is not currently used, included as example

set -Eeuo pipefail

# ---------------------------------------------------------------
# Network restrictions require the following in devcontainer.json:
#
#   "runArgs": [
#     "--cap-add=NET_ADMIN",
#     "--cap-add=NET_RAW"
#   ]
# ---------------------------------------------------------------

# --- Network egress restrictions ---
setup_network_restrictions() {
    # Flush any existing rules
    iptables -F OUTPUT 2>/dev/null

    # Allow loopback
    iptables -A OUTPUT -o lo -j ACCEPT

    # Allow DNS
    iptables -A OUTPUT -p udp --dport 53 -j ACCEPT
    iptables -A OUTPUT -p tcp --dport 53 -j ACCEPT

    # Allow established/related connections
    iptables -A OUTPUT -m state --state ESTABLISHED,RELATED -j ACCEPT

    # Allow HTTPS to specific hosts
    for host in \
        api.anthropic.com \
        github.com \
        objects.githubusercontent.com \
        ghcr.io \
        pkg-containers.githubusercontent.com \
        cran.r-project.org \
        cloud.r-project.org \
    ; do
        for ip in $(dig +short "$host" 2>/dev/null); do
            iptables -A OUTPUT -p tcp --dport 443 -d "$ip" -j ACCEPT
        done
    done

    # Allow SSH to GitHub (for git operations)
    for ip in $(dig +short github.com 2>/dev/null); do
        iptables -A OUTPUT -p tcp --dport 22 -d "$ip" -j ACCEPT
    done

    # Drop everything else
    iptables -A OUTPUT -j DROP

    echo "Network egress restrictions applied."
}

if command -v iptables &>/dev/null; then
    setup_network_restrictions
else
    echo "WARNING: iptables not available, no network restrictions applied."
fi

# --- Git config ---
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
