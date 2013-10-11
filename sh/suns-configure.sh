# This file is part of the Suns Search Engine
#
# The Suns Search Engine is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option) any
# later version.
#
# The Suns Search Engine is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# the Suns Search Engine.  If not, see <http://www.gnu.org/licenses/>.

#!/bin/bash

set -e

# Create a new vhost
rabbitmqctl add_vhost suns-vhost

# ADMINPASSWORD is what the `suns-admin` program will use
read -s -p "Enter admin password: " ADMINPASSWORD
rabbitmqctl add_user suns-admin ${ADMINPASSWORD}

# Sets the permissions necessary to configure the server
rabbitmqctl set_permissions -p suns-vhost suns-admin \
    "^suns-*" \
    "^suns-*" \
    "^suns-*"

# SERVERPASSWORD is what the `suns-server` program will use
read -s -p "Enter server password: " SERVERPASSWORD
rabbitmqctl add_user suns-server ${SERVERPASSWORD}

# Sets the permissions necessary to serve search requests
rabbitmqctl set_permissions -p suns-vhost suns-server \
    "^$" \
    "^suns-exchange-responses$" \
    "^suns-queue-.*"

# The client password is fixed and public
rabbitmqctl add_user suns-client suns-client

# Sets the permissions necessary to add requests and listen for results
rabbitmqctl set_permissions -p suns-vhost suns-client \
    "^amq\.gen.*" \
    "^amq\.gen.*|suns-exchange-requests$" \
    "^amq\.gen.*|suns-exchange-responses$"
