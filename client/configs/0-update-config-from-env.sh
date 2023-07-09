set -e

# Create a temporary file for in-place updates
tempFile="/tmp/config.temp.json"

# Specify config path
configFile="/usr/share/nginx/html/nocache/config.json"

# Update config from env variables, 1 line per 1 variable.
jq --arg a "$CLIENT_BACKEND_URL" '.backendUrl = $a' "$configFile" >"$tempFile" && mv "$tempFile" "$configFile"
