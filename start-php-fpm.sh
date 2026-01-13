#!/usr/bin/env bash
# Start PHP-FPM with dynamically resolved paths
# This script should be run from the project root directory

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Create a temporary PHP-FPM config with resolved absolute paths
TMP_CONFIG=$(mktemp)
sed "s|__MOODLE_CONFIG_PATH__|${SCRIPT_DIR}/config.php|g" php-fpm.conf > "$TMP_CONFIG"

# Start PHP-FPM with the temporary config
php-fpm -y "$TMP_CONFIG" -p . -D

# Clean up
rm -f "$TMP_CONFIG"

echo "PHP-FPM started with MOODLE_CONFIG=${SCRIPT_DIR}/config.php"

