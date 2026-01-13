# SCORM Codec Setup Guide

## Quick Start

Simply run:
```bash
nix develop
```

This will automatically:
1. Start all required services (MariaDB, PHP-FPM, Nginx)
2. Create and configure the Moodle database
3. Install Moodle if not already installed
4. Display the admin username and password

**Default Admin Credentials:**
- Username: `admin`
- Password: `Admin123!`

Access Moodle at: http://localhost:8080

## Overview

This project uses relative paths throughout to ensure portability across different systems and users.

## Configuration Files

### PHP-FPM Configuration (`php-fpm.conf`)

The PHP-FPM configuration uses relative paths and requires starting with the `-p` (prefix) flag:

```bash
cd /path/to/project
php-fpm -y php-fpm.conf -p $(pwd) -D
```

Or use the provided startup script:

```bash
./start-php-fpm.sh
```

The startup script automatically:
1. Resolves the project directory to an absolute path
2. Creates a temporary config with the `MOODLE_CONFIG` environment variable set correctly
3. Starts PHP-FPM with the appropriate prefix

### Moodle Configuration (`config.php`)

The Moodle configuration file is located at the project root and uses `__DIR__` to dynamically resolve all paths:

- `$CFG->dataroot` - Points to `moodle-data/` directory
- `$CFG->dirroot` - Points to `moodle/` symlink (Nix store)
- `$CFG->dboptions['dbsocket']` - Points to `moodle-data/mysql.sock`

The config is loaded via the `MOODLE_CONFIG` environment variable, which is set by the `start-php-fpm.sh` script.

### Nginx Configuration (`nginx.conf`)

Nginx uses relative paths for log files and the document root:
- `root moodle;` - Serves from the moodle directory
- `error_log nginx-error.log;` - Relative path for error logs
- `access_log nginx-access.log;` - Relative path for access logs

## Starting Services

### 1. MariaDB
```bash
cd /path/to/project
mariadbd --datadir=./moodle-data/db --socket=./moodle-data/mysql.sock --port=3306 --user=$(whoami) &
```

### 2. PHP-FPM
```bash
cd /path/to/project
./start-php-fpm.sh
```

### 3. Nginx
```bash
cd /path/to/project
nginx -c $(pwd)/nginx.conf -p $(pwd)
```

## Database Setup

The database is created automatically when needed:

```bash
cd /path/to/project
mysql -S ./moodle-data/mysql.sock -e "
  CREATE DATABASE IF NOT EXISTS moodle DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
  CREATE USER IF NOT EXISTS 'moodle'@'localhost' IDENTIFIED BY 'moodle';
  GRANT ALL PRIVILEGES ON moodle.* TO 'moodle'@'localhost';
  FLUSH PRIVILEGES;
"
```

## Accessing the Application

Once all services are running, access the application at:
- **URL**: http://localhost:8080
- **Moodle Installation**: http://localhost:8080/admin/index.php

## Important Notes

1. **No Hardcoded Paths**: All configuration files use relative paths or runtime path resolution
2. **Working Directory**: Services must be started from the project root directory
3. **Moodle Directory**: The `moodle` directory is a symlink to the Nix store (read-only)
4. **Temporary Config**: The `start-php-fpm.sh` script creates a temporary config file that is automatically cleaned up

## Troubleshooting

### PHP-FPM Fails to Start
- Ensure you're running `start-php-fpm.sh` from the project root
- Check that `php-fpm.log` doesn't contain errors
- Verify the `MOODLE_CONFIG` path is correctly resolved

### Database Connection Errors
- Verify MariaDB is running: `ps aux | grep mariadb`
- Check socket file exists: `ls -la moodle-data/mysql.sock`
- Test connection: `mysql -S ./moodle-data/mysql.sock -u moodle -pmoodle`

### Moodle Errors
- Check that `config.php` is readable
- Verify all paths in `config.php` resolve correctly
- Review `php-fpm.log` and `nginx-error.log` for details

