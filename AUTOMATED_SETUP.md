# Automated Moodle Setup

## What's New

The `nix develop` command now automatically sets up a complete working Moodle installation with the following features:

### Automatic Installation
- **First-time setup**: Detects if Moodle is not installed and runs the CLI installer automatically
- **Existing installation**: If Moodle is already installed, skips installation and shows credentials
- **Database setup**: Creates the `moodle` database and user automatically
- **Admin account**: Creates an admin account with pre-configured credentials

### Admin Credentials
The system uses consistent, pre-configured credentials:
- **Username**: `admin`
- **Password**: `Admin123!`
- **Email**: `admin@example.com`

These credentials are displayed every time you run `nix develop`, so you never need to remember them.

## How It Works

### 1. Directory Structure
The system creates a writable `moodle` directory with symlinks to the read-only Nix store:
```bash
moodle/
├── admin/ -> /nix/store/.../admin
├── auth/ -> /nix/store/.../auth
├── config.php -> /path/to/project/config.php
├── ... (all other directories symlinked)
```

This allows:
- Most files come from the Nix store (read-only, version-controlled)
- `config.php` is writable and project-specific
- No modifications to the Nix store required

### 2. Database Initialization
On startup, the script:
1. Waits for MariaDB to be ready (with timeout)
2. Creates the `moodle` database if it doesn't exist
3. Creates the `moodle` user with password `moodle`
4. Grants necessary privileges

### 3. Installation Check
The script checks if Moodle is installed by querying for the `mdl_user` table:
```bash
mysql ... -se "SHOW TABLES LIKE 'mdl_user'"
```

If the table doesn't exist, it runs the Moodle CLI installer.

### 4. CLI Installation
Uses Moodle's built-in CLI installer:
```bash
php moodle/admin/cli/install_database.php \
  --lang=en \
  --adminuser="admin" \
  --adminpass="Admin123!" \
  --adminemail="admin@example.com" \
  --fullname="SCORM Development Site" \
  --shortname="SCORM Dev" \
  --agree-license
```

### 5. Credential Display
After setup, the credentials are prominently displayed:
```
════════════════════════════════════════════════════════════
  MOODLE ADMIN CREDENTIALS
════════════════════════════════════════════════════════════
  Username: admin
  Password: Admin123!
════════════════════════════════════════════════════════════
```

## Usage

### First Time Setup
```bash
cd /path/to/scorm-codec
nix develop
```

Wait for installation to complete (about 30-60 seconds), then access:
- Moodle: http://localhost:8080
- Login with `admin` / `Admin123!`

### Subsequent Uses
```bash
nix develop
```

The system detects existing installation and shows credentials immediately.

### Resetting Moodle
To start fresh:
```bash
# Stop services
systemctl --user stop moodle-nginx moodle-mariadb
pkill -f 'php-fpm|python3 -m http.server'

# Remove data
rm -rf moodle-data/db moodle/

# Restart
nix develop
```

## Troubleshooting

### Installation Fails
If installation fails, check:
1. **MariaDB**: Is it running? `pgrep mariadbd`
2. **Socket**: Does it exist? `ls -la moodle-data/mysql.sock`
3. **Database**: Can you connect? `mysql -S moodle-data/mysql.sock -u moodle -pmoodle`

### Already Installed Error
If you see errors about existing tables:
- This is normal if the database already exists
- The script will verify installation and continue

### Can't Login
If you can't login with the displayed credentials:
1. Check if installation actually completed
2. Look for errors in `php-fpm.log` and `nginx-error.log`
3. Try accessing the Moodle site directly: http://localhost:8080

### Services Not Starting
If services don't start:
```bash
# Check for existing services
systemctl --user status moodle-nginx moodle-mariadb

# Stop them if needed
systemctl --user stop moodle-nginx moodle-mariadb

# Try again
nix develop
```

## Technical Details

### Modified Files
1. **flake.nix**: Added installation logic in `shellHook`
2. **SETUP.md**: Added quick start guide
3. **README.md**: Added development environment section

### Key Changes in flake.nix
- Writable moodle directory with symlinks to Nix store
- Database creation and user setup
- Installation detection (checking for `mdl_user` table)
- CLI installer invocation
- Credential display
- Installation verification

### Database Details
- **Host**: localhost (via socket)
- **Socket**: `moodle-data/mysql.sock`
- **Database**: `moodle`
- **Username**: `moodle`
- **Password**: `moodle`
- **Charset**: utf8mb4
- **Collation**: utf8mb4_unicode_ci

### Moodle Configuration
All configuration is in `config.php`:
- **WWW Root**: http://localhost:8080
- **Data Root**: `$PWD/moodle-data`
- **Dir Root**: `$PWD/moodle`
- **DB Socket**: `$PWD/moodle-data/mysql.sock`

## Benefits

1. **Zero Configuration**: Just run `nix develop` and everything works
2. **Consistent Credentials**: Always the same admin credentials
3. **Idempotent**: Safe to run multiple times
4. **Fast**: Installation only happens once
5. **Portable**: Works on any system with Nix
6. **No Manual Steps**: Fully automated

