# Automated Moodle Setup Flow

This document visualizes the automated setup process that runs when you execute `nix develop`.

## Setup Flow Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                    nix develop                               │
└─────────────────┬───────────────────────────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────────────────────────┐
│  1. Create Directory Structure                              │
│     • moodle-data/                                           │
│     • moodle-config/                                         │
│     • moodle-data/db/                                        │
│     • static-files/                                          │
└─────────────────┬───────────────────────────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────────────────────────┐
│  2. Create Moodle Directory with Symlinks                   │
│     • Create moodle/ directory                               │
│     • Symlink all Nix store files                           │
│     • Link config.php from project root                     │
└─────────────────┬───────────────────────────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────────────────────────┐
│  3. Initialize MariaDB                                       │
│     • Check if database initialized                          │
│     • Run mysql_install_db if needed                         │
└─────────────────┬───────────────────────────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────────────────────────┐
│  4. Start Services                                           │
│     ┌─────────────────────────────────────────────────┐    │
│     │  MariaDB (systemd-run)                          │    │
│     │  • Socket: moodle-data/mysql.sock               │    │
│     │  • Port: 3306                                   │    │
│     └─────────────────────────────────────────────────┘    │
│     ┌─────────────────────────────────────────────────┐    │
│     │  PHP-FPM                                         │    │
│     │  • Listen: 127.0.0.1:9000                       │    │
│     │  • Config: php-fpm.conf                         │    │
│     └─────────────────────────────────────────────────┘    │
│     ┌─────────────────────────────────────────────────┐    │
│     │  Nginx (systemd-run)                             │    │
│     │  • Port: 8080                                   │    │
│     │  • Root: moodle/                                │    │
│     └─────────────────────────────────────────────────┘    │
│     ┌─────────────────────────────────────────────────┐    │
│     │  Static File Server (Python)                     │    │
│     │  • Port: 8081                                   │    │
│     │  • Root: static-files/                          │    │
│     └─────────────────────────────────────────────────┘    │
└─────────────────┬───────────────────────────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────────────────────────┐
│  5. Wait for MariaDB Ready                                   │
│     • Try connection every 0.5s                              │
│     • Max 30 attempts (15 seconds)                           │
│     • Query: SELECT 1                                        │
└─────────────────┬───────────────────────────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────────────────────────┐
│  6. Create Database and User                                 │
│     • CREATE DATABASE moodle                                 │
│     • CREATE USER moodle@localhost                           │
│     • GRANT ALL PRIVILEGES                                   │
└─────────────────┬───────────────────────────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────────────────────────┐
│  7. Check if Moodle Installed                                │
│     • Query: SHOW TABLES LIKE 'mdl_user'                     │
└─────────────────┬───────────────────────────────────────────┘
                  │
                  ├─── No ───┐
                  │           │
                  │           ▼
                  │     ┌─────────────────────────────────────┐
                  │     │  8a. Install Moodle                 │
                  │     │     • Run install_database.php      │
                  │     │     • Create admin user              │
                  │     │     • Initialize tables              │
                  │     │     • Verify installation            │
                  │     └─────────────┬───────────────────────┘
                  │                   │
                  │ Yes               │
                  │                   │
                  └───────────────────┴───────────────────────┐
                                      │
                                      ▼
                  ┌─────────────────────────────────────────────┐
                  │  9. Display Credentials                     │
                  │                                             │
                  │  ════════════════════════════════════════   │
                  │    MOODLE ADMIN CREDENTIALS                 │
                  │  ════════════════════════════════════════   │
                  │    Username: admin                          │
                  │    Password: Admin123!                      │
                  │  ════════════════════════════════════════   │
                  │                                             │
                  │  Services:                                  │
                  │  • http://localhost:8080 (Moodle)           │
                  │  • http://localhost:8081 (Static Files)     │
                  └─────────────────────────────────────────────┘
```

## State Transitions

```
Initial State          → Services Started → Database Ready → Moodle Checked
                                                                    │
                                                ┌───────────────────┴──────────────────┐
                                                │                                      │
                                          Not Installed                          Installed
                                                │                                      │
                                                ▼                                      │
                                         Run Installer                                 │
                                                │                                      │
                                                └──────────────┬───────────────────────┘
                                                               │
                                                               ▼
                                                      Display Credentials
```

## Decision Points

### 1. Moodle Directory Exists?
- **No**: Create directory, create symlinks to Nix store
- **Yes**: Skip directory creation

### 2. MariaDB Initialized?
- **No**: Run `mysql_install_db`
- **Yes**: Skip initialization

### 3. Services Running?
- **No**: Start service with systemd-run or direct execution
- **Yes**: Skip service start

### 4. Moodle Installed? (mdl_user table exists)
- **No**: Run CLI installer with admin credentials
- **Yes**: Skip installation

## Key Features

### Idempotency
- Safe to run multiple times
- All operations check state first
- No errors from repeated execution

### Error Handling
- Database connection retry (30 attempts)
- Installation verification after install
- Graceful degradation on errors

### Performance
- Services start in parallel (systemd-run --no-block)
- Only installs once
- Subsequent runs are fast (<5 seconds)

### User Experience
- Clear progress messages
- Prominent credential display
- Service URLs shown
- Stop instructions provided

## Time Estimates

| Operation                    | First Run | Subsequent Runs |
|------------------------------|-----------|-----------------|
| Directory creation           | <1s       | <0.1s           |
| MariaDB initialization       | 5-10s     | <0.1s           |
| Service startup              | 2-3s      | 1-2s            |
| Database creation            | <1s       | <0.1s           |
| Moodle installation          | 30-60s    | <0.1s           |
| **Total**                    | **40-75s**| **2-4s**        |

## Exit Conditions

### Success
- All services running
- Database created and accessible
- Moodle installed (or already was)
- Credentials displayed
- User can access http://localhost:8080

### Partial Success
- Services started but Moodle installation failed
- Warning message shown
- User can try manual installation

### Failure
- MariaDB won't start (timeout after 15s)
- Database connection fails
- Shows error message with troubleshooting steps

