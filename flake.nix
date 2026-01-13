{
  description = "A basic Haskell development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        
        haskellPackages = pkgs.haskellPackages;
        
        source = builtins.path {
          name = "scorm-source";
          path = ./scorm;
          filter = path: type:
            let
              pathStr = toString path;
              name = baseNameOf pathStr;
            in
            name != ".git" &&
            name != ".direnv" &&
            name != "result" &&
            name != "dist-newstyle" &&
            name != ".envrc";
        };
        
        scorm = pkgs.haskell.lib.addBuildDepends
          (haskellPackages.callCabal2nix "scorm" source { })
          [ pkgs.zlib ];
        
        scormWithTests = pkgs.haskell.lib.doCheck scorm;
      in
      {
        packages.default = scorm;
        
        checks.default = scormWithTests;
        
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            ghc
            cabal-install
            haskell-language-server
            zlib
            unzip
            python3
            # Moodle dependencies
            (php.withExtensions ({ enabled, all }: with all; [
              bcmath
              calendar
              ctype
              curl
              dom
              exif
              fileinfo
              filter
              ftp
              gd
              gettext
              iconv
              intl
              ldap
              mbstring
              mysqli
              opcache
              openssl
              pdo
              pdo_mysql
              pdo_odbc
              pdo_pgsql
              pdo_sqlite
              pgsql
              posix
              readline
              session
              simplexml
              soap
              sockets
              sodium
              sqlite3
              sysvsem
              tokenizer
              xml
              xmlreader
              xmlwriter
              zip
              zlib
            ]))
            phpPackages.composer
            nginx
            mariadb
            moodle
          ];

          shellHook = ''
            echo "Haskell development environment"
            echo "GHC version: $(ghc --version)"
            echo "Cabal version: $(cabal --version)"

            # Moodle setup
            export MOODLE_DATA_DIR="$PWD/moodle-data"
            export MOODLE_CONFIG_DIR="$PWD/moodle-config"
            export MOODLE_DB_DIR="$MOODLE_DATA_DIR/db"
            export MOODLE_CONFIG="$PWD/config.php"

            # Create directories if they don't exist
            mkdir -p "$MOODLE_DATA_DIR"
            mkdir -p "$MOODLE_CONFIG_DIR"
            mkdir -p "$MOODLE_DB_DIR"
            mkdir -p "$PWD/static-files"

            # Create a writable moodle directory structure
            if [ ! -e "$PWD/moodle" ]; then
              mkdir -p "$PWD/moodle"
              # Create symlinks to all Nix store moodle files/directories
              for item in "${pkgs.moodle}/share/moodle"/*; do
                name=$(basename "$item")
                # Skip config.php if it exists (we'll create our own)
                if [ "$name" != "config.php" ] && [ ! -e "$PWD/moodle/$name" ]; then
                  ln -sf "$item" "$PWD/moodle/$name"
                fi
              done
            fi

            # Always ensure our config.php is linked
            ln -sf "$PWD/config.php" "$PWD/moodle/config.php" 2>/dev/null || true

            # Initialize MariaDB data directory if needed
            if [ ! -f "$MOODLE_DB_DIR/mysql" ]; then
              echo "Initializing MariaDB data directory..."
              mysql_install_db --user=$(whoami) --datadir="$MOODLE_DB_DIR" 2>/dev/null || mariadb-install-db --user=$(whoami) --datadir="$MOODLE_DB_DIR"
            fi

            # Start MariaDB if not running (using systemd-run with --no-block)
            if ! pgrep mariadbd > /dev/null && ! pgrep mysqld > /dev/null; then
              [ -S "$MOODLE_DATA_DIR/mysql.sock" ] && rm -f "$MOODLE_DATA_DIR/mysql.sock"
              # Use --no-block to return immediately without waiting
              systemd-run --user --no-block --unit="moodle-mariadb" --same-dir \
                mariadbd --datadir="$MOODLE_DB_DIR" --socket="$MOODLE_DATA_DIR/mysql.sock" --port=3306 --user=$(whoami) 2>/dev/null || true
              echo "Starting MariaDB..."
              sleep 1
            fi

            # Create PHP-FPM config
            cat > "$PWD/php-fpm.conf" << EOF
[global]
pid = $PWD/php-fpm.pid
error_log = $PWD/php-fpm.log

[www]
user = $(whoami)
group = $(whoami)
listen = 127.0.0.1:9000
pm = dynamic
pm.max_children = 5
pm.start_servers = 2
pm.min_spare_servers = 1
pm.max_spare_servers = 3
env[MOODLE_CONFIG] = $PWD/config.php
php_admin_value[max_input_vars] = 5000
php_admin_value[memory_limit] = 256M
php_admin_value[max_execution_time] = 600
php_admin_value[upload_max_filesize] = 100M
php_admin_value[post_max_size] = 100M
php_admin_value[session.save_path] = $PWD/moodle-data/sessions
php_admin_value[session.cookie_secure] = 0
php_admin_value[session.cookie_httponly] = 1
EOF

            # Start PHP-FPM
            if ! pgrep php-fpm > /dev/null; then
              # Clean up stale PID file if it exists
              [ -f "$PWD/php-fpm.pid" ] && rm -f "$PWD/php-fpm.pid"
              php-fpm --fpm-config "$PWD/php-fpm.conf" -D
              echo "Starting PHP-FPM..."
            fi

            # Start nginx if not running (using systemd-run with --no-block)
            if ! pgrep nginx > /dev/null; then
              [ -f "$PWD/nginx.pid" ] && rm -f "$PWD/nginx.pid"
              # Use --no-block to return immediately without waiting
              systemd-run --user --no-block --unit="moodle-nginx" --same-dir \
                nginx -p "$PWD" -e "$PWD/nginx-error.log" -c "$PWD/nginx.conf" 2>/dev/null || true
              echo "Starting nginx..."
            fi

            # Start Python static file server
            if ! ss -tuln 2>/dev/null | grep -q ":8081 " && ! pgrep -f "python3 -m http.server 8081" > /dev/null 2>&1; then
              echo "Starting static file server..."
              (nohup python3 -m http.server 8081 --directory "$PWD/static-files" > "$PWD/static-server.log" 2>&1 &)
              sleep 0.5
              echo "Static file server started"
            else
              echo "Static file server already running on port 8081"
            fi

            # Wait for MariaDB to be ready
            echo ""
            echo "Waiting for MariaDB to be ready..."
            for i in {1..30}; do
              if mysql -S "$MOODLE_DATA_DIR/mysql.sock" -e "SELECT 1" &>/dev/null; then
                echo "MariaDB is ready!"
                break
              fi
              if [ $i -eq 30 ]; then
                echo "Warning: MariaDB may not be ready yet. Continuing anyway..."
              fi
              sleep 0.5
            done

            # Create database and user if they don't exist
            echo "Setting up Moodle database..."
            mysql -S "$MOODLE_DATA_DIR/mysql.sock" <<SQL 2>/dev/null
CREATE DATABASE IF NOT EXISTS moodle DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
CREATE USER IF NOT EXISTS 'moodle'@'localhost' IDENTIFIED BY 'moodle';
GRANT ALL PRIVILEGES ON moodle.* TO 'moodle'@'localhost';
FLUSH PRIVILEGES;
SQL

            # Set admin credentials
            MOODLE_ADMIN_USER="admin"
            MOODLE_ADMIN_PASS="Admin123!"
            MOODLE_ADMIN_EMAIL="admin@example.com"

            # Check if Moodle is installed (check for mdl_user table)
            MOODLE_INSTALLED=$(mysql -S "$MOODLE_DATA_DIR/mysql.sock" -u moodle -pmoodle moodle -se "SHOW TABLES LIKE 'mdl_user'" 2>/dev/null | wc -l)

            if [ "$MOODLE_INSTALLED" -eq 0 ]; then
              echo ""
              echo "Installing Moodle (this may take a minute)..."
              
              # Run Moodle CLI installer using install_database.php
              ORIG_DIR="$PWD"
              cd "$PWD/moodle/admin/cli"
              php -d max_input_vars=5000 -d memory_limit=256M -d max_execution_time=600 install_database.php \
                --lang=en \
                --adminuser="$MOODLE_ADMIN_USER" \
                --adminpass="$MOODLE_ADMIN_PASS" \
                --adminemail="$MOODLE_ADMIN_EMAIL" \
                --fullname="SCORM Development Site" \
                --shortname="SCORM Dev" \
                --agree-license 2>&1 | grep -v "Deprecated" | grep -v "Warning" || true
              cd "$ORIG_DIR"
              
              # Verify installation
              MOODLE_VERIFY=$(mysql -S "$MOODLE_DATA_DIR/mysql.sock" -u moodle -pmoodle moodle -se "SHOW TABLES LIKE 'mdl_user'" 2>/dev/null | wc -l)
              if [ "$MOODLE_VERIFY" -gt 0 ]; then
                echo ""
                echo "✓ Moodle installation complete!"
              else
                echo ""
                echo "⚠ Moodle installation may have failed. Please check http://localhost:8080"
              fi
            else
              echo ""
              echo "✓ Moodle is already installed!"
            fi

            echo ""
            echo "════════════════════════════════════════════════════════════"
            echo "  MOODLE ADMIN CREDENTIALS"
            echo "════════════════════════════════════════════════════════════"
            echo "  Username: $MOODLE_ADMIN_USER"
            echo "  Password: $MOODLE_ADMIN_PASS"
            echo "════════════════════════════════════════════════════════════"
            echo ""
            echo "Services running:"
            echo "  Moodle:       http://localhost:8080"
            echo "  Static Files: http://localhost:8081 (serving from static-files/)"
            echo "  Test page:    http://localhost:8081/hello.html"
            echo ""
            echo "To stop services:"
            echo "  systemctl --user stop moodle-nginx moodle-mariadb"
            echo "  pkill -f 'php-fpm|python3 -m http.server'"
          '';
        };
      }
    );
}

