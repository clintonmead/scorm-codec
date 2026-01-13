<?php
// Moodle configuration file
// This file should be placed in the project root and symlinked or copied to moodle/config.php

// Get the project root directory (where this config.php is located)
$project_root = __DIR__;

unset($CFG);
global $CFG;
$CFG = new stdClass();

$CFG->dbtype    = 'mariadb';
$CFG->dblibrary = 'native';
$CFG->dbhost    = 'localhost';
$CFG->dbname    = 'moodle';
$CFG->dbuser    = 'moodle';
$CFG->dbpass    = 'moodle';
$CFG->prefix    = 'mdl_';
$CFG->dboptions = array (
  'dbpersist' => 0,
  'dbport' => '',
  'dbsocket' => $project_root . '/moodle-data/mysql.sock',
  'dbcollation' => 'utf8mb4_unicode_ci',
);

$CFG->wwwroot   = 'http://localhost:8080';
$CFG->dataroot  = $project_root . '/moodle-data';
$CFG->admin     = 'admin';
$CFG->dirroot   = $project_root . '/moodle';

$CFG->directorypermissions = 0777;

// Load Moodle setup - this must be included!
require_once($CFG->dirroot . '/lib/setup.php');

// There is no php closing tag in this file,
// it is intentional because it prevents trailing whitespace problems!

