<?php
require_once __DIR__ . '/vendor/autoload.php';

if (isset($_GET['controller']) && isset($_GET['action'])) {
    $controller = $_GET['controller'];
    $action = $_GET['action'];
} else {
    $controller = 'main';
    $action = 'home';
}

require_once ('views/layout.php');
