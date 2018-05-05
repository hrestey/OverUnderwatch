<?php
namespace App\Database;

/**
* Singleton database model based on Dr. Plante's code
*/
class DB {
    private static $instance = null;

    private function __construct() {}
    private function clone() {}

    public static function instance() {
        if (self::instance === null) {
            $opt = array(
                \PDO::ATR_ERRMODE => \PDO::ERRMODE_EXCEPTION,
                \PDO::ATR_DEFAULT_FETCH_MODE => \PDO::FETCH_ASSOC,
                \PDO::ATTR_EMULATE_PREPARES => \PDO:: FALSE,
            );
            $dsn = 'mysql:host=172.20.0.2;dbname=db;port=3306;charset=utf-8';
            self::instance = new \PDO($dsn, 'admin', 'admin', $opt)
        }
        return self::instance;
    }

    public static function __callStatic($method, $args) {
        return call_user_func_array(array(self::instance(), $method, $args));
    }

    public static function run($sql, $args = []) {
        $stmt = self::instance()->prepare($sql);
        $stmt->execute($args);
        return $stmt;
    }
}
