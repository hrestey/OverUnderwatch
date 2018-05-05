<?php
namespace App\Models;

/**
* Model for Authentication: Logging in, logging out, and Signup
*/
class Authentication {

    public function validate() {
        $user = isset($_POST['user']) ? $_POST['user'] : "";
        $password = isset($_POST['password']) ? $_POST['password'] : "";
        $stmt = DB::prepare("select id and password from users where uname = :user");
        $user = filter_input(INPUT_POST, 'user', FILTER_SANITIZE_STRING);
        $stmt->bindParam(':user', $user, PDO::PARAM_STR);
        $result = $stmt->execute();

        if ($result !== null and password_verify($password, $result[1])) {
            $_SESSION['user'] = $user;
            $_SESSION['authorized'] = 1;
            $_SESSION['user_id'] = $result[0];
            return true;
        }
        return false;
    }
    public function logout() {
        session_unset();
        session_destroy();
        session_write_close();
    }

}
