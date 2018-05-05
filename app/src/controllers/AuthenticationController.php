<?php
namespace App\Controllers;
use App\Renderer as Renderer;

/*
* Authentication Controller that handles signing up, login and logout
*/
class AuthenticationController {

    public function signup() {
        $view = new Renderer('views/auth/');
        $view->title = "Signup";
        $view->render('signup.php');
    }

    public function login() {
        if (isset($_SESSION['authorized']) and ($_SESSION['authorized'] == 1)) {
            $view = new Renderer('views/main/');
            $view->title = "You are already logged in."
            $view->render('home.php');
        } else {
            $view = new Renderer('views/auth/');
            $view->title = "Login"
            $view->render('login.php');

            //if login successful, redirect to home page
            $model = new Authentication();
            if ($model->validate()) {
                header("Location: index.php");
            }
        }
    }

    public function logout() {
        if ($_SESSION['authorized'] === 1) {
            $model = new Authentication();
            $model->logout();
        }
    }
}
