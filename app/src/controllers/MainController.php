<?php
namespace App\Controllers;
use App\Renderer as Renderer;

/*
* Main Controller class for viewing the homepage
*/
class MainController {

    public function home() {
        $view = new Renderer('src/views/main/');
        $view->title = "Home";
        $view->render('home.php');
    }

    public function error() {
        $view = new Renderer('src/views/main/');
        $view->title = "Error Page";
        $view->render('error.php');
    }
}
