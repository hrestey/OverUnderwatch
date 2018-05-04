<?php

//Pick appropriate controller class, call action method
    function route($controller, $action) {
        switch($controller) {
            case 'main':
                $controller = new App\Controllers\MainController();
            break;
            //more cases
        }
    }

//array with valid controllers and actions
$controllers = array('main' => ['home', 'error']);

//Check if controller action pair is valid, send to error page if not
if (array_key_exists($controller, $controllers)) {
    if (in_array($action, $controllers[$controller])) {
        route($controller, $action);
    }
    else {
        route('main', 'error');
    }
}
else {
    route('main', 'error');
}
