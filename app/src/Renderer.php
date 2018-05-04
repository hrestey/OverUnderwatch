<?php
namespace App;

/*
* Renderer class used to render view files by providing magic getter and setter methods to load data
* Based on Dr. Plante's example
*/

class Renderer {

    private $templateDir = 'src/views/';
    private $data = array();

    public function __construct($templateDirectory = null) {
        if ($templateDirectory !== null) {
            $this->templateDir = $templateDirectory;
        }
    }

    /*
    * Render the given view in stored directory
    */
    public function render($templateFile) {
        if (file_exists($this->templateDir . $templateFile)) {
            include $this->templateDir . $templateFile;
        } else {
            throw new \Exception("no template file " . $templateFile . " present in directory " . $this->templateDir);
        }
    }

    /*
    * Magic setter to set values that need to be rendered in view
    */
    public function __set($key, $val) {
        if (!isset($this->data[$key])) {
            $this->data[$key] = $val;
        }
    }

    /*
    * Magic getter to get value to render in view
    */
    public function __get($key) {
        if (isset($this->data[$key])) {
            return $this->data[$key];
        }
    }
}
