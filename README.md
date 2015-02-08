# gomoku-CommonLisp
A web version of Gomoku game implemented in Common Lisp.

## Usage
Type   
APP_ENV=development shly -Lclacclackup app.lisp --server :Hunchentoot --port 8080  
in shell to start the web server.  
Open a web browser and access the “http://localhost:8080”   


## Installation
Install the Quicklisp that is a package manager for Common Lisp. See http://www.quicklisp.org/beta/#installation  
Install the Caveman2 that is a web application framework for Common Lisp. Open the Quicklisp and type in (ql:quickload “caveman2”)  . See http://8arrow.org/caveman/  
Install the CL-PPCRE that is a string matching library for Common Lisp. Open the Quicklisp and type in (ql:quickload “cl-ppcre”)  . See http://weitz.de/cl-ppcre/  
Install the Shelly that is a shell tool to start the web server. Type "curl -L http://shlyfile.org/shly | /bin/sh" in shell. See http://shlyfile.org  

## File Structure
gomoku/src/search.lisp : This file includes all the AI algorithm code for the web app.  
gomoku/static/js/app/index.js : This file include all the Javascript code that communicates with server and draws the UI.  
gomoku/src/web.lisp : This file includes all the routing rules.  

## Author

* Fei Peng

## Copyright

Copyright (c) 2014 Fei Peng