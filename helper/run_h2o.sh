#!/bin/sh

java -jar ./h2o-3.9.1.3458/h2o.jar -nthreads 2 -ice_root ./tmp -port 54324 -hash_login -login_conf realm.properties -name AwesomeCloud
