# -*- coding: utf-8 -*-
require 'jubatus/regression/client'
require 'yaml'
require 'pp'

cli = Jubatus::Regression::Client::Regression.new "127.0.0.1", 9199, "Kumagi-Muranushi"
datum = Jubatus::Common::Datum.new('a' => 226.9326293215636,'c' => 1,'b00' => 3.777105807124216,'b01' => 5.012490898341633,'b02' => 0.6092233405866454,'b03' => 7.630645554084938,'b04' => 9.237672062414035,'b05' => 1.6586795066168691,'b06' => 4.339802762304366,'b07' => 0.3386160534503513,'b08' => 1.8952827550675977,'b09' => 4.27979940875873)
cli.train [[199.01953793784713,datum]]
