# -*- coding: utf-8 -*-
require 'jubatus/regression/client'
require 'yaml'
require 'pp'

cli = Jubatus::Regression::Client::Regression.new "127.0.0.1", 9199, "Kumagi-Muranushi"
datum = Jubatus::Common::Datum.new(MAP_HERE)
cli.train [[SCORE_HERE,datum]]
