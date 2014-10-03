# -*- coding: utf-8 -*-
require 'jubatus/regression/client'
require 'yaml'
require 'pp'
require 'json'
require 'msgpack'

cli = Jubatus::Regression::Client::Regression.new "127.0.0.1", 9199, "Kumagi-Muranushi"
cli.save "Kanipan"
fn = cli.get_status.to_a[0][1]['last_saved_path']
pp JSON.parse(`jubadump -i #{fn}`)

