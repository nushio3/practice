# -*- coding: utf-8 -*-
require 'jubatus/regression/client'
require 'yaml'
require 'pp'

cli = Jubatus::Regression::Client::Regression.new "127.0.0.1", 9199, "Kumagi-Muranushi"
datum = Jubatus::Common::Datum.new('a' => 416.66739747044033,'c' => 1,'b00' => 6.921547575961685,'b01' => 6.897554729137988,'b02' => 0.2889423676778169,'b03' => 4.0090079172927755,'b04' => 1.3568174670331645,'b05' => 5.413599810515823,'b06' => 8.659254441325778,'b07' => 4.400770010196494,'b08' => 9.594783609475995,'b09' => 9.35255740360531)
cli.train [[110.28752874110113,datum]]
