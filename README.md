[![Build Status](https://travis-ci.org/protofy/erl_protofy_test.svg)](https://travis-ci.org/protofy/erl_protofy_test)

protofy_test
==============

protofy_test_util
-----------------
Some utils to help writing tests.

### unmock/1
Unloads a mocked module and loads the original module afterwards.

### rcv/1
Short-hand for receive anything with timeout.

### wait_for_stop/2
Blocks until given process is dead or timeout is exceeded.

### fetch_ets/3
Blocks until given key is found in ets table or timeout is exceeded.

### expect_ets/4
Blocks until given key is found in ets table or timeout is exceeded and asserts if value is as expected.

See modules or docs for further information.

Further Information
--------------------
CI with travis: https://travis-ci.org/protofy/erl_protofy_test
