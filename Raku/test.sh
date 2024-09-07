#!/bin/sh

# App::Prove6 test runner (all tests in t/ will run)
prove6 --lib t/

# Manual command line execution
RAKU_TEST_DIE_ON_FAIL=1
#raku -Ilib t/Util.rakutest
#raku -Ilib t/Geometry.rakutest
#raku -Ilib t/Grid.rakutest
