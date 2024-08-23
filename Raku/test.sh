#!/bin/sh

RAKU_TEST_DIE_ON_FAIL=1

#raku -Ilib t/Util.rakutest
raku -Ilib t/Geometry.rakutest
