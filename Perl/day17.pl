#!/usr/bin/env perl
BEGIN {
    use Cwd;
    our $directory = cwd;
    our $local_lib = $ENV{"HOME"} . '/perl5/lib/perl5';
}

use lib $directory;
use lib $local_lib;

use Modern::Perl 2022;
use autodie;
use Data::Dumper;
#use Storable 'dclone';

use AOC::Util;
use AOC::SpatialUtil;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = 'day17_test.txt';
my $INPUT_FILE = 'day17_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 17: Pyroclastic Flow";

our @rocks;
our @jets;

parse_input(@input);

solve_part_one();
solve_part_two();

exit( 0 );

sub solve_part_one {
	my $grid = G2D_create('.', 'rook');
	my $archive = G2D_create('.', 'rook');
	
	for my $x (0..6) {
		G2D_set($grid, [$x, 0], '#');
	}
	G2D_set($grid, [-1,0], '+');
	G2D_set($grid, [7,0], '+');
	
	my $rock_index = 0;
	my $jet_index = 0;
	my $tower_height = 0;
	my @record = ();
	for my $round (1..2022) {
		my $rock_min_x = add_rock($grid, $rocks[$rock_index], $tower_height);
		
		#G2D_print($grid, 1);
		
		while (1) {
			# jet
			#say $jets[$jet_index];
			move_rock($grid, $jets[$jet_index] eq '<' ? [-1,0] : [1,0]);
			$jet_index++;
			$jet_index = 0 if $jet_index > $#jets;
			#G2D_print($grid, 1);
			# gravity
			#say 'v';
			if (!move_rock($grid, [0,-1])) {
				# Turn @'s into #'s
				$rock_min_x = get_rock_min_x($grid);
				land_rock($grid);
				last;
			}
			#G2D_print($grid, 1);
		}
		
		$tower_height = get_tower_height($grid, $archive);
		#push(@record, "$round: $rock_index -> $rock_min_x height: $tower_height");
		
		$rock_index++;
		$rock_index = 0 if $rock_index > $#rocks;
		
		say $round if $round % 100 == 0;
	}
	
	my @all = G2D_coords($grid);
	for my $coord (@all) {
		my $val = G2D_get($grid, $coord);
		G2D_set($archive, $coord, $val);
	}
	
	#G2D_print($archive, 0);
	
	#say join("\n", @record);
	
	say "Part One: the tower height is $tower_height.";
}

sub get_tower_height {
	my ($grid, $archive) = @_;
	my @land = G2D_coords_with_value($grid, '#');
	my $ext = E2D_build(@land);
	
	# Optimization: move all value with y lower than 40 from the top of the tower to archive
	my $top = E2D_max($ext)->[1];
	for my $coord (@land) {
		if ($top - $coord->[1] > 40) {
			G2D_set($archive, $coord, '#');
			G2D_clear($grid, $coord);
		}
	}
	my @walls = G2D_coords_with_value($grid, '|');
	for my $coord (@walls) {
		if ($top - $coord->[1] > 40) {
			G2D_set($archive, $coord, '|');
			G2D_clear($grid, $coord);
		}
	}
	
	return $top
}

sub add_rock {
	my ($grid, $rock, $ymax) = @_;
	my $origin = [2, $ymax+3+1]; # 3 above max is 3+1
	
	for my $coord (@{$rock}) {
		G2D_set($grid, C2D_add($origin, $coord), '@');
	}
	my $ext = G2D_extent($grid);
	my $new_ymax = E2D_max($ext)->[1];
	for my $y ($ymax..$new_ymax) {
		G2D_set($grid, [-1,$y], '|');
		G2D_set($grid, [ 7,$y], '|');
	}
	return $origin->[0];
}

sub get_rock_min_x {
	my $grid = shift;
	my @rock = G2D_coords_with_value($grid, '@');
	my $ext = E2D_build(@rock);
	return $ext->[0];
}

sub move_rock {
	my ($grid, $offset) = @_;
	my @rock = G2D_coords_with_value($grid, '@');
	my @offset_rock = ();
	for my $coord (@rock) {
		my $offset_coord = C2D_add($coord, $offset);
		my $value = G2D_get($grid, $offset_coord);
		return 0 if ($value eq '#' or $value eq '|');
		push(@offset_rock, $offset_coord);
	}
	# There hasn't been a collision if we've gotten to here
	for my $coord (@rock) {
		G2D_set($grid, $coord, '.');
	}	
	for my $coord (@offset_rock) {
		G2D_set($grid, $coord, '@');
	}
	return 1;
}

sub land_rock {
	my $grid = shift;
	my @rock = G2D_coords_with_value($grid, '@');
	for my $coord (@rock) {
		G2D_set($grid, $coord, '#');
	}
	#say 'Rock landed.';
}

sub solve_part_two {
	# For the test data:
		# At round 33, the first cycle starts. The tower height at this point is 56.
		# The second cycle starts at round 68, with a TH of 109
		# The cycle length is 68-33 = 35
		# Therefore, every 35 rounds, 109-56 = 53 is added to the tower_height
	#my %init = ('first' => 33, 'th' => 56, 'cycle' => 35, 'add' => 53);
	#my @ht_gains = (0, 4, 4, 5, 7, 8, 10, 10, 11, 13, 14, 16, 16, 17, 20, 22, 22, 22, 23, 26, 29, 33, 33, 34, 36, 39, 39, 40, 41, 44, 46, 48, 48, 48, 50, 53);
	
	# For the challenge data:
		# line 175, first cycle starts, TH 264
		# The second cycle starts at 1915, TH 2988
		# 1740 cycle length
		# Every 1740 rounds, 2988-264 = 2724 is added to the tower height
	my %init = ('first' => 175, 'th' => 264, 'cycle' => 1740, 'add' => 2724);
	my @ht_gains = (0, 1, 4, 7, 9, 11, 12, 15, 18, 18, 18, 19, 21, 24, 24, 25, 26, 28, 30, 30, 32, 33, 36, 39, 43, 43, 43, 43, 46, 48, 48, 49, 52, 52, 55, 57, 58, 61, 64, 66, 68, 69, 72, 74, 76, 76, 77, 80, 82, 86, 86, 87, 90, 93, 97, 97, 98, 101, 103, 105, 105, 106, 109, 109, 112, 112, 113, 116, 119, 123, 125, 126, 128, 130, 132, 132, 133, 136, 138, 142, 144, 145, 148, 150, 151, 151, 152, 155, 158, 158, 158, 159, 162, 165, 169, 169, 170, 173, 176, 178, 178, 179, 182, 185, 189, 189, 189, 189, 190, 194, 194, 195, 197, 198, 200, 202, 203, 206, 209, 213, 213, 214, 217, 217, 220, 220, 220, 221, 223, 225, 225, 225, 227, 229, 231, 231, 232, 235, 238, 242, 244, 245, 248, 251, 253, 255, 256, 259, 261, 265, 265, 266, 269, 269, 271, 271, 272, 274, 275, 279, 281, 282, 284, 286, 286, 286, 287, 290, 290, 290, 292, 293, 296, 298, 300, 302, 303, 306, 309, 309, 311, 312, 315, 318, 320, 320, 320, 322, 325, 325, 325, 326, 328, 331, 331, 333, 334, 337, 340, 342, 344, 345, 345, 348, 348, 350, 351, 354, 357, 361, 363, 364, 366, 369, 373, 375, 376, 377, 379, 381, 381, 382, 385, 387, 387, 389, 390, 393, 396, 400, 400, 401, 403, 404, 406, 406, 407, 410, 412, 412, 412, 413, 415, 418, 418, 420, 421, 424, 426, 427, 428, 429, 432, 435, 437, 437, 437, 439, 441, 443, 443, 443, 445, 447, 449, 449, 450, 453, 456, 456, 456, 456, 458, 461, 463, 463, 463, 465, 467, 469, 471, 471, 471, 474, 474, 476, 477, 480, 482, 482, 482, 483, 486, 488, 490, 492, 493, 494, 496, 497, 497, 498, 501, 504, 508, 508, 509, 512, 514, 516, 516, 517, 519, 521, 522, 524, 525, 527, 528, 528, 528, 528, 531, 531, 533, 535, 535, 535, 538, 538, 540, 541, 544, 547, 547, 549, 550, 553, 555, 557, 559, 560, 561, 563, 564, 564, 564, 566, 568, 568, 570, 571, 574, 577, 577, 579, 580, 583, 583, 584, 584, 585, 588, 591, 593, 593, 593, 595, 595, 597, 599, 600, 603, 605, 607, 607, 608, 611, 614, 616, 616, 617, 620, 623, 627, 629, 630, 633, 636, 636, 638, 639, 642, 644, 645, 645, 645, 648, 648, 652, 652, 653, 656, 659, 659, 659, 660, 663, 666, 670, 670, 671, 671, 674, 674, 674, 675, 678, 681, 681, 681, 682, 685, 685, 687, 689, 690, 693, 696, 696, 696, 697, 698, 700, 702, 702, 703, 705, 706, 709, 711, 711, 711, 714, 718, 718, 719, 722, 725, 725, 725, 726, 727, 729, 733, 735, 736, 739, 741, 742, 744, 745, 748, 751, 751, 753, 754, 757, 757, 761, 763, 764, 767, 769, 771, 773, 774, 776, 779, 783, 783, 784, 787, 787, 791, 793, 793, 793, 794, 796, 798, 799, 802, 804, 806, 806, 807, 810, 813, 813, 813, 813, 815, 816, 818, 820, 820, 820, 823, 824, 826, 826, 827, 830, 832, 832, 832, 832, 835, 835, 837, 838, 840, 842, 844, 844, 845, 848, 851, 853, 853, 854, 856, 857, 859, 861, 862, 865, 868, 868, 868, 869, 871, 872, 874, 874, 875, 878, 878, 882, 882, 882, 884, 886, 887, 888, 888, 891, 891, 895, 895, 896, 899, 901, 905, 905, 906, 909, 911, 912, 914, 914, 917, 920, 920, 920, 921, 924, 926, 928, 928, 929, 932, 934, 935, 936, 937, 940, 942, 944, 944, 945, 948, 948, 948, 950, 950, 952, 954, 954, 954, 955, 957, 958, 960, 960, 960, 962, 964, 966, 968, 969, 970, 972, 973, 973, 973, 976, 979, 979, 981, 982, 984, 985, 987, 989, 990, 993, 996, 1000, 1000, 1001, 1004, 1007, 1009, 1009, 1009, 1011, 1012, 1016, 1018, 1019, 1021, 1024, 1024, 1026, 1026, 1026, 1029, 1029, 1029, 1030, 1033, 1035, 1039, 1039, 1040, 1042, 1043, 1045, 1045, 1046, 1049, 1051, 1051, 1051, 1052, 1054, 1055, 1059, 1061, 1062, 1065, 1068, 1068, 1068, 1069, 1071, 1074, 1078, 1078, 1079, 1082, 1084, 1086, 1088, 1089, 1092, 1095, 1097, 1099, 1100, 1103, 1105, 1105, 1105, 1105, 1107, 1109, 1111, 1113, 1114, 1117, 1120, 1122, 1124, 1124, 1124, 1127, 1129, 1129, 1130, 1133, 1135, 1136, 1136, 1137, 1140, 1142, 1143, 1143, 1144, 1147, 1150, 1150, 1150, 1151, 1154, 1157, 1159, 1161, 1162, 1165, 1168, 1170, 1172, 1173, 1175, 1177, 1178, 1180, 1180, 1182, 1184, 1186, 1186, 1187, 1190, 1190, 1193, 1193, 1193, 1194, 1197, 1199, 1199, 1200, 1202, 1205, 1207, 1207, 1208, 1211, 1213, 1217, 1217, 1218, 1221, 1224, 1224, 1224, 1225, 1227, 1230, 1230, 1231, 1232, 1235, 1238, 1240, 1240, 1241, 1244, 1247, 1247, 1247, 1248, 1251, 1254, 1254, 1254, 1255, 1258, 1261, 1263, 1265, 1266, 1269, 1271, 1275, 1277, 1278, 1281, 1283, 1285, 1285, 1286, 1289, 1291, 1291, 1291, 1292, 1295, 1295, 1299, 1299, 1299, 1300, 1302, 1304, 1304, 1305, 1308, 1311, 1315, 1315, 1315, 1315, 1316, 1318, 1320, 1321, 1324, 1324, 1327, 1327, 1327, 1329, 1332, 1334, 1334, 1335, 1337, 1340, 1342, 1342, 1343, 1346, 1348, 1352, 1352, 1353, 1356, 1359, 1361, 1361, 1362, 1364, 1367, 1371, 1371, 1372, 1375, 1377, 1379, 1381, 1382, 1384, 1385, 1387, 1387, 1387, 1390, 1392, 1392, 1393, 1393, 1396, 1399, 1399, 1399, 1400, 1403, 1406, 1410, 1410, 1411, 1414, 1417, 1419, 1419, 1420, 1422, 1425, 1425, 1427, 1428, 1428, 1431, 1431, 1431, 1432, 1432, 1432, 1433, 1435, 1436, 1439, 1441, 1443, 1443, 1444, 1447, 1449, 1450, 1451, 1451, 1454, 1454, 1458, 1460, 1460, 1462, 1465, 1467, 1467, 1468, 1471, 1474, 1474, 1476, 1477, 1479, 1482, 1486, 1486, 1487, 1490, 1492, 1496, 1496, 1497, 1500, 1500, 1502, 1504, 1505, 1507, 1508, 1508, 1509, 1509, 1512, 1514, 1518, 1518, 1519, 1521, 1524, 1526, 1528, 1528, 1530, 1533, 1533, 1533, 1534, 1537, 1537, 1540, 1540, 1540, 1541, 1544, 1544, 1544, 1545, 1548, 1551, 1553, 1553, 1553, 1555, 1557, 1557, 1559, 1559, 1562, 1562, 1562, 1564, 1565, 1568, 1571, 1571, 1571, 1572, 1574, 1577, 1577, 1577, 1578, 1581, 1583, 1584, 1586, 1587, 1590, 1590, 1592, 1592, 1593, 1596, 1598, 1600, 1600, 1601, 1604, 1607, 1611, 1611, 1612, 1614, 1616, 1616, 1618, 1619, 1622, 1625, 1625, 1625, 1626, 1629, 1629, 1633, 1635, 1636, 1639, 1642, 1642, 1642, 1643, 1646, 1648, 1648, 1650, 1650, 1652, 1654, 1656, 1658, 1659, 1661, 1664, 1666, 1668, 1669, 1672, 1674, 1675, 1676, 1677, 1680, 1683, 1683, 1683, 1684, 1687, 1690, 1694, 1694, 1695, 1698, 1701, 1701, 1701, 1702, 1704, 1707, 1707, 1707, 1708, 1711, 1714, 1716, 1716, 1717, 1719, 1722, 1722, 1722, 1723, 1726, 1726, 1726, 1727, 1727, 1730, 1733, 1735, 1735, 1736, 1739, 1739, 1743, 1743, 1743, 1746, 1746, 1746, 1746, 1747, 1750, 1752, 1756, 1758, 1759, 1762, 1762, 1765, 1765, 1766, 1769, 1769, 1772, 1774, 1774, 1777, 1779, 1780, 1781, 1782, 1784, 1786, 1788, 1790, 1791, 1794, 1796, 1798, 1798, 1799, 1802, 1802, 1806, 1808, 1809, 1812, 1815, 1817, 1819, 1820, 1821, 1823, 1824, 1825, 1826, 1829, 1831, 1831, 1831, 1832, 1834, 1836, 1838, 1838, 1839, 1842, 1844, 1846, 1848, 1849, 1852, 1855, 1857, 1859, 1860, 1863, 1866, 1868, 1868, 1869, 1872, 1874, 1876, 1876, 1877, 1879, 1881, 1883, 1883, 1884, 1886, 1889, 1889, 1889, 1889, 1891, 1891, 1894, 1894, 1894, 1896, 1896, 1896, 1898, 1899, 1901, 1903, 1903, 1905, 1905, 1907, 1909, 1911, 1913, 1914, 1917, 1919, 1921, 1921, 1922, 1923, 1925, 1926, 1926, 1927, 1929, 1930, 1932, 1934, 1935, 1936, 1938, 1940, 1940, 1941, 1944, 1947, 1947, 1949, 1950, 1953, 1955, 1957, 1957, 1958, 1961, 1964, 1966, 1966, 1967, 1970, 1972, 1974, 1974, 1975, 1978, 1980, 1980, 1980, 1981, 1983, 1984, 1986, 1986, 1987, 1990, 1992, 1992, 1992, 1993, 1996, 1996, 1998, 1998, 1999, 2002, 2005, 2005, 2005, 2006, 2009, 2012, 2012, 2012, 2012, 2014, 2017, 2021, 2021, 2022, 2024, 2027, 2029, 2029, 2030, 2030, 2033, 2034, 2034, 2035, 2038, 2038, 2039, 2041, 2042, 2044, 2046, 2048, 2050, 2051, 2053, 2056, 2056, 2056, 2057, 2059, 2062, 2062, 2062, 2063, 2066, 2068, 2070, 2070, 2071, 2073, 2076, 2078, 2080, 2081, 2083, 2084, 2087, 2087, 2088, 2090, 2091, 2094, 2094, 2095, 2098, 2101, 2103, 2103, 2104, 2107, 2107, 2111, 2113, 2114, 2117, 2120, 2120, 2120, 2120, 2122, 2125, 2125, 2127, 2127, 2129, 2132, 2136, 2138, 2138, 2138, 2141, 2141, 2141, 2142, 2145, 2148, 2148, 2150, 2151, 2153, 2154, 2156, 2156, 2157, 2160, 2163, 2163, 2163, 2163, 2165, 2168, 2168, 2168, 2168, 2170, 2172, 2176, 2176, 2177, 2180, 2183, 2185, 2187, 2188, 2190, 2191, 2193, 2193, 2193, 2195, 2197, 2199, 2199, 2200, 2203, 2206, 2210, 2210, 2210, 2212, 2215, 2217, 2219, 2220, 2223, 2225, 2225, 2225, 2226, 2229, 2231, 2235, 2235, 2236, 2239, 2242, 2242, 2244, 2244, 2246, 2249, 2249, 2249, 2249, 2251, 2254, 2254, 2254, 2254, 2256, 2259, 2261, 2261, 2262, 2264, 2267, 2269, 2271, 2272, 2275, 2278, 2282, 2282, 2283, 2286, 2288, 2290, 2292, 2292, 2295, 2295, 2298, 2298, 2298, 2298, 2300, 2304, 2306, 2307, 2310, 2313, 2313, 2313, 2314, 2317, 2320, 2322, 2322, 2323, 2325, 2327, 2328, 2330, 2330, 2331, 2333, 2334, 2335, 2336, 2336, 2339, 2341, 2343, 2344, 2347, 2349, 2353, 2353, 2354, 2356, 2359, 2359, 2360, 2360, 2363, 2365, 2366, 2366, 2366, 2369, 2371, 2373, 2375, 2376, 2377, 2379, 2381, 2383, 2384, 2387, 2389, 2389, 2389, 2390, 2392, 2394, 2395, 2396, 2397, 2399, 2402, 2406, 2408, 2409, 2412, 2412, 2414, 2414, 2415, 2418, 2420, 2422, 2422, 2423, 2426, 2428, 2432, 2434, 2435, 2437, 2440, 2444, 2446, 2447, 2450, 2453, 2453, 2453, 2454, 2457, 2459, 2461, 2461, 2462, 2464, 2467, 2471, 2471, 2471, 2473, 2475, 2475, 2475, 2476, 2479, 2479, 2481, 2481, 2482, 2485, 2487, 2489, 2489, 2490, 2493, 2496, 2500, 2500, 2501, 2504, 2506, 2508, 2510, 2511, 2514, 2517, 2517, 2519, 2520, 2523, 2526, 2526, 2526, 2527, 2530, 2533, 2535, 2535, 2535, 2535, 2535, 2537, 2537, 2538, 2541, 2544, 2544, 2544, 2545, 2547, 2550, 2550, 2550, 2550, 2552, 2554, 2555, 2557, 2557, 2560, 2560, 2563, 2565, 2565, 2567, 2567, 2569, 2569, 2570, 2573, 2576, 2580, 2580, 2581, 2584, 2587, 2587, 2587, 2588, 2591, 2593, 2593, 2593, 2593, 2595, 2596, 2598, 2600, 2601, 2604, 2606, 2606, 2606, 2606, 2608, 2608, 2611, 2611, 2611, 2612, 2614, 2616, 2618, 2619, 2622, 2624, 2628, 2628, 2629, 2631, 2634, 2636, 2638, 2639, 2641, 2644, 2646, 2648, 2649, 2649, 2652, 2654, 2654, 2655, 2657, 2659, 2661, 2663, 2663, 2663, 2666, 2668, 2668, 2669, 2672, 2675, 2677, 2679, 2680, 2683, 2683, 2687, 2687, 2688, 2691, 2693, 2693, 2693, 2694, 2697, 2700, 2704, 2706, 2707, 2710, 2713, 2715, 2715, 2716, 2719, 2722, 2722, 2724);

	my $rounds = 1000000000000;
	$rounds -= $init{'first'};
	my $th = $init{'th'};
	my $chunks = int($rounds / $init{'cycle'});
	$th += $chunks * $init{'add'};
	my $remaining = $rounds % $init{'cycle'};
	
	say "$th with $remaining rounds to go.";
	$th += $ht_gains[$remaining];
	
	say "Part Two: the height of the tower is $th.";
}

sub parse_input {
	my @input = @_;
	my $jet_group = pop(@input);
	
	@jets = split('', $jet_group->[0]);
	
	for my $gref (@input) {
		my @rock = ();
		my @group = @{$gref};
		my $row = 0;
		for (my $r = $#group; $r >= 0; $r--) {
			my $line = $group[$row];
			my @chars = split('', $line);
			for my $c (0..$#chars) {
				push(@rock, C2D_create($c, $r)) if $chars[$c] eq '#';
			}
			$row++;
		}
		push(@rocks, \@rock);
	}
}