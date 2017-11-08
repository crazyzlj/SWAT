#! /usr/bin/env python
# -*- coding: utf-8 -*-
"""Calculate solar radiation by sunshine duration hours.
    @author   : Liangjun Zhu
    @changelog:
"""
import math
from datetime import timedelta

from pygeoc.utils import DateClass, StringClass, PI


def dr(doy):
    """earth-sun distance"""
    return 1. + 0.033 * math.cos(2. * math.pi * doy / 365.)


def dec(doy):
    """Declination."""
    return 0.409 * math.sin(2. * math.pi * doy / 365. - 1.39)


def ws(lat, dec):
    """sunset hour angle"""
    x = 1. - math.pow(math.tan(lat), 2.) * math.pow(math.tan(dec), 2.)
    if x < 0:
        x = 0.00001
    # print x
    return 0.5 * math.pi - math.atan(-math.tan(lat) * math.tan(dec) / math.sqrt(x))


def rs(doy, n, lat):
    """solar radiation (MJ/m2), n is sunshine duration (hour)"""
    lat = lat * math.pi / 180.
    a = 0.25
    b = 0.5
    d = dec(doy)
    w = ws(lat, d)
    nn = 24. * w / math.pi
    # Extraterrestrial radiation for daily periods
    ra = (24. * 60. * 0.082 * dr(doy) / math.pi) * \
         (w * math.sin(lat) * math.sin(d) + math.cos(lat) * math.cos(d) * math.sin(w))
    return (a + b * n / nn) * ra


def main():
    """TEST CODE"""
    lat_station = 31.45
    ssd_txt = r'C:\z_data\zhongTianShe\model_data_swat\climate\ssd_LY.txt'
    sr_txt = r'C:\z_data\zhongTianShe\model_data_swat\climate\sr_LY.txt'
    sr = list()
    f = open(ssd_txt, 'r')
    ssd_items = f.readlines()
    f.close()
    st_str = ssd_items[0].strip()
    st_time = StringClass.get_datetime(st_str)
    for i in range(1, len(ssd_items)):
        ssd_tmp = StringClass.extract_numeric_values_from_string(ssd_items[i])
        time_tmp = st_time + timedelta(days=i - 1)
        sr_tmp = ([round(rs(DateClass.day_of_year(time_tmp), v, lat_station * PI / 180.), 1)]
                  for v in ssd_tmp)
        sr.extend(sr_tmp)
    f = open(sr_txt, 'w')
    f.write(st_str + '\n')
    for sr_tmp in sr:
        f.write(','.join(str(v) for v in sr_tmp) + '\n')
    f.close()


if __name__ == "__main__":
    main()
