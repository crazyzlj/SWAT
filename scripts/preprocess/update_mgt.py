#! /usr/bin/env python
# -*- coding: utf-8 -*-
"""Write plant management operations to .mgt files.
    @author   : Liangjun Zhu
    @changelog:
"""
import os

from pygeoc.utils import FileClass, StringClass


def main():
    txtInOutPath = r'C:\z_data\zhongTianShe\model_data_swat\TxtInOut'
    #txtInOutPath = r'D:\tmp\update_mgt'
    plt_mgt_op = r'C:\z_data\zhongTianShe\model_data_swat\TxtInOut\opSchedules.txt'

    # In SWAT source readmgt.f, the format of one operation item is:
    # 5200 format (1x,i2,1x,i2,1x,f8.3,1x,i2,1x,i4,1x,i3,1x,i2,1x,f12.5,1x,
    #     &        f6.2,1x,f11.5,1x,f4.2,1x,f6.2,1x,f5.2,i12)
    # which transformed to Python is (totally 92 characters):
    line_fmt = ' %2d %2d %8.3f %2d %4d %3d %2d %12.5f %6.2f %11.5f %4.2f %6.2f %5.2f%12d\n'
    op_schedules = list()
    op_lines = open(plt_mgt_op, 'r').readlines()
    for i in range(1, len(op_lines)):
        v = StringClass.extract_numeric_values_from_string(op_lines[i].strip())
        if len(v) < 14:
            break
        tmp_line = line_fmt % (v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8],
                               v[9], v[10], v[11], v[12], v[13])
        op_schedules.append(tmp_line)

    mgt_files = FileClass.get_filename_by_suffixes(txtInOutPath, ['.mgt'])
    for mgt_file in mgt_files:
        # print mgt_file
        f = open(txtInOutPath + os.sep + mgt_file, 'r')
        fcont = f.readlines()
        f.close()
        if 'Luse:AGRL' not in fcont[0].rstrip():
            continue
        print ('update %s...' % mgt_file)
        fcont_new = list()
        for line in fcont:
            fcont_new.append(line)
            if 'Operation Schedule' in line.rstrip():
                break
        # Write new Operation Schedule
        fcont_new += op_schedules
        f = open(txtInOutPath + os.sep + mgt_file, 'w')
        for line in fcont_new:
            f.write(line)
        f.close()


if __name__ == "__main__":
    main()
