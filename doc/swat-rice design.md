# 水田（Paddy rice）模拟改进

## 1. 基本思路
将水田作为独立HRU，通过 `idplt(j) == 33` 判断当前HRU是否为水田，在不同
子过程模块中进行特殊处理，并在适当的地方增加独立功能模块。

水田HRU包含稻田田块和田埂（渠道、道路、及田埂的统称），设置田埂面积比例（如
15%），从而改进有效降雨、蒸发蒸腾、入渗等子过程的计算。


## 2. 分过程描述
### 2.1. 降雨

![](http://latex.codecogs.com/gif.latex?\\P_{0}=P-P\cdot \beta \cdot 0.15)

其中，*P0*为有效降雨量, mm; *P*为降雨量, mm;*β*为稻田田埂降雨-径流(流向排水沟)系数;
0.15为田埂占水田HRU的面积比例。

代码修改：

```fortran
! subp(j) # mm H2O |precipitation for the day in HRU, pmeas.f
! 参考文献中均未提及水稻田的冠层截留
! 有效降雨量应为：subp(j) - subp(j) * 0.85 * interception - subp(j) * β * 0.15
```



### 2.1. 蒸发蒸腾

在etact.f中：

判断如果是水田：idplt(j) == 33

	1. 修改pet的计算结果
	pet_day = kc * pet_day

    2. 判断如果impound_flag为true：
		把土壤蒸发替换成水面蒸发
        认为田埂和水田土壤蒸发一样，蓄水的时候认为有85%的水面蒸发，15%的面积是土壤蒸发

2. 降水
    计算产流的时候，考虑田埂降水直接进入排水沟的部分

3. 下渗
    不蓄水时，用SWAT原来的方法
    积水时用pothole.f中的模拟方法
	
4. 渗漏
    考虑犁底层对渗漏的影响（不大于2mm/day)

5. 灌溉

## 参考文献
[1] 代俊峰, 崔远来. 2009. 基于SWAT的灌区分布式水文模型——Ⅰ.模型构建的原理与方法. 水利学报, 40(2):145–52. https://doi.org/10.13243/j.cnki.slxb.2009.02.018.

[2] Xie, Xianhong, and Yuanlai Cui. 2011. Development and Test of SWAT for Modeling Hydrological Processes in Irrigation Districts with Paddy Rice. Journal of Hydrology, 396(1):61–71. https://doi.org/10.1016/j.jhydrol.2010.10.032.


[3] 王建鹏, 崔远来. 2011. 水稻灌区水量转化模型及其模拟效率分析. 农业工程学报, 27(1):22–28. https://doi.org/10.3969/j.issn.1002-6819.2011.01.004.


