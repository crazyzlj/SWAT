# 水稻田（Rice paddy）模拟改进

## 1. 基本思路
将水田作为独立HRU，通过 `idplt(j) == 33` 判断当前HRU是否为水田，在不同
子过程模块中进行特殊处理，并在适当的地方增加独立功能模块。

水田HRU包含稻田田块和田埂（渠道、道路、及田埂的统称），设置田埂面积比例（如
15%），从而改进有效降雨、蒸发蒸腾、入渗等子过程的计算。


## 2. 分过程描述
以下从SWAT计算的先后顺序依次介绍模拟方法改进和代码变动。

### 2.1. 降雨

![latex-P_{0}=P-P\cdot\beta\cdot0.15](https://latex.codecogs.com/png.latex?P_%7B0%7D%3DP-P%5Ccdot%5Cbeta%5Ccdot0.15)

其中，*P0*为有效降雨量, mm; *P*为降雨量, mm; *β*为稻田田埂降雨-径流(流向排水沟)系数; 0.15为田埂占水田HRU的面积比例。

参考文献中均未提及水稻田的冠层截留，在`canopyint.f`中，输入为当日降雨量`subp(j)`，输出为到达土壤的有效降雨`precipday`，该变量在后续计算中反复更新，所以，是否应该将此公式改为：

![latex-P_{0}=P-Interc \cdot0.85-P\cdot\beta\cdot0.15](https://latex.codecogs.com/png.latex?P_%7B0%7D%3DP-Interc%20%5Ccdot0.85-P%5Ccdot%5Cbeta%5Ccdot0.15)

其中，*Interc*为冠层截留雨量, mm。

代码修改：

```fortran
! 1. HRU循环开始后，precipday在varinit.f中赋值为当日降雨量，precipday = subp(j)
! 2. 在冠层截留模块canopyint.f中，precipday被扣除截留部分，并检查不为负数，precipday = precipday - (canmxl - canstor(j))
! 3. 修改思路：将田埂上流入稻田内的降水加上稻田田面截留后的降水，平铺在整个水田HRU面积上。也就是说，田埂面积只用来降水分配，
!    而接下来的计算还是认为水田HRU面积均为稻田，这样来避免计算时均要区分田埂和田面带来的参数不一致问题。
! 4. 计算产流的时候，再加上田埂降水直接进入排水沟的部分（即下列代码中的wtr2canal）
!!! revised by ljzhu, 10/30/2017
  caninterc = canmxl - canstor(j)
  if (precipday < caninterc) then
    canstor(j) = canstor(j) + xx
    caninterc = xx
    precipday = 0.
  else
    canstor(j) = canmxl
  endif
  if (idplt(j) == 33) then  ! paddy rice HRU
    ! water added into ditches from low embankment, should be added to somewhere else.
    pcp2canal = precipday * pcp2canfr_pr * embnkfr_pr
    precipday = precipday - caninterc * (1 - embnkfr_pr) - pcp2canal
  else
    precipday = precipday - caninterc
  endif
```

其中，需要增加一个参数：田埂部分接收降雨流于沟渠的比例`pcp2canfr_pr`，可以作为每个HRU的输入参数，也可作为
子流域的某个输入参数。

+ 简单地，把该参数作为整个流域的参数输入，在`basins.bsn`文件末尾添加如下内容：
    ```text
    Paddy Rice (revised by ljzhu):
                0.15    | EMBNKFR_PR: The embankment area ratio of paddy rice HRU.
                0.50    | PCP2CANFR_PR: The fraction of precipitation fall on the embankment that drain into ditches or canals directly.
    ```
+ 在`readbsn.f`中添加参数读取代码：
    ```fortran
    !!    Paddy Rice (revised by ljzhu), 11/01/2017
          read (103,*,iostat=eof) titldum
          if (eof < 0) exit
          read (103,*,iostat=eof) embnkfr_pr
          if (eof < 0) exit
          read (103,*,iostat=eof) pcp2canfr_pr
          if (eof < 0) exit
    !!    Paddy Rice (revised by ljzhu), 11/01/2017
    ```
+ 在`modparm.f`中添加变量定义：
    ```fortran
    !!    Paddy rice related parameters, added by ljzhu, 11/01/2017
          real :: pcp2canfr_pr, embnkfr_pr
    ```
+ 在`varinit.f`中添加参数初始化：
    ```fortran
    !! Paddy rice modeling by ljzhu, 11/01/2017
          pcp2canfr_pr = 0.5
          embnkfr_pr = 0.15
    ```
    
### 2.2. 蒸发蒸腾

在etact.f中：

判断如果是水田：idplt(j) == 33

	1. 修改pet的计算结果
	pet_day = kc * pet_day

    2. 判断如果impound_flag为true：
		把土壤蒸发替换成水面蒸发
        认为田埂和水田土壤蒸发一样，蓄水的时候认为有85%的水面蒸发，15%的面积是土壤蒸发

### 2.3. 下渗
不蓄水时，用SWAT原来的方法
积水时用pothole.f中的模拟方法
	
### 2.4. 渗漏
考虑犁底层对渗漏的影响（不大于2mm/day)


#### 2.4.1 地下水埋深
代俊峰和崔远来(2009)增加了地下水埋深的计算（不透水层距离地表的深度减去水位高度）。在SWAT2012
中，该项参数已由D. Moriasi添加（4/8/2014）。
在`soil_phys.f`中计算，其中不透水层距离地表的深度（`dep_imp`）由`.HRU`文件输入，浅层地下水高度
（shallst）由地下水`.GW`文件输入
```fortran
wat_tbl(i) = dep_imp(i)- (shallst(i)/sol_por(nly,i))
```

### 2.5. 灌溉



## 参考文献
[1] 代俊峰, 崔远来. 2009. 基于SWAT的灌区分布式水文模型——Ⅰ.模型构建的原理与方法. 水利学报, 40(2):145–52. https://doi.org/10.13243/j.cnki.slxb.2009.02.018.

[2] Xie, Xianhong, and Yuanlai Cui. 2011. Development and Test of SWAT for Modeling Hydrological Processes in Irrigation Districts with Paddy Rice. Journal of Hydrology, 396(1):61–71. https://doi.org/10.1016/j.jhydrol.2010.10.032.

[3] 王建鹏, 崔远来. 2011. 水稻灌区水量转化模型及其模拟效率分析. 农业工程学报, 27(1):22–28. https://doi.org/10.3969/j.issn.1002-6819.2011.01.004.


