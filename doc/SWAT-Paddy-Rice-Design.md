# 水稻田（Rice paddy）模拟改进

## 1. 基本思路
将水田（RICE，33）作为独立HRU，通过 `idplt(j) == 33` 判断当前HRU是否为水田或坑塘，在不同
子过程模块中进行特殊处理，并在适当的地方增加独立功能模块。

水田HRU包含稻田田块和田埂（渠道、道路、及田埂的统称），设置田埂面积比例（如15%），从而改进有效降雨、蒸发蒸腾、入渗等子过程的计算。


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
! 2. 在冠层截留模块canopyint.f中，precipday扣除截留部分，并检查不为负数，precipday = precipday - (canmxl - canstor(j))
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
            if (idplt(j) == 33) then  ! paddy rice HRU
              ! water added into ditches from low embankment, should be added to somewhere else.
              pcp2canal = precipday * pcp2canfr_pr * embnkfr_pr
              precipday = precipday - caninterc - pcp2canal
            else
              precipday = precipday - caninterc
            endif
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

### 2.2. 下渗/产流
不蓄水时，用SWAT原来的方法
积水时用pothole.f中的模拟方法
在surq_daycn.f中，判断是否处于水田的蓄水期。如果是，把降水全部加到水田的蓄水量中，并让产流暂时为0（这样surface中计算的侵蚀量也为0，在水田蓄水期这是合理的）。在执行完operatn模块之后，再根据水田的水层深度设置计算水田径流量。

```fortran
       ! for paddy rice during impoundment, set surfq = 0 for the moment,
       ! and after operatn, recalculate surfq for paddy rice according the water depth configuration
      if (idplt(j) == 33 .and. imp_trig(j) == 0) then
           surfq(j) = 0.0
      end if
```

在subbasin.f文件中，operatn之后添加对水稻产流模块的调用
```fortran
        !! perform management operations
        if (yr_skip(j) == 0) call operatn

        !!  recalculate surfq for paddy rice according the water depth configuration, By Junzhi Liu 2017-12-03
        if (idplt(j) == 33 .and. imp_trig(j) == 0) call surq_rice

```

水稻产流模块的实现:超过水田的最大蓄水深度才产流，采用固定下渗率计算入渗量。
```fortran
      subroutine surq_rice

    !!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
    !!    name        |units         |definition
    !!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    !!    precipday   |mm H2O        |precipitation for the day in HRU
    !!    pot_k       |(mm/hr)       |hydraulic conductivity of soil surface of pothole
    !!                   [defaults to conductivity of upper soil (0.01--10.) layer]
    !!    hru_ha(:)     |ha            |area of HRU in hectares

    !!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
    !!    name           |units         |definition
    !!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    !!    pot_vol(:)     |mm            |current volume of water stored in the
    !!                                  |depression/impounded area
    !!    pot_spillo(:)  |mm            |amount of water released to main channel from
    !!                                  |impounded water body due to spill-over
    !!    pot_seep(:)    |mm            |amount of water seepage to soil

      use parm

      real :: yy, potvol_sep

      j = 0
      j = ihru

       !!    conversion factors
      cnv = 10. * hru_ha(j)
      rto = 1.

       ! add precipation to the water layer of paddy rice
      pot_vol(j) = pot_vol(j) + precipday

       ! if overflow, then send the overflow to the HRU surface flow
      if (pot_vol(j) > prpnd_max(j)) then
        qdr(j) = qdr(j) + (pot_vol(j)- prpnd_max(j))
        !          qday = qday + (pot_vol(j)- pot_volxmm(j))
        pot_spillo(j) = pot_vol(j) - prpnd_max(j)
        pot_vol(j) = prpnd_max(j)
      end if       !! if overflow

    !      compute seepage, pot_seep will be used in percmain.f
      if (pot_vol(j) > 1.e-6) then
!        limit seepage into soil if profile is near field capacity
         if (pot_k(j) > 0.) then
           yy = pot_k(j)
         else
           yy = sol_k(1,j)
         endif

!        calculate seepage into soil
         potsep = yy * 24.
         potsep = Min(potsep, pot_vol(j))
         potvol_sep = pot_vol(j)
         pot_vol(j) = pot_vol(j) - potsep
         pot_seep(j) = potsep
      endif

      ! evaporation will be calculated in etact.f

      end subroutine surq_rice
```

### 2.4. 渗漏
考虑犁底层对渗漏的影响，需设置一个生育期内的平均渗漏强度，如2mm/day。
+ 简单地，把该参数作为整个流域的参数输入，在`basins.bsn`文件末尾添加如下内容：
    ```text
    Paddy Rice (revised by ljzhu):
            2.0    | PERCO_AVE_PR: Mean percolation rate during growing season of paddy rice (mm/day).
    ```
+ 在`modparm.f`中添加变量定义：
    ```fortran
    !!    Paddy rice related parameters, added by ljzhu, 11/01/2017
    real :: pcp2canfr_pr, embnkfr_pr, perco_max_paddy
    ```
+ 在`varinit.f`中添加参数初始化：
    ```fortran
    !! Paddy rice modeling by ljzhu, 11/01/2017
    perco_max_paddy = 2.0
    ```
+ 在`readbsn.f`中添加参数读取代码：
    ```fortran
    !!    Paddy Rice (revised by ljzhu), 11/01/2017
    read (103,*,iostat=eof) perco_max_paddy
    if (eof < 0) exit
    !!    Paddy Rice (revised by ljzhu), 11/01/2017
    ```
+ 在`percmicro.f`中添加对水稻田渗漏量的限制
```fortran
       !! for paddy rice, limit the seepage to groundwater less than 2mm/day, By Junzhi Liu, 2017-12-03
      if (ly1 == sol_nly(j) .and. idplt(j) == 33) sepday = min(sepday, perco_max_paddy)
```

另外，当土壤中的水满足渗漏、壤中流之后仍然超过饱和含水量时，原来代码中已经做了特殊处理使水重新回到地表。
代码在`sat_excess.f`中
```fortran
              if (ly == 1 .and. ul_excess > 0.) then
                !! add ul_excess to depressional storage and then to surfq
                pot_vol(j) = pot_vol(j) + ul_excess
              end if
```

### 2.4. 蒸发蒸腾
SWAT源码中，设置最大蒸发与最大蒸腾之和（ETmax）**不大于**参考作物
蒸发蒸腾量，

SWAT中有三种蒸散发模拟方法：
+ Penman-Monteith
+ Priestley-Taylor
+ Hargreaves

在etact.f中,计算后两种方法的实际蒸发。首先计算水稻的最大蒸腾量和最大土壤/水面蒸发量（ORYZA模型中的方法），然后再计算土壤/水面蒸发时，先从田面水中减去相应的水量。

```fortran
         ! for paddy rice, recalculate es_max and ep_max
        if (idplt(j) == 33) then
             ! split the total pet to the radiation-driven part and drying power part
             etrd = 0.
             etae = 0.
             etrd = 0.75 * pet
             etae = pet - etrd

             es_max = exp(-0.5 * laiday(j)) * pet
             ep_max = etrd * (1. - exp(-0.5 * laiday(j) )) + etae * min(2., laiday(j) );

             esleft = es_max

             ! for impound paddy rice, source for evaporation is taken from water layer first
             if (pot_vol(j) >= esleft) then
                 !take all soil evap from pot
                 pot_vol(j) = pot_vol(j) - esleft
                 pot_evap(j) = esleft
                 esleft = 0.0
             else
                 !first taking from pot then start taking from soil
                 esleft = esleft - pot_vol(j)
                 pot_evap(j) = pot_vol(j)
                 pot_vol(j) = 0.0
             end if
        end if
```



#### 2.4.1 地下水埋深
代俊峰和崔远来(2009)增加了地下水埋深的计算（不透水层距离地表的深度减去水位高度）。在SWAT2012
中，该项参数已由D. Moriasi添加（4/8/2014）。
在`soil_phys.f`中计算，其中不透水层距离地表的深度（`dep_imp`）由`.HRU`文件输入，浅层地下水高度
（shallst）由地下水`.GW`文件输入
```fortran
wat_tbl(i) = dep_imp(i)- (shallst(i)/sol_por(nly,i))
```

### 2.5. 灌溉和排水

增加坑塘（POND）的灌溉功能。在HRU循环之前，统计当前子流域坑塘的所有可用水量；在HRU循环中，如果需要灌溉，则优先从POND中取水，如有需要再从模型设置的灌溉水来源供水；HRU循环之后，再重分配坑塘剩余水。

#### 2.5.1. 作物管理措施输入
SWAT中作物管理措施数据由`OpSchedules`表输入，该表内容参照《SWAT IO Document 2012》P259,
Figure 20-1整理。

为了模拟稻田不同生长期水位变化，需要对`13-release/impound`措施的参数进行扩充，下表黑体部分为
新增参数，分别为最大灌水深度（maximum ponding depth）、最小适宜蓄水深度（minimum fitting
depth）和最大适宜蓄水深度（maximum fitting depth）。

| mgtop | mgt1i    | mgt2i       | mgt3i       | mgt4        |
| ----- | -------- | ----------- | ----------- | ----------- |
| 13    | IMP_TRIG | **MAX_PND** | **MIN_FIT** | **MAX_FIT** |

参考Xie和Cui（2011）文章表1进行稻田灌排水设置。

+ 在`modparm.f`中添加变量定义：
    ```fortran
    !!    Paddy rice related parameters, added by ljzhu, 11/01/2017
          real, dimension (:), allocatable :: prpnd_max, prfit_min, prfit_max
    ```
+ 在`allocate_parms.f`中申请数组空间：
    ```fortran
    !!    Paddy rice related parameters, added by ljzhu, 11/01/2017
    allocate(prpnd_max(mhru))
    allocate(prfit_min(mhru))
    allocate(prfit_max(mhru))
    ```
+ 在`zero2.f`中添加参数初始化：
    ```fortran
    imp_trig = 0   !!Srini pothole
    !! Paddy rice related parameters, added by ljzhu, 11/01/2017
    prpnd_max = 0.
    prfit_min = 0.
    prfit_max = 0.
    ```
+ 在`sched_mgt.f`中添加赋值代码：
    ```fortran
    case (13)    !! release/impound water in rice fields
        imp_trig(j) = mgt1iop(nop(j),j)
        !! added by ljzhu, 11/02/2017
        prpnd_max = mgt2iop(nop(j),j)
        prfit_min = mgt3iop(nop(j),j)
        prfit_max = mgt4op(nop(j),j)
    ```


#### 2.5.2 坑塘灌溉

在`subbasin.f`中，在HRU循环（`do iihru = 1, hrutot(inum1)` ）之前，增加当前时间子流域内所有HRU中POND的总可用水量，并在HRU循环结束后对HRU内POND水量进行重分配。

```fortran
      !! calculate the total amount of water in ponds within the subbasin
      pnd_vol_tot = 0.0
      do iihru = 1, hrutot(inum1)
          pnd_vol_tot = pnd_vol_tot + pnd_vol(ihru)
          ihru = ihru + 1
      end do
      !! end the calculation of

      ihru = 0
      ihru = hru1(inum1)
```


如果是水稻生长季的蓄水期，则在渗漏模块之前，进行自动灌溉的判断(这个时期就不需要调用原来的autoirr函数了)，即农田蓄水低于prfit_min时，进行灌溉。灌溉首先从坑塘（整个子流域的坑塘水量）中抽取，坑塘中水量不足时，再从其他灌溉水源取水。

在`subbasin.f`中
```fortran
        !!  recalculate surfq for paddy rice according the water depth configuration, By Junzhi Liu 2017-12-03
        if (idplt(j) == 33 .and. imp_trig(j) == 0) then
            call surq_rice
        ! for impound paddy rice, auto irragation is performed in surq_rice instead of in autoirr
        else if (auto_wstr(j) > 1.e-6 .and. irrsc(j) > 2) then
            call autoirr
        end if
        
        !! perform soil water routing
        call percmain
```

在`surq_rice.f`中
```fortran



```


## 参考文献

[1] 代俊峰, 崔远来. 2009. 基于SWAT的灌区分布式水文模型——Ⅰ.模型构建的原理与方法. 水利学报, 40(2):145–52. https://doi.org/10.13243/j.cnki.slxb.2009.02.018.

[2] Xie, Xianhong, and Yuanlai Cui. 2011. Development and Test of SWAT for Modeling Hydrological Processes in Irrigation Districts with Paddy Rice. Journal of Hydrology, 396(1):61–71. https://doi.org/10.1016/j.jhydrol.2010.10.032.

[3] 王建鹏, 崔远来. 2011. 水稻灌区水量转化模型及其模拟效率分析. 农业工程学报, 27(1):22–28. https://doi.org/10.3969/j.issn.1002-6819.2011.01.004.


