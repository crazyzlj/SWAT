# SWAT Call Structure

SWAT模型源代码解析。
+ SWAT版本：SWAT2012rev664
+ 最近更新：2017-11-02
+ 整理：朱良君
+ Email：zlj@lreis.ac.cn


## 通用函数

+ aunif：产生0~1之间的随机数
+ ascrv：通过给定的2个(x, y)坐标，计算曲线`x = y/(y + exp(x5 + x6*y))`的参数`x5`和`x6`
+ caps：将输入字符串（文件名）转成小写
+ ee：计算给定气温下的饱和蒸汽压（saturation vapor pressure）
+ erfc：complementary error function
+ estimate_ksat：从土壤粘粒含量计算ksat饱和导水率（base on equation by Jimmy Willimas）
+ expo：指数函数（上下限分别为20和-20）
+ gcycl：初始化随机数种子，以使每次运行模型产生的随机数均不同
+ jdt：计算Julian day
+ xmon：计算给定Julian day所在的月份
+ 

## modparm

+ 定义全局变量
  其余所有代码开头均需引入该模块

  ```fortran
  use parm
  ```
+ 定义数据类型基本语法
  一般格式为：

  类型说明[(种别说明)][,属性说明表] :: 变量名表[=初值]
  ```fortran
  !定义整型、浮点型
  integer :: icalen
  real :: prf_bsn
  !种别为2的一维实数数组，10个元素
  real(kind=2), dimension(1:10) :: X
  ! 动态一维数组
  real, dimension (:), allocatable :: alph_e
  ! 动态二维数组
  real, dimension (:,:), allocatable :: hru_rufr
  ```

+ 常用变量
  nhru：所有之前子流域HRU总数
  hrutot：每个子流域内HRU个数
  hru1：每个子流域HRU开始的索引号
  mhru: 流域HRU总数
  
  比如，共有3个子流域，其HRU个数分别为12,10,15，则：
  nhru = 0, 12, 22
  hrutot = [12,10,15]
  hru1 = [1, 13, 23]
  mhru = 37
  
  
  

## Call getallo

+ 功能
  从`file.cio`文件中读取HRU(`mhru`)、SUBBASIN(`msub`)个数等，用于分配数组大小，即`allocate_parms`

## Call allocate_parms

+ 功能
  为`modparm`中定义的所有全局数组变量分配空间，并进行初始化，初始化调用函数为：
  + zero0
  + zero1
  + zero2
  + zeroini
  + zero_urbn

## Call readfile

+ 功能
  从`file.cio`文件中读取模型输入文件名：
  + 配置文件`fig.fig`
  + 气象数据文件，如`pcp.pcp`, `tmp.tmp`
  + 数据库文件名，如`plant.dat`
  + 模拟选项文件，如`basin.bsn`
  + 大气沉降配置文件，`ATMO.ATM`


## Call readbsn

+ 功能
  读取`basins.bsn`中定义的流域尺度模拟参数，并进行数据有效性检查（上下限）

## Call readwwq

+ 功能
  读取`basins.wwq`中定义的河流水质模拟参数，并初始化QUAL2E模型参数

## Call readfcst (optional)

+ 功能
  如果需要，从`cst.cst`中读取天气发生器参数

## Call readplant

+ 功能
  从`plant.dat`中读取Landuse/Landcover数据


## Call readtill

+ 功能
  从`till.dat`中读取Tillage数据


## Call readpest

+ 功能
  从`pest.dat`中读取toxin/pesticide数据


## Call readfert

+ 功能
  从`fert.dat`中读取Fertilizer/manure(e.g., Nutrient)数据库


## Call readurban

+ 功能
  从`urban.dat`中读取urban数据库

## Call readseptwq

+ 功能
  从`septwq.dat`中读取septic water quality数据库

## Call readlup

+ 功能
  从`<HRU_ID>.mgt`中读取HRU/Subbasin管理措施数据库
+ TODO
  好像没有open文件的代码呐！

## Call readfig

+ 功能
  从`fig.fig`中读取汇流网络信息，根据汇流命令代码`icodes`和继续读取对应的子流域、河道、HRU等空间单元的数据文件

**注意：该文件中注释行以`*`开头**

icodes|command|备注
-|-|-
0|**finished**|汇流网络构建结束
1|**subbasin**|land phase of the hydrologic and determine the loadings to the main channel
2|**route**|model the movement and transformations occurring in the main channel
3|routres|routing of water through a reservoir
4|transfer|调水灌溉（弃用）
5|**add**|sum the output from different subbasins together
6|rechour|incorporate point source data
7|recmon|incorporate point source data
8|recyear|incorporate point source data
9|save|format of watershed outflow for input into a different SWAT simulation
10|recday|incorporate point source data
11|reccnst|incorporate point source data, call readcnst
12|structure|aeration of water resulting from flow through structures along the channel
13|apex|read from APEX output file
14|saveconc|format water quality results of specified points in the reach network
16|autocal|identify auto-calibration points in the watershed

注：
4-transfer：SWAT allows water to be transferred from one water body (only 1-reach and 2-reservoir are allowed) to another. The transfer is performed every day of the simulation. 原始版本中，`transfer`命令是用来控制HRU灌溉的唯一方式，而目前灌溉操作组主要在管理措施文件中定义，这个命令就很少用得到了，仅为了模型灵活配置而保留。

### 1-subbasin (Call readsub)

+ 功能
  从`.sub`文件中读取流域和HRU相关数据

#### Call readsno

+ 功能
  读取snow data from the HRU/Subbasin soil chemical input

#### Call readhru

+ 功能
  从`.hru`文件中读取HRU尺度模拟相关参数

#### Call readchm

+ 功能
  从`.chm`文件中读取土壤化学参数，其中包括表层土壤初始pesticides/nutrients含量（其他土壤属性在`.sol`文件中）。
  
  注：该文件为可选输入，可为空。

#### Call readmgt

+ 功能
  从`.mgt`文件中读取管理措施数据

#### Call readsol

+ 功能
  从`.sol`文件中读取土壤理化性质

##### Call layersplit

+ 功能
  从第一层土壤中分出10mm的表层土壤

#### Call readgw

+ 功能
  从`.gw`文件中读取地下水输入参数

#### Call readops

+ 功能
  从`.ops`文件中读取Scheduled management operations，如grassed waterways和filter strips

#### Call readsepticbz

+ 功能
  从`.sep`文件中读取流域尺度septic模拟相关参数

#### Call readsdr

+ 功能
  从`.sdr`文件中读取drainage参数

#### Call readwgn

+ 功能
  从`.wgn`文件中读取天气发生器参数

#### Call readpnd

+ 功能
  从`.pnd`文件中读取ponds/wetlands参数

#### Call readwus

+ 功能
  从`.wus`文件中读取从该子流域**调水出去**的参数设置

### 2-route

#### Call readrte

+ 功能
  从`.rte`文件中读取河道参数，每个子流域只允许有一个河道参数文件

#### Call readswq

+ 功能
  从`.swq`文件中读取河道水质参数

### 3-routres

+ 功能
  The routres command routes water, sediment, and chemical loadings through a reservoir.

#### Call readres

+ 功能
  从`.res`文件中读取水库（reservoir）数据

#### Call readlwq

+ 功能
  从`.lwq`文件中读取湖泊（lake）或水库（reservoir）水质参数，其中包括初始pesticide及Nutrient含量，转化过程参数等

#### Call lwqdef

+ 功能
  检查湖泊（lake）水质参数，如不存在则设置为默认值

## Call readatmodep

+ 功能
  读取大气干沉降参数，包括硝态氮和铵态氮

## Call readinpt

+ 功能
  HRU/Subbasin/Reach参数初始化，如对于每个HRU，调用`soil_chem`和`soil_phys`进行土壤理化性质的初始化计算。

### Call soil_chem

+ 功能
  土壤化学参数初始化

### Call soil_phys

+ 功能
  土壤物理参数初始化

### Call rteinit

+ 功能
  河道参数初始化，包括点源输入数据的处理，汇水面积计算等

### Call h2omgt_init

+ 功能
  水管理（灌溉，消耗用水等）参数初始化

### Call hydroinit

+ 功能
  流域水文参数初始化，如子流域汇流时间，迟滞因子，洪峰流量校正因子等

### Call impnd_init

+ 功能
  ponds，wetlands，reservoirs及potholes参数初始化

## Call std1

+ 功能
  标准输出文件的头信息

## Call std2

+ 功能
  标准输出文件的头信息

## Call openwth

+ 功能
  打开气象数据文件，降水、气温、太阳辐射、相对湿度及风速。

## Call headout

+ 功能
  主要输出文件的头信息（`call header`）

## Call simulate

+ 功能
  执行SWAT模拟工作流：
  
  ```fortran
  do curyr = 1, nbyr ! annual loop
    sim_inityr ! initialize annual vars.
    std3 ! write header ==> .std file
    do i = id1, idlst ! daily loop
      sim_initday ! initialize daily vars.
      command ! routing commands loop
      do ihru = 1, nhru ! hru loop
        operatn ! performs all management operations
      end do ! end hru loop
      writed ! write daily and/or monthly output
    end do ! end daily loop
    ! perform end-of-year processes
    ! e.g., tillmix
  end do ! end annual loop
  ```

### Call sim_inityr

### Call std3

### Call sim_initday

### Call clicon

#### Call Pmeas

##### Call pgen/aunif/dstnl/Call pgenhr

#### Call Tmeas

##### Call weatgn/aunif/dstnl/Call tgen

#### Call Smeas

##### Call clgen/Call slrgen

#### Call Hmeas

##### Call rhgen/ee/aunif/dstnl

#### Call Wmeas

##### Call wndgen/aunif

#### Call Pgen

##### Call pgenhr/aunif/dstnl

#### Call Weatgn

##### Call aunif/dstnl

#### Call Tgen

#### Call Clgen

#### Call Slrgen

#### Call Rhgen

##### Call Ee/Aunif/Dstnl

#### Call Wndgen

##### Call Aunif

#### Call PINUESAMP

### Call command

+ 功能
  根据`fig.fig`中定义的汇流命令执行模拟操作
  
  ```fortran
  do ii = 1, mhyd_bsn ! 汇流命令条数
    select case (icode)
      case(0)
        return ! finished
      case([1-16])
        call [corresponding routing command, e.g., subbasin, route, addh]
    end select
  end do
  ```

#### Call subbasin

+ 功能
  陆面水文过程循环
  
  ```fortran
  call sub_subbasin
  do iihru = 1, hrutot(inum1) ! HRU loop
    call varinit
    if "WATR" then
      call water_hru
    else
      ! update ratio of phu
      call schedule_ops
      call albedo
      call solt
      call surface
      call operatn
      call autoirr
      call percmain
      call etpot
      call etact
      call wattable
      call confert
      call conapply
      call graze
      call plantmod
    end if
  end do
  ```
  

##### Call sub_subbasin

+ 功能
  更新距上次降水的时间步长数

##### Call varinit

+ 功能
  初始化HRU参数

##### call water_hru

+ 功能
  对于水体HRU，只计算其潜在蒸散发PET（Priestly-Taylor公式）和蒸发量（`etday=0.7*pet_day`）

##### Call schedule_ops

+ 功能
  设置Scheduled management operations参数，如grassed waterways和filter strips

##### Call albedo

+ 功能
  计算地表反照率

##### Call solt

+ 功能
  计算各层土壤温度

##### Call surface

+ 功能
  计算坡面水文过程，包括冠层截留、融雪、坡面产流、土壤侵蚀等过程。
  
  ```fortran
  if (idplt(j) > 0) call canopyint
  call snom
  if (icrk == 1) call crackvol
  ! 为灌溉HRU进行灌溉，如果灌溉水量超过sol_ul-sol_st,则使土壤含水量（sol_st）至饱和（sol_ul）
  call dailycn
  call volq
  ! 产流校正：BMP及Crack flow
  ! 产流 += 灌溉
  call surfst_h2o
  call alph
  call pkq
  call tran ! transmission loss
  call eiusle
  call ovr_sed
  call cfactor
  call ysed
  ```

###### Call canopyint

+ 功能
  计算植被冠层截留

###### Call snom

+ 功能
  气温大于0℃时计算融雪过程

###### Call crackvol

+ 功能
  计算crack volume for crack flow simulation，由`.bsn`文件中定义：
  
  ICRK: crack flow code: 1=model crack flow in soil

###### Call dailycn

+ 功能
  计算HRU当日、当前土壤含水量条件下的径流曲线数（curve number）

###### Call volq

+ 功能
  计算产流
  + CN method：`Call surq_daycn`
  + Green & Ampt method：`Call surq_greenampt`

###### Call crackflow

+ 功能
  crack flow simulation，由`.bsn`文件中定义：
  
  ICRK: crack flow code: 1=model crack flow in soil

###### Call surfst_h2o

+ 功能
  计算当日流入主河道的净产流量。

###### Call alph

+ 功能
  计算当日降雨总量中发生在雨强最大的半小时降雨的比例（`al5`：无量纲参数）

###### Call pkq

+ 功能
  计算洪峰流量（peak runoff rate）

###### Call tran

+ 功能
  计算tributary channel transmission losses

###### Call eiusle

+ 功能
  计算USLE降雨侵蚀力指数（USLE Rainfull Erosion Index）

###### Call ovr_sed

+ 功能
  坡面侵蚀量计算，包括降雨击溅侵蚀、细沟侵蚀和细沟间侵蚀。

###### Call cfactor

+ 功能
  计算USLE C因子

###### Call ysed

+ 功能
  利用MUSLE模型计算土壤流失量

##### Call operatn

+ 功能
  通过日期或者积温比例控制实施作物管理措施。

###### Call sched_mgt

+ 功能
  根据不同作物管理措施代码（`mgtop`）进行不同模拟。
  
mgtop|operation|module
-|-|-
1|plant|plantop.f
2|irrigation|irrsub.f
3|fertilizer|fert.f
4|pesticide|apply.f
5|harvest & kill|harvkillop.f
6|tillage|newtillmix.f
7|harvest only|harvestop.f
8|kill|killop.f
9|graze|graze.f
10|auto irrigation|-
11|auto fertilizer|-
12|street sweeping|-
13|release/impound|-
14|continuous fertilization|confert.f
15|continuous pesticide|conapplyf
16|burning|burn.f
17|skip a year|-

+ SWAT-RICE对灌溉排水操作进行了扩充

mgtop|mgt1i|mgt2i|mgt3i|mgt4
--|--|--|--|--
13|IMP_TRIG|**MAX_PND**|**MIN_FIT**|**MAX_FIT**

##### Call autoirr

+ 功能
  从浅层地下水、深层地下水或无限制来源水进行自动灌溉

##### Call percmain

+ 功能
  土壤渗漏模块，包括：
  + 裂隙渗漏（call percmacro）（ICRK=1）
  + call percmicro：计算 tile flow (lyrtile), lateral flow (latlyr) and percolation (sepday)
  + call sat_excess: redistribute soil water if above field capacity (high water table)

##### Call etpot

+ 功能
  计算蒸散发（ipet方法）
  + 0-Priestley-Taylor
  + 1-Penman-Monteith，同时也计算潜在植物蒸腾
  + 2-Hargreaves
  + 3-输入的观测值

##### Call etact

+ 功能
  计算潜在植物蒸腾（0-Priestley-Taylor和2-Hargreaves方法）、潜在和实际土壤蒸发，同时计算了由土壤蒸发引起的NO3从第2层到第1层土壤的运动。


##### Call wattable

+ 功能
  计算30天气象因子驱动（precipitation-qday-pet_day）的地下水位（wtab，water table height）
  
  注：`wtab`这个参数目前仅用于HRU输出文件`output.hru`。

##### Call confert

+ 功能
  自动连续施肥操作，直到达到施肥天数（`ndcfrt == fert_days`）

##### Call conapply

+ 功能
  自动连续施农药（applying pesticide）操作，直到达到施肥天数（`ndcpst == pest_days`）

##### Call graze

+ 功能
  模拟放牧导致的生物量减少，并apply manure

##### Call plantmod

+ 功能
  模拟植物生长。模拟每日的潜在植物生长，计算叶面积指数LAI，模拟作物残茬的腐殖。基于水分胁迫调整每日干物质量。

###### Call swu

+ 功能
  计算植物水利用，水分胁迫，实际植物蒸腾等

###### Call grow

+ 功能
  考虑水分、温度、营养物胁迫计算植物的生物量、叶面积指数和冠层高度

####### Call tstr(temp stress)

####### Call nup(nitrogen uptake)

######## Call nfix/nuts

####### Call npup(phosphorus uptake)

######## Call nuts

####### Call anfert(autofert)

##### Call nminrl

##### Call nitvol

##### Call pminrl

##### Call gwmod

##### Call apply

##### Call washp

##### Call decay

##### Call pestlch

##### Call enrsb

##### Call pesty

##### Call orgn

##### Call psed

##### Call nrain

##### Call nlch

##### Call solp

##### Call subwq

##### Call bacteria

##### Call urban

###### Call sweep

##### Call pothole

##### Call latsed

##### Call gwnutr

##### Call gw_no3

##### Call surfstor

##### Call filter

##### Call buffer

##### Call wetlan

##### Call hrupond

###### Call pond

##### Call irrsub

###### Call irrigate

##### Call autoirr

###### Call irrigate

##### Call watuse

##### Call watbal

##### Call sumv

##### Call virtual

###### Call hruday

###### Call impnday

###### Call subday

#### Call route

##### Call rchinit

##### Call rtover

##### Call rtday

##### Call rtmusk

##### Call rthourly

##### Call rthmusk

##### Call rtsed

###### Call ttcoef

##### Call rthsed

###### Call ttcoef

##### Call watqual2

##### Call watqual

##### Call noqual

##### Call hhwtqual

##### Call hhnoqual

##### Call rtpest

##### Call rthpest

##### Call rtbact

##### Call irr_rch

###### Call irrigate

##### Call rchuse

##### Call rtout

#### Call routres

##### Call resinit

##### Call irr_res

###### Call irrigate

##### Call res

##### Call resnut

##### Call lakeq

#### Call Transfer

#### Call addh

#### Call rechour

#### Call recmon

#### Call recyear

#### Call save

#### Call recday

#### Call reccnst

#### Call structure

#### Call saveconc

#### Call autocal

### Call writed

### Call writem

#### Call hrumon

#### Call impndmon

#### Call submon

#### Call rchmon

#### Call writea

### Call newtillmix

#### Call curno

## Call finalbal

+ 功能
  检查流域整体水量平衡
  + 土壤水量平衡`call swbl`
  + ponds和reservoirs的水量和泥沙平衡`call vbl`

## Call writeaa

+ 功能
  输出年均量
  + HRU`call hruaa` ==> `output.hru`文件
  + HRU 蓄水信息`call impndaa` ==> `output.wtr`文件
  + 河道水、污染物等`call rchaa` ==> `.rch`文件
  + 泥沙`call rsedaa` ==> `.sed`文件
  + 子流域信息`call subaa` ==> `output.sub`文件
  + 标准信息`call stdaa` ==> `output.std`文件

## Call pestw

+ 功能
  输出pesticide总结信息至流域标准信息文件 ==> `output.std`
