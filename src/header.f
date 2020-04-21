      subroutine header

!!    ~ ~ ~ PURPOSE ~ ~ ~                                               
!!    This subroutine defines header titles for the different output files

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~                                    
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    hedb(:)     |NA            |column titles in subbasin output file
!!    hedr(:)     |NA            |column titles in reach output file
!!    hedrsv(:)   |NA            |column titles in reservoir output file
!!    heds(:)     |NA            |column titles in HRU output file
!!    hedwtr(:)   |NA            |column titles in HRU impoundment output 
!!                               |file
!!    icolb(:)    |none          |space number for beginning of column in
!!                               |subbasin output file
!!    icolr(:)    |none          |space number for beginning of column in
!!                               |reach output file
!!    icolrsv(:)  |none          |space number for beginning of column in
!!                               |reservoir output file
!!    icols(:)    |none          |space number for beginning of column in
!!                               |HRU output file
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~                        

      use parm

!!    column headers for HRU output file
      heds = (/"  PRECIPmm"," SNOFALLmm"," SNOMELTmm","     IRRmm",     &
     &         "     PETmm","      ETmm"," SW_INITmm","  SW_ENDmm",     &
     &         "    PERCmm"," GW_RCHGmm"," DA_RCHGmm","   REVAPmm",     &
     &         "  SA_IRRmm","  DA_IRRmm","   SA_STmm","   DA_STmm",     &
     &         "SURQ_GENmm","SURQ_CNTmm","   TLOSSmm"," LATQGENmm",     &
     &         "    GW_Qmm","    WYLDmm","   DAILYCN"," TMP_AVdgC",     &
     &         " TMP_MXdgC"," TMP_MNdgC","SOL_TMPdgC","SOLARMJ/m2",     &
     &         "  SYLDt/ha","  USLEt/ha","N_APPkg/ha","P_APPkg/ha",     &
     &         "NAUTOkg/ha","PAUTOkg/ha"," NGRZkg/ha"," PGRZkg/ha",     &
     &         "NCFRTkg/ha","PCFRTkg/ha","NRAINkg/ha"," NFIXkg/ha",     &
     &         " F-MNkg/ha"," A-MNkg/ha"," A-SNkg/ha"," F-MPkg/ha",     &
     &         "AO-LPkg/ha"," L-APkg/ha"," A-SPkg/ha"," DNITkg/ha",     &
     &         "  NUPkg/ha","  PUPkg/ha"," ORGNkg/ha"," ORGPkg/ha",     &
     &         " SEDPkg/ha","NSURQkg/ha","NLATQkg/ha"," NO3Lkg/ha",     &
     &         "NO3GWkg/ha"," SOLPkg/ha"," P_GWkg/ha","    W_STRS",     &
     &         "  TMP_STRS","    N_STRS","    P_STRS","  BIOMt/ha",     &
     &         "       LAI","   YLDt/ha","  BACTPct ","  BACTLPct",     &
     &         " WTAB CLIm"," WTAB SOLm","     SNOmm"," CMUPkg/ha",     &
     &         "CMTOTkg/ha","   QTILEmm"," TNO3kg/ha"," LNO3kg/ha",     &
     &         "  GW_Q_Dmm"," LATQCNTmm"/)


!!    numbers printed to VB interface HRU output file 
      icols = (/43,53,63,73,83,93,103,113,123,133,143,153,              &
     &163,173,183,193,203,213,223,233,243,253,263,273,283,              &
     &293,303,313,323,333,343,353,363,373,383,393,403,413,              &
     &423,433,443,453,463,473,483,493,503,513,523,533,543,              &
     &553,563,573,583,593,603,613,623,633,643,653,663,673,              &
     &683,693,703,713,723,733,743,753,763,773,783,793,803,              &
     &813/)

!!    column headers for subbasin output file
      hedb = (/"  PRECIPmm"," SNOMELTmm","     PETmm","      ETmm",     &
     &         "      SWmm","    PERCmm","    SURQmm","    GW_Qmm",     &
     &         "    WYLDmm","  SYLDt/ha"," ORGNkg/ha"," ORGPkg/ha",     &
     &         "NSURQkg/ha"," SOLPkg/ha"," SEDPkg/ha"," LAT Q(mm)",     &
     &         "LATNO3kg/h","GWNO3kg/ha","CHOLAmic/L","CBODU mg/L",     &
     &         " DOXQ mg/L"," TNO3kg/ha"/)

!!    numbers printed to VB interface subbasin output file 
      icolb = (/35,45,55,65,75,85,95,105,115,125,135,145,               &
     &155,165,175,185,195,205,215,225,235,245/)
!!  added headers TOTAL N/TOTALP/NO3 Concentration TO HEADING FOR OUTPUT.RCH GSM 10/26/2011
!!    column headers for reach output file
      hedr = (/"  FLOW_INcms"," FLOW_OUTcms","     EVAPcms",            &
     &         "    TLOSScms","  SED_INtons"," SED_OUTtons",            &
     &         "SEDCONCmg/kg","   ORGN_INkg","  ORGN_OUTkg",            &
     &         "   ORGP_INkg","  ORGP_OUTkg","    NO3_INkg",            &
     &         "   NO3_OUTkg","    NH4_INkg","   NH4_OUTkg",            &
     &         "    NO2_INkg","   NO2_OUTkg","   MINP_INkg",            &
     &         "  MINP_OUTkg","   CHLA_INkg","  CHLA_OUTkg",            &
     &         "   CBOD_INkg","  CBOD_OUTkg","  DISOX_INkg",            &
     &         " DISOX_OUTkg"," SOLPST_INmg","SOLPST_OUTmg",            &
     &         " SORPST_INmg","SORPST_OUTmg","  REACTPSTmg",            &
     &         "    VOLPSTmg","  SETTLPSTmg","RESUSP_PSTmg",            &
     &         "DIFFUSEPSTmg","REACBEDPSTmg","   BURYPSTmg",            &
     &         "   BED_PSTmg"," BACTP_OUTct","BACTLP_OUTct",            &
     &         "  CMETAL#1kg","  CMETAL#2kg","  CMETAL#3kg",            &
     &         "     TOT Nkg","     TOT Pkg"," NO3ConcMg/l"/)        
     
!!    numbers printed to VB interface reach output file 
      icolr = (/38,50,62,74,86,98,110,122,134,146,158,170,182,194,206,  &
     &218,230,242,254,266,278,290,302,314,326,338,350,362,374,386,398,  &
     &410,422,434,446,458,470,482,494,506,518,530,542,554,566,578,590,  &
     &602,614,626,638,650,662,674,686,698,710,722,734,746,758/)        

!!    column headers for reservoir output file
      hedrsv = (/"    VOLUMEm3","  FLOW_INcms"," FLOW_OUTcms",          &
     &           "    PRECIPm3","      EVAPm3","   SEEPAGEm3",          &
     &           "  SED_INtons"," SED_OUTtons"," SED_CONCppm",          &
     &           "   ORGN_INkg","  ORGN_OUTkg"," RES_ORGNppm",          &
     &           "   ORGP_INkg","  ORGP_OUTkg"," RES_ORGPppm",          &
     &           "    NO3_INkg","   NO3_OUTkg","  RES_NO3ppm",          &
     &           "    NO2_INkg","   NO2_OUTkg","  RES_NO2ppm",          &
     &           "    NH3_INkg","   NH3_OUTkg","  RES_NH3ppm",          &
     &           "   MINP_INkg","  MINP_OUTkg"," RES_MINPppm",          &
     &           "   CHLA_INkg","  CHLA_OUTkg","SECCHIDEPTHm",          &
     &           "   PEST_INmg","  REACTPSTmg","    VOLPSTmg",          &
     &           "  SETTLPSTmg","RESUSP_PSTmg","DIFFUSEPSTmg",          &
     &           "REACBEDPSTmg","   BURYPSTmg","  PEST_OUTmg",          &
     &           "PSTCNCWmg/m3","PSTCNCBmg/m3"/)

!!    numbers printed to VB interface reservoir output file 
      icolrsv = (/38,50,62,74,86,98,110,122,134,146,158,170,182,194,    &
     &206,218,230,242,254,266,278,290,302,314,326,338,350,362,374,386,  &
     &398,410,422,434,446,458,470,482,494,506,518/)

!!    column headers for HRU impoundment output file
      hedwtr = (/"  PNDPCPmm","  PND_INmm","PSED_It/ha","  PNDEVPmm",   &
     &           "  PNDSEPmm"," PND_OUTmm","PSED_Ot/ha"," PNDVOLm^3",   &
     &           "PNDORGNppm"," PNDNO3ppm","PNDORGPppm","PNDMINPppm",   &
     &           "PNDCHLAppm","  PNDSECIm","  WETPCPmm","  WET_INmm",   &
     &           "WSED_It/ha","  WETEVPmm","  WETSEPmm"," WET_OUTmm",   &
     &           "WSED_Ot/ha"," WETVOLm^3","WETORGNppm"," WETNO3ppm",   &
     &           "WETORGPppm","WETMINPppm","WETCHLAppm","  WETSECIm",   &
     &           "  POTPCPmm","  POT_INmm","OSED_It/ha","  POTEVPmm",   &
     &           "  POTSEPmm"," POT_OUTmm","OSED_Ot/ha"," POTVOLm^3",   &
     &           "  POT_SAha","HRU_SURQmm","PLANT_ETmm"," SOIL_ETmm"/)

      return
      end                                           
