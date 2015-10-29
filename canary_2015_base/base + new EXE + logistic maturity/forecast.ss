#V3.24V
# for all year entries except rebuilder; enter either: actual year, -999 for styr, 0 for endyr, neg number for rel. endyr
1 # Benchmarks: 0=skip; 1=calc F_spr,F_btgt,F_msy 
1 # MSY: 1= set to F(SPR); 2=calc F(MSY); 3=set to F(Btgt); 4=set to F(endyr) 
0.50 # SPR target (e.g. 0.40)
0.4 # Biomass target (e.g. 0.40)
#_Bmark_years: beg_bio, end_bio, beg_selex, end_selex, beg_relF, end_relF (enter actual year, or values of 0 or -integer to be rel. endyr)
 -4 0 -4 0 -4 0
#  2010 2014 2010 2014 2010 2014 # after processing 
1 #Bmark_relF_Basis: 1 = use year range; 2 = set relF same as forecast below
#
1 # Forecast: 0=none; 1=F(SPR); 2=F(MSY) 3=F(Btgt); 4=Ave F (uses first-last relF yrs); 5=input annual F scalar
12 # N forecast years 
1 # F scalar (only used for Do_Forecast==5)
#_Fcast_years:  beg_selex, end_selex, beg_relF, end_relF  (enter actual year, or values of 0 or -integer to be rel. endyr)
 -4 0 -4 0
#  2010 2014 2010 2014 # after processing 
1 # Control rule method (1=catch=f(SSB) west coast; 2=F=f(SSB) ) 
0.4 # Control rule Biomass level for constant F (as frac of Bzero, e.g. 0.40); (Must be > the no F level below) 
0.1 # Control rule Biomass level for no F (as frac of Bzero, e.g. 0.10) 
0.9557699 # Control rule target as fraction of Flimit (e.g. 0.75) 
3 #_N forecast loops (1=OFL only; 2=ABC; 3=get F from forecast ABC catch with allocations applied)
3 #_First forecast loop with stochastic recruitment
0 #_Forecast loop control #3 (reserved for future bells&whistles) 
0 #_Forecast loop control #4 (reserved for future bells&whistles) 
0 #_Forecast loop control #5 (reserved for future bells&whistles) 
2050  #FirstYear for caps and allocations (should be after years with fixed inputs) 
0 # stddev of log(realized catch/target catch) in forecast (set value>0.0 to cause active impl_error)
0 # Do West Coast gfish rebuilder output (0/1) 
0 # Rebuilder:  first year catch could have been set to zero (Ydecl)(-1 to set to 1999)
0 # Rebuilder:  year for current age structure (Yinit) (-1 to set to endyear+1)
1 # fleet relative F:  1=use first-last alloc year; 2=read seas(row) x fleet(col) below
# Note that fleet allocation is used directly as average F if Do_Forecast=4 
2 # basis for fcast catch tuning and for fcast catch caps and allocation  (2=deadbio; 3=retainbio; 5=deadnum; 6=retainnum)
# Conditional input if relative F choice = 2
# Fleet relative F:  rows are seasons, columns are fleets
#_Fleet:  1_CA_TWL 2_OR_TWL 3_WA_TWL 4_CA_NTWL 5_OR_NTWL 6_WA_NTWL 7_CA_REC 8_OR_REC 9_WA_REC 10_CA_AHSOP 11_OR_ASHOP 12_WA_ASHOP 13_CA_FOR 14_OR_FOR 15_WA_FOR 16_CA_NWFSC 17_OR_NWFSC 18_WA_NWFSC 19_CA_Tri_early 20_OR_Tri_early 21_WA_Tri_early 22_CA_Tri_late 23_OR_Tri_late 24_WA_Tri_late
#  0.0245915 0.192096 0.122514 0.048624 0.0907702 0.0522121 0.225412 0.159034 0.0274059 8.64871e-005 0.0052285 0.00821176 0 0 0 0.00161 0.016227 0.0259769 0 0 0 0 0 0
# max totalcatch by fleet (-1 to have no max) must enter value for each fleet
 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
# max totalcatch by area (-1 to have no max); must enter value for each fleet 
 -1 -1 -1
# fleet assignment to allocation group (enter group ID# for each fleet, 0 for not included in an alloc group)
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#_Conditional on >1 allocation group
# allocation fraction for each of: 0 allocation groups
# no allocation groups
288 # Number of forecast catch levels to input (else calc catch from forecast F) 
2 # code means to read fleet/time specific basis (2=dead catch; 3=retained catch; 99=F)  as below (units are from fleetunits; note new codes in SSV3.20)
# Input fixed catch values
#Year Seas Fleet Catch(or_F) Basis
2015	1	1	4.398408035
2015	1	2	19.96482439
2015	1	3	12.60139696
2015	1	4	16.81445796
2015	1	5	15.98723462
2015	1	6	14.64793361
2015	1	7	13.49114243
2015	1	8	11.03597083
2015	1	9	3.810328432
2015	1	10	0.035662263
2015	1	11	1.40391755
2015	1	12	1.517006471
2015	1	13	0
2015	1	14	0
2015	1	15	0
2015	1	16	0.355387924
2015	1	17	2.522639767
2015	1	18	3.413688747
2015	1	19	0
2015	1	20	0
2015	1	21	0
2015	1	22	0
2015	1	23	0
2015	1	24	0
2016	1	1	4.50656561
2016	1	2	20.4557627
2016	1	3	12.91126738
2016	1	4	17.22792824
2016	1	5	16.38036334
2016	1	6	15.00812869
2016	1	7	13.82289184
2016	1	8	11.30734717
2016	1	9	3.904025033
2016	1	10	0.036539204
2016	1	11	1.438440113
2016	1	12	1.554309909
2016	1	13	0
2016	1	14	0
2016	1	15	0
2016	1	16	0.364126972
2016	1	17	2.584671892
2016	1	18	3.497631913
2016	1	19	0
2016	1	20	0
2016	1	21	0
2016	1	22	0
2016	1	23	0
2016	1	24	0
2017	1	1	61.79
2017	1	2	280.47
2017	1	3	177.03
2017	1	4	236.21
2017	1	5	224.59
2017	1	6	205.78
2017	1	7	189.53
2017	1	8	155.04
2017	1	9	53.53
2017	1	10	0.50
2017	1	11	19.72
2017	1	12	21.31
2017	1	13	0.00
2017	1	14	0.00
2017	1	15	0.00
2017	1	16	4.99
2017	1	17	35.44
2017	1	18	47.96
2017	1	19	0.00
2017	1	20	0.00
2017	1	21	0.00
2017	1	22	0.00
2017	1	23	0.00
2017	1	24	0.00
2018	1	1	55.01
2018	1	2	249.69
2018	1	3	157.60
2018	1	4	210.29
2018	1	5	199.95
2018	1	6	183.20
2018	1	7	168.73
2018	1	8	138.02
2018	1	9	47.65
2018	1	10	0.45
2018	1	11	17.56
2018	1	12	18.97
2018	1	13	0.00
2018	1	14	0.00
2018	1	15	0.00
2018	1	16	4.44
2018	1	17	31.55
2018	1	18	42.69
2018	1	19	0.00
2018	1	20	0.00
2018	1	21	0.00
2018	1	22	0.00
2018	1	23	0.00
2018	1	24	0.00
2019	1	1	51.01
2019	1	2	231.54
2019	1	3	146.15
2019	1	4	195.01
2019	1	5	185.41
2019	1	6	169.88
2019	1	7	156.47
2019	1	8	127.99
2019	1	9	44.19
2019	1	10	0.41
2019	1	11	16.28
2019	1	12	17.59
2019	1	13	0.00
2019	1	14	0.00
2019	1	15	0.00
2019	1	16	4.12
2019	1	17	29.26
2019	1	18	39.59
2019	1	19	0.00
2019	1	20	0.00
2019	1	21	0.00
2019	1	22	0.00
2019	1	23	0.00
2019	1	24	0.00
2020	1	1	48.53
2020	1	2	220.28
2020	1	3	139.04
2020	1	4	185.52
2020	1	5	176.39
2020	1	6	161.62
2020	1	7	148.85
2020	1	8	121.76
2020	1	9	42.04
2020	1	10	0.39
2020	1	11	15.49
2020	1	12	16.74
2020	1	13	0.00
2020	1	14	0.00
2020	1	15	0.00
2020	1	16	3.92
2020	1	17	27.83
2020	1	18	37.66
2020	1	19	0.00
2020	1	20	0.00
2020	1	21	0.00
2020	1	22	0.00
2020	1	23	0.00
2020	1	24	0.00
2021	1	1	46.76
2021	1	2	212.25
2021	1	3	133.97
2021	1	4	178.76
2021	1	5	169.97
2021	1	6	155.73
2021	1	7	143.43
2021	1	8	117.33
2021	1	9	40.51
2021	1	10	0.38
2021	1	11	14.93
2021	1	12	16.13
2021	1	13	0.00
2021	1	14	0.00
2021	1	15	0.00
2021	1	16	3.78
2021	1	17	26.82
2021	1	18	36.29
2021	1	19	0.00
2021	1	20	0.00
2021	1	21	0.00
2021	1	22	0.00
2021	1	23	0.00
2021	1	24	0.00
2022	1	1	45.41
2022	1	2	206.14
2022	1	3	130.11
2022	1	4	173.61
2022	1	5	165.07
2022	1	6	151.24
2022	1	7	139.30
2022	1	8	113.95
2022	1	9	39.34
2022	1	10	0.37
2022	1	11	14.50
2022	1	12	15.66
2022	1	13	0.00
2022	1	14	0.00
2022	1	15	0.00
2022	1	16	3.67
2022	1	17	26.05
2022	1	18	35.25
2022	1	19	0.00
2022	1	20	0.00
2022	1	21	0.00
2022	1	22	0.00
2022	1	23	0.00
2022	1	24	0.00
2023	1	1	44.40
2023	1	2	201.52
2023	1	3	127.19
2023	1	4	169.72
2023	1	5	161.37
2023	1	6	147.85
2023	1	7	136.18
2023	1	8	111.39
2023	1	9	38.46
2023	1	10	0.36
2023	1	11	14.17
2023	1	12	15.31
2023	1	13	0.00
2023	1	14	0.00
2023	1	15	0.00
2023	1	16	3.59
2023	1	17	25.46
2023	1	18	34.46
2023	1	19	0.00
2023	1	20	0.00
2023	1	21	0.00
2023	1	22	0.00
2023	1	23	0.00
2023	1	24	0.00
2024	1	1	43.63
2024	1	2	198.04
2024	1	3	125.00
2024	1	4	166.79
2024	1	5	158.58
2024	1	6	145.30
2024	1	7	133.82
2024	1	8	109.47
2024	1	9	37.80
2024	1	10	0.35
2024	1	11	13.93
2024	1	12	15.05
2024	1	13	0.00
2024	1	14	0.00
2024	1	15	0.00
2024	1	16	3.53
2024	1	17	25.02
2024	1	18	33.86
2024	1	19	0.00
2024	1	20	0.00
2024	1	21	0.00
2024	1	22	0.00
2024	1	23	0.00
2024	1	24	0.00
2025	1	1	43.03
2025	1	2	195.32
2025	1	3	123.28
2025	1	4	164.50
2025	1	5	156.40
2025	1	6	143.30
2025	1	7	131.98
2025	1	8	107.97
2025	1	9	37.28
2025	1	10	0.35
2025	1	11	13.73
2025	1	12	14.84
2025	1	13	0.00
2025	1	14	0.00
2025	1	15	0.00
2025	1	16	3.48
2025	1	17	24.68
2025	1	18	33.40
2025	1	19	0.00
2025	1	20	0.00
2025	1	21	0.00
2025	1	22	0.00
2025	1	23	0.00
2025	1	24	0.00
2026	1	1	42.53
2026	1	2	193.06
2026	1	3	121.85
2026	1	4	162.59
2026	1	5	154.60
2026	1	6	141.64
2026	1	7	130.46
2026	1	8	106.72
2026	1	9	36.85
2026	1	10	0.34
2026	1	11	13.58
2026	1	12	14.67
2026	1	13	0.00
2026	1	14	0.00
2026	1	15	0.00
2026	1	16	3.44
2026	1	17	24.39
2026	1	18	33.01
2026	1	19	0.00
2026	1	20	0.00
2026	1	21	0.00
2026	1	22	0.00
2026	1	23	0.00
2026	1	24	0.00
#
999 # verify end of input 
