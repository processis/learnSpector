2025.06.25

代码放在learnSpector/TmwR2506/ TmwR分支

OLD文件夹放置了老方法抽样和1-7为test train 代码和结果

其中读取数据是（cleanDeshrnaisLogEffort77.csv）Lange 取log的81数据，其中的-1值左边变为平均值2，右边变为平均值3，分别做了logeffort与teamexp 和pointadjust 两个变量和 logeffort与teamexp 和pointadjust managerexp 的分析 结果用final数据表再次计算

tidymodeldata放了需要读取的数据，tidydeepseek 放置了一些deepseek自动生成的代码

test为1-7时结果几乎对应，随机抽样（73,66,56,41,32,22,13）teamexp+pointadjust 随机PCR RMSE RF RMSE RR 差异比较大

teamexp+pointadjust+managerexp 随机LM RF NNET RR 和 PCR RF RMSE 差异都比较大 1-7数据 teamexp+pointadjust+managerexpPCR RMSE 差异比较大 


2025.12.25备注：
  Reich文件夹里面CH3-CH5 附录A5
  bayes rules ch2-ch19  都有代码
  besian 里面shelly文件夹   是类似8.2.1那种代码，krusake是第二版代码，里面有jas和stan