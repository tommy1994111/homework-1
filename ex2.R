# 三顆球，抽1000次(抽出放回)，證明標準差標準化後會符合N(0, 1)

getStandardization = function(sampleList, samplingTime)
{
  # 計算標準化標準差
  
  mu = mean(sampleList) # 母體平均數
  result = sample(sampleList, samplingTime, replace = TRUE) # 抽樣
  xBar = mean(result) # 樣本平均數
 
  # 計算變異數
  # Var(X) = E(X^2) - (E(X))^2
  # EX = mu (in Normal Distribution)
  temp = 0
  for (i in sampleList)
  {
    temp = temp + (i ^ 2) * (1 / length(sampleList))
  }
  variance = temp - mu ^ 2
  
  standard = sqrt(variance) # 標準差
  Standardization = (xBar - mu) / (standard / sqrt(samplingTime)) # 標準化
  return(Standardization) # 回傳標準化標準差
}

getStandardizationList = function(sampleList, samplingTime, listElementAmount)
{
  # 得到標準化標準差向量(陣列)
  # 預設向量1000個element
  
  tempList = c()
  for (i in 1:listElementAmount)
  {
    temp = getStandardization(sampleList, samplingTime)
    tempList = c(tempList, temp) # 將新值與舊向量合併為新向量
  }
  return(tempList)
}

calibration = function(topPoint)
{
    # 算y軸刻度
    
    i = 0
    while (TRUE)
    {
        if (topPoint / 10 ^ i <= 1)
        {
          return(10 ^ (i - 1) / 2)
          break
        }
        i = i + 1
    }
}

drawHistAndLines = function(resultList)
{
  # 畫出直方圖與曲線(以下皆參考下列網址)
  # http://stackoverflow.com/a/20079349
  
  myhist <- hist(resultList, nclass = 20)
  
  # 畫曲線
  multiplier <- myhist$counts / myhist$density
  mydensity <- density(resultList)
  mydensity$y <- mydensity$y * multiplier[1]
    
  mymean <- 0
  mysd <- 1
  myx <- seq(min(resultList), max(resultList), length.out= 100)
  # mymean <- mean(resultList)
  # mysd <- sd(resultList)
  
  sd_x <- seq(mymean - 4 * mysd, mymean + 4 * mysd, by = mysd)
  sd_y <- dnorm(x = sd_x, mean = mymean, sd = mysd) * multiplier[1]
  
  topPoint <- max(max(sd_y), max(myhist$counts), max(mydensity$y))
  
  topPoint_y = (as.integer(topPoint / calibration(topPoint)) + 1) * calibration(topPoint)
  calibration_x = seq(-5, 5, 0.5)
  calibration_y = seq(0, topPoint_y, calibration(topPoint))
  
  print(topPoint)
  print(calibration_y)
  
  # 因為lines()無法在hist()上畫曲線, 需另開一個plot()把原先的hist()畫上後再用lines()畫上曲線
  plot(
      # 字型為macOS專用
      myhist, ylim = c(0, topPoint), xlim = c(-4, 4),
      main = '抽球次數分布圖\n藍線為標準常態分配',
      xlab = '標準化x值',
      ylab = '次數',
      family='LiGothicMed',
      xaxt = 'n',
      yaxt = 'n'
  )
  axis( # 圖片座標參數
      side = 1, # 在底部建立軸(x軸)
      at = calibration_x, # 標誌tickmarks
      tck = -0.02, # tickmarks的長度-0.02，方向向外
      labels = calibration_x # tickmarks分別對應的標識(labels)
  )
  axis( # 圖片座標參數
      side = 2, # 在底部建立軸(y軸)
      at = calibration_y, # 標誌tickmarks
      tck = -0.02, # tickmarks的長度-0.02，方向向外
      labels = calibration_y # tickmarks分別對應的標識(labels)
  )
  
  lines(mydensity, col = 'gray', lwd = 2)
  
  normal <- dnorm(x = myx, mean = 0, sd = 1)
  lines(myx, normal * multiplier[1], col = "blue", lwd = 2)
  
  segments(x0 = sd_x, y0= 0, x1 = sd_x, y1 = sd_y, col = "red", lwd = 3)
}

drawQQplotAndQQline = function(resultList)
{
  # 畫出常態機率圖以檢視是否符合常態分配
  
  qqnorm(resultList)
  qqline(resultList)
}

main = function(sampleList, samplingTime, listElementAmount = 1000)
{
  # 主function
  
  resultList = getStandardizationList(sampleList, samplingTime, listElementAmount)
  drawHistAndLines(resultList)
  # drawQQplotAndQQline(resultList)
  # shapiro.test(resultList) # 因符合常態，此檢定方法會不拒絕虛無假設(H0:符合常態分配)，無法以這檢定方法檢驗常態
}

# sampleList = c(1, 4, 7) # 樣本向量(球的號碼)
# samplingTime = 1000 # 抽樣1000次
# main(sampleList, samplingTime)