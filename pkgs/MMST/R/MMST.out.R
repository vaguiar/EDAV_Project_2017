MMST.out <-
function (dest.folder, datasets = 'all')
{
  if (datasets == 'all')
    datasets <-   c('AirlineDistances','soldat','bodyfat','boston',
    'BritishTowns','bupa','tobacco','cleveland','alontop',
    'COMBO17','morse','covertype','foetal','diabetes','ecoli',
    'samp05','samp25','samp05d','samp25d','glass','tumors',
    'gilgaied.soil','Hidalgo1872','MEG','ionosphere','iris',
    'satimage','detergent','leukemia','letter.recognition','baseball',
    'x498.matrix','ncifinal','norwaypaper1','food','geyser',
    'pendigits','color.stimuli','pet','pima','primate.scapulae',
    'psych24r','root.stocks','shoplifting','shuttle','sonar',
    'spambase','steganography','SwissBankNotes','swiss.roll',
    'turtles','ushighways','vehicle','wine','wdbc','yeast')
  
  data(list = datasets)
  for (dsi in datasets) {
    gdsi <- get(dsi)
    if (class(gdsi) != 'data.frame') gdsi <- as.matrix(gdsi)
    write.table(gdsi,
                file = paste(dest.folder,  '/', dsi, '.txt', sep = ''),
                sep = '\t',
                eol = '\n',
                quote = FALSE,
                row.names = FALSE)
  }
}

