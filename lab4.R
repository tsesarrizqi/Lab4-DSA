setwd("F:/Lab4")
getwd()

install.packages("tm")
library("tm")


nrow(tweet_raw)
tweet_raw <- read.csv("Manually-Annotated-Tweets.csv", stringsAsFactors = FALSE)
View(tweet_raw)

tweet_raw$class = as.factor(tweet_raw$class)
class(tweet_raw$class)

tweet_corpus <- VCorpus(VectorSource(tweet_raw$tweet))
print(tweet_corpus)

as.character(tweet_corpus[[3]])
halflength = length(tweet_corpus)/2
for (i in 1:halflength){
  print(as.character(tweet_corpus[[i]]))
}

tweet_corpus_clean = tm_map(tweet_corpus,content_transformer(tolower))
#10 clean tweet pertama
for (i in 1:10){
  print(as.character(tweet_corpus_clean[[i]]))
}
#10 original tweet pertama
for (i in 1:10){
  print(as.character(tweet_corpus[[i]]))
}

tweet_corpus_clean <- tm_map(tweet_corpus_clean, removeNumbers)
tweet_corpus_clean <- tm_map(tweet_corpus_clean, removeWords, stopwords())
tweet_corpus_clean <- tm_map(tweet_corpus_clean, removePunctuation)

install.packages("SnowballC")
library(SnowballC)
tweet_corpus_clean <- tm_map(tweet_corpus_clean, stemDocument)

for (i in 11:30){
  print(as.character(tweet_corpus_clean[[i]]))
}

tweet_dtm <- DocumentTermMatrix(tweet_corpus_clean)
tweet_dtm_train <- tweet_dtm[1:(7*nrow(tweet_dtm)/10),]
tweet_dtm_test <- tweet_dtm[((7*nrow(tweet_dtm)/10)+1):nrow(tweet_dtm),]
nrow(tweet_dtm_train)
nrow(tweet_dtm_test)

tweet_train_labels <- tweet_raw[1:5957, ]$class
tweet_test_labels <- tweet_raw[(5957+1):8510, ]$class
length(tweet_train_labels)
length(tweet_test_labels)

install.packages("wordcloud")
library(wordcloud)
wordcloud(tweet_corpus_clean, min.freq = 50, random.order = FALSE)

objnspam_index = c()
for(i in 1:nrow(tweet_raw)){
  if(tweet_raw[i,]$class == 'objnspam'){
    objnspam_index = c(objnspam_index, i)
  }
}
tweet_corpus_clean_objnspam = tweet_corpus_clean[objnspam_index]
wordcloud(tweet_corpus_clean_objnspam, min.freq = 30, random.order = FALSE)

objspam_index = c()
for(i in 1:nrow(tweet_raw)){
  if(tweet_raw[i,]$class == 'objspam'){
    objspam_index = c(objspam_index, i)
  }
}
tweet_corpus_clean_objspam = tweet_corpus_clean[objspam_index]
wordcloud(tweet_corpus_clean_objspam, min.freq = 30, random.order = FALSE)

tweet_freq_words <- findFreqTerms(tweet_dtm_train, 5)
tweet_dtm_freq_train <- tweet_dtm_train[,tweet_freq_words]
tweet_dtm_freq_test <- tweet_dtm_test[, tweet_freq_words]

tweet_train_boolean <- apply(tweet_dtm_freq_train, 1:2, function(x) if(x>0){1} else{0})
tweet_test_boolean <- apply(tweet_dtm_freq_test, 1:2, function(x) if(x>0){1} else{0})

inspect(tweet_dtm_freq_test[2,])
View(tweet_train_boolean)

install.packages('FSelector')
library(FSelector)
training_freq = data.frame(as.matrix(tweet_dtm_freq_train))
View(training_freq)
training_freq$tweetlabel = tweet_train_labels
weights <- chi.squared(tweetlabel ~., training_freq)
View(weights)

training_boolean = data.frame(as.matrix(tweet_train_boolean))
View(training_boolean)
training_boolean$tweetlabel = tweet_train_labels
weights_boolean <- chi.squared(tweetlabel ~., training_boolean)
View(weights_boolean)

source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("EBImage")
library(EBImage)

# membaca citra
img <- readImage("daun_1.jpg");
# menampilkan properti citra
print(img);
# melihat dimensi citra (lebar x tinggi)
dim_img <- dim(img);
# menampilkan citra ke GUI (jika tidak dengan method="raster", maka citra akan muncul melalui internet browser)
display(img, title="Gambar: Daun 1", method="raster");
# menulis citra ke berkas
writeImage(img, 'daunku.jpeg', quality=85);

# menampilkan citra dengan ukuran 200x350 pixel dan semua properti warna (merah, hijau, biru)
display(img[1:200,1:350,], method="raster");
# menampilkan citra dengan hanya warna merah (nilai parameter ke-3: 1)
display(img[,,1], method="raster");

display(img[,,3], method="raster");
display(img[,,2], method="raster");

img_hist <- hist(img)
# menampilkan struktur data dari img_hist
str(img_hist)

# mengambil nilai properti warna (merah, hijau, atau biru) dari citra
img_ch_r = channel(img, "asred");
img_ch_g = channel(img, "asgreen");
img_ch_b = channel(img, "asblue");
# menampilkan struktur img_ch_r
str(img_ch_r)
# alternatif lain adalah mengambil nilai dan memasukkan ke data frame
channels = sapply(c("red", "green", "blue"),
                  function(ch) as.vector(channel(img, ch)),
                  simplify = FALSE)
channels = as.data.frame(channels)
# menampilkan channel red pada data frame
channels$red
# menampilkan struktur data channel red pada data frame
str(channels$red)

img_R_sd = sd(channels$red)
img_G_sd = sd(channels$green)
img_B_sd = sd(channels$blue)

img_R_mean = mean(channels$red)
img_G_mean = mean(channels$green)
img_B_mean = mean(channels$blue)
img_R_mean
img_G_mean
img_B_mean

img_hsv <- rgb2hsv(channels$red*255,channels$green*255,
                   channels$blue*255, maxColorValue = 255);
# menampilkan struktur data nilai properti HSV
str(img_hsv)
print(img_hsv[1,]) # menampilkan nilai H untuk semua pixel
print(img_hsv[2,]) # menampilkan nilai S untuk semua pixel
print(img_hsv[3,]) # menampilkan nilai V untuk semua pixel

img_H_sd = sd(img_hsv[1,])
img_S_sd = sd(img_hsv[2,])
img_V_sd = sd(img_hsv[3,])
img_H_sd
img_S_sd
img_V_sd
img_H_mean = mean(img_hsv[1,])
img_S_mean = mean(img_hsv[2,])
img_V_mean = mean(img_hsv[3,])
img_H_mean
img_S_mean
img_V_mean

channels$red[235:240]
channels$green[235:240]
channels$blue[235:240]
cmax = pmax(channels$red[235:240], channels$green[235:240], channels$blue[235:240])
cmin = pmin(channels$red[235:240], channels$green[235:240], channels$blue[235:240])
delta = cmax-cmin
cmax

cmax_all_pixels = pmax(channels$red, channels$green, channels$blue)
identical(cmax_all_pixels, img_hsv[3,])

cmax_all_pixels = pmax(channels$red, channels$green, channels$blue)
cmin_all_pixels = pmin(channels$red, channels$green, channels$blue)
delta_all_pixels = cmax_all_pixels-cmin_all_pixels
identical(delta_all_pixels/cmax_all_pixels, img_hsv[2,])

# menghitung nilai I
img_I <- 0.596 * channels$red - 0.274 * channels$green - 0.322*channels$blue
img_Y <- 0.299 * channels$red + 0.587 * channels$green + 0.114*channels$blue
img_Q <- 0.211 * channels$red - 0.523 * channels$green + 0.312*channels$blue

img_Y_mean = mean(img_Y)
img_I_mean = mean(img_I)
img_Q_mean = mean(img_Q)
img_Y_sd = sd(img_Y)
img_I_sd = sd(img_I)
img_Q_sd = sd(img_Q)
img_Y_mean
img_I_mean
img_Q_mean
img_Y_sd
img_I_sd
img_Q_sd

img_gray = channel(img,"gray")#convert image to grayscale
fts = computeFeatures.shape(img_gray)#generate shape features
fts

img2 <- readImage("daun_2.jpg")
img3 <- readImage("daun_3.jpg")

img_gray2 = channel(img2,"gray")#convert image to grayscale
fts2 = computeFeatures.shape(img_gray2)#generate shape features
fts2

img_gray3 = channel(img3,"gray")#convert image to grayscale
fts3 = computeFeatures.shape(img_gray3)#generate shape features
fts3

install.packages("glcm")
install.packages("rgdal")
install.packages("raster") # needed for plotRGB function
library(glcm)
library(rgdal)
library(raster)
r <- raster("daun_1.jpg") # need "rgdal" package
textures<- glcm(r, window = c(3, 3), shift = c(1, 1), statistics =
                  c("mean", "mean_ENVI", "variance", "variance_ENVI", "homogeneity",
                    "contrast", "dissimilarity", "entropy", "second_moment",
                    "correlation"), na_opt="any", na_val=NA)
names(textures)
plot(textures$glcm_mean)
plot(textures$glcm_variance)

textures$glcm_variance

plot(textures$glcm_homogeneity)
plot(textures$glcm_contrast)
plot(textures$glcm_dissimilarity)
plot(textures$glcm_entropy)
plot(textures$glcm_second_moment)
plot(textures$glcm_correlation)

######################################################################
##########no4#########################################################
######################################################################
img1 <- readImage("daun_1.jpg")
img2 <- readImage("daun_2.jpg")
img3 <- readImage("daun_3.jpg")

channels1 = sapply(c("red", "green", "blue"),
                  function(ch) as.vector(channel(img1, ch)),
                  simplify = FALSE)
channels1 = as.data.frame(channels1)
channels2 = sapply(c("red", "green", "blue"),
                   function(ch) as.vector(channel(img2, ch)),
                   simplify = FALSE)
channels2 = as.data.frame(channels2)
channels3 = sapply(c("red", "green", "blue"),
                   function(ch) as.vector(channel(img3, ch)),
                   simplify = FALSE)
channels3 = as.data.frame(channels3)

img1_R_mean = mean(channels1$red)
img1_G_mean = mean(channels1$green)
img1_B_mean = mean(channels1$blue)
img2_R_mean = mean(channels2$red)
img2_G_mean = mean(channels2$green)
img2_B_mean = mean(channels2$blue)
img3_R_mean = mean(channels3$red)
img3_G_mean = mean(channels3$green)
img3_B_mean = mean(channels3$blue)

img1_hsv <- rgb2hsv(channels1$red*255,channels1$green*255,
                   channels1$blue*255, maxColorValue = 255);
img2_hsv <- rgb2hsv(channels2$red*255,channels2$green*255,
                    channels2$blue*255, maxColorValue = 255);
img3_hsv <- rgb2hsv(channels3$red*255,channels3$green*255,
                    channels3$blue*255, maxColorValue = 255);

img1_H_mean = mean(img1_hsv[1,])
img1_S_mean = mean(img1_hsv[2,])
img1_V_mean = mean(img1_hsv[3,])
img2_H_mean = mean(img2_hsv[1,])
img2_S_mean = mean(img2_hsv[2,])
img2_V_mean = mean(img2_hsv[3,])
img3_H_mean = mean(img3_hsv[1,])
img3_S_mean = mean(img3_hsv[2,])
img3_V_mean = mean(img3_hsv[3,])

img1_Y <- 0.299 * channels1$red + 0.587 * channels1$green + 0.114*channels1$blue
img1_I <- 0.596 * channels1$red - 0.274 * channels1$green - 0.322*channels1$blue
img1_Q <- 0.211 * channels1$red - 0.523 * channels1$green + 0.312*channels1$blue
img2_Y <- 0.299 * channels2$red + 0.587 * channels2$green + 0.114*channels2$blue
img2_I <- 0.596 * channels2$red - 0.274 * channels2$green - 0.322*channels2$blue
img2_Q <- 0.211 * channels2$red - 0.523 * channels2$green + 0.312*channels2$blue
img3_Y <- 0.299 * channels3$red + 0.587 * channels3$green + 0.114*channels3$blue
img3_I <- 0.596 * channels3$red - 0.274 * channels3$green - 0.322*channels3$blue
img3_Q <- 0.211 * channels3$red - 0.523 * channels3$green + 0.312*channels3$blue

img1_Y_mean = mean(img1_Y)
img1_I_mean = mean(img1_I)
img1_Q_mean = mean(img1_Q)
img2_Y_mean = mean(img2_Y)
img2_I_mean = mean(img2_I)
img2_Q_mean = mean(img2_Q)
img3_Y_mean = mean(img3_Y)
img3_I_mean = mean(img3_I)
img3_Q_mean = mean(img3_Q)

img1_gray = channel(img1,"gray")#convert image to grayscale
fts1 = computeFeatures.shape(img1_gray)#generate shape features
img2_gray = channel(img2,"gray")#convert image to grayscale
fts2 = computeFeatures.shape(img2_gray)#generate shape features
img3_gray = channel(img3,"gray")#convert image to grayscale
fts3 = computeFeatures.shape(img3_gray)#generate shape features

img1_area = fts1[1]
img1_perimeter = fts1[2]
img1_radius_mean = fts1[3]
img1_radius_sd = fts1[4]
img1_radius_min = fts1[5]
img1_radius_max = fts1[6]
img2_area = fts2[1]
img2_perimeter = fts2[2]
img2_radius_mean = fts2[3]
img2_radius_sd = fts2[4]
img2_radius_min = fts2[5]
img2_radius_max = fts2[6]
img3_area = fts3[1]
img3_perimeter = fts3[2]
img3_radius_mean = fts3[3]
img3_radius_sd = fts3[4]
img3_radius_min = fts3[5]
img3_radius_max = fts3[6]

r1 <- raster("daun_1.jpg") # need "rgdal" package
textures1 <- glcm(r1, window = c(3, 3), shift = c(1, 1), statistics =
                  c("mean", "mean_ENVI", "variance", "variance_ENVI", "homogeneity",
                    "contrast", "dissimilarity", "entropy", "second_moment",
                    "correlation"), na_opt="any", na_val=NA)
r2 <- raster("daun_2.jpg") # need "rgdal" package
textures2 <- glcm(r2, window = c(3, 3), shift = c(1, 1), statistics =
                    c("mean", "mean_ENVI", "variance", "variance_ENVI", "homogeneity",
                      "contrast", "dissimilarity", "entropy", "second_moment",
                      "correlation"), na_opt="any", na_val=NA)
r3 <- raster("daun_3.jpg") # need "rgdal" package
textures3 <- glcm(r3, window = c(3, 3), shift = c(1, 1), statistics =
                    c("mean", "mean_ENVI", "variance", "variance_ENVI", "homogeneity",
                      "contrast", "dissimilarity", "entropy", "second_moment",
                      "correlation"), na_opt="any", na_val=NA)

img1_glcm_mean = (minValue(textures1$glcm_mean)+maxValue(textures1$glcm_mean))/2
img1_glcm_variance = (minValue(textures1$glcm_variance)+maxValue(textures1$glcm_variance))/2
img1_glcm_homogenity = (minValue(textures1$glcm_homogeneity)+maxValue(textures1$glcm_homogeneity))/2
img1_glcm_contrast = (minValue(textures1$glcm_contrast)+maxValue(textures1$glcm_contrast))/2
img1_glcm_dissimilarity = (minValue(textures1$glcm_dissimilarity)+maxValue(textures1$glcm_dissimilarity))/2
img1_glcm_entropy = (minValue(textures1$glcm_entropy)+maxValue(textures1$glcm_entropy))/2
img1_glcm_second_moment = (minValue(textures1$glcm_second_moment)+maxValue(textures1$glcm_second_moment))/2
img1_glcm_correlation = (minValue(textures1$glcm_correlation)+maxValue(textures1$glcm_correlation))/2

img2_glcm_mean = (minValue(textures2$glcm_mean)+maxValue(textures2$glcm_mean))/2
img2_glcm_variance = (minValue(textures2$glcm_variance)+maxValue(textures2$glcm_variance))/2
img2_glcm_homogenity = (minValue(textures2$glcm_homogeneity)+maxValue(textures2$glcm_homogeneity))/2
img2_glcm_contrast = (minValue(textures2$glcm_contrast)+maxValue(textures2$glcm_contrast))/2
img2_glcm_dissimilarity = (minValue(textures2$glcm_dissimilarity)+maxValue(textures2$glcm_dissimilarity))/2
img2_glcm_entropy = (minValue(textures2$glcm_entropy)+maxValue(textures2$glcm_entropy))/2
img2_glcm_second_moment = (minValue(textures2$glcm_second_moment)+maxValue(textures2$glcm_second_moment))/2
img2_glcm_correlation = (minValue(textures2$glcm_correlation)+maxValue(textures2$glcm_correlation))/2

img3_glcm_mean = (minValue(textures3$glcm_mean)+maxValue(textures3$glcm_mean))/2
img3_glcm_variance = (minValue(textures3$glcm_variance)+maxValue(textures3$glcm_variance))/2
img3_glcm_homogenity = (minValue(textures3$glcm_homogeneity)+maxValue(textures3$glcm_homogeneity))/2
img3_glcm_contrast = (minValue(textures3$glcm_contrast)+maxValue(textures3$glcm_contrast))/2
img3_glcm_dissimilarity = (minValue(textures3$glcm_dissimilarity)+maxValue(textures3$glcm_dissimilarity))/2
img3_glcm_entropy = (minValue(textures3$glcm_entropy)+maxValue(textures3$glcm_entropy))/2
img3_glcm_second_moment = (minValue(textures3$glcm_second_moment)+maxValue(textures3$glcm_second_moment))/2
img3_glcm_correlation = (minValue(textures3$glcm_correlation)+maxValue(textures3$glcm_correlation))/2

###Dibuat 0 karena nilainya NaN
img1_glcm_correlation = 0
img2_glcm_correlation = 0
img3_glcm_correlation = 0

R_mean = c(img1_R_mean, img2_R_mean, img3_R_mean, img1_R_mean, img2_R_mean, img3_R_mean)
G_mean = c(img1_G_mean, img2_G_mean, img3_G_mean, img1_G_mean, img2_G_mean, img3_G_mean)
B_mean = c(img1_B_mean, img2_B_mean, img3_B_mean, img1_B_mean, img2_B_mean, img3_B_mean)
H_mean = c(img1_H_mean, img2_H_mean, img3_H_mean, img1_H_mean, img2_H_mean, img3_H_mean)
S_mean = c(img1_S_mean, img2_S_mean, img3_S_mean, img1_S_mean, img2_S_mean, img3_S_mean)
V_mean = c(img1_V_mean, img2_V_mean, img3_V_mean, img1_V_mean, img2_V_mean, img3_V_mean)
Y_mean = c(img1_Y_mean, img2_Y_mean, img3_Y_mean, img1_Y_mean, img2_Y_mean, img3_Y_mean)
I_mean = c(img1_I_mean, img2_I_mean, img3_I_mean, img1_I_mean, img2_I_mean, img3_I_mean)
Q_mean = c(img1_Q_mean, img2_Q_mean, img3_Q_mean, img1_Q_mean, img2_Q_mean, img3_Q_mean)
area = c(img1_area, img2_area, img3_area, img1_area, img2_area, img3_area)
perimeter = c(img1_perimeter, img2_perimeter, img3_perimeter, img1_perimeter, img2_perimeter, img3_perimeter)
radius_mean = c(img1_radius_mean, img2_radius_mean, img3_radius_mean, img1_radius_mean, img2_radius_mean, img3_radius_mean)
radius_sd = c(img1_radius_sd, img2_radius_sd, img3_radius_sd, img1_radius_sd, img2_radius_sd, img3_radius_sd)
radius_min = c(img1_radius_min, img2_radius_min, img3_radius_min, img1_radius_min, img2_radius_min, img3_radius_min)
radius_max = c(img1_radius_max, img2_radius_max, img3_radius_max, img1_radius_max, img2_radius_max, img3_radius_max)
glcm_mean = c(img1_glcm_mean, img2_glcm_mean, img3_glcm_mean, img1_glcm_mean, img2_glcm_mean, img3_glcm_mean)
glcm_variance = c(img1_glcm_variance, img2_glcm_variance, img3_glcm_variance, img1_glcm_variance, img2_glcm_variance, img3_glcm_variance)
glcm_homogenity = c(img1_glcm_homogenity, img2_glcm_homogenity, img3_glcm_homogenity, img1_glcm_homogenity, img2_glcm_homogenity, img3_glcm_homogenity)
glcm_contrast = c(img1_glcm_contrast, img1_glcm_contrast, img2_glcm_contrast, img3_glcm_contrast, img2_glcm_contrast, img3_glcm_contrast)
glcm_dissimilarity = c(img1_glcm_dissimilarity, img2_glcm_dissimilarity, img3_glcm_dissimilarity, img1_glcm_dissimilarity, img2_glcm_dissimilarity, img3_glcm_dissimilarity)
glcm_entropy = c(img1_glcm_entropy, img2_glcm_entropy, img3_glcm_entropy, img1_glcm_entropy, img2_glcm_entropy, img3_glcm_entropy)
glcm_second_moment = c(img1_glcm_second_moment, img2_glcm_second_moment, img3_glcm_second_moment, img1_glcm_second_moment, img2_glcm_second_moment, img3_glcm_second_moment)
glcm_correlation = c(img1_glcm_correlation, img2_glcm_correlation, img3_glcm_correlation, img1_glcm_correlation, img2_glcm_correlation, img3_glcm_correlation)
Kelas = as.factor(c('A','B','C','A','B','C'))

df <- data.frame(Kelas=Kelas, R_mean=R_mean, G_mean=G_mean, B_mean=B_mean, H_mean=H_mean, S_mean, V_mean, Y_mean, 
                I_mean=I_mean, Q_mean=Q_mean, area=area, perimeter=perimeter, radius_mean=radius_mean, radius_sd=radius_sd, 
                radius_min=radius_min, radius_max=radius_max, glcm_mean=glcm_mean, glcm_variance=glcm_variance, 
                glcm_homogenity=glcm_homogenity,  glcm_contrast=glcm_contrast, glcm_dissimilarity=glcm_dissimilarity, 
                glcm_entropy=glcm_entropy, glcm_second_moment=glcm_second_moment, glcm_correlation=glcm_correlation)

View(df)

class(df)

library(FSelector)
weights <- chi.squared(Kelas~., df)
# Menampilkan bobot
print(weights)

class(HouseVotes84)
class(df)

label <- df[c(1,2,4,5),]$Kelas
ttest <- lapply(df[c(1,2,4,5),-1], function(x) { t.test(x ~ label)$statistic })
ttest

for(i in 1:ncol(df)) {
  print(class(df[,i]))
}

install.packages("mlbench")
library(mlbench)# For data
library(FSelector)#For method

data(HouseVotes84)
class(HouseVotes84$Class)

weights<- chi.squared(Class~., HouseVotes84)


# Print the results 
print(weights)



set.seed(1)
DF1 <- data.frame(y=rep(1:4, 25), x1=rnorm(100), x2=rnorm(100), x3=rnorm(100))
head(DF1)

class(DF1$x1)

group <- DF1$y
lapply(DF1[,-1], function(x) { t.test(x ~ group)$statistic })