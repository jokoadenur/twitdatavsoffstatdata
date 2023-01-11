#Change cabaiyes dataframe  to pedas dataframe 
pedas <- cabaiyes
pedas

#Preprocessing teks
#get unduplicate data
nodup <- pedas[!duplicated(pedas$content),]

#Replace text to character
library(dplyr)
tweets <- nodup$content %>% as.character()
head(tweets)

#Remove \n character from sentence
tweets <- gsub( "\n"," ",tweets)
head(tweets)

#Remove \r character from sentence
tweets <- gsub( "\r"," ",tweets)
head(tweets)

#Remove "-" character from sentence
tweets <- gsub( "[-]"," ",tweets)
head(tweets)

#Replace emoji in sentence to english 
library(textclean)
tweets <- replace_html(replace_emoji(tweets))
head(tweets)

#Replace comma in sentence to blank
tweets <- gsub("[,]", "", tweets)
head(tweets)

#Replace question mark in sentence to blank
tweets <- gsub("[?]", "", tweets)
head(tweets)

#Replace question mark in sentence to blank
tweets <- gsub("[-]", " ", tweets)
head(tweets)

#Chane all text to lower case 
library(stringr)
tweets <- str_to_lower(tweets)
head(tweets, 20)

#If needed replace a spesific word
#For example "jln" to "jalan" or specific character like "~" and "*"
tweets <- gsub("[~|*]", " ", tweets)
head(tweets, 20)

#Replace uncorrect word, for example "milihin" to "memilih", "mahal" to "mihil", "bingit" to "banget" ect
#"affordable" to "terjangkau"
#"w" to "saya", "gw" to "saya", "gwe" to "saya"
tweets <- gsub("milihin", "memilih", tweets)
tweets <- gsub("mihil", "mahal", tweets)
tweets <- gsub("bingit", "banget", tweets)
tweets <- gsub("w ", "saya ", tweets)
tweets <- gsub("gw ", "saya ", tweets)
tweets <- gsub("gwe ", "saya ", tweets)
tweets <- gsub("affordable", "terjangkau", tweets)
tweets <- gsub("face with smiling eyes", "senang", tweets)
tweets <- gsub("loudly crying face", "nangis", tweets)
tweets <- gsub("chest loudly crying face", "nangis", tweets)
tweets <- gsub("grinning face", "bahagia", tweets)
tweets <- gsub("face with tears", "sedih", tweets)
tweets <- gsub("hitung2an", "hitung hitung", tweets)
tweets <- gsub("sms/wa", " ", tweets)
tweets <- gsub("grade", "tingkat", tweets)
tweets <- gsub("mentah2", "mentah mentah", tweets)
tweets <- gsub("dll", " ", tweets)
tweets <- gsub("cabe", "cabai", tweets)
head(tweets, 20)

#Change to character
tweetschar <- as.character(tweets)

#Remove duplicate text 
tweetschar <- data.frame(text=unlist(sapply(tweetschar, `[`)), stringsAsFactors=F)
tweetschar <- data.frame(nodup, tweetschar)
nodup <- tweetschar[!duplicated(tweetschar$text),]

#Save first step data
library(openxlsx)
write.xlsx(nodup, "dc1.xlsx")

#Visualize number of daily tweet 
nodup %>%
  group_by(datetime) %>%
  select(datetime, text) %>%
  summarise(jml = length(text)) %>%
  plot(type = "l")

#Save number of daily tweet
nodup %>%
  group_by(datetime) %>%
  select(datetime, text) %>%
  summarise(jml = length(text)) -> voltwitday
write.xlsx(voltwitday, "dayvoltwit.xlsx")

#Change text bersih to text_cleaned
text_cleaned <- nodup$text

#Create Corpus vectorization of all texts
library(tm)
docs <- Corpus(VectorSource(nodup$text))

#Delete unnecessary keywords and words
#Unnecessary conjuction
#Remove people name
docs <- tm_map(docs, removeWords, c('ini','dan','padahal','akan', 'yg','akn','ttg','pdhl',
                                    'thd','itu','juga','jg','dr', 'dari', 'pada','tentang','klo','kalau',
                                    'bahwa','yg','disana','disitu','ttg',
                                    'oleh','olh','adalah','adl','di situ',
                                    'tetapi','tapi','namun','melainkan','lalu','serta','dengan','dgn',
                                    'meski','meskipun','demikian','karena','krn','nih','tuh',
                                    'kalo','orang','saja','aja','walau','walaupun','daripada','gue','gua','gtu',
                                    'iya','gais','emng','and','emg','gitu','hai','hallo','haloo','hrs','gimana',
                                    'gengs','ges','hai','hii','halo','dri','dsb','dll','dkk','dlm','dpt'))

#Delete words with Stopwords
stop <- readLines("~/stopwords-indonesia.txt")
docs <- tm_map(docs, removeWords, stop)
inspect(docs)

#Create wordcloud
#Create Corpus vectorization of all texts
tweets_stopid <- wordcloudcabai
library(tm)
docs2 <- Corpus(VectorSource(nodup$text))

#Delete unnecessary keywords and words
#Check if keywords are independent to analysis 
#Unnecessary conjuction
#Delete people name
docs2 <- tm_map(docs2, removeWords, c('harga','cabai','cabe','ini','dan','padahal', 'yang','akan', 'yg','akn','ttg','pdhl',
                                    'thd','itu','juga','jg','dr', 'dari', 'pada','tentang','klo','kalau',
                                    'bahwa','yg','wkwk','wkwkwk','wkwkwkwk','disana','disitu','haha','hehe',
                                    'wkwkwkwkwkwk','oleh','olh','adalah','adl','di situ',
                                    'tetapi','tapi','namun','melainkan','lalu','serta','dengan','dgn',
                                    'meski','meskipun','demikian','karena','krn','wkwkwkwkwk','nih','tuh',
                                    'nya','kalo','orang','saja','aja', 'tco', 'http', 'https', 'gw', 'gwe', 'sy',
                                    '0330', 'wib','trus','gitu','emang', 'biar','kaga'))

#Remove word with Stopwords
stop <- readLines("~/stopwords-indonesia.txt")
docs2 <- tm_map(docs2, removeWords, stop)
inspect(docs2)

#Create Term Document Matrix

{
  dtm <- TermDocumentMatrix(docs2)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
}
head(d,n=10)

write.xlsx(d, "wc01.xlsx")

#Wordcloud
library(wordcloud2)
wordcloud2(d, backgroundColor = "white",
           color = 'random-light' ,size = 2)

#Delete duplicate after text cleaning with stopwords
tweetschar <- data.frame(text=unlist(sapply(docs, `[`)), stringsAsFactors=F)
tweetschar <- data.frame(nodup, tweetschar)
nodup <- tweetschar[!duplicated(tweetschar$text.1),]

#Filter tweets that contain chili price, "Ribu", "rp" and "Rp"
library(stringr)
nodup %>%
  filter(str_detect(text.1, c("ribu | rp | rupiah"))) %>%
  group_by(datetime) %>%
  select(datetime, text.1) %>%
  summarise(jml = length(text.1)) %>%
  plot(type = "l")

#Save tweets that contain chili price part 2
nodup %>%
  filter(str_detect(text.1, c("ribu | rp | rupiah"))) %>%
  group_by(datetime) %>%
  select(datetime, text.1) %>%
  summarise(jml = length(text.1)) -> voltwitdayprice
write.xlsx(voltwitdayprice, "daypricevoltwit.xlsx")

#Save all tweets that contain chili price, "Ribu", "rp" and "Rp"
nodup %>%
  filter(str_detect(text.1, c("ribu | rp | rupiah"))) %>%
  group_by(datetime) %>%
  select(datetime, username, content, replyCount, likeCount, quoteCount, quotedTweet, media, text.1) -> cleandata
cleandata <- data.frame(cleandata)
write.xlsx(cleandata, "chilipricedata.xlsx")

#Price extract (angka)
library(strex)
cleandata[,c(9)] -> harga

#Price extraction function on tweets that contain "Rp"
ekstrak <- function(x){
  gsub('(?i)\\b(rp\\S+)|.\\b(ribu\\S+)(?i)', '\\1', x, perl = TRUE)
}

#Apply price extraction function
sapply(harga, FUN = ekstrak) %>%
  str_replace_all("\n","") %>%
  str_replace_all("/kg","") %>%
  str_replace_all("kg","") %>%
  str_replace_all("kilogram","") %>%
  str_replace_all("!","") %>%
  str_replace_all("rp","-") %>%
  str_replace_all("[[.]]","") %>%
  str_replace_all("[[-]]"," ") %>%
  str_replace_all("per","") %>%
  str_extract_numbers() -> hargaku

#Change list to vecto character with comma separate
sapply(hargaku, paste0, collapse = ";") -> hargaoke

#Create data frame from change list result to vector character
library(splitstackshape)
dataharga <- cSplit(data.frame(hargaoke), 'hargaoke', ";")

#Combine price data with tweets and save as third data
valdatacabai <- data.frame(cleandata, dataharga)
write.xlsx(valdatacabai, "pricevaldata.xlsx")

#Price validation
#Note that "2162022" is character
valdatacabai[valdatacabai == "2162022"] <- "216000"

#Change dataharga type to numeric
df <- as.data.frame(sapply(valdatacabai, as.numeric))

#Replace NA value with 0
df[is.na(df)] <- 0
str(df)

#Choose absolute price minimum from all value in each coloumn
df[,"oke"] <- pmin(df[1], df[2], df[3], df[4], df[5], df[6], df[7], df[8], df[9]
                   df[10], df[11], df[12], df[13], df[14], df[15], df[16],na.rm = F)

#Choose absolute price maximum from all value in each coloumn
df[,"oke"] <- pmax(df[1], df[2], df[3], df[4], df[5], df[6], df[7], df[8], df[9]
                   df[10], df[11], df[12], df[13], df[14], df[15], df[16],na.rm = F)

#Choose average price from all value in each coloumn
df[,"oke"] <- pmean(df[1], df[2], df[3], df[4], df[5], df[6], df[7], df[8], df[9]
                   df[10], df[11], df[12], df[13], df[14], df[15], df[16],na.rm = F)

#Combite hargaku with hcku
cabaipedas <- data.frame(hcku, df$oke, stringsAsFactors = F)

#Replace coloumn name from dataharga.max to hrgmax
colnames(cabaipedas)[colnames(cabaipedas) == "df.oke"] = "hargafix"

#Change hrgmax type
cabaipedas$hargafix <- as.character(cabaipedas$hargafix)

#Visualize chili price tweet distribution according to province
library(ggplot2)
library(dplyr)
df <- cabaipedas
attach(df)
names(df)
str(df)

df %>%
  filter(hargafix>0 & provinsi>0) %>%
  select(datetime, hargafix, provinsi) -> datacabaifix
library(openxlsx)
write.xlsx(datacabaifix, "pricelocdata.xlsx")

ggplot(data = datacabaifix,
       aes(x=provinsi, y = hargafix, fill = as.factor(provinsi))) +
  geom_boxplot() +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "Provinsi", y = "Sebaran harga cabai maksimum", caption = paste0("Sumber: diolah dari Twitter, ",Sys.Date())) +
  ggtitle("Rentang harga cabai menurut provinsi Tweets 01 Januari - 31 Desember 2022")

df %>%
  filter(hargafix>0) %>%
  select(datetime, hargafix) -> datacabaiharga
write.xlsx(datacabaiharga, "pricedata.xlsx")

datacabaiharga %>%
  group_by(datetime) %>%
  select(datetime, hargafix) %>%
  summarise(rata = mean(hargafix)) %>%
  plot(type = "l")

library(openxlsx)
write.xlsx(datacabaifix, "pricelocdata.xlsx")

#Correlation matrix
df <- abscorr
attach(df)
names(df)
str(df)

library(Hmisc)
library(reshape2)
corrku <- cor(df)
melt_corr <- melt(corrku)
ggplot(na.omit(melt_corr), aes(x = Var1, y = Var2, fill= value)) +
  geom_tile(aes(fill=round(value,4))) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  geom_text(aes(Var1, Var2, label = round(value, 4)), size = 4)

#significance Matrix Pearson correlation test
cortest_m <- rcorr(corrku, type = c("pearson"))
melt_corr <- melt(cortest_m$P)
ggplot(na.omit(melt_corr), aes(x = Var1, y = Var2, fill= value)) +
  geom_tile(aes(fill=round(value,4))) +
  scale_fill_gradient(low = "steelblue", high = "white") +
  geom_text(aes(Var1, Var2, label = round(value, 4)), size = 4)
