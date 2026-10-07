#########################################################
##### Measles Cleaning #####

# This script is used to clean both the MSF vaccination
# data and the MoH measles data. 

# Last updated 9/28/26, with edits and annotations added
# on 10/5/26.

#########################################################

library(readxl)
library(dplyr)
library(stringr)
library(stringi)
library(stringdist)

setwd("/Volumes/LaCie/")

#########################################################
##### VACCINE DATA #####

#Specify column types before importing into R
coltype <- c("guess","guess","guess","guess","date","date",
             "guess","guess","guess","guess","guess","date",
             "date","guess","guess")

#Read in the vax data (make sure to change the path if necessary)
vaxdata <- read_excel("Data/MOH Case and Vax Data/Donnees_Rougeole_2011_2024_Alexandre.xlsx", 
                      col_names=TRUE, col_types=coltype, skip=2)

#Edit the column names
colnames(vaxdata) <- c("year", "oc", "province", "zs", "start date", "end date", "age group",
                       "total_vaxxed", "vax_target", "cv_admin", "cv_investigation", "start_pec",
                       "end_pec", "total_pec", "comments")

rm(coltype)

#########################################################
##### CASE DATA #####

#Read in the case data (make sure to change path if needed!)
casedata <- read_excel("Data/MOH Case and Vax Data/BASE ROUGEOLE.xlsx", sheet=2, col_names=TRUE)
colnames(casedata) <- c("year", "week", "province", "dps", "zs", "cases", "deaths", "population", "disease")

#Make corrections across all columns made of text strings, for uniformity
for (i in names(casedata)[sapply(casedata, is.character)]) {
  #fix cases of text string to be uniform
  casedata[[i]] <- str_to_title(casedata[[i]])
  #sub spaces for hyphens
  casedata[[i]] <- gsub(" ", "-", casedata[[i]])
  #convert special characters
  casedata[[i]] <- stri_trans_general(casedata[[i]], "Latin-ASCII")
}

#There are several instances in which a health zone will be spelled one way
#then will change partway through the time series. The code below reconciles
#those differences. I did try to use fuzzy matching for this, but sometimes the
#stringdist was too far and it wouldn't catch something. I ended up just hand coding it.
hz_corrections <- c(
  #Kongo-Central
    "Kwimba"="Kuimba",
  #Equateur
    "Mampoko"="Lolanga-Mampoko",
  #Mongala
    "Bosomanzi"="Boso-Manzi","Bosomondanda"="Boso-Mondanda",
  #Nord-Ubangi
    "Bili2"="Bili","Gbado-Lite"="Gbadolite",
  #Sud-Ubangi
    "Bogosenubea"="Bogosenubia",
  #Kasai-Central
    "Bena-Tshadi"="Bena-Tshiadi","Lubunga1"="Lubunga","Mwetshi"="Muetshi",
  #Kasai-Oriental
    "Bimpemba"="Bipemba","Citenge"="Tshitenge",
  #Lomami
    "Kalambayi"="Kalambayi-Kabanga","Kalambayi-Kaban"="Kalambayi-Kabanga","Muene-Ditu"="Mwene-Ditu",
  #Sankuru
    "Bena-Dibel"="Bena-Dibele","Omondjadi"="Omendjadi","Vangakete"="Vanga-Kete","Wembonyama"="Wembo-Nyama",
  #Tanganyika
    "Mbulala"="Mbulula",
  #Ituri
    "Mambassa"="Mambasa","Mongbwalu"="Mongbalu",
  #Tshopo
    "Bafwabgobgo"="Bafwagbogbo","Bafwabogbo"="Bafwagbogbo","Lubunga2"="Lubunga","Makiso-Kisangan"="Makiso-Kisangani",
  #Sud-Kivu
    "Bagira-Kasha"="Bagira","Lulenge-Kimbi"="Kimbi-Lulenge","Shabunda-Centre"="Shabunda",
  #Mai-Ndombe
    "Bandjow-Moke"="Banjow-Moke","Pendjwa"="Penjwa",
  #Kasai
    "Kalonda"="Kalonda-Ouest","Kamonya"="Kamonia","Kamuesha"="Kamwesha","Ndjoko-Punda"="Ndjoko-Mpunda",
  #Haut-Lomami
    "Malemba-Nkulu"="Malemba",
  #Haut-Katanga
    "Kapolobwe"="Kapolowe",
  #Tshuapa
    "Busanga"="Bosanga","Yalifafu"="Yalifafo",
  #Kwango
    "Kimbao"="Kimbau","Kisandji"="Kisanji",
  #Bas-Uele
    "Bili1"="Bili","Viandana"="Viadana")

#For some reason, these two health zones were listed but there was no data recorded.
#I couldn't even find anything about them when Googling. 
hz_remove <- c("Adia", "Moaza")

#Remove the two health zones indicated above, and reconcile the differences in health zone names. 
casedata_clean <- casedata %>% filter(!zs %in% hz_remove) %>% mutate(zs_clean = coalesce(hz_corrections[zs], zs))

#########################################################
##### The code below was from testing and from Alex's
# previus cleaning code that I didn't end up using, but might be a 
# helpful reference. 

#included a bit for fuzzy matching health zones with typos to the correct name.
#however, Equateur province doesn't really have any of these issues, so it's probably
#not complete.
# match_indices <- amatch(casedata$zs, hz, method="jw", maxDist=0.25)
# casedata$corrected_zs <- hz[match_indices]
# 
# #list of all health zones, for reference
# hz <- c("KAFUBU", "KAMALONDO", "KAMBOVE", "KAMPEMBA", "KAPOLOWE", "KASENGA", "KASHOBWE", "KATUBA",
#         "KENYA", "KIKULA", "KILELA-BALANDA", "KILWA", "KIPUSHI", "KISANGA", "KOWE", "LIKASI",
#         "LUBUMBASHI", "LUKAFU", "MITWABA", "MUFUNGA-SAMPWE", "MUMBUNDA", "PANDA", "RWASHI",
#         "SAKANIA", "TSHAMILEMBA", "VANGU","PWETO", "BUKAMA", "BUTUMBA", "KABONDO-DIANDA", 
#         "KABONGO", "KAMINA", "KANIAMA", "BAKA", "KAYAMBA", "KINDA", "KINKONDJA", "KITENGE", 
#         "LWAMBA", "MALEMBA-NKULU", "MUKANGA", "MULONGO", "SONGA", "BUNKEYA", "DILALA", 
#         "DILOLO", "FUNGURUME", "KAFAKUMBA", "KALAMBA", "KANZENZE", "KAPANGA",
#         "KASAJI", "LUALABA", "LUBUDI", "MANIKA", "MUTSHATSHA", "SANDOA", "ANKORO", "KABALO", 
#         "KALEMIE", "KANSIMBA", "KIYAMBI", "KONGOLO", "MANONO", "MBULALA", "MOBA", "NYEMBA", "NYUNZU",
#         "KABAMBARE", "KAMPENE", "KASONGO", "KIBOMBO", "KUNDA", "LUSANGI", "SAMBA", "SARAMABILA", "TUNDA",
#         "ALUNGULI", "FEREKENI", "KAILO", "KALIMA", "KINDU", "LUBUTU", "OBOKOTE", "PANGI", "PUNIA", 
#         "BIBANGA","BIPEMBA","BONZOLA","CILUNDU","CITENGE","DIBINDI","DIULU","KABEYA-KAMUANGA","KANSELE",
#         "KASANSA","LUBILANJI", "LUKALENGE","MIABI","MPOKOLO","MUKUMBI","MUYA","NZABA","TSHILENGE",
#         "TSHISHIMBI", "KALAMBAYI-KABANGA","KABINDA","KALENDA","KALONDA-EST","KAMANA","KAMIJI", 
#         "KANDA-KANDA","LUBAO", "LUDIMBI-LUKULA", "LUPUTA", "MAKOTA", "MWENE-DITU", "MULUMBA", 
#         "GANDAJIKA", "TSHOFA","WIKONG", "BENA-DIBELE", "DIKUNGU","DJALO-DJEKA","KATAKO-KOMBE", 
#         "KOLE","LODJA", "LOMELA", "LUSAMBO", "MINGA","OMENDJADI", "OTOTO","PIANA-MUTOMBO",
#         "TSHUDI-LOTO","TSHUMBE","VANGAKETE","WEMBONYAMA", "BANGA-LUBAKA", "BULAPE", "DEKESE", 
#         "ILEBO", "KAKENGE", "KAMONIA", "KAMWESHA", "KANZALA", "KITANGWA", "LUEBO", "MIKOPE", "MUSHENGE",
#          "MUTENA", "MWEKA", "NDJOKO-PUNDA", "NYANGA","TSHIKAPA","KALONDA-OUEST", "BENA-LEKA", 
#         "BENA-TSHIADI", "BILOMBA","BOBOZO", "BUNKONDE", "DEMBA","DIBAYA", "KALOMBA","KANANGA",
#         "KATENDE","KATOKA","LUAMBO","LUBONDAYI", "LUBUNGA-2","LUIZA","LUKONGA", "MASUIKA",
#         "MIKALAYI","MUTOTO","MUETSHI","NDEKESHA","NDESHA", "TSHIBALA", "TSHIKAJI","YANGALA","TSHIKULA",
#         "ALUNGULI","FEREKENI","KABAMBARE","KAILO","KALIMBA","KAMPENE","KASONGO","KIBOMBO","KINDU",
#         "KUNDA","LUBUTU","LUSANGI","OBOKOTE","PANGI","PUNIA","SAMBA","SARAMABILA","TUNDA","BAGIRA-KASHA",
#         "BUNYAKIRI","FIZI","UVIRA","HAUTS-PLATEAUX-D'UVIRA","IBANDA","IDJWI","ITOMBWE","KABARE","KADUTU",
#         "KALEHE","KALOLE","KALONGE","KAMITUGA","KANIOLA","KATANA","KAZIBA","KIMBI-LULENGE","KITUTU",
#         "LEMERA","LULINGU","MINEMBWE","MINOVA","MITI-MURHESA","MUBUMBANO","MULUNGU","MWANA","MWENGA",
#         "NUNDU","NYANGEZI","NYANTENDE","RUZIZI","SHABUNDA-CENTRE","WALUNGU","BAFWAGBOGBO","BAFWASENDE",
#         "BANALIA","BASALI","BASOKO","BENGAMISA","ISANGI","KABONDO","LOWA","LUBUNGA","MAKISO-KISANGANI",
#         "MANGOBO","OPALA","OPIENGE","TSHOPO","UBUNDU","WANIE-RUKULA","YABAONDO","YAHISULI","YAHUMA",
#         "YAKUSU","YALEKO","YALIMBONGO", "AKETI","ANGO","BILI-2","BONDO","BUTA","GANGA-DINGILA",
#         "LIKATI","MONGA","POKO","TITULE","VIADANA","ABA","BOMA-MANGBETU","DORUMA","DUNGU","FARADJE",
#         "GOMBARI","ISIRO","MAKORO","NIANGARA","PAWA","RUNGU","WAMBA","WATSA","BASAKUNSU","BIKORO",
#         "BOLENGE","BOLOMBA","BOMONGO","DJOMBO","IBIKO","IBOKO","INGENDE","IREBU","LILANGA-BOBANGI",
#         "LOLANGA-MAMPOKO","LOTUMBE","LUKOLELA","MAKANZA","MBANDAKA","MONIEKA","NTONDO","WANGATA",
#         "ADI","ADJA","ANGUMU","ARIWARA","ARU","AUNGBA","BAMBU-MINES","BIRINGI","BOGA","BUNIA","DAMAS",
#         "DRODRO","FATAKI","GETY","JIBA","KAMBALA","KILO","KOMANDA","LAYBO","LINGA","LITA","LOGO","LOLWA",
#         "MAHAGI","MAMBASA","MANDIMA","MANGALA","MONGBWALU","NIA-NIA","NIZI","NYANKUNDE","NYARAMBE",
#         "RETHY","RIMBA","RWAMPARA","TCHOMIA","BANDALUNGWA","BARUMBU","BINZA-METEO","BINZA-OZONE",
#         "BUMBU","GOMBE","KALAMU-I","KALAMU-II","KASA-VUBU","KINGABWA","KINSHASA","KINTAMBO","KISENGO",
#         "KOKOLO","LEMBA","LIMETE","LINGWALA","MAKALA","MALUKU-I","MALUKU-II","MASINA-I","MASINA-II",
#         "MATETE","MONT-NGAFULA-I","MONT-NGAFULA-II","NDJILI","NGABA","NGIRI-NGIRI","POLICE","SELEMBAO",
#         "KISENSO","NSELE", "KIKIMI", "BIYELA", "KINGASANI", "KIMBANSEKE","BOKO-KIVULU","BOMA","BOMA-BUNGU",
#         "GOMBE-MATADI","INGA","KANGU","KIBUNZI","KIMPANGU","KIMPESE","KIMVULA","KINKONZI","KISANTU",
#         "KWILU-NGONGO","LUKULA","LUOZI","MANGEMBO","MASA","MATADI","MBANZA-NGUNGU","MUANDA","NGIDINGA",
#         "NSELO","NSONA-PANGU","NZANZA","SONA-BATA","TSHELA","VAKU","KIZU","KUIMBA","SEKEBANZA","KITONA",
#         "BOKO","FESHI","KAHEMBA","KAJIJI","KASONGOLUNDA","KENGE","KIMBAU","KISANDJI","KITENDA",
#         "MWELA-LEMBWA","PANZI","POPOKABAKA","TEMBO","WAMBA-LUADI","BAGATA","BANDUNDU","BULUNGU",
#         "DJUMA","GUNGU","IDIOFA","IPAMU","KIKONGO","KIKWIT-NORD","KIKWIT-SUD","KIMPUTU","KINGANDU",
#         "KOSHIBANDA","LUSANGA","MASI-MANIMBA","MOANZA","MOSANGO","MOKALA","MUKEDI","MUNGINDU",
#         "PAY-KONGILA","SIA","VANGA","YASA-BONGA","BANJOW-MOKE","BOKORO","BOLOBO","BOSOBE","INONGO",
#         "KIRI","KWAMOUTH","MIMIA","MUSHIE","NIOKI","NTANDEMBELO","OSHWE","PENDJUA","YUMBI", "BANDJOW-MOKE",
#         "BINGA","BONGANDANGA","BOSOMANZI","BOSOMODANDA","BOSONDJO","BUMBA","LISALA","LOLO","PIMU",
#         "YAMALUKA","YAMBUKU","YAMONGILI","ALIMBONGO","BAMBU","BENI","BIENA","BINZA","BIRAMBIZO",
#         "BUTEMBO","GOMA","ITEBERO","KALUNGUTA","KAMANGO","KARISIMBI","KATOYI","KATWA","KAYNA","KIBIRIZI",
#         "KIBUA","KIROTSHE","KYONDO","LUBERO","MBALAKO","MANGUREDJIPA","MASEREKA","MASISI","MUSIENENE",
#         "MUTWANGA","MWESO","NYIRAGONGO","OICHA","PINGA","RUTSHURU","RWANGUBA","VUHOVI","WALIKALE",
#         "ABUZI","BILI","BOSOBOLO","BUSINGA","GBADOLITE","KARAWA","LOKO","MOBAYI","WAPINDA","WASOLO",
#         "YAKOMA","BANGABOLA","BOGOSE-NUBEA","BOKONZI","BOMINENGE","BOTO","BUDJALA","BULU","BWAMANDA",
#         "GEMENA","KUNGU","LIBENGE","MAWUYA","MBAYA","NDAGE","TANDALA","ZONGO","BEFALE","BOENDE","BOKUNGU",
#         "BUSANGA","DJOLU","IKELA","LINGOMO","MOMPONO","MONDOMBE","MONKOTO","WEMA","YALIFAFU")
# hz <- str_to_title(hz)
# hz <- sort(hz)

