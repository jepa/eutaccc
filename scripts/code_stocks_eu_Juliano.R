#library(RPostgreSQL)
library(plyr)
library(dplyr)
library(reshape)
library(ggplot2)
library(sf)
library(sp)
library(raster) 
library(ncdf4)
library(rgdal)
library(maptools)
library(egg)

#options(scipen=10000)
#drv          = dbDriver("PostgreSQL")

#con3         = dbConnect(drv, host = 'localhost',
#                         dbname='oldmydas',
#                         port = 5432,
#                         user = 'clock_admin',
 #                        password = 'tunafish')

#TACs EU waters annex 1 https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:32020R0123#ntr28-L_2020025EN.01003601-E0028

#dealing with norwegian waters
#
eezshp= read_sf("C:\\Clock\\shapefile\\EEZ_land_union_v2_201410\\EEZ_land_v2_201410.shp")
#icesrects      = dbGetQuery(con3, "select * from data_icesrects")
icesrects <- read.delim("C:\\Juliano\\icesrects.txt", header = TRUE, sep = ",")
point = st_as_sf(x = icesrects ,   coords = c("longitude", "latitude"),crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
out   = st_intersection(point, eezshp)

nor   = subset(out, Country=="Norway")
fro   = subset(out, Country=="Faroe Is.")
rus   = subset(out, Country=="Russia")
ice   = subset(out, Country=="Iceland")
svb   = subset(out, Country=="Svalbard")
jmn   = subset(out, Country=="Jan Mayen")
jsy   = subset(out, Country=="Jersey")
gsy   = subset(out, Country=="Guernsey")
noneu = rbind(nor, fro, rus, ice, svb, jmn, jsy, gsy)
noneu = as.data.frame(noneu[c(2,9)])

 
nor=nor%>% dplyr::mutate(lat = sf::st_coordinates(.)[,1],
                lon = sf::st_coordinates(.)[,2])
norway       = as.data.frame(nor)
#icesdiv      = dbGetQuery(con3, "select * from data_icesrects_div")
icesdiv      = read.delim("C:\\Juliano\\rects_divs.txt", header = TRUE, sep = ",")
nordiv       = join(icesdiv[c(1,2,4)], norway, type="inner") 
nordiv       = nordiv[c(1:3,8,11,15,16,17)]
four         = subset(nordiv, ices_division %in% c("4B","4A"))#ices division four norway waters
foura        = subset(nordiv, ices_division %in% c("4A"))
threea       = subset(nordiv, ices_division %in% c("3A"))
twoa         = subset(nordiv, ices_division %in% c("2A"))
twob         = subset(nordiv, ices_division %in% c("2B"))
#onea         = subset(nordiv, ices_division %in% c("1A"))
oneb         = subset(nordiv, ices_division %in% c("1B"))
two          = subset(nordiv, ices_division %in% c("2A","2B"))
frodiv       = inner_join(icesdiv[c(1,2,4)], fro)  %>% mutate(code="faroese")
frodiv       = frodiv[c(1:3,8,11, 15,16)]
twoafoura    = rbind(twoa, foura)
tacs         = read.csv("C:\\Juliano\\EUTACS2020lookup.csv")
tacs$code    = as.character(tacs$code )
tacs$code    = gsub(" ", "", tacs$code)
tacs$species    = as.character(tacs$species )
tacs$species    = gsub(" ", "", tacs$species)
tacs$country    = as.character(tacs$country )
tacs$country    = gsub(" ", "", tacs$country)

#"10A" "9B"  "9A"  "10B" "8E"  "8C"  "8B"  "8D"  "8A"  "12C" "7K"  "7J"  "7H"  "7E"  "7D"  "7G"  "7F"  "4C"  "7A"  "12A" "7C"  "7B"  "4B"  "3D"  "3C"  "6A" 
#"12B" "6B"  "3B"  "3A"  "4A"  "14B" "5B"  "1B"  "5A"  "2A"  "14A" "1A"  "2B" 
icesdiva=right_join(noneu, icesdiv)


area_1_2     = subset(icesdiva, ices_division %in% c("1A","2A", "1B","2B")) %>% mutate(code="area_1_2")
area_3a_4    = subset(icesdiva, ices_division %in% c("3A", "4A","4B","4C")) %>% mutate(code="area_3a_4")
area_5_6_7   = subset(icesdiva, ices_division %in% c("5A","5B","6A","6B","7K","7J","7H","7E","7D","7G","7F","7C","7B","7A")) %>% mutate(code="area_5_6_7")
area_1_2_14  = subset(icesdiva, ices_division %in% c("1A","2A", "1B","2B","14A","14B")) %>% mutate(code="area_1_2_14")
area_3a      = subset(icesdiva, ices_division %in% c("3A")) %>% mutate(code="area_3a")
area_4       = subset(icesdiva, ices_division %in% c("4A","4B","4C"))%>% mutate(code="area_4")
area_6_7_8   = subset(icesdiva, ices_division %in% c("6A","6B","7K","7J","7H","7E","7D","7G","7F","7C","7B","7A","8E","8C","8B","8D","8A")) %>% mutate(code="area_6_7_8")
area_2a_4_7d = subset(icesdiva, ices_division %in% c("4A","4B","4C","7D","2A")) %>% mutate(code="area_2a_4_7d") 
area_4c_7d   = subset(icesdiva, ices_division %in% c("4C","7D")) %>% mutate(code="area_4c_7d")
area_7a      = subset(icesdiva, ices_division %in% c("7A")) %>% mutate(code="area_7a")
area_7e_7f   = subset(icesdiva, ices_division %in% c("7E","7F")) %>% mutate(code="area_7e_7f")
area_7g_7h_7j_7k = subset(icesdiva, ices_division %in% c("7G","7H","7K","7J")) %>% mutate(code="area_7g_7h_7j_7k")
area_8       = subset(icesdiva, ices_division %in% c("8E","8C","8B","8D","8A" )) %>% mutate(code="area_8")
area_7d      = subset(icesdiva, ices_division %in% c("7D")) %>% mutate(code="area_7d")
area_2a_4    = subset(icesdiva, ices_division %in% c("2A","4A","4B","4C")) %>% mutate(code="area_2a_4")
area_7       = subset(icesdiva, ices_division %in% c("7K","7J","7H","7E","7D","7G","7F","7C","7B","7A"))%>% mutate(code="area_7")
area_8a_8b_8d_8e = subset(icesdiva, ices_division %in% c("8E","8B","8D","8A" )) %>% mutate(code="area_8a_8b_8d_8e")
area_5b_6_12_14 = subset(icesdiva, ices_division %in% c("5B","6A","6B","12A","12B","12C","14A","14B")) %>% mutate(code="area_5b_6_12_14")
area_6b_12_14 = subset(icesdiva, ices_division %in% c("6B","12A","12B","12C","14A","14B")) %>% mutate(code="area_6b_12_14")
area_5b_6a   = subset(icesdiva, ices_division %in% c("5B","6A"))%>%mutate(code="area_5b_6a")
area_7b_7c_7d_7e_7f_7g_7h_7j_7k = subset(icesdiva, ices_division %in% c("7K","7J","7H","7E","7D","7G","7F","7C","7B")) %>% mutate(code="area_7b_7c_7d_7e_7f_7g_7h_7j_7k")
area_5b_6_7_12_14 = subset(icesdiva, ices_division %in% c("5B","6A","6B","7K","7J","7H","7E","7D","7G","7F","7C","7B","7A","12A","12B","12C","14A","14B")) %>% mutate(code="area_5b_6_7_12_14")
area_1_2_3_4_5_6_7_8a_8b_8d_8e_12_14 = subset(icesdiva, ices_division %in% c("1A","2A", "1B","2B","3A","3B","4A","4B","4C","5A","5B","6A","6B","7K","7J","7H","7E","7D","7G","7F","7C","7B","7A","8E","8B","8D","8A","12A","12B","12C","14A","14B")) %>% mutate(code = "area_1_2_3_4_5_6_7_8a_8b_8d_8e_12_14")
area_5b_6_7  = subset(icesdiva, ices_division %in% c("5B","6A","6B","7K","7J","7H","7E","7D","7G","7F","7C","7B","7A")) %>% mutate(code= "area_5b_6_7")
area_12      = subset(icesdiva, ices_division %in% c("12A","12B","12C")) %>% mutate(code="area_12")
area_5       = subset(icesdiva, ices_division %in% c("5A","5B"))%>% mutate(code="area_5")
area_6_7_8_9_10_12_14 = subset(icesdiva, ices_division %in% c("6A","6B","7K","7J","7H","7E","7D","7G","7F","7C","7B","7A","8C","8E","8B","8D","8A","9A","9B","10A","10B","12A","12B","12C","14A","14B")) %>% mutate(code="area_6_7_8_9_10_12_14")
area_5b_6     = subset(icesdiva, ices_division %in% c("5B","6A","6B"))%>%mutate(code="area_5b_6")
area_8c      = subset(icesdiva, ices_division %in% c("8C")) %>% mutate(code="area_8c")
area_7d_7e   = subset(icesdiva, ices_division %in% c("7D","7E")) %>% mutate(code="area_7d_7e")
area_7f_7g   = subset(icesdiva, ices_division %in% c("7F","7G")) %>% mutate(code="area_7f_7g")
area_7h_7j_7k= subset(icesdiva, ices_division %in% c("7h","7J","7K")) %>% mutate(code="area_7h_7j_7k")
area_2a_3a_4 = subset(icesdiva, ices_division %in% c("2A","3A","4A","4B","4C"))%>% mutate(code="area_2a_3a_4")
area_6_7a_c_7e_k = subset(icesdiva, ices_division %in% c("6A","6B","7A","7B","7C","7E","7F","7G","7H","7J","7K"))%>% mutate(code="area_6_7a_c_7e_k")
area_8_9     = subset(icesdiva, ices_division %in% c("8E","8C","8B","8D","8A","9A","9B")) %>% mutate(code="area_8_9")
area_2a_4_5_6 = subset(icesdiva, ices_division %in% c("2A","4A","4B","4C","5A","5B","6A","6B")) %>% mutate(code="area_2a_4_5_6")
area_3a_4b_4c = subset(icesdiva, ices_division %in% c("3A","4B","4C")) %>% mutate(code="area_3a_4b_4c")
area_4b       = subset(icesdiva, ices_division %in% c("4B")) %>% mutate(code="area_4b")
area_4c       = subset(icesdiva, ices_division %in% c("4C")) %>% mutate(code="area_4c")
area_2a_5b_6_7_8a_8b_8d_8e_12_14 = subset(icesdiva, ices_division %in% c("2A","5B","6A","6B","7K","7J","7H","7E","7D","7G","7F","7C","7B","7A","8E","8B","8D","8A","12A","12B","12C","14A","14B")) %>% mutate(code="area_2a_5b_6_7_8a_8b_8d_8e_12_14")
area_8c_9_10  = subset(icesdiva, ices_division %in% c("8C","9A","9B","10A","10B")) %>% mutate(code="area_8c_9_10") 
area_8b       = subset(icesdiva, ices_division %in% c("8B")) %>% mutate(code="area_8b")
area_7b_7c    = subset(icesdiva, ices_division %in% c("7B","7C")) %>% mutate(code="area_7b_7c")
area_7e       = subset(icesdiva, ices_division %in% c("7E")) %>% mutate(code="area_7e")
area_8a_8b    = subset(icesdiva, ices_division %in% c("8A","8B")) %>% mutate(code="area_8a_8b")
area_4b_4c_7d = subset(icesdiva, ices_division %in% c("4B","4C","7D")) %>% mutate(code="area_4b_4c_7d")
area_1_5_6_7_8_12_14 = subset(icesdiva, ices_division %in% c("1A","1B","5A","5B","6A","6B","7K","7J","7H","7E","7D","7G","7F","7C","7B","7A","8E","8B","8D","8C","8A","12A","12B","12C","14A","14B")) %>% mutate(code="area_1_5_6_7_8_12_14")
area_2a_4a_5b_6_7a_c_7e_k_8a_8b_8d_8e_12_14 = subset(icesdiva, ices_division %in% c("2A","4A","5B","6A","6B","7A","7B","7C","7E","7F","7G","7H","7J","7K","8E","8B","8D","8A","12A","12B","12C","14A","14B"))%>%mutate(code="area_2a_4a_6_7a_c_7e_k_8a_8b_8d_8e_12_14")
area_8c       = subset(icesdiva, ices_division %in% c("8C")) %>% mutate(code="area_8c")
area_9        = subset(icesdiva, ices_division %in% c("9A","9B"))%>% mutate(code="area_9")
area_2_4      = subset(icesdiva, ices_division %in% c("2A","2B","4A","4B","4C"))%>% mutate(code="area_2_4")

skaggerak     = subset(icesdiva, area_27 %in% c("3.a.20")) %>% mutate(code="skaggerak")
kattegat      = subset(icesdiva, area_27 %in% c("3.a.21")) %>% mutate(code="kattegat")
area_2a_4_ska_kat1 = subset(icesdiva, ices_division %in% c("4A","4B","4C","3A")) %>% mutate(code="area_2a_4_ska_kat")
area_2a_4_ska_kat2 = subset(icesdiva, ices_division %in% c("2A") & is.na(Country)) %>% mutate(code="area_2a_4_ska_kat")
area_2a_4_ska_kat  = rbind(area_2a_4_ska_kat1, area_2a_4_ska_kat2)


norwegian4    = four %>% mutate(code="nor4")#ices division four norway waters
norwegian4    = norwegian4[c(1:6,9)]
nor62s        = subset(nordiv, lat <=62) %>%  mutate(code="nor62s")
nor62s        = nor62s[c(1:6,9)]
norwegian2a   = twoa[c(1:6)] %>% mutate(code="nor2a")
norwegian2a4  = twoa[c(1:6)] %>% mutate(code="nor2a4")
norwegian4a   = subset(norwegian4, ices_division=="4A")
norwegian2a4a   =rbind(norwegian2a,norwegian4a )%>% mutate(code="nor2a4a")

fiveb=area_5b_6a %>% inner_join(icesrects) %>% filter(ices_division %in% c("5B") & longitude >=-12)
sixa =subset(area_5b_6a, ices_division %in% c("6A"))
area_5b12E_6a = rbind(fiveb[c(1:6)], sixa[c(1:6)])%>% mutate(code="area_5b12E_6a")

fiveb1=area_5b_6_12_14 %>% inner_join(icesrects) %>% filter(ices_division %in% c("5B") & longitude <=-12)
sixb =subset(area_5b_6_12_14, ices_division %in% c("6B","12A","12B","12C","14A","14B"))
area_5b12W_6b_12_14 = rbind(fiveb1[c(1:6)], sixb[c(1:6)])%>% mutate(code="area_5b12W_6b_12_14")

twoa1 = area_2a_4 %>% filter(ices_division=="2A" & is.na(Country)) %>% mutate(code="area_2aEU_4")
area_2aEU_4 = area_4 %>% mutate(code="area_2aEU_4") %>% rbind(twoa1)

area_2_4a_5_6N56W191 = subset(icesdiva, ices_division %in% c("2A","2B","4A","5A","5B"))  %>% filter(is.na(Country)) %>% mutate(code="area_2_4a_5_6N56W19")
area_2_4a_5_6N56W192 = subset(icesdiva, ices_division %in% c("6A","6B")) %>% inner_join(icesrects) %>% filter(latitude >=56.5 & longitude >= -19 & longitude <= -12) %>% mutate(code="area_2_4a_5_6N56W19")
area_2_4a_5_6N56W19  = rbind(area_2_4a_5_6N56W191[c(1:7)],area_2_4a_5_6N56W192[c(1:6,13)])

func16               = subset(icesdiva, ices_rectangle %in% c("31D5","32D5","33D5","34D5","35D5", "31D6","32D6","33D6","34D6","35D6","32D7","33D7","34D7","35D7", "32D8","33D8","34D8","35D8")) %>% mutate(code="func16")
area_2a_4_6a565N1    = subset(icesdiva, ices_division %in% c("2A","4A","4B","4C","5A","5B","6A") & is.na(Country)) %>% inner_join(icesrects) %>% mutate(code="area_2a_4_6565N")
area_2a_4_6a565N2    = area_2a_4_6a565N1 %>% filter(ices_division=="6A") %>%  filter(latitude >=56.5)
area_2a_4_6a565N     = area_2a_4_6a565N1[c(1:6,13)] %>%filter(ices_division !="6A") %>% rbind(area_2a_4_6a565N2[c(1:6,13)]) %>% mutate(code="area_2a_4_6a565N")

area_3a_4_2a_3b_3c_22_321 = subset(icesdiva, ices_division %in% c("2A", "3B","3C")) %>% mutate(code="area_3a_4_2a_3b_3c_22_32")
area_3a_4_2a_3b_3c_22_322 = subset(icesdiva, area_27 %in% c("3.c.22", "3.b.23", "3.d.24","3.d.25", "3.d.26", "3.d.27", "3.d.28.1", "3.d.28.2", "3.d.29", "3.d.30", "3.d.31", "3.d.32")) %>% mutate(code="area_3a_4_2a_3b_3c_22_32")
area_3a_4_2a_3b_3c_22_32  = area_3a_4 %>% mutate(code="area_3a_4_2a_3b_3c_22_32") %>% rbind(area_3a_4_2a_3b_3c_22_321) %>% rbind(area_3a_4_2a_3b_3c_22_322)
         
area_3a_22_24 = subset(icesdiva, ices_division =="3A") %>% rbind(subset(icesdiva, area_27 %in% c("3.c.22", "3.b.23", "3.d.24") & is.na(Country))) %>% mutate(code="area_3a_22_24")
area_2a_6     = subset(icesdiva, ices_division %in% c("2A", "6A","6B")) %>% mutate(code="area_2a_6")

area_2a_4a1   = subset(icesdiva, ices_division %in% c("2A") & is.na(Country)) %>% mutate(code="area_2a_4a")
area_2a_4a2   = subset(icesdiva, ices_division %in% c("4A")) %>% mutate(code="area_2a_4a")
area_2a_4a    = rbind(area_2a_4a1,area_2a_4a2)
area_4_535    = subset(icesdiva, ices_division %in% c("4A","4B")) %>% mutate(code="area_4_535")
area_2a_4_7d1 = area_2a_4_7d  %>% filter(ices_division %in% c("4A","4B","4C","7D"))
area_2a_4_7d2 = area_2a_4_7d  %>% filter(ices_division %in% c("2A") & is.na(Country))
area_2aEU_4_7d = rbind(area_2a_4_7d1,area_2a_4_7d2)%>% mutate(code="area_2aEU_4_7d")

area_5b_6b_6aN = subset(icesdiva, ices_division %in% c("5B","6B")) %>% rbind(subset(icesdiva, ices_rectangle %in% c("39E3","40E3","41E3","41E4","42E2","42E3","42E4","43E0","43E1","43E2","43E3","43E4","44E0",
                                                                                          "44E1","44E2","44E3","44E4","45E0","45E1","45E2","45E3","45E4","46E0","46E1","46E2","46E3","46E4",
                                                                                          "46E5","47E0","47E1","47E2","47E3","47E4","47E5","48E0","48E1","48E2","48E3","48E4","48E5","49E5")))%>% mutate(code="area_5b_6b_6aN")

area_6aS_7b_7c = subset(icesdiva, ices_division %in% c("6A")) %>% filter(!(ices_rectangle %in% c("39E3","40E3","41E3","41E4","42E2","42E3","42E4","43E0","43E1","43E2","43E3","43E4","44E0",
                                                                                                                    "44E1","44E2","44E3","44E4","45E0","45E1","45E2","45E3","45E4","46E0","46E1","46E2","46E3","46E4",
                                                                                                                    "46E5","47E0","47E1","47E2","47E3","47E4","47E5","48E0","48E1","48E2","48E3","48E4","48E5","49E5"))) %>% rbind(subset(icesdiva, ices_division %in% c("7B","7C")))%>%mutate(code="area_6aS_7b_7c")

area_2a_3a_41 = area_2a_3a_4 %>% filter(ices_division %in% c("3A","4A","4B","4C"))
area_2a_3a_42 = area_2a_3a_4 %>% filter(ices_division %in% c("2A") & is.na(Country))
area_2a_3a_4  = rbind(area_2a_3a_41,area_2a_3a_42)

area_2a_4_5b_61  = area_2a_4_5_6 %>% filter(ices_division %in% c("5B","6A","6B"))
area_2a_4_5b_62  = area_2a_4_5_6 %>% filter(ices_division %in% c("2A","4A","4B","4C") & is.na(Country))
area_2a_4_5b_6   = rbind(area_2a_4_5b_61,area_2a_4_5b_62) %>% mutate(code="area_2a_4_5b_6")

area_2a_4a_5b_6_7a_c_7e_k_8a_8b_8d_8e_12_141 = area_2a_4a_5b_6_7a_c_7e_k_8a_8b_8d_8e_12_14 %>% filter(ices_division %in% c("2A","4A","6A","6B","7A","7B","7C","7E","7F","7G","7H","7J","7K","8E","8B","8D","8A") & is.na(Country))
area_2a_4a_5b_6_7a_c_7e_k_8a_8b_8d_8e_12_142 = area_2a_4a_5b_6_7a_c_7e_k_8a_8b_8d_8e_12_14 %>% filter(ices_division %in% c("5B", "12A","12B","12C","14A","14B"))
area_2a_4a_5b_6_7a_c_7e_k_8a_8b_8d_8e_12_14  = rbind(area_2a_4a_5b_6_7a_c_7e_k_8a_8b_8d_8e_12_141, area_2a_4a_5b_6_7a_c_7e_k_8a_8b_8d_8e_12_142) %>% mutate(code='area_2a_4a_5b_6_7a_c_7e_k_8a_8b_8d_8e_12_14')

area_2a_3a_411 = area_2a_3a_4 %>% filter(ices_division %in% c("3A"))
area_2a_3a_422 = area_2a_3a_4 %>% filter(ices_division %in% c("2A","4A","4B","4C") & is.na(Country))
area_2a_3a_4nop  = rbind(area_2a_3a_411,area_2a_3a_422) %>% mutate(code="area_2a_3a_4N")


#CEFCAF AREAS - https://eur-lex.europa.eu/legal-content/EN/TXT/PDF/?uri=CELEX:32009R0216&from=en Annex 1
area_8c_9_10_CEFCAF      = subset(icesdiva, ices_division %in% c("8C","9A","9B","10A","10B"))%>% mutate(code="area_8c_9_10_CEFCAF")
area_7b_k_8c_9_10_CEFCAF = subset(icesdiva, ices_division %in% c("8C","7B","7C", "7D","7E","7F","7G","7H","7J","7K","9A","9B","10A","10B"))%>%mutate(code="area_7b_k_8c_9_10_CEFCAF")
area_7_8_9_10_CEFCAF     = subset(icesdiva, ices_division %in% c("7K","7J","7H","7E","7D","7G","7F","7C","7B","7A","8E","8C","8B","8D","8A", "9A","9B","10A","10B")) %>% mutate(code="area_7_8_9_10_CEFCAF")
area_9_10_CEFCAF         = subset(icesdiva, ices_division %in% c("9A","9B","10A","10B"))%>%mutate(code="area_9_10_CEFCAF")
area_8_9_10_CEFCAF       = subset(icesdiva, ices_division %in% c("8E","8C","8B","8D","8A", "9A","9B","10A","10B"))%>%mutate(code="area_8_9_10_CEFCAF")
area_1_8c_8d_8e_9_CEFCAF = subset(icesdiva, ices_division %in% c("1A", "1B","8E","8C","8D", "9A","9B"))%>%mutate(code="area_1_8c_8d_8e_9_CEFCAF")
area_7b_7c_7e_k_8_9_10_CEFCAF = subset(icesdiva, ices_division %in% c("8E","8C","8B","8D","8A","7B","7C", "7E","7F","7G","7H","7J","7K","9A","9B","10A","10B"))%>%mutate(code="area_7b_7c_7e_k_8_9_10_CEFCAF")
area_8c_8d_8e_9_10_CEFCAF      = subset(icesdiva, ices_division %in% c("8C","8D","8E","9A","9B","10A","10B"))%>% mutate(code="area_8c_8d_8e_9_10_CEFCAF")


stocks=rbind(area_1_2, area_3a_4, area_2aEU_4, area_5_6_7, area_1_2_14, area_3a, area_4, area_6_7_8, area_2a_4_7d, area_4c_7d, area_7a,
      area_7e_7f, area_7g_7h_7j_7k, area_4b_4c_7d,norwegian2a4a, area_5b_6b_6aN, area_6aS_7b_7c, area_8, area_7d, area_2a_4, area_7, area_8a_8b_8d_8e, area_5b_6_12_14, area_6b_12_14,
      area_5b_6a, area_7b_7c_7d_7e_7f_7g_7h_7j_7k, area_5b_6_7_12_14,area_1_2_3_4_5_6_7_8a_8b_8d_8e_12_14, area_5b_6_7,
      area_12, area_5, area_6_7_8_9_10_12_14, area_5b_6, area_8c, area_7d_7e, area_7f_7g, area_7h_7j_7k,area_2a_3a_4,
      area_6_7a_c_7e_k, area_2a_4_5b_6, area_8_9, area_2a_4_5_6, area_3a_4b_4c, area_4b, area_4c, area_2a_5b_6_7_8a_8b_8d_8e_12_14, area_8c_9_10,
      area_8b, area_7b_7c, area_7e, area_8a_8b, area_1_5_6_7_8_12_14, area_5b12W_6b_12_14, area_8c, area_2a_4a_5b_6_7a_c_7e_k_8a_8b_8d_8e_12_14,
      area_9, area_2_4, area_5b12E_6a, area_2_4a_5_6N56W19, skaggerak,kattegat, area_2a_4_ska_kat, norwegian4, nor62s, norwegian2a, 
      norwegian2a4, frodiv, area_4_535, area_2aEU_4_7d,func16,area_2a_4_6a565N,area_3a_4_2a_3b_3c_22_32,area_3a_22_24,area_2a_6, area_2a_4a, area_8c_9_10_CEFCAF,area_2a_3a_4nop,
      area_7b_k_8c_9_10_CEFCAF,area_7_8_9_10_CEFCAF,area_9_10_CEFCAF,area_8_9_10_CEFCAF,area_1_8c_8d_8e_9_CEFCAF,area_7b_7c_7e_k_8_9_10_CEFCAF,area_8c_8d_8e_9_10_CEFCAF  )

byspecies=inner_join(tacs, stocks)

ARU_1_2    = byspecies %>% filter(species %in% 'ARU' & code=="area_1_2") %>% mutate(stock="ARU_1_2")#international
ARU_3A_4_C = byspecies %>% filter(species %in% 'ARU' & code=="area_3a_4" & is.na(Country)) %>% mutate(stock="ARU_3A_4_C") #union waters.
ARU_5_6_7  = byspecies %>% filter(species %in% 'ARU' & code=="area_5_6_7") %>% mutate(stock="ARU_5_6_7") 
USK_1_2_14 = byspecies %>% filter(species %in% 'USK' & code=="area_1_2_14") %>% mutate(stock="USK_1_2_14") 
USK_3a     = byspecies %>% filter(species %in% 'USK' & code=="area_3a") %>% mutate(stock="USK_3a") 
USK_4      = byspecies %>% filter(species %in% 'USK' & code=="area_4" & is.na(Country)) %>% mutate(stock="USK_4_C") 
USK_5_6_7  = byspecies %>% filter(species %in% 'USK' & code=="area_5_6_7") %>% mutate(stock="USK_5_6_7") 
USK_4_N    = byspecies %>% filter(species %in% 'USK' & code=="nor4") %>% mutate(stock="USK_4_N") 
BOR_6_7_8  = byspecies %>% filter(species %in% 'BOR' & code=="area_6_7_8") %>% mutate(stock="BOR_6_7_8") 
HER_3A     = byspecies %>% filter(species %in% 'HER' & code=="area_3a") %>% mutate(stock="HER_3A") 
HER_4A_4B  = byspecies %>% filter(species %in% 'HER' & code=="area_4_535") %>% mutate(stock="HER_4A_4B") 
HER_4_N    = byspecies %>% filter(species %in% 'HER' & code=="nor62s") %>% mutate(stock="HER_4_N") 
HER_3A_C   = byspecies %>% filter(species %in% 'HER' & code=="area_3a" & is.na(Country)) %>% mutate(stock="HER_3A_C") 
HER_2A_4_7D= byspecies %>% filter(species %in% 'HER' & code=="area_2aEU_4_7d") %>% mutate(stock="HER_2A_4_7D") 
HER_4C_7D  = byspecies %>% filter(species %in% 'HER' & code=="area_4c_7d") %>% mutate(stock="HER_4C_7D") 
HER_5B_6B_6aN  = byspecies %>% filter(species %in% 'HER' & code=="area_5b_6b_6aN") %>% mutate(stock="HER_5B_6B_6aN") 
HER_6aS_7B_7C = byspecies %>% filter(species %in% 'HER' & code=="area_6aS_7b_7c") %>% mutate(stock="HER_6aS_7B_7C") 
HER_7A     = byspecies %>% filter(species %in% 'HER' & code=="area_7a") %>% mutate(stock="HER_7A") 
HER_7E_7F  = byspecies %>% filter(species %in% 'HER' & code=="area_7e_7f") %>% mutate(stock="HER_7E_7F") 
HER_7G_7H_7J_7K = byspecies %>% filter(species %in% 'HER' & code=="area_7g_7h_7j_7k") %>% mutate(stock="HER_7G_7H_7J_7K") 
ANE_8      = byspecies %>% filter(species %in% 'ANE' & code=="area_8") %>% mutate(stock="ANE_8") 
#ANE_9_10_CEFCAF = byspecies %>% filter(species %in% 'ANE' & code=="area_9_10_CEFCAF") %>% mutate(stock="ANE_9_10_CEFCAF")
COD_3AN    = byspecies %>% filter(species %in% 'COD' & code=="skaggerak") %>% mutate(stock="COD_3AN") 
COD_3AS    = byspecies %>% filter(species %in% 'COD' & code=="kattegat") %>% mutate(stock="COD_3AS") 
COD_2A_3A_4 = byspecies %>% filter(species %in% 'COD' & code=="area_2a_4_ska_kat") %>% mutate(stock="COD_2A_3A_4")
COD_4_N    = byspecies %>% filter(species %in% 'COD' & code=="nor4") %>% mutate(stock="COD_4_N")
COD_4_Ns   = byspecies %>% filter(species %in% 'COD' & code=="nor62s") %>% mutate(stock="COD_4_Ns")
COD_5BW_6B_12_14 = byspecies %>% filter(species %in% 'COD' & code=="area_5b12W_6b_12_14") %>% mutate(stock="COD_5BW_6B_12_14")
COD_5BE_6A = byspecies %>% filter(species %in% 'COD' & code=="area_5b12E_6a") %>% mutate(stock="COD_5BE_6A")
#COD_7XAD34 = byspecies %>% filter(species %in% 'COD' & code=="area_7b_7c_7e_k_8_9_10_CEFCAF") %>% mutate(stock="COD_7XAD34")
COD_7D     = byspecies %>% filter(species %in% 'COD' & code=="area_7d") %>% mutate(stock="COD_7D")
LEZ_2A_4   = byspecies %>% filter(species %in% 'LEZ' & code=="area_2a_4" & is.na(Country)) %>% mutate(stock="LEZ_2A_4")
LEZ_5B_6_12_14 = byspecies %>% filter(species %in% 'LEZ' & code=="area_5b_6_12_14") %>% mutate(stock="LEZ_5B_6_12_14")
LEZ_7      = byspecies %>% filter(species %in% 'LEZ' & code=="area_7") %>% mutate(stock="LEZ_7")
LEZ_8A_8B_8D_8E  = byspecies %>% filter(species %in% 'LEZ' & code=="area_8a_8b_8d_8e") %>% mutate(stock="LEZ_8A_8B_8D_8E")
#LEZ_8C_9_10 = byspecies %>% filter(species %in% 'LEZ' & code=="area_8c_9_10_CEFCAF") %>% mutate(stock="LEZ_8C_9_10")
ANF_2A_4   = byspecies %>% filter(species %in% 'ANF' & code=="area_2a_4" & is.na(Country)) %>% mutate(stock="ANF_2A_4")
ANF_4N     = byspecies %>% filter(species %in% 'ANF' & code=="nor4" ) %>% mutate(stock="ANF_4N")
ANF_5B_6_12_14 = byspecies %>% filter(species %in% 'ANF' & code=="area_5b_6_12_14") %>% mutate(stock="ANF_5B_6_12_14")
ANF_7      = byspecies %>% filter(species %in% 'ANF' & code=="area_7") %>% mutate(stock="ANF_7")
ANF_8A_8B_8D_8E  = byspecies %>% filter(species %in% 'ANF' & code=="area_8a_8b_8d_8e") %>% mutate(stock="ANF_8A_8B_8D_8E")
#ANF_8C_9_10 = byspecies %>% filter(species %in% 'ANF' & code=="area_8c_9_10_CEFCAF") %>% mutate(stock="ANF_8C_9_10")
HAD_3A     = byspecies %>% filter(species %in% 'HAD' & code=="area_3a") %>% mutate(stock="HAD_3A") 
HAD_2A_4   = byspecies %>% filter(species %in% 'HAD' & code=="area_2aEU_4") %>% mutate(stock="HAD_2A_4") 
HAD_4N     = byspecies %>% filter(species %in% 'HAD' & code=="nor4") %>% mutate(stock="HAD_4N") 
HAD_4N62   = byspecies %>% filter(species %in% 'HAD' & code=="nor62s") %>% mutate(stock="HAD_4N62") 
HAD_6B_12_14 = byspecies %>% filter(species %in% 'HAD' & code=="area_6b_12_14") %>% mutate(stock="HAD_6B_12_14")
HAD_5B_6A  = byspecies %>% filter(species %in% 'HAD' & code=="area_5b_6a") %>% mutate(stock="HAD_5B_6A")
HAD_7X7A34 = byspecies %>% filter(species %in% 'HAD' & code=="area_7b_k_8c_9_10_CEFCAF") %>% mutate(stock="HAD_7X7A34")
HAD_7A     = byspecies %>% filter(species %in% 'HAD' & code=="area_7a") %>% mutate(stock="HAD_7A") 
WHG_3A     = byspecies %>% filter(species %in% 'WHG' & code=="area_3a") %>% mutate(stock="WHG_3A") 
WHG_2A_4   = byspecies %>% filter(species %in% 'WHG' & code=="area_2aEU_4") %>% mutate(stock="WHG_2A_4") 
WHG_4N     = byspecies %>% filter(species %in% 'WHG' & code=="nor4") %>% mutate(stock="WHG_4N") 
WHG_5B_6_12_14 = byspecies %>% filter(species %in% 'WHG' & code=="area_5b_6_12_14") %>% mutate(stock="WHG_5B_6_12_14")
WHG_7A     = byspecies %>% filter(species %in% 'WHG' & code=="area_7a") %>% mutate(stock="WHG_7A") 
WHG_7B_7C_7D_7E_7F_7G_7H_7J_7K = byspecies %>% filter(species %in% 'WHG' & code=="area_7b_7c_7d_7e_7f_7g_7h_7j_7k") %>% mutate(stock="WHG_7B_7C_7D_7E_7F_7G_7H_7J_7K")
WHG_8      = byspecies %>% filter(species %in% 'WHG' & code=="area_8") %>% mutate(stock="WHG_8")
WHG_POL_N62 = byspecies %>% filter(species %in% 'WHG/POL' & code=="nor62s") %>% mutate(stock="WHG_POL_N62")
HKE_3A     = byspecies %>% filter(species %in% 'HKE' & code=="area_3a") %>% mutate(stock="HKE_3A") 
HKE_2A_4   = byspecies %>% filter(species %in% 'HKE' & code=="area_2a_4" & is.na(Country)) %>% mutate(stock="HKE_2A_4") 
HKE_5B_6_7_12_14 = byspecies %>% filter(species %in% 'HKE' & code=="area_5b_6_7_12_14") %>% mutate(stock="HKE_5B_6_7_12_14") #Special conditions involved here, careful
HKE_8A_8B_8D_8E  = byspecies %>% filter(species %in% 'HKE' & code=="area_8a_8b_8d_8e") %>% mutate(stock="HKE_8A_8B_8D_8E") #Special conditions involved here, careful
#HKE_8C_9_10 = byspecies %>% filter(species %in% 'HKE' & code=="area_8c_9_10_CEFCAF") %>% mutate(stock="HKE_8C_9_10")
WHB_1_2_3_4_5_6_7_8A_8B_8D_8E_12_14  = byspecies %>% filter(species %in% 'WHB' & code=="area_1_2_3_4_5_6_7_8a_8b_8d_8e_12_14") %>% mutate(stock="WHB_1_2_3_4_5_6_7_8A_8B_8D_8E_12_14") 
#WHB_8C_9_10 = byspecies %>% filter(species %in% 'WHB' & code=="area_8c_9_10_CEFCAF") %>% mutate(stock="WHB_8C_9_10")
WHB_2_4A_5_6N56W19 = byspecies %>% filter(species %in% 'WHB' & code=="area_2_4a_5_6N56W19") %>% mutate(stock="WHB_2_4A_5_6N56W19") 
LEM_WIT_2A_4   = byspecies %>% filter(species %in% 'LEM/WIT' & code=="area_2a_4" & is.na(Country)) %>% mutate(stock="LEM_WIT_2A_4") 
BLI_5B_6_7 = byspecies %>% filter(species %in% 'BLI' & code=="area_5b_6_7") %>% mutate(stock="BLI_5B_6_7") 
BLI_12     = byspecies %>% filter(species %in% 'BLI' & code=="area_12") %>% mutate(stock="BLI_12")
BLI_2_4    = byspecies %>% filter(species %in% 'BLI' & code=="area_2_4") %>% mutate(stock="BLI_2_4") 
BLI_3A     = byspecies %>% filter(species %in% 'BLI' & code=="area_3a") %>% mutate(stock="BLI_3A") 
LIN_1_2    = byspecies %>% filter(species %in% 'LIN' & code=="area_1_2") %>% mutate(stock="LIN_1_2") 
LIN_3A     = byspecies %>% filter(species %in% 'LIN' & code=="area_3a" & is.na(Country)) %>% mutate(stock="LIN_3A") 
LIN_4      = byspecies %>% filter(species %in% 'LIN' & code=="area_4" & is.na(Country)) %>% mutate(stock="LIN_4") 
LIN_5      = byspecies %>% filter(species %in% 'LIN' & code=="area_5") %>% mutate(stock="LIN_5") 
LIN_6_7_8_9_10_12_14 = byspecies %>% filter(species %in% 'LIN' & code=="area_6_7_8_9_10_12_14") %>% mutate(stock="LIN_6_7_8_9_10_12_14") 
LIN_4N     = byspecies %>% filter(species %in% 'LIN' & code=="nor4") %>% mutate(stock="LIN_4N") 
NEP_3A     = byspecies %>% filter(species %in% 'NEP' & code=="area_3a") %>% mutate(stock="NEP_3A") 
NEP_2A_4   = byspecies %>% filter(species %in% 'NEP' & code=="area_2a_4" & is.na(Country)) %>% mutate(stock="NEP_2A_4") 
NEP_4N     = byspecies %>% filter(species %in% 'NEP' & code=="nor4") %>% mutate(stock="NEP_4N") 
NEP_5B_6   = byspecies %>% filter(species %in% 'NEP' & code=="area_5b_6") %>% mutate(stock="NEP_5B_6") 
NEP_7      = byspecies %>% filter(species %in% 'NEP' & code=="area_7") %>% mutate(stock="NEP_7") 
NEP_8A_8B_8D_8E  = byspecies %>% filter(species %in% 'NEP' & code=="area_8a_8b_8d_8e") %>% mutate(stock="NEP_8A_8B_8D_8E") 
NEP_8C      = byspecies %>% filter(species %in% 'NEP' & code=="area_8c") %>% mutate(stock="NEP_8C") 
#NEP_9_10_CEFCAF = byspecies %>% filter(species %in% 'NEP' & code=="area_9_10_CEFCAF") %>% mutate(stock="NEP_9_10_CEFCAF")
PRA_3A     = byspecies %>% filter(species %in% 'PRA' & code=="area_3a") %>% mutate(stock="PRA_3A") 
PRA_2A_4   = byspecies %>% filter(species %in% 'PRA' & code=="area_2a_4" & is.na(Country)) %>% mutate(stock="PRA_2A_4") 
PRA_N      = byspecies %>% filter(species %in% 'PRA' & code=="nor62s") %>% mutate(stock="PRA_N") 
PLE_3AN    = byspecies %>% filter(species %in% 'PLE' & code=="skaggerak") %>% mutate(stock="PLE_3AN") 
PLE_3AS    = byspecies %>% filter(species %in% 'PLE' & code=="kattegat") %>% mutate(stock="PLE_3AS") 
PLE_2A_4_SKA_KAT    = byspecies %>% filter(species %in% 'PLE' & code=="area_2a_4_ska_kat") %>% mutate(stock="PLE_2A_4_SKA_KAT") 
PLE_4N     = byspecies %>% filter(species %in% 'PLE' & code=="nor4") %>% mutate(stock="PLE_4N") 
PLE_5B_6_12_14 = byspecies %>% filter(species %in% 'PLE' & code=="area_5b_6_12_14") %>% mutate(stock="PLE_5B_6_12_14")
PLE_7A     = byspecies %>% filter(species %in% 'PLE' & code=="area_7a") %>% mutate(stock="PLE_7A") 
PLE_7B_7C  = byspecies %>% filter(species %in% 'PLE' & code=="area_7b_7c") %>% mutate(stock="PLE_7B_7C") 
PLE_7D_7E  = byspecies %>% filter(species %in% 'PLE' & code=="area_7d_7e") %>% mutate(stock="PLE_7D_7E") 
PLE_7F_7G  = byspecies %>% filter(species %in% 'PLE' & code=="area_7f_7g") %>% mutate(stock="PLE_7F_7G") 
PLE_7H_7J_7K  = byspecies %>% filter(species %in% 'PLE' & code=="area_7h_7j_7k") %>% mutate(stock="PLE_7H_7J_7K") 
#PLE_8_9_10_CEFCAF  = byspecies %>% filter(species %in% 'PLE' & code=="area_8_9_10_CEFCAF") %>% mutate(stock="PLE_8_9_10_CEFCAF") 
POL_5B_6_12_14 = byspecies %>% filter(species %in% 'POL' & code=="area_5b_6_12_14") %>% mutate(stock="POL_5B_6_12_14")
POL_7      = byspecies %>% filter(species %in% 'POL' & code=="area_7") %>% mutate(stock="POL_7") 
POL_8A_8B_8D_8E  = byspecies %>% filter(species %in% 'POL' & code=="area_8a_8b_8d_8e") %>% mutate(stock="POL_8A_8B_8D_8E") 
POL_8C     = byspecies %>% filter(species %in% 'POL' & code=="area_8c") %>% mutate(stock="POL_8C") 
#POL_9_10_CEFCAF = byspecies %>% filter(species %in% 'POL' & code=="area_9_10_CEFCAF") %>% mutate(stock="POL_9_10_CEFCAF")
POK_2A_3A_4 = byspecies %>% filter(species %in% 'POK' & code=="area_2a_3a_4") %>% mutate(stock="POK_2A_3A_4") 
POK_5B_6_12_14 = byspecies %>% filter(species %in% 'POK' & code=="area_5b_6_12_14") %>% mutate(stock="POK_5B_6_12_14")
POK_N      = byspecies %>% filter(species %in% 'POK' & code=="nor62s") %>% mutate(stock="POK_N") 
#POK_7_8_9_10_CEFCAF  = byspecies %>% filter(species %in% 'POK' & code=="area_7_8_9_10_CEFCAF") %>% mutate(stock="POK_7_8_9_10_CEFCAF") 
TUR_BLL_2A_4   = byspecies %>% filter(species %in% 'TUR/BLL' & code=="area_2a_4" & is.na(Country)) %>% mutate(stock="TUR_BLL_2A_4")
SRX_2A_4   = byspecies %>% filter(species %in% 'SRX' & code=="area_2a_4" & is.na(Country)) %>% mutate(stock="SRX_2A_4")
SRX_3A     = byspecies %>% filter(species %in% 'SRX' & code=="area_3a" & is.na(Country)) %>% mutate(stock="SRX_3A")
SRX_6_7A_C_7E_K = byspecies %>% filter(species %in% 'SRX' & code=="area_6_7a_c_7e_k" & is.na(Country)) %>% mutate(stock="SRX_6_7A_C_7E_K")
SRX_7D     = byspecies %>% filter(species %in% 'SRX' & code=="area_7d" & is.na(Country)) %>% mutate(stock="SRX_7D")
SRX_8_9     = byspecies %>% filter(species %in% 'SRX' & code=="area_8_9" & is.na(Country)) %>% mutate(stock="SRX_8_9")
RJU_7D_7E   = byspecies %>% filter(species %in% 'RJU' & code=="area_7d_7e" & is.na(Country)) %>% mutate(stock="RJU_7D_7E")
GHL_2A_4_5b_6   = byspecies %>% filter(species %in% 'GHL' & code=="area_2a_4_5b_6") %>% mutate(stock="GHL_2A_4_5_6")
MAC_3A_4_2A_3B_3C_22_32  = byspecies %>% filter(species %in% 'MAC' & code=="area_3a_4_2a_3b_3c_22_32") %>% mutate(stock="MAC_3A_4_2A_3B_3C_22_32")
MAC_3A  = byspecies %>% filter(species %in% 'MAC' & code=="area_3a") %>% mutate(stock="MAC_3A")
MAC_3A_4B_4C  = byspecies %>% filter(species %in% 'MAC' & code=="area_3a_4b_4c") %>% mutate(stock="MAC_3A_4B_4C")
MAC_4B  = byspecies %>% filter(species %in% 'MAC' & code=="area_4b") %>% mutate(stock="MAC_4B")
MAC_4C  = byspecies %>% filter(species %in% 'MAC' & code=="area_4c") %>% mutate(stock="MAC_4C")
MAC_2A_6  = byspecies %>% filter(species %in% 'MAC' & code=="area_2a_6") %>% mutate(stock="MAC_2A_6")
MAC_2A_5B_6_7_8A_8B_8D_8E_12_14  = byspecies %>% filter(species %in% 'MAC' & code=="area_2a_5b_6_7_8a_8b_8d_8e_12_14") %>% mutate(stock="MAC_2A_5B_6_7_8A_8B_8D_8E_12_14")
MAC_2A_4A     = byspecies %>% filter(species %in% 'MAC' & code=="area_2a_4a") %>% mutate(stock="MAC_2A_4A")
MAC_2AN       = byspecies %>% filter(species %in% 'MAC' & code=="nor2a") %>% mutate(stock="MAC_2AN")
MAC_FAR       = byspecies %>% filter(species %in% 'MAC' & code=="faroese") %>% mutate(stock="MAC_FAR")
#MAC_8C_9_10_CEFCAF  = byspecies %>% filter(species %in% 'MAC' & code=="area_8c_9_10_CEFCAF") %>% mutate(stock="MAC_8C_9_10_CEFCAF")
MAC_8B        = byspecies %>% filter(species %in% 'MAC' & code=="area_8b") %>% mutate(stock="MAC_8B")
MAC_2A_4A     = byspecies %>% filter(species %in% 'MAC' & code=="nor2a4a") %>% mutate(stock="MAC_2A_4A")
SOL_3A_22_24  = byspecies %>% filter(species %in% 'SOL' & code=="area_3a_22_24") %>% mutate(stock="SOL_3A_22_24")
SOL_2A_4      = byspecies %>% filter(species %in% 'SOL' & code=="area_2a_4" & is.na(Country)) %>% mutate(stock="SOL_2A_4")
SOL_5B_6_12_14  = byspecies %>% filter(species %in% 'SOL' & code=="area_5b_6_12_14") %>% mutate(stock="SOL_5B_6_12_14")
SOL_7A  = byspecies %>% filter(species %in% 'SOL' & code=="area_7a") %>% mutate(stock="SOL_7A")
SOL_7B_7C  = byspecies %>% filter(species %in% 'SOL' & code=="area_7b_7c") %>% mutate(stock="SOL_7B_7C")
SOL_7D  = byspecies %>% filter(species %in% 'SOL' & code=="area_7d") %>% mutate(stock="SOL_7D")
SOL_7E  = byspecies %>% filter(species %in% 'SOL' & code=="area_7e") %>% mutate(stock="SOL_7E")
SOL_7F_7G  = byspecies %>% filter(species %in% 'SOL' & code=="area_7f_7g") %>% mutate(stock="SOL_7F_7G")
SOL_7H_7J_7K  = byspecies %>% filter(species %in% 'SOL' & code=="area_7h_7j_7k") %>% mutate(stock="SOL_7H_7J_7K")
SOL_8A_8B  = byspecies %>% filter(species %in% 'SOL' & code=="area_8a_8b") %>% mutate(stock="SOL_8A_8B")
#SOO_8C_8D_8E_9_10_CEFCAF  = byspecies %>% filter(species %in% 'SOO' & code=="area_8c_8d_8e_9_10_CEFCAF") %>% mutate(stock="SOO_8C_8D_8E_9_10_CEFCAF")
SPR_3A  = byspecies %>% filter(species %in% 'SPR' & code=="area_3a") %>% mutate(stock="SPR_3A")
SPR_7D_7E      = byspecies %>% filter(species %in% 'SPR' & code=="area_7d_7e") %>% mutate(stock="SPR_7D_7E")
DGS_1_5_6_7_8_12_14      = byspecies %>% filter(species %in% 'DGS' & code=="area_1_5_6_7_8_12_14") %>% mutate(stock="DGS_1_5_6_7_8_12_14")
JAX_4B_4C_7D      = byspecies %>% filter(species %in% 'JAX' & code=="area_4b_4c_7d" ) %>% mutate(stock="JAX_4B_4C_7D")
JAX_2A_4A_5B_6_7A_C_7E_K_8A_8B_8D_8E_12_14  = byspecies %>% filter(species %in% 'JAX' & code=="area_2a_4a_5b_6_7a_c_7e_k_8a_8b_8d_8e_12_14" ) %>% mutate(stock="JAX_2A_4A_5B_6_7A_C_7E_K_8A_8B_8D_8E_12_14")
JAX_8C      = byspecies %>% filter(species %in% 'JAX' & code=="area_8c" ) %>% mutate(stock="JAX_8C")
JAX_9       = byspecies %>% filter(species %in% 'JAX' & code=="area_9" ) %>% mutate(stock="JAX_9")
NOP_2a_3a_4N  = byspecies %>% filter(species %in% 'NOP' & code=="area_2a_3a_4N" ) %>% mutate(stock="NOP_2a_3a_4N")
IND_4N  = byspecies %>% filter(species %in% 'Industrial_fish' & code=="nor4" ) %>% mutate(stock="IND_4N")
OTH_5B_6_7  = byspecies %>% filter(species %in% 'Other_species' & code=="area_5b_6_7"  & is.na(Country)) %>% mutate(stock="OTH_5B_6_7")
OTH_N4  = byspecies %>% filter(species %in% 'Other species' & code=="nor4") %>% mutate(stock="OTH_N4")
OTH_2A_4_6A565N  = byspecies %>% filter(species %in% 'Other species' & code=="area_2a_4_6a565N") %>% mutate(stock="OTH_2A_4_6A565N")
NEP_F16  = byspecies %>% filter(species %in% 'NEP' & code=="func16") %>% mutate(stock="NEP_F16")
RJE_7F_7G  = byspecies %>% filter(species %in% 'RJE' & code=="area_7f_7g" & is.na(Country)) %>% mutate(stock="RJE_7F_7G")
RJU_8   = byspecies %>% filter(species %in% 'RJU' & code=="area_8" & is.na(Country)) %>% mutate(stock="RJU_8")
RJU_9   = byspecies %>% filter(species %in% 'RJU' & code=="area_9" & is.na(Country)) %>% mutate(stock="RJU_9")
MAC_2AN   = byspecies %>% filter(species %in% 'MAC' & code=="nor2a" ) %>% mutate(stock="MAC_2AN")
MAC_FAR   = byspecies %>% filter(species %in% 'MAC' & code=="faroese" ) %>% mutate(stock="MAC_FAR")

alldat = rbind(ARU_1_2,
      ARU_3A_4_C,
      ARU_5_6_7,
      USK_1_2_14,
      USK_3a,
      USK_4,
      USK_5_6_7,
      USK_4_N,
      BOR_6_7_8,
      HER_3A,
      HER_4A_4B,
      HER_4_N,
      HER_3A_C,
      HER_2A_4_7D,
        HER_4C_7D,
      HER_5B_6B_6aN,
      HER_6aS_7B_7C,
      HER_7A,
      HER_7E_7F,
      HER_7G_7H_7J_7K,
      ANE_8,
      #ANE_9_10_CEFCAF,
      COD_3AN,
      COD_3AS,
      COD_2A_3A_4,
      COD_4_N,
      COD_4_Ns,
      COD_5BW_6B_12_14,
      COD_5BE_6A,
      #COD_7XAD34,
      COD_7D,
      LEZ_2A_4,
      LEZ_5B_6_12_14,
      LEZ_7,
      LEZ_8A_8B_8D_8E,
      #LEZ_8C_9_10,
      ANF_2A_4,
      ANF_4N,
      ANF_5B_6_12_14,
      ANF_7,
      ANF_8A_8B_8D_8E,
      #ANF_8C_9_10,
      HAD_3A,
      HAD_2A_4,
      HAD_4N,
      HAD_4N62,
      HAD_6B_12_14,
      HAD_5B_6A,
      HAD_7X7A34,
      HAD_7A,
      WHG_3A,
      WHG_2A_4,
      WHG_4N,
      WHG_5B_6_12_14,
      WHG_7A,
      WHG_7B_7C_7D_7E_7F_7G_7H_7J_7K,
      WHG_8,
      WHG_POL_N62,
      HKE_3A,
      HKE_2A_4,
      HKE_5B_6_7_12_14,
      HKE_8A_8B_8D_8E,
      #HKE_8C_9_10,
      WHB_1_2_3_4_5_6_7_8A_8B_8D_8E_12_14,
      #WHB_8C_9_10,
      WHB_2_4A_5_6N56W19,
      LEM_WIT_2A_4,
      BLI_5B_6_7,
      BLI_12,
      BLI_2_4,
      BLI_3A,
      LIN_1_2,
      LIN_3A,
      LIN_4,
      LIN_5,
      LIN_6_7_8_9_10_12_14,
      LIN_4N,
      NEP_3A,
      NEP_2A_4,
      NEP_4N,
      NEP_5B_6,
      NEP_7,
      NEP_8A_8B_8D_8E,
      NEP_8C,
      #NEP_9_10_CEFCAF,
      PRA_3A,
      PRA_2A_4,
      PRA_N,
      PLE_3AN,
      PLE_3AS,
      PLE_2A_4_SKA_KAT,
      PLE_4N,
      PLE_5B_6_12_14,
      PLE_7A,
      PLE_7B_7C,
      PLE_7D_7E,
      PLE_7F_7G,
      PLE_7H_7J_7K,
      #PLE_8_9_10_CEFCAF,
      POL_5B_6_12_14,
      POL_7,
      POL_8A_8B_8D_8E,
      POL_8C,
      #POL_9_10_CEFCAF,
      POK_2A_3A_4,
      POK_5B_6_12_14,
      POK_N,
      #POK_7_8_9_10_CEFCAF,
      TUR_BLL_2A_4,
      SRX_2A_4,
      SRX_3A,
      SRX_6_7A_C_7E_K,
      SRX_7D,
      SRX_8_9,
      RJU_7D_7E,
      GHL_2A_4_5b_6,
      MAC_3A_4_2A_3B_3C_22_32,
      MAC_3A,
      MAC_3A_4B_4C,
      MAC_4B,
      MAC_4C,
      MAC_2A_6,
      MAC_2A_5B_6_7_8A_8B_8D_8E_12_14,
      MAC_2A_4A,
      MAC_2AN,
      MAC_FAR,
      #MAC_8C_9_10_CEFCAF,
      MAC_8B,
      MAC_2A_4A,
      SOL_3A_22_24,
      SOL_2A_4,
      SOL_5B_6_12_14,
      SOL_7A,
      SOL_7B_7C,
      SOL_7D,
      SOL_7E,
      SOL_7F_7G,
      SOL_7H_7J_7K,
      SOL_8A_8B,
      #SOO_8C_8D_8E_9_10_CEFCAF,
      SPR_3A,
      SPR_7D_7E,
      DGS_1_5_6_7_8_12_14,
      JAX_4B_4C_7D,
      JAX_2A_4A_5B_6_7A_C_7E_K_8A_8B_8D_8E_12_14,
      JAX_8C,
      JAX_9,
      NOP_2a_3a_4N,
      IND_4N,
      OTH_5B_6_7,
      OTH_N4,
      OTH_2A_4_6A565N,
      NEP_F16,
      RJE_7F_7G,
      RJU_8,
      RJU_9,
      MAC_2AN,
      MAC_FAR
)

alldat2 = alldat[c(1:7,10,12,13)] %>% filter(!(conditions %in% c("special conditions")))
alldat3 = distinct(alldat2[c(1:5,7:10)])

coords=read.csv("C:\\Clock\\ices_rectangles.csv")
colnames(coords) = c("id","ices_rectangle","south","west","north","east","area_km2")
coords$ices_rectangle  =  as.character(coords$ices_rectangle)
mergerects = inner_join(alldat3,coords)
mergerects$country=as.character(mergerects$country)


mergerects$species = as.character(mergerects$species)
mergerects$latin=ifelse(mergerects$species %in% c("ARU"), "Argentina silus", 
ifelse(mergerects$species %in% c("USK"), "Brosme brosme",     
ifelse(mergerects$species %in% c("BOR"), "Caproidae",        
ifelse(mergerects$species %in% c("HER"), "Clupea harengus",
ifelse(mergerects$species %in% c("COD"),"Gadus morhua", 
ifelse(mergerects$species %in% c("ANE"),"Engraulis encrasicolus", 
ifelse(mergerects$species %in% c("LEZ"), "Lepidorhombus spp.",  
ifelse(mergerects$species %in% c("ANF"), "Lophiidae", 
ifelse(mergerects$species %in% c("WHG/POL"), "Merlangius merlangus/Pollachius pollachius", 
ifelse(mergerects$species %in% c("WHG"), "Merlangius merlangus",      
ifelse(mergerects$species %in% c("POL"),"Pollachius pollachius", 
ifelse(mergerects$species %in% c("HKE"),"Merluccius merluccius",      
ifelse(mergerects$species %in% c("WHB"),"Micromesistius poutassou",       
ifelse(mergerects$species %in% c("LEM/WIT"),"Microstomus kitt/Glyptocephalus cynoglossus",    
ifelse(mergerects$species %in% c("LEM"),"Microstomus kitt", 
ifelse(mergerects$species %in% c("WIT"),"Glyptocephalus cynoglossus",   
ifelse(mergerects$species %in% c("BLI"),"Molva dypterygia",      
ifelse(mergerects$species %in% c("LIN"),"Molva molva",    
ifelse(mergerects$species %in% c("NEP"),"Nephrops norvegicus",      
ifelse(mergerects$species %in% c("PRA"),"Pandalus borealis",    
ifelse(mergerects$species %in% c("TUR/BLL"),"Psetta maxima/Scophthalmus rhombus",      
ifelse(mergerects$species %in% c("SRX"),"Rajiformes",      
ifelse(mergerects$species %in% c("POK"),"Pollachius virens",   
ifelse(mergerects$species %in% c("RJU"),"Raja undulata", 
ifelse(mergerects$species %in% c("GHL"),"Reinhardtius hippoglossoides",  
ifelse(mergerects$species %in% c("MAC"),"Scomber scombrus",  
ifelse(mergerects$species %in% c("SOL"),"Solea solea",       
ifelse(mergerects$species %in% c("SPR"),"Sprattus sprattus",   
ifelse(mergerects$species %in% c("SOO"),"Solea spp.",          
ifelse(mergerects$species %in% c("RJE"),"Raja microocellata",            
ifelse(mergerects$species %in% c("DGS"),"Squalus acanthias",       
ifelse(mergerects$species %in% c("Other_fish"),"Other",
ifelse(mergerects$species %in% c("Industrial_fish"),"Industrial",       
ifelse(mergerects$species %in% c("JAX"),"Trachurus spp.",
ifelse(mergerects$species %in% c("NOP"),"Trisopterus esmarkii",
ifelse(mergerects$species %in% c("PLE"),"Pleuronectes platessa",  
ifelse(mergerects$species %in% c("HAD"),"Melanogrammus aeglefinus",         
       mergerects$species)))))))))))))))))))))))))))))))))))))

mergerects$colour[mergerects$country=="IRL"]="#5F9EA0"  
mergerects$colour[mergerects$country=="UK"] ="#E1B378"
mergerects$colour[mergerects$country=="FRA"]="#40b8d0"  
mergerects$colour[mergerects$country=="NLD"]="#b2d183" 
mergerects$colour[mergerects$country=="NOR"]="#56B4E9"  
mergerects$colour[mergerects$country=="DEN"]="#F0E442"
mergerects$colour[mergerects$country=="GER"]="#599ad3"  
mergerects$colour[mergerects$country=="ESP"]="#f9a65a" 
mergerects$colour[mergerects$country=="POR"]="#9e66ab" 
mergerects$colour[mergerects$country=="BEL"]="#00AFBB"
mergerects$colour[mergerects$country=="SWE"]="#E7B800" 
mergerects$colour[mergerects$country=="POL"]="#FC4E07" 
mergerects$colour[mergerects$country=="LAT"]="#green" 
mergerects$colour[mergerects$country=="LIT"]="red"
mergerects$colour[mergerects$country=="EST"]="yellow" 
mergerects$colour[mergerects$country=="Others"]="black" 
mergerects$colour[mergerects$country=="FRO"]="#17BECF"
#
#saveRDS(mergerects, "C:\\EUTAC\\eudat.RDS")

map <- map_data("world")
dattest = subset(mergerects,stock %in% ("SOL_7E"))

 p1 = ggplot(map, aes(long, lat, group=group )) + 
  geom_polygon(fill="lightgrey") + 
  coord_quickmap(xlim = c(-50,50), ylim=c(25,75))+
  geom_rect(data=dattest, mapping=aes(xmin=west, xmax=east, ymin=south, ymax=north),  fill="yellow", alpha=0.2,inherit.aes = FALSE )+theme_bw()
  



##17BECF", "#BCBD22", "#7F7F7F", "#CFECF9", "#8C564B", "#9467BD", "#D62728", "#2CA02C", "#FF7F0E", "#1F77B4"
forplot = distinct(dattest[c(1:5,16,17)])
forplot=forplot%>%group_by(species) %>%
  
  mutate(pct = quantity/sum(quantity)*100,
         lbl = scales::percent(pct))

p2=ggplot(forplot, aes(x=country,y = quantity, fill=country)) +
  geom_bar(stat = "identity") + facet_wrap(~division+latin)+
  scale_fill_manual(values = c(forplot$colour))+
  theme_classic()+  
  theme(strip.background=element_blank(),strip.text = element_text(size=9),legend.position="none") + 
  ylab("tonnes")

p3=ggplot(forplot, aes(x=country,y = pct, fill=country)) +
  geom_bar(stat = "identity") + facet_wrap(~division+latin)+
  scale_fill_manual(values = c(forplot$colour))+
  theme_classic()+  
  theme(strip.background=element_blank(),strip.text = element_text(size=9),legend.position="none") + 
  ylab("%")
ggarrange(p2,p3,p1,nrow=2)






