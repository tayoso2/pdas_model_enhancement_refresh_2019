x_tenm <- nearest_blockage_10m_age 
colnames(x_tenm)[colnames(x_tenm) == "pipe_mat.x"] <- "pipe_mat"
colnames(x_tenm)[colnames(x_tenm) == "pipe_diam.x"] <- "pipe_diam"
colnames(x_tenm)[colnames(x_tenm) == "length.x"] <- "length"
str(x_tenm)
str(pipe_x2)
sum(x_tenm$incidents)

#by pipe-type, length and diameter#####
pipe_x22 <- pipe_x1
pipe_x22$pipe_diam[which(is.na(pipe_x22$pipe_diam))] <- median(pipe_x22$pipe_diam, na.rm = TRUE)
pipe_x22$pipe_diam[pipe_x22$pipe_diam <= 50] <- 50
pipe_x22$pipe_mat[which(is.na(pipe_x22$pipe_mat))] <- "VC"
pipe_x22$pipe_typ[which(is.na(pipe_x22$pipe_typ))] <- "F"
pipe_x22$pipe_typ <- as.character(pipe_x22$pipe_typ)
pipe_x22 <- pipe_x22 %>% dplyr::filter(pipe_diam <= 225)
summary(pipe_x22)
str(pipe_x22) #151000 x 6

pipe_x22 <- as.data.frame(pipe_x22)
x_tenm <- as.data.frame(x_tenm)
pipe_x22$pipe_mat <- as.character(pipe_x22$pipe_mat)
x_tenm$pipe_mat <- as.character(x_tenm$pipe_mat)
x_tenm$pipe_typ <- as.character(x_tenm$pipe_typ)

xtenm <- x_tenm 
xtenm$pipe_mat <- as.character(xtenm$pipe_mat) 
pipe_x22$pipe_mat <- as.character(pipe_x22$pipe_mat) 
xtenm$pipe_typ <- replace(xtenm$pipe_typ, xtenm$pipe_typ != "S", "C") #change pipe type to C or S
pipe_x22$pipe_typ <- replace(pipe_x22$pipe_typ, pipe_x22$pipe_typ != "S", "C") #change pipe type to C or S
xtenm$pipe_mat <- replace(xtenm$pipe_mat, xtenm$pipe_mat == "vc", "VC")
pipe_x22$pipe_mat <- replace(pipe_x22$pipe_mat, pipe_x22$pipe_mat == "vc", "VC")
xtenm$pipe_mat <- replace(xtenm$pipe_mat, xtenm$pipe_mat == "V C", "VC")
pipe_x22$pipe_mat <- replace(pipe_x22$pipe_mat, pipe_x22$pipe_mat == "V C", "VC")
unique(pipe_x22$pipe_mat)
unique(xtenm$pipe_mat)



find_greater_equals_2 <- function(d,e,t,m,y){
  pipe_x22 <- as.data.frame(pipe_x22)
  y <- as.data.frame(y)
  pipe_x22$pipe_mat <- as.character(pipe_x22$pipe_mat)
  y$pipe_mat <- as.character(y$pipe_mat)
  v <- pipe_x22 %>%
    dplyr::filter((pipe_diam <= d & pipe_mat == m & pipe_typ == t) & (pipe_diam > e & pipe_mat == m & pipe_typ == t)) %>%
    dplyr::group_by(pipe_mat) %>%
    dplyr::summarise(sum.l = sum(length))
  z <- y %>%
    dplyr::filter((pipe_diam <= d & pipe_mat == m & pipe_typ == t) & (pipe_diam > e & pipe_mat == m & pipe_typ == t)) %>%
    dplyr::group_by(pipe_mat) %>%
    dplyr::summarise(incidents = sum(incidents)) %>%
    left_join(v,by = c("pipe_mat")) %>%
    mutate(Failure.rate = (incidents/(sum.l/1000))/4)
  print(z)
  paste0(v$sum.l, " is the sum of length")
}


find_greater_equals_2(75,0,"C","PF",x_tenm)
find_greater_equals_2(75,0,"S","PF",x_tenm)
find_greater_equals_2(100,75,"C","PF",x_tenm)
find_greater_equals_2(100,75,"S","PF",x_tenm)
find_greater_equals_2(150,100,"C","PF",x_tenm)
find_greater_equals_2(150,100,"S","PF",x_tenm)
find_greater_equals_2(225,150,"C","PF",x_tenm)
find_greater_equals_2(225,150,"S","PF",x_tenm)


blockage_new <- data.table(read.csv("new blockages data/Performance - Data - Blockages v2 filtered.csv", header=TRUE, stringsAsFactors=FALSE))
pcd <- data.table(read.csv("new blockages data/postcodes_data.csv", header=TRUE, stringsAsFactors=FALSE))
pcd <- pcd[,c(1,7:8)]
colnames(blockage_new)[colnames(blockage_new) == "CAUSE"] <- "IRF.Blockage.Cause"
colnames(blockage_new)[colnames(blockage_new) == "CATEGORY"] <- "IRF.Other.Blockage.Cause"
colnames(blockage_new)[colnames(blockage_new) == "INCDNTDATE"] <- "IRF.Incident.date..post.PDaS."
colnames(blockage_new)[colnames(blockage_new) == "TRANSASSET"] <- "IRF.Transferred.Asset"
colnames(blockage_new)[colnames(blockage_new) == "ASSETTYPE"] <- "IRF.Cause.Asset.Type"
pcd$Postcode <- gsub("[[:blank:]]", "", pcd$PCD)
blockage_new$POSTCODE <- gsub("[[:blank:]]", "", blockage_new$POSTCODE)
tail(pcd)
blockage_new <- blockage_new %>%
  left_join(unique(pcd),by = c("pipe_mat"="MaterialTable_UnbandedMaterial")) %>% 
  dplyr::mutate(pipe_mat2 = (pipe_mat = as.character(MaterialTable_BandedMaterial)))

colnames(blockage_new)[colnames(blockage_new) == "Grid.Ref.X"] <- "longitude"
colnames(blockage_new)[colnames(blockage_new) == "Grid.Ref.Y"] <- "latitude"
blockage_new$longitude <-str_pad(blockage_new$longitude, 6, side = "right", pad = "0")
blockage_new$latitude <-str_pad(blockage_new$latitude, 6, side = "right", pad = "0")
blockage_new$longitude <- as.numeric(blockage_new$longitude)
blockage_new$latitude <- as.numeric(blockage_new$latitude)
blockage_new$No..of.Incidents <- 1
blockage2 <- blockage_new %>% filter((IRF.Transferred.Asset == "Y") & (IRF.Cause.Asset.Type %in% c("Combined sewer", "Foul sewer", "Surface water pipe", "Former S24 sewer")))
blockage2 <- as.data.table(blockage2[,c("IRF.Blockage.Cause","IRF.Incident.date..post.PDaS.",
                                        "IRF.Transferred.Asset","IRF.Cause.Asset.Type","longitude", "latitude", "No..of.Incidents")])

