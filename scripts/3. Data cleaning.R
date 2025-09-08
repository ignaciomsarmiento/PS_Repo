
#####
#2. Data Cleansing

#Keep only individuals above 18

master_data <- master_data %>%
  filter(age >= 18)

# Missing values that are not missing (Keep only ocuppied individuals)

db_ocupados = master_data %>% filter(ocu == 1)

#Only keep variables that will be used

final_db = db_ocupados %>% select(-12, -13, -15, -18, -(20:62), -64, -66, -(68:100))

#Missing Values count

colSums(is.na(db_ocupados))

final_db = final_db %>% filter(!is.na(final_db$maxEducLevel)) #There was an individual w/ missing value, we decided to drop it

#Recode sex var to make Female = 1

final_db$female <- ifelse(final_db$sex == 1, 0, 1)

#Household head dummy (= 1 if hh)

final_db$household_head <- ifelse(final_db$p6050 == 1, 1, 0)

#Education level dummy

final_db$maxEducLevel<- factor(final_db$maxEducLevel)
dummy_maxEducLevel <- as.data.frame(model.matrix(~ maxEducLevel - 1, data = final_db))
final_db <- cbind(final_db, dummy_maxEducLevel)

#Estrato dummy

final_db$estrato1<- factor(final_db$estrato1)
dummy_estrato1 <- as.data.frame(model.matrix(~ estrato1 - 1, data = final_db)) 
final_db <- cbind(final_db, dummy_estrato1)

#Firm Size dummy

final_db$sizeFirm<- factor(final_db$sizeFirm)
dummy_sizeFirm <- as.data.frame(model.matrix(~ sizeFirm - 1, data = final_db)) 
final_db <- cbind(final_db, dummy_sizeFirm)

#Relab dummy

final_db$relab<- factor(final_db$relab)
dummy_relab <- as.data.frame(model.matrix(~ relab - 1, data = final_db)) 
final_db <- cbind(final_db, dummy_relab)

#Age squared

final_db$age2 <- final_db$age^2







