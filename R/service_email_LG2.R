service_email_LG2 <- function(today, yesterday
                              , systems
                              , wd_export
                              , servicemail, serviceimap
                              , pw
                              , verbose = T
                              , ssl = T
                              , folder = "INBOX"
                              , delete_files = F
                              , delete_emails = F
                              , unzip_server = T
                              , unzip_type = NA){
  
  lg2_service_email <- list()  
  # convert date to outlook-date ####
  lg2_service_email$today_outlook <- format(today, "%d-%b-%Y")
  lg2_service_email$today_outlook <- gsub(substr(lg2_service_email$today_outlook, 4, 6)
                                          , month.abb[as.numeric(format(today,"%m"))]
                                          , lg2_service_email$today_outlook)
  
  lg2_service_email$yesterday_outlook <- format(yesterday, "%d-%b-%Y")
  lg2_service_email$yesterday_outlook <- gsub(substr(lg2_service_email$yesterday_outlook, 4, 6)
                                              , month.abb[as.numeric(format(yesterday,"%m"))]
                                              , lg2_service_email$yesterday_outlook)
  
  # Configure Email ####
  con <- configure_imap(url=serviceimap,
                        username=servicemail,
                        password=pw,
                        use_ssl = ssl,
                        verbose = verbose,
                        buffersize = 512000,
                        timeout_ms = 10000)
  
  con$reset_timeout_ms(x = 30000) # Long timeout to ensure the system has enough time to process the emails
  con$reset_verbose(x = FALSE) # The responding text is just too long, therefore suppress it
  
  # con$list_mail_folders() # List folders
  
  # Loop to run script for all LG1 and LG2 ####
  # systems[ , 1:5]
  for(i in 1:nrow(systems)){
    
    con$select_folder(name = folder) # Select Inbox
    
    # Create and navigate to folder customer/location/line
    dir.create(wd_export, showWarnings = F)
    setwd(wd_export)
    dir.create(systems$Kunde[i], showWarnings = F)
    setwd(paste0("./",systems$Kunde[i]))
    dir.create(systems$Short[i], showWarnings = F)
    setwd(paste0("./",systems$Short[i]))
    dir.create(systems$Anlage[i], showWarnings = F)
    setwd(paste0("./",systems$Anlage[i]))
    
    lg2_service_email$wdc <- getwd() # Save current path
    
    # Search for log_zip emails ####
    lg2_service_email$search_log <- paste0(yesterday, "_", systems$Kunde[i], "_", systems$Short[i], "_", systems$Anlage[i], "_", systems$LG[i], "_log_zip")
    lg2_service_email$find_log <- con$search(request = AND(string(expr = lg2_service_email$search_log, where = "SUBJECT"),
                                                           since(date_char = lg2_service_email$today_outlook)), use_uid = T)
    
    message("Found "
            , length(lg2_service_email$find_log)
            , " attachments with .log and .spc files in INBOX"
            , " from ", paste0(systems$Kunde[i], "_", systems$Standort[i], "_", systems$Anlage[i], "_", systems$LG[i]))

    # Search for csv_zip emails ####
    lg2_service_email$search_csv <- paste0(yesterday, "_", systems$Kunde[i], "_", systems$Short[i], "_", systems$Anlage[i], "_", systems$LG[i], "_csv_zip")
    lg2_service_email$find_csv <- con$search(request = AND(string(expr = lg2_service_email$search_csv, where = "SUBJECT"),
                                                           since(date_char = lg2_service_email$today_outlook)), use_uid = T)
    message("Found "
            , ifelse(is.na(lg2_service_email$find_csv), 0, length(lg2_service_email$find_csv))
            , " attachments with .csv files in INBOX"
            , " from ", paste0(systems$Kunde[i], "_", systems$Standort[i], "_", systems$Anlage[i], "_", systems$LG[i]))

    # Download log ####
    setwd(lg2_service_email$wdc)
    
    if(any(!is.na(lg2_service_email$find_log))){
      lg2_service_email$find_log %>%
        con$fetch_attachments(use_uid = T)
      # message("\n.log and .spc files downloaded")
    }
    # Sys.sleep(60) # The system can be kind of busy, this is just to feel more secure
    
    # Download csv ####
    if(any(!is.na(lg2_service_email$find_csv))){
      lg2_service_email$find_csv %>%
        con$fetch_attachments(use_uid = T)
      # message("\n.csv ", ifelse(length(lg2_service_email$find_csv) > 1, "files", "file"), " downloaded")
    }
    
    # Unzip ####
    setwd(lg2_service_email$wdc)
    dir.create(servicemail, showWarnings = F)
    setwd(paste0("./", servicemail))
    dir.create(folder, showWarnings = F)
    setwd(paste0("./", folder))
    
    dir.create("spclog", showWarnings = F) # Unzip folder
    if(any(!is.na(lg2_service_email$find_log))){
      for(j in lg2_service_email$find_log){
        if(!paste0("UID",j) %in% dir()) next
        unzip(paste0("./UID", j, "/", dir(paste0("./UID", j))), exdir = "./spclog")}
      # message(".log and .spc files unzipped into folder spclog")
    }
    
    dir.create("csv", showWarnings = F) # Unzip folder
    if(any(!is.na(lg2_service_email$find_csv))){ 
      for(j in lg2_service_email$find_csv) unzip(paste0("./UID", j, "/", dir(paste0("./UID", j))), exdir = "./csv")
      # message(".csv files unzipped into folder csv")
    }
    
    # Delete Files from folder ####
    unlink(dir()[which(dir() != "csv" & dir() != "spclog")], force = T, recursive = T)
    # message("delete all files except folders csv and spclog")
    
    # Move csv ####
    lg2_service_email$export_path <- .service_backup_path(customer = systems$Kunde[i]
                                                          , location = systems$Standort[i]
                                                          , line = systems$Anlage[i])
    
    setwd(lg2_service_email$wdc)
    setwd(paste0("./", servicemail))
    setwd(paste0("./", folder))
    setwd("csv")
    
    if(length(dir()[grep(yesterday, dir())]) > 0){
      file.copy(dir()[grep(yesterday, dir())]
                , paste0(lg2_service_email$export_path, "CSV/", dir()[grep(yesterday, dir())]))
      # message( toString(dir()[grep(yesterday, dir())]), " copied to ", paste0(lg2_service_email$export_path, "CSV/"))
    }
    
    setwd(lg2_service_email$wdc)
    setwd(paste0("./", servicemail))
    setwd(paste0("./", folder))
    setwd("spclog")
    
    if(length(dir()[grep(yesterday, dir())]) > 0){
      zip(zipfile = paste0(yesterday, ".zip")
          , files = dir()[grep(yesterday, dir())])
      # message(".log and .spc files zipped to ", paste0(yesterday, ".zip"))
      file.copy(paste0(yesterday, ".zip")
                , paste0(lg2_service_email$export_path, "ZIP/", yesterday, ".zip")
      )
      # message(paste0(yesterday, ".zip"), " copied to ", paste0(lg2_service_email$export_path, "ZIP/"))
    }
    
    # Delete Files####
    setwd(lg2_service_email$wdc)
    setwd(paste0("./", servicemail))
    setwd(paste0("./", folder))
    
    if(delete_files == T){
      unlink(dir(), recursive = T, force = T)
      if(length(dir())>0) file.remove(dir(), all.files = T, recursive = T)
    }
    
    # Delete Emails  on Wednesday ####
    if(delete_emails == T){
      lg2_service_email$search_log_week7 <- paste0(yesterday - 7, "_", systems$Kunde[i], "_", systems$Short[i], "_", systems$Anlage[i], "_", systems$LG[i], "_log_zip")
      lg2_service_email$search_log_week6 <- paste0(yesterday - 6, "_", systems$Kunde[i], "_", systems$Short[i], "_", systems$Anlage[i], "_", systems$LG[i], "_log_zip")
      lg2_service_email$search_log_week5 <- paste0(yesterday - 5, "_", systems$Kunde[i], "_", systems$Short[i], "_", systems$Anlage[i], "_", systems$LG[i], "_log_zip")
      lg2_service_email$search_log_week4 <- paste0(yesterday - 4, "_", systems$Kunde[i], "_", systems$Short[i], "_", systems$Anlage[i], "_", systems$LG[i], "_log_zip")
      lg2_service_email$search_log_week3 <- paste0(yesterday - 3, "_", systems$Kunde[i], "_", systems$Short[i], "_", systems$Anlage[i], "_", systems$LG[i], "_log_zip")
      lg2_service_email$search_log_week2 <- paste0(yesterday - 2, "_", systems$Kunde[i], "_", systems$Short[i], "_", systems$Anlage[i], "_", systems$LG[i], "_log_zip")
      lg2_service_email$search_log_week1 <- paste0(yesterday - 1, "_", systems$Kunde[i], "_", systems$Short[i], "_", systems$Anlage[i], "_", systems$LG[i], "_log_zip")
      lg2_service_email$search_log_week0 <- paste0(yesterday - 0, "_", systems$Kunde[i], "_", systems$Short[i], "_", systems$Anlage[i], "_", systems$LG[i], "_log_zip")
      
      lg2_service_email$find_log_week7 <- con$search(request = string(expr = lg2_service_email$search_log_week7, where = "SUBJECT"), use_uid = T)
      lg2_service_email$find_log_week6 <- con$search(request = string(expr = lg2_service_email$search_log_week6, where = "SUBJECT"), use_uid = T)
      lg2_service_email$find_log_week5 <- con$search(request = string(expr = lg2_service_email$search_log_week5, where = "SUBJECT"), use_uid = T)
      lg2_service_email$find_log_week4 <- con$search(request = string(expr = lg2_service_email$search_log_week4, where = "SUBJECT"), use_uid = T)
      lg2_service_email$find_log_week3 <- con$search(request = string(expr = lg2_service_email$search_log_week3, where = "SUBJECT"), use_uid = T)
      lg2_service_email$find_log_week2 <- con$search(request = string(expr = lg2_service_email$search_log_week2, where = "SUBJECT"), use_uid = T)
      lg2_service_email$find_log_week1 <- con$search(request = string(expr = lg2_service_email$search_log_week1, where = "SUBJECT"), use_uid = T)
      lg2_service_email$find_log_week0 <- con$search(request = string(expr = lg2_service_email$search_log_week0, where = "SUBJECT"), use_uid = T)
      
      if(any(!is.na(lg2_service_email$find_log_week7)))  con$delete_msg(msg_id = lg2_service_email$find_log_week7, use_uid = T, mute = T, retries = 2)
      if(any(!is.na(lg2_service_email$find_log_week6)))  con$delete_msg(msg_id = lg2_service_email$find_log_week6, use_uid = T, mute = T, retries = 2)
      if(any(!is.na(lg2_service_email$find_log_week5)))  con$delete_msg(msg_id = lg2_service_email$find_log_week5, use_uid = T, mute = T, retries = 2)
      if(any(!is.na(lg2_service_email$find_log_week4)))  con$delete_msg(msg_id = lg2_service_email$find_log_week4, use_uid = T, mute = T, retries = 2)
      if(any(!is.na(lg2_service_email$find_log_week3)))  con$delete_msg(msg_id = lg2_service_email$find_log_week3, use_uid = T, mute = T, retries = 2)
      if(any(!is.na(lg2_service_email$find_log_week2)))  con$delete_msg(msg_id = lg2_service_email$find_log_week2, use_uid = T, mute = T, retries = 2)
      if(any(!is.na(lg2_service_email$find_log_week1)))  con$delete_msg(msg_id = lg2_service_email$find_log_week1, use_uid = T, mute = T, retries = 2)
      if(any(!is.na(lg2_service_email$find_log_week0)))  con$delete_msg(msg_id = lg2_service_email$find_log_week0, use_uid = T, mute = T, retries = 2)
      
      if(any(!is.na(lg2_service_email$find_log_week7)))  con$expunge(msg_uid = lg2_service_email$find_log_week7, mute = T, retries = 2)
      if(any(!is.na(lg2_service_email$find_log_week6)))  con$expunge(msg_uid = lg2_service_email$find_log_week6, mute = T, retries = 2)
      if(any(!is.na(lg2_service_email$find_log_week5)))  con$expunge(msg_uid = lg2_service_email$find_log_week5, mute = T, retries = 2)
      if(any(!is.na(lg2_service_email$find_log_week4)))  con$expunge(msg_uid = lg2_service_email$find_log_week4, mute = T, retries = 2)
      if(any(!is.na(lg2_service_email$find_log_week3)))  con$expunge(msg_uid = lg2_service_email$find_log_week3, mute = T, retries = 2)
      if(any(!is.na(lg2_service_email$find_log_week2)))  con$expunge(msg_uid = lg2_service_email$find_log_week2, mute = T, retries = 2)
      if(any(!is.na(lg2_service_email$find_log_week1)))  con$expunge(msg_uid = lg2_service_email$find_log_week1, mute = T, retries = 2)
      if(any(!is.na(lg2_service_email$find_log_week0)))  con$expunge(msg_uid = lg2_service_email$find_log_week0, mute = T, retries = 2)
      
      lg2_service_email$search_csv_week7 <- paste0(yesterday - 7, "_", systems$Kunde[i], "_", systems$Short[i], "_", systems$Anlage[i], "_", systems$LG[i], "_csv_zip")
      lg2_service_email$search_csv_week6 <- paste0(yesterday - 6, "_", systems$Kunde[i], "_", systems$Short[i], "_", systems$Anlage[i], "_", systems$LG[i], "_csv_zip")
      lg2_service_email$search_csv_week5 <- paste0(yesterday - 5, "_", systems$Kunde[i], "_", systems$Short[i], "_", systems$Anlage[i], "_", systems$LG[i], "_csv_zip")
      lg2_service_email$search_csv_week4 <- paste0(yesterday - 4, "_", systems$Kunde[i], "_", systems$Short[i], "_", systems$Anlage[i], "_", systems$LG[i], "_csv_zip")
      lg2_service_email$search_csv_week3 <- paste0(yesterday - 3, "_", systems$Kunde[i], "_", systems$Short[i], "_", systems$Anlage[i], "_", systems$LG[i], "_csv_zip")
      lg2_service_email$search_csv_week2 <- paste0(yesterday - 2, "_", systems$Kunde[i], "_", systems$Short[i], "_", systems$Anlage[i], "_", systems$LG[i], "_csv_zip")
      lg2_service_email$search_csv_week1 <- paste0(yesterday - 1, "_", systems$Kunde[i], "_", systems$Short[i], "_", systems$Anlage[i], "_", systems$LG[i], "_csv_zip")
      lg2_service_email$search_csv_week0 <- paste0(yesterday - 0, "_", systems$Kunde[i], "_", systems$Short[i], "_", systems$Anlage[i], "_", systems$LG[i], "_csv_zip")
      
      lg2_service_email$find_csv_week7 <- con$search(request = string(expr = lg2_service_email$search_csv_week7, where = "SUBJECT"))
      lg2_service_email$find_csv_week6 <- con$search(request = string(expr = lg2_service_email$search_csv_week6, where = "SUBJECT"))
      lg2_service_email$find_csv_week5 <- con$search(request = string(expr = lg2_service_email$search_csv_week5, where = "SUBJECT"))
      lg2_service_email$find_csv_week4 <- con$search(request = string(expr = lg2_service_email$search_csv_week4, where = "SUBJECT"))
      lg2_service_email$find_csv_week3 <- con$search(request = string(expr = lg2_service_email$search_csv_week3, where = "SUBJECT"))
      lg2_service_email$find_csv_week2 <- con$search(request = string(expr = lg2_service_email$search_csv_week2, where = "SUBJECT"))
      lg2_service_email$find_csv_week1 <- con$search(request = string(expr = lg2_service_email$search_csv_week1, where = "SUBJECT"))
      lg2_service_email$find_csv_week0 <- con$search(request = string(expr = lg2_service_email$search_csv_week0, where = "SUBJECT"))
      
      if(any(!is.na(lg2_service_email$find_csv_week7)))  con$delete_msg(msg_id = lg2_service_email$find_csv_week7, use_uid = T, mute = T, retries = 3)
      if(any(!is.na(lg2_service_email$find_csv_week6)))  con$delete_msg(msg_id = lg2_service_email$find_csv_week6, use_uid = T, mute = T, retries = 3)
      if(any(!is.na(lg2_service_email$find_csv_week5)))  con$delete_msg(msg_id = lg2_service_email$find_csv_week5, use_uid = T, mute = T, retries = 3)
      if(any(!is.na(lg2_service_email$find_csv_week4)))  con$delete_msg(msg_id = lg2_service_email$find_csv_week4, use_uid = T, mute = T, retries = 3)
      if(any(!is.na(lg2_service_email$find_csv_week3)))  con$delete_msg(msg_id = lg2_service_email$find_csv_week3, use_uid = T, mute = T, retries = 3)
      if(any(!is.na(lg2_service_email$find_csv_week2)))  con$delete_msg(msg_id = lg2_service_email$find_csv_week2, use_uid = T, mute = T, retries = 3)
      if(any(!is.na(lg2_service_email$find_csv_week1)))  con$delete_msg(msg_id = lg2_service_email$find_csv_week1, use_uid = T, mute = T, retries = 3)
      if(any(!is.na(lg2_service_email$find_csv_week0)))  con$delete_msg(msg_id = lg2_service_email$find_csv_week0, use_uid = T, mute = T, retries = 3)
      
      if(any(!is.na(lg2_service_email$find_csv_week7)))  con$expunge(msg_uid = lg2_service_email$find_csv_week7, mute = T, retries = 2)
      if(any(!is.na(lg2_service_email$find_csv_week6)))  con$expunge(msg_uid = lg2_service_email$find_csv_week6, mute = T, retries = 2)
      if(any(!is.na(lg2_service_email$find_csv_week5)))  con$expunge(msg_uid = lg2_service_email$find_csv_week5, mute = T, retries = 2)
      if(any(!is.na(lg2_service_email$find_csv_week4)))  con$expunge(msg_uid = lg2_service_email$find_csv_week4, mute = T, retries = 2)
      if(any(!is.na(lg2_service_email$find_csv_week3)))  con$expunge(msg_uid = lg2_service_email$find_csv_week3, mute = T, retries = 2)
      if(any(!is.na(lg2_service_email$find_csv_week2)))  con$expunge(msg_uid = lg2_service_email$find_csv_week2, mute = T, retries = 2)
      if(any(!is.na(lg2_service_email$find_csv_week1)))  con$expunge(msg_uid = lg2_service_email$find_csv_week1, mute = T, retries = 2)
      if(any(!is.na(lg2_service_email$find_csv_week0)))  con$expunge(msg_uid = lg2_service_email$find_csv_week0, mute = T, retries = 2)
      
      # message("Emails from last 7 days deleted")
    }
  }
}

