library(rvest)
library(plyr)
library(stringr)
library(xml2)

find_last_page = function(subcategory_url){
  pages = read_html(subcategory_url)
  pages_list=pages %>%
    html_nodes("body") %>% html_nodes("div") %>% 
    html_nodes("div") %>% html_nodes("section") %>% 
    html_nodes("div") %>% html_nodes("ul")
  
  
  if (length(pages_list)!=0){
    pages_list = pages_list[[1]]
    pages_list = pages_list %>%
      html_nodes("li")
    
    pages_list = pages_list[[length(pages_list)]]
    
    if (length(which(names(xml_attrs(xml_child(pages_list, 1)))=="action"))!=0){
      last_page_url = xml_attrs(xml_child(pages_list, 1))[["action"]]
      main_page = str_remove(subcategory_url, NOTATEK_SITE)
      
      last_page = str_remove(last_page_url, main_page)
      last_page = str_remove(last_page, "/")
    }else{
      last_page = 1
    }
  }else{
    last_page=0
  }
  
  return(as.integer(last_page))
  
}

NOTATEK_SITE = "https://notatek.pl"

MAIN_FOLDER = "" #INPUT_FOLDER_PATH

url = paste0(NOTATEK_SITE, "/pomoce")

columns = c("category_url", "category_path", "subcategory_url", "subcategory_path", 
            "note_url", "note_path", "note_name", "note_course", "note_place", 
            "note_preview_url", "note_preview_path", "note_file_url", "note_file_path")
df = data.frame(matrix(NA,1,length(columns)))
names(df) = columns

# category
category = read_html(url)
category_list=category %>%
  html_nodes("body") %>% html_nodes("div") %>% 
  html_nodes("div") %>% html_nodes("section") %>% 
  html_nodes("ul") %>% html_nodes("li") %>% html_nodes("a")

index = 1
for (i in 1:length(category_list)){
  category_url=(xml_attrs(category_list[[i]])[["href"]])
  
  if (str_detect(category_url, "/pomoce/") & str_count(category_url,"/")==2){

    category_path = str_remove(str_remove(category_url, NOTATEK_SITE), "/pomoce/")
    category_path = paste0(MAIN_FOLDER, category_path)
    dir.create(category_path, showWarnings = FALSE)
    
    category_url = paste0(NOTATEK_SITE,category_url)
    
    print(paste("Scraping:", category_url))
    
    # subcategory
    
    subcategory = read_html(category_url)
    subcategory_list=subcategory %>%
      html_nodes("body") %>% html_nodes("div") %>% 
      html_nodes("div") %>% html_nodes("section") %>% 
      html_nodes("section") %>% html_nodes("ul") %>% 
      html_nodes("li") %>% html_nodes("h2") %>% html_nodes("a")
    
    category_name=subcategory %>%
      html_nodes("body") %>% html_nodes("div") %>% 
      html_nodes("div") %>% html_nodes("h1") %>% html_text()
    
    category_name=str_replace(category_name, "  - przedmioty", "")
    
    for (j in 1:length(subcategory_list)){
      subcategory_url=(xml_attrs(subcategory_list[[j]])[["href"]])
      subcategory_name=(xml_attrs(subcategory_list[[j]])[["title"]])
      
      if (str_detect(subcategory_url, "/pomoce/") & str_count(subcategory_url,"/")==3){

        subcategory_path = str_remove(str_remove(subcategory_url, NOTATEK_SITE), "/pomoce/")
        subcategory_path = paste0(MAIN_FOLDER, subcategory_path)
        dir.create(subcategory_path, showWarnings = FALSE)
        
        subcategory_url = paste0(NOTATEK_SITE,subcategory_url)
        
        print(paste("Scraping:", subcategory_url))
        
        #notes
        
        last_page = find_last_page(subcategory_url)
        
        if (last_page>0){
        
          for (k in 1:last_page){
            
            subcategory_url_page = paste0(subcategory_url, "/", k)
            
            print(paste("Scraping:", subcategory_url_page))
            
            notes = read_html(subcategory_url_page)
            notes_list=notes %>%
              html_nodes("body") %>% html_nodes("div") %>% 
              html_nodes("div") %>% html_nodes("section") %>% 
              html_nodes("div") %>% html_nodes("article") %>% 
              html_nodes("div") %>% html_nodes("div") %>% 
              html_nodes("a") %>% html_nodes("img")
            
            if (length(notes_list)>0){
            
              for (l in 1:length(notes_list)){
                
                # print(paste("scraping file", index))
                
                note_preview_url = paste0(NOTATEK_SITE, xml_attrs(notes_list[[l]])[["src"]])
                note_url = paste0(NOTATEK_SITE, "/", xml_attrs(notes_list[[l]])[["alt"]])
                
                print(paste("Scraping:", note_url))
                
                note = read_html(note_url)
                note_detail=note %>%
                  html_nodes("body") %>% html_nodes("div") %>% 
                  html_nodes("div") %>% html_nodes("header") %>% 
                  html_nodes("div") %>% html_nodes("div") %>% 
                  html_nodes("ul") %>% html_nodes("li") %>% 
                  html_nodes("h3") %>% html_nodes("a")
                
                if (length(note_detail)>1){
                  note_course = xml_attrs(note_detail[[1]])[["title"]]
                }
                
                if (length(note_detail)>1){
                  note_place = xml_attrs(note_detail[[2]])[["title"]]
                }
                
                note_detail=note %>%
                  html_nodes("body") %>% html_nodes("div") %>% 
                  html_nodes("div") %>% html_nodes("header") %>% 
                  html_nodes("div") %>% html_nodes("h1")
                
                note_title = xml_attrs(note_detail[[1]])[["title"]]
    
                note_path = str_remove(str_remove(note_url, NOTATEK_SITE), "/pomoce/")
                note_path = paste0(subcategory_path, note_path)
                # dir.create(note_path, showWarnings = FALSE)
                
                note_preview_path = paste0(note_path,"\\preview.jpg")
                # download.file(note_preview_url, note_preview_path, method = "curl", quiet = TRUE)
                
                note_file_url = paste0(note_url, "/download/")
                note_file_path = paste0(note_path,"\\file.pdf")
                
                
                df[index,1] = category_url
                df[index,2] = category_name
                df[index,3] = subcategory_url
                df[index,4] = subcategory_name
                df[index,5] = note_url
                df[index,6] = note_path
                df[index,7] = note_title
                df[index,8] = note_course
                df[index,9] = note_place
                df[index,10] = note_preview_url
                df[index,11] = note_preview_path
                df[index,12] = note_file_url
                df[index,13] = note_file_path
                
                index = index + 1
                
              }
            }
          }
        }
      }
    }
  }
}
write.csv(df, paste0(MAIN_FOLDER, "data",format(Sys.time(), "%d%m%y%H%M"),".csv"),row.names = FALSE)
